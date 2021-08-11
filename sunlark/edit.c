#include <errno.h>
#include <fcntl.h>
#include <libgen.h>

#if INTERFACE
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#endif
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"

#include "sunlark.h"
#include "edit.h"

UT_string *build_file;

UT_string *buffer;

// just for testing:
/* struct parse_state_s *sealark_parse_file(char *fname); */

int main(int argc, char *argv[]) // , char **envp)
{
    /* for (char **env = envp; *env != 0; env++) { */
    /*     char *thisEnv = *env; */
    /*     printf("%s\n", thisEnv); */
    /* } */
    /* return 0; */

    /* CAVEAT: bazel behaves erratically when used as a launcher. The
       RUNFILES env. vars may or may not be defined, depending on
       whether run cmd was 'run' or 'test', and whether 'run' is used
       to launch a cc_binary or a cc_test target. */
    /* char *rfdir = getenv("RUNFILES_DIR"); */
    /* log_debug("RUNFILES_DIR: %s", rfdir); */
    /* char *rfmanifest = getenv("RUNFILES_MANIFEST_FILE"); */
    /* log_debug("RUNFILES_MANIFEST_FILE: %s", rfmanifest); */

    int opt;
    /* bazel_s7_cb is determined by data attrib of build rule; used
       to find bazel_s7dir */
    char *callback_script_file = "edit.scm"; // rename default_script_file
    char *callback = "ast_handler"; /* fn in callback_script_file  */
    char *user_script_dir = ".sunlark.d";
    char *load_script = NULL;
    char *build_file = NULL;
    /* utstring_new(build_file); */

    while ((opt = getopt(argc, argv, "f:s:u:hv")) != -1) {
        switch (opt) {
        case 'f':
            /* BUILD.bazel or BUILD file */
            /* log_info("build file: %s", optarg); */
            /* utstring_printf(build_file, "%s", optarg); */
            build_file = optarg;
            break;
        case 's':
            log_info("scheme script file: %s", optarg);
            load_script = optarg;
            break;
        case 'u':
            user_script_dir = optarg;
            break;
        case 'h':
            log_info("Help: ");
            exit(EXIT_SUCCESS);
        case 'v':
            log_info("verbose option (unimplemented) ");
        default:
            log_error("Usage: bazel run moonlark:edit -- [-f buildfile] [-l s7file]");
            exit(EXIT_FAILURE);
        }
    }
    /* if (utstring_len(build_file) == 0) { */
    if (build_file == NULL) {
        log_error("-f <buildfile> must be provided.");
        exit(EXIT_FAILURE);
    }

    s7_scheme *s7 = sunlark_init();

    char *cwd = getcwd(NULL, 0);
    log_debug("cwd: %s", cwd);

    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    if (wd) {
        log_debug("launched by `bazel run`: %s", wd);
        /* launched by bazel run cmd */

        char *bazel_script_dir = get_bazel_script_dir(callback_script_file);
        sunlark_augment_load_path(s7, bazel_script_dir);

        /* user script dir is relative to launch dir; set it after chdir */
        chdir(wd);

        if( access( user_script_dir, F_OK ) != 0 ) {
            log_warn("WARNING: user_luadir does not exist: %s", user_script_dir);
        }
        sunlark_augment_load_path(s7, user_script_dir);

        s7_pointer lp = s7_load_path(s7);
        log_debug("load path: %s", s7_object_to_c_string(s7, lp));

        /* sunlark_config_moonlark_table(L); */

        /* sunlark_load_script_file(L, load_script); */
        s7_pointer lf;
        if (load_script) {
            log_debug("loading user script: %s", load_script);
            lf =  s7_load(s7, load_script);
        } else {
            log_debug("loading default script: %s", callback_script_file);
            lf =  s7_load(s7, callback_script_file);
        }
        /* log_debug("load result: %s", s7_object_to_c_string(s7, lf)); */
        fflush(stdout);

        /* s7_pointer s7_call(s7_scheme *sc, s7_pointer func, s7_pointer args); */


        /* s7_pointer s7_make_list(s7_scheme *sc, s7_int length, s7_pointer initial_value); */
        /* s7_pointer args = s7_make_list(s7, */
        /*                                1, /\* length *\/ */
        /*                                s7_make_integer(s7, 1)); */
    } else {
        log_error("BUILD_WORKING_DIRECTORY not found. This program is designed to be run from the root directory of a Bazel repo.");
    }

    /* log_debug("ml CWD: %s", getcwd(NULL, 0)); */
    /* int r = access(build_file, F_OK); */
    /* log_debug("access %s ? %d", build_file, r); */

    /* now parse the file using libstarlark */
    /* log_debug("lark_parse_build_file: %s", build_file); */

    /* struct parse_state_s *parse_state = sealark_parse_file(build_file); */
    /* log_debug("parsed file %s", parse_state->lexer->fname); */

    /* /\* log_debug("converting ast"); *\/ */
    /* s7_pointer ast = sunlark_ast2scm(s7, parse_state); */

    s7_pointer ast = sunlark_parse_build_file(s7,
                                      s7_list(s7, 1,
                                      s7_make_string(s7, build_file)));

    s7_pointer args =  s7_list(s7, 1, ast);

    log_debug("calling ast_handler");
    s7_pointer result = s7_call(s7,
                                s7_name_to_value(s7, "ast-handler"),
                                args);
    /* result of call is last form evaluated */
    /* log_debug("result: %s", s7_object_to_c_string(s7, result)); */
    s7_quit(s7);
}
