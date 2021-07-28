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

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "edit.h"

UT_string *build_file;

UT_string *buffer;

int main(int argc, char *argv[]) // , char **envp)
{
    /* CAVEAT: bazel behaves erratically when used as a launcher. The
       RUNFILES env. vars may or may not be defined, depending on
       whether run cmd was 'run' or 'test', and whether 'run' is used
       to launch a cc_binary or a cc_test target. */
    /* char *rfdir = getenv("RUNFILES_DIR"); */
    /* log_debug("RUNFILES_DIR: %s", rfdir); */
    /* char *rfmanifest = getenv("RUNFILES_MANIFEST_FILE"); */
    /* log_debug("RUNFILES_MANIFEST_FILE: %s", rfmanifest); */

    int opt;
    /* callback_script_file is determined by data attrib of build rule; used
       to find bazel_script_dir */
    char *callback_script_file = "edit.lua";
    char *callback = "ast_handler"; /* fn in callback_script_file  */
    char *user_script_dir = ".bazel.d/lua";
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
            log_info("lua script file: %s", optarg);
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
            log_error("Usage: bazel run moonlark:edit -- [-f buildfile] [-l luafile]");
            exit(EXIT_FAILURE);
        }
    }

    /* if (utstring_len(build_file) == 0) { */
    if (build_file == NULL) {
        log_error("-f <buildfile> must be provided.");
        exit(EXIT_FAILURE);
    }

    lua_State *L = luaL_newstate();
    luaL_openlibs(L);

    /* we do not need to preload lmoonlark. the lmoonlark api is not
       needed, since we can call the libstarlark API directly */
    /* luaL_getsubtable(L, LUA_REGISTRYINDEX, LUA_PRELOAD_TABLE); */
    /* lua_pushcfunction(L, luaopen_moonlark); */
    /* lua_setfield(L, -2, "moonlark"); */
    /* lua_pop(L, 1);  // remove PRELOAD table */

    /* but what about our default lua files, like serialization.lua?
       why not preload them in the 'moonlark' table? */

    /* lbazel_config(L, bazel_lua_cb, user_script_dir, load_script); */
    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    if (wd) {
        /* launched by bazel run cmd */

        char *bazel_script_dir = get_bazel_script_dir(callback_script_file);
        moonlark_augment_load_path(L, bazel_script_dir);

        /* user script dir is relative to launch dir; set it after chdir */
        chdir(wd);

        if( access( user_script_dir, F_OK ) != 0 ) {
            log_warn("WARNING: user_script_dir does not exist: %s", user_script_dir);
        }
        moonlark_augment_load_path(L, user_script_dir);

        moonlark_config_moonlark_table(L);
        if (load_script)
            moonlark_load_script_file(L, load_script);
        else
            moonlark_load_script_file(L, callback_script_file);
    } else {
        log_error("BUILD_WORKING_DIRECTORY not found. This program is designed to be run from the root directory of a Bazel repo.");
    }

    /* now parse the file using libstarlark */
    struct parse_state_s *parse_state = sealark_parse_file(build_file);
    /* log_debug("parsed file %s", parse_state->lexer->fname); */

    /* convert build file to Lua AST table */
    moonlark_ast2lua(L, parse_state);
    /* L now contains global bazel.build array of ASTs */
    /* log_debug("stack gettop %d", lua_gettop(L)); */

    /* call callback on (Lua) AST */
    moonlark_call_user_handler(L, callback);

    sealark_parse_state_free(parse_state);
}
