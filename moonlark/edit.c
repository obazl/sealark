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

#include "moonlark.h"
#include "edit.h"

UT_string *build_file;

UT_string *buffer;

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
    char *rfdir = getenv("RUNFILES_DIR");
    log_debug("RUNFILES_DIR: %s", rfdir);
    char *rfmanifest = getenv("RUNFILES_MANIFEST_FILE");
    log_debug("RUNFILES_MANIFEST_FILE: %s", rfmanifest);

    int opt;
    /* bazel_lua_cb is determined by data attrib of build rule; used
       to find bazel_luadir */
    char *bazel_lua_cb = "edit.lua";
    char *user_luadir = NULL;
    char *lua_file = NULL;
    char *callback = "moonlark_handler"; /* callback defined in lua_file */
    char *build_file;
    /* utstring_new(build_file); */

    while ((opt = getopt(argc, argv, "f:l:u:hv")) != -1) {
        switch (opt) {
        case 'f':
            /* BUILD.bazel or BUILD file */
            log_info("build file: %s", optarg);
            /* utstring_printf(build_file, "%s", optarg); */
            build_file = optarg;
            break;
        case 'l':
            log_info("lua file: %s", optarg);
            lua_file = optarg;
            break;
        case 'u':
            user_luadir = optarg;
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
    if (strlen(build_file) == 0) {
        log_error("-f <buildfile> must be provided.");
        exit(EXIT_FAILURE);
    }

    lua_State *L = moonlark_config_for_bazel(bazel_lua_cb, user_luadir, lua_file);

    /* now parse the file using libstarlark */
    struct parse_state_s *parse_state = starlark_parse_file(build_file);
    log_debug("parsed file %s", parse_state->lexer->fname);

    /* convert build file to Lua AST table */
    moonlark_ast2lua(L, parse_state);
    /* L now contains global bazel.build array of ASTs */
    /* log_debug("stack gettop %d", lua_gettop(L)); */

    /* call callback on (Lua) AST */
    moonlark_lua_call_user_handler(L, callback);
}
