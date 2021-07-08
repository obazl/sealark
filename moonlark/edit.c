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

#include "libmoonlark.h"
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

    int opt;
    char *user_luadir;
    char *lua_file;
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

    /* Interrogate env to get lua load paths and cwd */
     char *bazel_luadir = bazel_get_luadir("edit.lua");
     log_debug("bazel_luadir: %s", bazel_luadir);

     if (user_luadir == NULL) {
         /* user_luadir = config_get_luadir(); */
         /* log_debug("bazel_luadir: %s", bazel_luadir); */
     } else {
         log_info("user luadir: %s", user_luadir);
     }
     //FIXME: verify user_luadir exists *after* chdir to launchdir

    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    if (wd) {
        /* log_info("BUILD_WORKING_DIRECTORY: %s", wd); */
        chdir(wd);
    } else {
        log_error("BUILD_WORKING_DIRECTORY not found. This program must be run from the root directory of a Bazel repo.");
    }

     if( access( user_luadir, F_OK ) != 0 ) {
         log_error("ERROR: user_luadir does not exist %s", user_luadir);
     }

     /* startup lua (luaL_newstate()) */
     lua_State *L = luaL_newstate();
     luaL_openlibs(L);

     /* set lua load paths */
     moonlark_augment_load_path(L, bazel_luadir);
     moonlark_augment_load_path(L, user_luadir);

     moonlark_config_bazel_table(L);

     starlark_lua_load_handlers(L, lua_file);

    /* now parse the file using libstarlark */
    struct parse_state_s *parse_state = starlark_parse_file(build_file);
    log_debug("parsed file %s", parse_state->lexer->fname);

    /* convert build file to Lua AST table */
    starlark_ast2lua(L, parse_state);
    /* L now contains global bazel.build array of ASTs */
    /* log_debug("stack gettop %d", lua_gettop(L)); */

    /* call handler on (Lua) AST */
    starlark_lua_call_user_handler(L);

    /* moonlark_process_buildfile(build_file, lua_file); */
}
