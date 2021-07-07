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
#include "moonlark_setup.h"

/* UT_string *build_file; */

UT_string *buffer;


int moonlark_setup(char *build_file, char *lua_file)
{
    /* char *wd = getenv("BUILD_WORKING_DIRECTORY"); */
    /* if (wd) { */
    /*     /\* log_info("BUILD_WORKING_DIRECTORY: %s", wd); *\/ */
    /*     chdir(wd); */
    /* } */
    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    if (wd) {
        log_info("BUILD_WORKING_DIRECTORY: %s", wd);
        chdir(wd);
    } else {
        log_info("BUILD_WORKING_DIRECTORY not found");
        chdir("/Users/gar/bazel/obazl/moonlark");
    }

    char cwd[PATH_MAX];
    if (getcwd(cwd, sizeof(cwd)) != NULL) {
        log_info("Current working dir: %s\n", cwd);
    } else {
        perror("getcwd() error");
        return EXIT_FAILURE;
    }

    lua_State *L;
    L = luaL_newstate();        /* set global lua state var */
    if (L == NULL) {
        log_error("luaL_newstate failure");
    }

    luaL_openlibs(L);

    starlark_lua_set_path(L);
    /* load default and user handlers (Lua files) */
    starlark_lua_load_handlers(L, lua_file);

    /* create global bazel table, token tables, etc. */
    starlark_lua_init(L);

    /* now parse the file using libstarlark */
    struct parse_state_s *parse_state = starlark_parse_file(build_file);
    log_debug("parsed file %s", parse_state->lexer->fname);

    /* convert build file to Lua AST table */
    starlark_ast2lua(L, parse_state);
    /* L now contains global bazel.build array of ASTs */

    /* call handler on (Lua) AST */
    starlark_lua_call_user_handler(L);

    /* serialization routines expect a UT_string, not a char buffer */
    /* utstring_new(buffer); */
    /* starlark_node2string(parse_state->root, buffer); */
    /* printf("%s", utstring_body(buffer)); */

    utstring_free(buffer);
    /* free(parse_state->lexer->fname); */
    /* node_dtor(parse_state->root); */
    /* return r; */
}
