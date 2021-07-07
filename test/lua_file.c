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
/* #include "starlark.h" */
#include "libmoonlark.h"

#include "lua_file.h"

UT_string *build_file;

UT_string *buffer;

int main(int argc, char *argv[])
{
    int opt;
    utstring_new(build_file);

    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    if (wd) {
        /* log_info("BUILD_WORKING_DIRECTORY: %s", wd); */
        chdir(wd);
    }

    while ((opt = getopt(argc, argv, "f:hv")) != -1) {
        switch (opt) {
        case 'f':
            /* BUILD.bazel or BUILD file */
            utstring_printf(build_file, "%s", optarg);
            break;
        case 'h':
            log_info("Help: ");
            exit(EXIT_SUCCESS);
        default:
            log_error("Usage: %s [-f] [buildfile]", argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    if (utstring_len(build_file) == 0) {
        log_error("-f <buildfile> must be provided.");
        exit(EXIT_FAILURE);
    }

    struct parse_state_s *parse_state = starlark_parse_file(utstring_body(build_file));
    log_debug("parsed file %s", parse_state->lexer->fname);
    dump_node(parse_state->root);

    // FIXME: use moonlark api to config lua?
    lua_State *L;
    L = luaL_newstate();        /* set global lua state var */
    if (L == NULL) {
        log_error("luaL_newstate failure");
    }
    luaL_openlibs(L);

    /* create new 'bazel' global table to hold ASTs */
    lua_newtable(L);
    lua_pushstring(L, "build");
    lua_newtable(L);
    lua_settable(L, -3);
    lua_setglobal(L, "bazel");

    starlark_lua_set_path(L);

    /* lua_getglobal(L, "package"); */
    /* lua_getfield(L, -1, "path"); */
    /* const char *p = lua_tostring(L, -1); */
    /* log_debug("LUA PATH: %s", p); */

    /* load default and user handlers (Lua files) */
    starlark_lua_load_handlers(L, "test/lua/lua_file.lua");

    /* parse and convert build file to Lua AST table */
    /* call handler on (Lua) AST */

    starlark_ast2lua(L, parse_state);
    /* L now contains global bazel.build array of ASTs */

    starlark_lua_call_user_handler(L);

    /* serialization routines expect a UT_string, not a char buffer */
    /* utstring_new(buffer); */
    /* starlark_node2string(parse_state->root, buffer); */
    /* printf("%s", utstring_body(buffer)); */

    utstring_free(buffer);
    free(parse_state->lexer->fname);
    node_dtor(parse_state->root);
    /* return r; */
}
