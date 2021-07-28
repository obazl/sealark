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

/* #include "libmoonlark.h" */

#include "lua_file.h"

UT_string *buffer;

int main(int argc, char *argv[])
{
    int opt;
    /* bazel_lua_cb is determined by data attrib of build rule; used
       to find bazel_luadir */
    char *bazel_lua_cb = "edit.lua";
    char *user_luadir = NULL;
    char *lua_file = NULL;
    char *callback = "moonlark_handler"; /* callback defined in lua_file */
    char *build_file = NULL;

    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    if (wd) {
        /* log_info("BUILD_WORKING_DIRECTORY: %s", wd); */
        chdir(wd);
    }

    while ((opt = getopt(argc, argv, "f:hv")) != -1) {
        switch (opt) {
        case 'f':
            /* BUILD.bazel or BUILD file */
            build_file = optarg;
            break;
        case 'h':
            log_info("Help: ");
            exit(EXIT_SUCCESS);
        default:
            log_error("Usage: %s [-f] [buildfile]", argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    if (strlen(build_file) == 0) {
        log_error("-f <buildfile> must be provided.");
        exit(EXIT_FAILURE);
    }

    lua_State *L;
    L = luaL_newstate();        /* set global lua state var */
    if (L == NULL) {
        log_error("luaL_newstate failure");
    }
    luaL_openlibs(L);

    lbazel_config(L, bazel_lua_cb, user_luadir, lua_file);

    /* /\* create new 'bazel' global table to hold ASTs *\/ */
    /* lua_newtable(L); */
    /* lua_pushstring(L, "build"); */
    /* lua_newtable(L); */
    /* lua_settable(L, -3); */
    /* lua_setglobal(L, "bazel"); */

    /* moonlark_augment_load_path(L); */

    /* lua_getglobal(L, "package"); */
    /* lua_getfield(L, -1, "path"); */
    /* const char *p = lua_tostring(L, -1); */
    /* log_debug("LUA PATH: %s", p); */

    /* load default and user handlers (Lua files) */
    /* starlark_lua_load_handlers(L, "test/lua/lua_file.lua"); */

    /* now parse the file using libstarlark */
    struct parse_state_s *parse_state = sealark_parse_file(build_file);
    /* log_debug("parsed file %s", parse_state->lexer->fname); */

    /* convert build file to Lua AST table */
    moonlark_ast2lua(L, parse_state);
    /* L now contains global bazel.build array of ASTs */
    /* log_debug("stack gettop %d", lua_gettop(L)); */

    /* call callback on (Lua) AST */
    moonlark_lua_call_user_handler(L, callback);

    /* starlark_ast2lua(L, parse_state); */
    /* /\* L now contains global bazel.build array of ASTs *\/ */

    /* starlark_lua_call_user_handler(L); */

    /* serialization routines expect a UT_string, not a char buffer */
    /* utstring_new(buffer); */
    /* starlark_node2string(parse_state->root, buffer); */
    /* printf("%s", utstring_body(buffer)); */

    utstring_free(buffer);
    free(parse_state->lexer->fname);
    node_dtor(parse_state->root);
    /* return r; */
}
