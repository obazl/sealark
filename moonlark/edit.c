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

int main(int argc, char *argv[])
{
    int opt;
    char *lua_file;
    char *build_file;
    /* utstring_new(build_file); */

    while ((opt = getopt(argc, argv, "f:l:hv")) != -1) {
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

    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    if (wd) {
        log_info("BUILD_WORKING_DIRECTORY: %s", wd);
        chdir(wd);
    }
    char *wsd = getenv("BUILD_WORKSPACE_DIRECTORY");
    log_info("BUILD_WORKSPACE_DIRECTORY: %s", wsd);

    moonlark_setup(build_file, lua_file);
    /* FIXME: extract lua code common to this, test/lua/sys, and
       bindings/lua */
    /* LUA setup */
    /* lua_State *L; */
    /* L = luaL_newstate();        /\* set global lua state var *\/ */
    /* if (L == NULL) { */
    /*     log_error("luaL_newstate failure"); */
    /* } */
    /* luaL_openlibs(L); */

    /* starlark_lua_set_path(L); */

    /* /\* load default and user handlers (Lua files) *\/ */
    /* starlark_lua_load_handlers(L, lua_file); */

    /* /\* create global bazel table, token tables, etc. *\/ */
    /* starlark_lua_init(L); */

    /* /\* now parse the file *\/ */
    /* struct parse_state_s *parse_state = starlark_parse_file(utstring_body(build_file)); */
    /* log_debug("parsed file %s", parse_state->lexer->fname); */

    /* /\* convert build file to Lua AST table *\/ */
    /* starlark_ast2lua(L, parse_state); */
    /* /\* L now contains global bazel.build array of ASTs *\/ */

    /* /\* call handler on (Lua) AST *\/ */
    /* starlark_lua_call_user_handler(L); */

    /* /\* serialization routines expect a UT_string, not a char buffer *\/ */
    /* /\* utstring_new(buffer); *\/ */
    /* /\* starlark_node2string(parse_state->root, buffer); *\/ */
    /* /\* printf("%s", utstring_body(buffer)); *\/ */

    /* utstring_free(buffer); */
    /* /\* free(parse_state->lexer->fname); *\/ */
    /* /\* node_dtor(parse_state->root); *\/ */
    /* /\* return r; *\/ */
}
