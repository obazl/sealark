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
#include "moonlark_setup.h"

/* UT_string *build_file; */

UT_string *buffer;


int moonlark_process_buildfile(char *build_file, char *lua_file, char *callback)
{
    log_debug("moonlark_process_buildfile %s, %s",
              build_file, lua_file);

    lua_State *L;
    L = luaL_newstate();        /* set global lua state var */
    if (L == NULL) {
        log_error("luaL_newstate failure");
    }

    luaL_openlibs(L);

    /* create global moonlark table, token tables, etc. */
    moonlark_config_moonlark_table(L);
    /* moonlark_augment_load_path(L, path); */

    /* load default and user handlers (Lua files) */
    moonlark_lua_load_handlers(L, lua_file);

    /* now parse the file using libstarlark */
    struct parse_state_s *parse_state = starlark_parse_file(build_file);
    log_debug("parsed file %s", parse_state->lexer->fname);

    /* convert build file to Lua AST table */
    moonlark_ast2lua(L, parse_state);
    /* L now contains global bazel.build array of ASTs */

    /* call handler on (Lua) AST */
    moonlark_lua_call_user_handler(L, callback);

    /* serialization routines expect a UT_string, not a char buffer */
    /* utstring_new(buffer); */
    /* starlark_node2string(parse_state->root, buffer); */
    /* printf("%s", utstring_body(buffer)); */

    /* utstring_free(buffer); */
    /* free(parse_state->lexer->fname); */
    /* node_dtor(parse_state->root); */
    /* return r; */
}
