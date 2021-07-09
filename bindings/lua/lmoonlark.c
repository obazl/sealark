/*
** lua lib for Bazel (Starlark) Parser
*/

#define bazel_c
#define LUA_LIB

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <unistd.h>

#include "lua.h"

#include "lauxlib.h"
#include "lualib.h"

#include "log.h"
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"

#include "lmoonlark.h"

/* **************************************************************** */
static int config_bazel (lua_State *L) {
    /* log_debug("config.bazel"); */

    lbazel_config(L, "repl.lua", ".moonlark.d", NULL);

    /* log_debug("xxxxxxxxxxxxxxxx"); */
    /* int t = lua_getglobal(L, "package"); */
    /* if (t == LUA_TNIL) { */
    /*     log_error("ERROR: Lua table 'package' not found"); */
    /*     exit(EXIT_FAILURE); */
    /* } */
    /* lua_getfield(L, -1, "path"); */
    /* const char *curr_path = lua_tostring(L, -1); */
    /* log_debug("current load path: %s", curr_path); */
    /* lua_pop(L, 2); */


    return 1;
}

/**
   result: new table in global ns, representing BUILD.bazel file
     flds: fname, root
 */
static int parse_file (lua_State *L) {
    size_t l;
    const char *fname = luaL_checklstring(L, 1, &l);
    struct parse_state_s *parsed = starlark_parse_file((char*)fname);
    if (parsed == NULL) {
        return luaL_error(L, "%s: %s", fname, strerror(errno));
    /* } else { */
    /*     log_debug("parsed file: %s\n", fname); */
    }

    /* lua_newtable(L); */

    /* lua_pushstring(L, "fname"); */
    /* lua_pushstring(L, parsed->lexer->fname); */
    /* lua_settable(L, -3); */

    /* prereq: starlark_lua_init has run, creating global bazel.build */

    moonlark_ast2lua(L, parsed);
    /* log_debug("/starlark_ast2lua"); */
    /* lua_settable(L, -3); */

    return 1;
}

/* FIXME: map this to a field in moonlark table */
/*  versions: libstarlark, libmoonlark, liblbazel, and moonlark pkg */
/* so this should return a table */
static int version (lua_State *L) {
    char *v = "0.1.0";
    lua_newtable(L);
    lua_pushstring(L, v);
    lua_setfield(L, -2, "libstarlark");
    lua_pushstring(L, v);
    lua_setfield(L, -2, "libmoonlark");
    lua_pushstring(L, v);
    lua_setfield(L, -2, "liblbazel");
    lua_pushstring(L, v);
    lua_setfield(L, -2, "moonlark");
    return 1;
}

static const luaL_Reg moonlark[] = {
  {"config_bazel", config_bazel},
  {"parse_file"  , parse_file},
  {NULL         , NULL}
};

/*
** Open library - e.g. require 'starlark'
*/
LUAMOD_API int luaopen_moonlark(lua_State *L) {

    /* create the library */
    luaL_newlib(L, moonlark);
    lua_pushstring(L, "0.1.0");
    lua_setfield(L, -2, "version");

    /* luaL_newmetatable(L, "moonlark.meta"); */

    /* add starlark token enums to 'moonlark' table */
    moonlark_create_token_enums(L);

    return 1;
}
