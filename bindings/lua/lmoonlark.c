/*
** lua lib for Bazel (Starlark) Parser
*/

#define bazel_c
#define LUA_LIB

#include <errno.h>
#include <limits.h>
#include <stdlib.h>

#include "lua.h"

#include "lauxlib.h"
#include "lualib.h"

#include "log.h"
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"

#include "moonlark.h"           /* public api for libmoonlark */
#include "lmoonlark.h"

/* **************************************************************** */
static int config_bazel (lua_State *L) {
    log_debug("config.bazel");
    //TODO:  add bazel_luadir to load path?
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
    } else {
        log_debug("parsed file: %s\n", fname);
    }

    /* lua_newtable(L); */

    /* lua_pushstring(L, "fname"); */
    /* lua_pushstring(L, parsed->lexer->fname); */
    /* lua_settable(L, -3); */

    /* prereq: starlark_lua_init has run, creating global bazel.build */

    moonlark_ast2lua(L, parsed);
    log_debug("/starlark_ast2lua");
    /* lua_settable(L, -3); */

    return 1;
}

static int version (lua_State *L) {
    char *v = "0.1.0"; // bazel_version();
    lua_pushstring(L, v);
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

    /* add starlark token enums to 'moonlark' table */
    moonlark_create_token_enums(L);

    /* bazel-specific */
    /* create global bazel table with bazel.config, bazel.TOK, etc. */
    /* starlark_lua_init(L); */
    /* elaborates 'bazel' table created by init */
    /* moonlark_augment_load_path(L); */

    return 1;
}
