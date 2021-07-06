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

// #include "starlark.h"           /* libstarlark public API */
/* #include "starlark_lua.h" */
#include "moonlark.h"

/* **************************************************************** */
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

    starlark_ast2lua(L, parsed);
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
  {"parse_file" , parse_file},
  {NULL         , NULL}
};

/*
** Open library - e.g. require 'starlark'
*/
EXPORT LUAMOD_API int luaopen_moonlark(lua_State *L) {
    /* log_set_level(LOG_TRACE); */
    /* log_set_quiet(false); */

    /* log_info("luaopen_starlark"); */

    /* create the library */

    luaL_newlib(L, moonlark);
    lua_pushstring(L, "0.1.0");
    lua_setfield(L, -2, "version");

    /* bazel-specific */
    starlark_lua_set_path(L);
    starlark_lua_init(L);    /* create global bazel table */

    return 1;
}
