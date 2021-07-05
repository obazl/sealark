/*
  src/lua/lua_debug.c

  serialize lnodes (Lua tables representing AST nodes) to strings
*/

#include "log.h"

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "lua_debug.h"

char *starlark_lnode2string(lua_State *L)
{
    log_debug("starlark_lnode2string");
}
