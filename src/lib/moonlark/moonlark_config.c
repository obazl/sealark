/*
  src/lua/lua_config.c

  set search paths etc.
*/

#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>

#include "log.h"
#include "utstring.h"

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

/* #include "starlark.h" */
#include "moonlark_config.h"
/* #include "libmoonlark.h" */

UT_string *proj_root;
UT_string *runfiles_root;

/* location of default lua files: <runfiles_root>/moonlark/ */
UT_string *default_handlers_dir;

/* location of user-defined lua files: $cwd/.moonlark.d/ */
UT_string *user_handlers_dir;
/* UT_string *runtime_data_dir; */

char *default_handler_file_name = "handler.lua";
UT_string *user_lua_file;


void lerror (lua_State *L, const char *fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    lua_close(L);
    exit(EXIT_FAILURE);
}

/**
   configure lua search paths for bazel env

   assumption: launched via `$ bazel run ...`
   uses: obazl_d, runfiles_root

   lua search path always contains:
       <proj_root>/.moonlark.d/  -- contains user-defined handler
       if run by `bazel run`:
           <exec_root>/<runfiles_dir>/moonlark/lua -- default handler
           (linked from @moonlark//moonlark/lua)
       else:
           ???
    for runtime files: see https://github.com/bazelbuild/bazel/issues/10022
        https://github.com/laszlocsomor/bazel/commit/21989926c1a002709ec3eba9ee7a992506f2d50a
 */

EXPORT void moonlark_augment_load_path(lua_State *L, char *path)
{
    log_debug("moonlark_augment_load_path %s", path);
    UT_string *load_path;
    utstring_new(load_path);

    /* starlark_bazel_config(L); */

    int r = lua_getglobal(L, "package");
    lua_getfield(L, -1, "path");
    const char *curr_path = lua_tostring(L, -1);
    log_debug("current load path: %s", curr_path);
    lua_pop(L, 1);

    utstring_printf(load_path, "%s/?.lua;%s", path, curr_path);
    log_debug("new load_path: %s", utstring_body(load_path));


    lua_pushstring(L, utstring_body(load_path));
    lua_setfield(L, -2, "path");
    lua_pop(L, 1);

    utstring_free(load_path);
}

/*
  called by //moonlark:edit, but not //moonlark:repl
 */
EXPORT void starlark_lua_load_handlers(lua_State *L, char *lua_file)
{
    log_debug("starlark_lua_load_handlers");

    /* log_debug("loading lua file: %s", default_handler_file_name); */
    /* if (luaL_dostring(L, "require'handler'")) { */
    /*     lerror(L, "luaL_dostring fail for: %s\n", */
    /*            lua_tostring(L, -1)); */
    /* } */
    /* log_debug("loaded"); */

    if (lua_file == NULL) {
        if (luaL_dostring(L, "require'edit'")) {
            lerror(L, "luaL_dostring fail for: %s\n",
                   lua_tostring(L, -1));
        }
        log_debug("loaded default lua handler");
    } else {
        if (luaL_loadfile(L, lua_file) || lua_pcall(L, 0, 0, 0))
            lerror(L, "cannot run configuration file: %s\n",
                   lua_tostring(L, -1));
    }
    /* utstring_clear(user_lua_file); */
    /* utstring_printf(user_lua_file, "%s/%s", utstring_body(obazl_d), utstring_body(default_lua_file)); */
    /* int rc = access(utstring_body(user_lua_file), R_OK); */
    /* if (!rc) { /\* found *\/ */
    /*     log_debug("loading user lua_file: %s", utstring_body(user_lua_file)); */
    /*     if (luaL_loadfile(L, utstring_body(user_lua_file)) || lua_pcall(L, 0, 0, 0)) */
    /*         lerror(L, "cannot run configuration file: %s\n", */
    /*                lua_tostring(L, -1)); */
    /*     log_debug("loaded lua file: %s", utstring_body(user_lua_file)); */
    /* } else { */
    /*     log_debug("no user lua file found: %s", utstring_body(user_lua_file)); */
    /*     utstring_clear(user_lua_file); */
    /* } */
}
