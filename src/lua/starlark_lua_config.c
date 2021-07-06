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
#include "starlark_lua_config.h"

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
           <exec_root>/<runfiles_dir>/  -- contains default handler
       else:
           ???
    for runtime files: see https://github.com/bazelbuild/bazel/issues/10022
        https://github.com/laszlocsomor/bazel/commit/21989926c1a002709ec3eba9ee7a992506f2d50a
 */

EXPORT void starlark_lua_set_path(lua_State *L)
{
    log_debug("starlark_lua_config");
    UT_string *lua_path;
    utstring_new(lua_path);

    starlark_bazel_config();

    /* char *_proj_root = getenv("BUILD_WORKSPACE_DIRECTORY"); */
    utstring_printf(lua_path, "%s/?.lua", utstring_body(user_handlers_dir));
    /* log_debug("lua_path: %s", utstring_body(lua_path)); */

    size_t sz;
    /* NB: trailing ';;' means append previous val of LUA_PATH */
    utstring_printf(lua_path, ";%s/%s/?.lua;;",
                    utstring_body(runfiles_root),
                    "moonlark");
    log_debug("lua_path: %s", utstring_body(lua_path));

    lua_getglobal(L, "package");
    lua_pushstring(L, utstring_body(lua_path));
    lua_setfield(L, -2, "path");
    lua_pop(L, 1);

    utstring_free(lua_path);
}

/**
   sets: user_handlers_dir, runfiles_root
 */
int starlark_bazel_config(void) /* was: obazl_config.c:obazl_configure */
{
    log_debug("starlark_bazel_config");
    char *_proj_root = getenv("BUILD_WORKSPACE_DIRECTORY");
    if (_proj_root == NULL) {
        /* log_error("Env var 'BUILD_WORKSPACE_DIRECTORY' not found. This program must be run using `$ bazel run ...` in a Bazel project."); */

        _proj_root =getcwd(NULL, 0);
    }
    /* log_debug("BUILD_WORKSPACE_DIRECTORY: %s", bazel_proj_root); */
    utstring_new(proj_root);
    utstring_printf(proj_root, "%s", _proj_root);
    log_debug("proj_root: %s", utstring_body(proj_root));

    /* utstring_new(exec_root); */
    /* utstring_printf(exec_root, "%s", _exec_root); */
    /* log_debug("exec_root: %s", utstring_body(exec_root)); */

    /* when launched via `$ build run`, cwd == runfiles root */
    utstring_new(runfiles_root);
    utstring_printf(runfiles_root, "%s", getcwd(NULL, 0));
    log_debug("runfiles_root: %s", utstring_body(runfiles_root));

    /* default handlers dir */
    /* FIXME: this will only work if launcheb by $ bazel run */
    utstring_new(default_handlers_dir);
    utstring_printf(default_handlers_dir, "%s/%s",
                    utstring_body(runfiles_root),
                    "moonlark");
    log_debug("default_handlers_dir: %s",
              utstring_body(default_handlers_dir));

    /* user-defined handlers dir */
    utstring_new(user_handlers_dir);
    utstring_printf(user_handlers_dir, "%s/%s", utstring_body(proj_root), ".moonlark.d");
    log_debug("user_handlers_dir: %s", utstring_body(user_handlers_dir));

    /* always mkdir moonlark.d? */
    /* log_debug("mkdir %s", utstring_body(user_handlers_dir)); */
    int rc = mkdir(utstring_body(user_handlers_dir), S_IRWXU | S_IRGRP | S_IWGRP);
    if (rc != 0) {
        if (errno != EEXIST) {
            perror(utstring_body(user_handlers_dir));
            log_error("mkdir error");
        }
    }

    /* do we need an ini/config file? */

    /* .moonlarkrc config file */
    /* utstring_new(obazl_ini_path); */
    /* utstring_printf(obazl_ini_path, "%s/%s", utstring_body(proj_root), obazl_ini_file); */

    /* rc = access(utstring_body(obazl_ini_path), R_OK); */
    /* if (rc) { */
    /*     log_warn("Config file %s not found.", utstring_body(obazl_ini_path)); */
    /* } else { */
    /*     ini_error = false; */
    /*     utarray_new(obazl_config.src_dirs, &ut_str_icd); */
    /*     utarray_new(obazl_config.watch_dirs, &ut_str_icd); */
    /*     rc = ini_parse(utstring_body(obazl_ini_path), config_handler, &obazl_config); */
    /*     /\* log_debug("ini_parse rc: %d", rc); *\/ */
    /*     if (rc < 0) { */
    /*         //FIXME: deal with missing .obazl */
    /*         log_fatal("Can't load ini file: %s", utstring_body(obazl_ini_path)); */
    /*         return -1; */
    /*     } */
    /*     if (ini_error) { */
    /*         log_error("Error parsing ini file"); */
    /*         exit(EXIT_FAILURE); */
    /*     /\* } else { *\/ */
    /*     /\*     log_debug("Config loaded from %s", utstring_body(obazl_ini_path)); *\/ */
    /*     } */
    /* } */
}

EXPORT void starlark_lua_load_handlers(lua_State *L)
{
    log_debug("starlark_lua_load_handlers");

    /* utstring_new(user_lua_file); */
    /* utstring_printf(user_lua_file, "%s/%s", */
    /*                 utstring_body(default_lua_dir), */
    /*                 utstring_body(default_lua_file)); */

    /* utstring_new(handler_file); */
    /* utstring_printf(handler_file, */
    /*                 "%s/%s", */
    /*                 utstring_body(default_lua_dir), */
    /*                 utstring_body(default_lua_file)); */

    log_debug("loading lua file: %s", default_handler_file_name);
    if (luaL_dostring(L, "require'handler'")) {
        lerror(L, "luaL_dostring fail for: %s\n",
               lua_tostring(L, -1));
    }
    log_debug("loaded");
    /* if (luaL_dofile(L, handler)) { */
    /*     lerror(L, "luaL_dofile fail for: %s\n", */
    /*            lua_tostring(L, -1)); */
    /* } */

    /* if (luaL_loadfile(L, handler) || lua_pcall(L, 0, 0, 0)) */
    /*         lerror(L, "cannot run configuration file: %s\n", */
    /*                lua_tostring(L, -1)); */
    /* log_debug("loaded lua handler: %s", utstring_body(user_lua_file)); */

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

LOCAL void starlark_lua_create_tokens_enum(lua_State *L)
{
    log_debug("starlark_lua_create_tokens_enum");
    lua_pushstring(L, "TOK");
    lua_newtable(L);
    int i;
    for (i = 0; i < 256; i++) {
        if (token_name[i][0] != NULL) {
        /* log_debug("tok[%d]: %s", i, token_name[i][0] + 3); */
        lua_pushstring(L, token_name[i][0] + 3);
        lua_pushinteger(L, i);
        lua_settable(L, -3);
        }
    }
    lua_settable(L, -3);

    lua_pushstring(L, "iTOK");
    lua_newtable(L);
    for (i = 0; i < 256; i++) {
        if (token_name[i][0] != NULL) {
        /* log_debug("tok[%d]: %s", i, token_name[i][0] + 3); */
        lua_pushinteger(L, i);
        lua_pushstring(L, token_name[i][0] + 3);
        lua_settable(L, -3);
        }
    }
    lua_settable(L, -3);

    /* pTOK: printable tokens */
    lua_pushstring(L, "pTOK");
    lua_newtable(L);
    for (i = 0; printable_tokens[i] != 0; i++) {
        log_debug("%d: printable_token[%d]: %s",
                  i, printable_tokens[i], token_name[printable_tokens[i]][0]);
        lua_pushinteger(L, printable_tokens[i]);
        lua_pushstring(L, token_name[printable_tokens[i]][1]);
        lua_settable(L, -3);
    }
    lua_settable(L, -3);
}

EXPORT void starlark_lua_init(lua_State *L)
{
    /* log_debug("starlark_lua_init"); */
    lua_newtable(L);
    starlark_lua_create_tokens_enum(L);
    lua_pushstring(L, "build");
    lua_newtable(L);
    lua_settable(L, -3);
    lua_setglobal(L, "bazel");
}
