#include <libgen.h>             /* dirname */
#include<stdio.h>               /* fopen, getline, perror */
#include<stdlib.h>               /* getenv */
#include<string.h>              /* strcmp, strrchr, strsep */
#include <unistd.h>             /* getcwd */

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "lbazel.h"

void lbazel_config(lua_State *L,char *bazel_lua_cb,char *_user_luadir,char *lua_file)
{
    log_debug("lbazel_config");

    /* Interrogate env to get lua load paths and cwd */
    char *bazel_luadir = lbazel_get_luadir(bazel_lua_cb);
    log_debug("bazel_luadir: %s", bazel_luadir);

    char *user_luadir = ".moonlark.d";
    if (_user_luadir == NULL) {
        log_warn("WARNING: no user luadir specified; using default: %s",
                 user_luadir);
    } else {
        user_luadir = _user_luadir;
    }
    //FIXME: verify user_luadir exists *after* chdir to launchdir

    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    if (wd) {
        /* log_info("BUILD_WORKING_DIRECTORY: %s", wd); */
        chdir(wd);
    } else {
        log_error("BUILD_WORKING_DIRECTORY not found. This program must be run from the root directory of a Bazel repo.");
    }

    if (user_luadir) {
        if( access( user_luadir, F_OK ) != 0 ) {
            log_warn("WARNING: user_luadir does not exist: %s", user_luadir);
        /* } else { */
        /*     log_info("user_luadir: %s", user_luadir); */
        }
    }

    /* set lua load paths */
    moonlark_augment_load_path(L, bazel_luadir);
    moonlark_augment_load_path(L, user_luadir);

    moonlark_config_moonlark_table(L);

    moonlark_lua_load_file(L, lua_file);

}

char *lbazel_get_luadir(char *luafile)
{
    log_debug("bazel_get_luadir %s", luafile);
    char *bazel_luadir = NULL;
    char *s = getcwd(NULL, 0);
    log_info("CURRENT WORKING DIRECTORY: %s", s);
    char *mdir = dirname(s);
    log_info("MANIFEST DIR: %s", mdir);
    UT_string *manifest;
    utstring_new(manifest);
    utstring_printf(manifest, "%s%s", mdir, "/MANIFEST");
    log_info("MANIFEST: %s", utstring_body(manifest));

    FILE * fp;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;

    fp = fopen(utstring_body(manifest), "r");
    if (fp == NULL) {
        log_error("fopen failure %s", utstring_body(manifest));
        exit(EXIT_FAILURE);
    }

    char *token;
    while ((read = getline(&line, &len, fp)) != -1) {
        printf("Retrieved line of length %zu:\n", read);
        printf("%s", line);
        bool hit = false;
        /* two tokens per line */
        while ((token = strsep(&line, " ")) != NULL) {
            if (hit) {
                goto exit;
            } else {
                char *dot = strrchr(token, '/');
                if (dot && !strcmp(dot+1, luafile))
                    hit = true;
            }
        }
    }
    log_error("default //moonlark/lua/edit.lua not configured");
    exit(EXIT_FAILURE);
 exit:
    if (token != NULL) {
        /* printf("  tok: %s\n", token); */
        bazel_luadir = dirname(token);
        /* printf("  bazel_luadir: %s\n", bazel_luadir); */
    }

    fclose(fp);
    log_debug("bazel_luadir: %s", bazel_luadir);
    return bazel_luadir;
}

/**
   sets: user_handlers_dir, runfiles_root
 */
void starlark_bazel_config(lua_State *L)
{
    log_debug("starlark_bazel_config");

    /* when run using `bazel test`, this will be cwd: */
    char *proj_root = getenv("BUILD_WORKSPACE_DIRECTORY");
    if (proj_root == NULL) {
        log_info("Env var 'BUILD_WORKSPACE_DIRECTORY' not found. This program must be run using `$ bazel run ...` in a Bazel project.");

        proj_root =getcwd(NULL, 0);
    }
    log_debug("BUILD_WORKSPACE_DIRECTORY: %s", proj_root);
    /* utstring_new(proj_root); */
    /* utstring_printf(proj_root, "%s", _proj_root); */

    lua_getglobal(L, "bazel");
    lua_pushstring(L, "config");
    lua_newtable(L);
    lua_pushstring(L, proj_root);
    lua_setfield(L, -2, "proj_root");

    /* utstring_new(exec_root); */
    /* utstring_printf(exec_root, "%s", _exec_root); */
    /* log_debug("exec_root: %s", utstring_body(exec_root)); */

    /* when launched via `$ build run`, cwd == runfiles root */
    /* utstring_new(runfiles_root); */
    /* utstring_printf(runfiles_root, "%s", getcwd(NULL, 0)); */
    /* log_debug("RUNFILES_ROOT: %s", utstring_body(runfiles_root)); */
    /* lua_pushstring(L, utstring_body(runfiles_root)); */
    /* lua_setfield(L, -2, "runfiles_root"); */

    /* default handlers dir */
    /* FIXME: this will only work if launcheb by $ bazel run */
    //FIXME: get the path from runfiles MANIFEST
    /* utstring_new(default_handlers_dir); */
    /* utstring_printf(default_handlers_dir, "%s/%s", */
    /*                 utstring_body(runfiles_root), */
    /*                 "moonlark"); */
    /* lua_pushstring(L, utstring_body(default_handlers_dir)); */
    /* lua_setfield(L, -2, "default_handlers_dir"); */

    /* user-defined handlers dir */
    //FIXME: check for existance, relative to launch dir
    /* utstring_new(user_handlers_dir); */
    /* utstring_printf(user_handlers_dir, "%s/%s", proj_root, ".moonlark.d"); */
    /* lua_pushstring(L, utstring_body(user_handlers_dir)); */
    /* lua_setfield(L, -2, "user_handlers_dir"); */

    /* lua_settable(L, -3); */

    /* /\* always mkdir moonlark.d? *\/ */
    /* /\* log_debug("mkdir %s", utstring_body(user_handlers_dir)); *\/ */
    /* int rc = mkdir(utstring_body(user_handlers_dir), S_IRWXU | S_IRGRP | S_IWGRP); */
    /* if (rc != 0) { */
    /*     if (errno != EEXIST) { */
    /*         perror(utstring_body(user_handlers_dir)); */
    /*         log_error("mkdir error"); */
    /*     } */
    /* } */

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

