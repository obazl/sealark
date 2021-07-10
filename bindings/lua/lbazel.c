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
    /* log_debug("lbazel_config"); */

    /* Interrogate env to get lua load paths and cwd */
    char *bazel_luadir = lbazel_get_luadir(bazel_lua_cb);
    /* log_debug("bazel_luadir: %s", bazel_luadir); */

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
        log_error("BUILD_WORKING_DIRECTORY not found. This program is designed to be run from the root directory of a Bazel repo.");
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
    /* log_debug("bazel_get_luadir %s", luafile); */
    char *bazel_luadir = NULL;
    char *s = getcwd(NULL, 0);
    /* log_info("CURRENT WORKING DIRECTORY: %s", s); */
    char *mdir = dirname(s);
    /* log_info("MANIFEST DIR: %s", mdir); */
    UT_string *manifest;
    utstring_new(manifest);
    utstring_printf(manifest, "%s%s", mdir, "/MANIFEST");
    /* log_info("MANIFEST: %s", utstring_body(manifest)); */

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
        /* printf("Retrieved line of length %zu:\n", read); */
        /* printf("%s", line); */
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
    /* log_debug("bazel_luadir: %s", bazel_luadir); */
    return bazel_luadir;
}
