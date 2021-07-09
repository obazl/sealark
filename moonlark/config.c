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
#include "config.h"

UT_string *buffer;

void moonlark_config_for_bazel(lua_State *L,char *bazel_lua_cb,char *_user_luadir,char *lua_file)
{
    /* log_debug("moonlark_config_for_bazel"); */

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

    /* return L; */
}

/* int moonlark_process_buildfile(char *build_file, char *lua_file, char *callback) */
/* { */
/*     log_debug("moonlark_process_buildfile %s, %s", */
/*               build_file, lua_file); */

/*     lua_State *L; */
/*     L = luaL_newstate();        /\* set global lua state var *\/ */
/*     if (L == NULL) { */
/*         log_error("luaL_newstate failure"); */
/*     } */

/*     luaL_openlibs(L); */

/*     /\* create global moonlark table, token tables, etc. *\/ */
/*     moonlark_config_moonlark_table(L); */
/*     /\* moonlark_augment_load_path(L, path); *\/ */

/*     /\* load default and user handlers (Lua files) *\/ */
/*     moonlark_lua_load_file(L, lua_file); */

/*     /\* now parse the file using libstarlark *\/ */
/*     struct parse_state_s *parse_state = starlark_parse_file(build_file); */
/*     log_debug("parsed file %s", parse_state->lexer->fname); */

/*     /\* convert build file to Lua AST table *\/ */
/*     moonlark_ast2lua(L, parse_state); */
/*     /\* L now contains global bazel.build array of ASTs *\/ */

/*     /\* call handler on (Lua) AST *\/ */
/*     moonlark_lua_call_user_handler(L, callback); */

/*     /\* serialization routines expect a UT_string, not a char buffer *\/ */
/*     /\* utstring_new(buffer); *\/ */
/*     /\* starlark_node2string(parse_state->root, buffer); *\/ */
/*     /\* printf("%s", utstring_body(buffer)); *\/ */

/*     /\* utstring_free(buffer); *\/ */
/*     /\* free(parse_state->lexer->fname); *\/ */
/*     /\* node_dtor(parse_state->root); *\/ */
/*     /\* return r; *\/ */
/* } */
