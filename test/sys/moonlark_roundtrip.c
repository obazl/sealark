#include <errno.h>
#include <unistd.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"

#include "moonlark.h"

#include "moonlark_roundtrip.h"

UT_string *build_file;

UT_string *buffer;

/* bazel_lua_cb is determined by data attrib of build rule; used
   to find bazel_luadir */
char *bazel_lua_cb = "edit.lua";
char *bazel_luadir;
char *user_luadir;
char *lua_file = NULL;
char *callback = "moonlark_handler"; /* callback defined in lua_file */

int compareFiles(FILE *file1, FILE *file2)
{
    char ch1 = getc(file1);
    char ch2 = getc(file2);
    int error = 0, pos = 0, line = 1;
    while (ch1 != EOF && ch2 != EOF){
        pos++;
        if (ch1 == '\n' && ch2 == '\n'){
            line++;
            pos = 0;
        }
        if (ch1 != ch2){
            error++;
            printf("File mismatch at (%d:%d)\n", line, pos);
            return -1;
        }
        ch1 = getc(file1);
        ch2 = getc(file2);
    }
    return 0;
}

int moonlark_roundtrip(char *build_file, char *_lua_file)
{
    log_debug("moonlark_roundtrip %s", build_file);

    char *rfdir = getenv("RUNFILES_DIR");
    log_debug("RUNFILES_DIR: %s", rfdir);
    char *rfmanifest = getenv("RUNFILES_MANIFEST_FILE");
    log_debug("RUNFILES_MANIFEST_FILE: %s", rfmanifest);

    /* utstring_renew(build_file); */

    /* utstring_printf(build_file, "%s", _build_file); */

    /* moonlark_process_buildfile(_build_file, _lua_file); */

    lua_State *L = lbazel_config(bazel_lua_cb, user_luadir, lua_file);

    /* /\* Interrogate env to get lua load paths and cwd *\/ */
    /* /\* char *bazel_luadir = lbazel_get_luadir(bazel_lua_cb); *\/ */
    /* /\* log_debug("bazel_luadir: %s", bazel_luadir); *\/ */

    /* /\* if (user_luadir == NULL) { *\/ */
    /* /\*     /\\* user_luadir = config_get_luadir(); *\\/ *\/ */
    /* /\*     log_warn("WARNING: no user luadir specified"); *\/ */
    /* /\* } else { *\/ */
    /* /\* } *\/ */
    /* /\* //FIXME: verify user_luadir exists *after* chdir to launchdir *\/ */

    /* char *wd = getenv("BUILD_WORKING_DIRECTORY"); */
    /* if (wd) { */
    /*     /\* log_info("BUILD_WORKING_DIRECTORY: %s", wd); *\/ */
    /*     chdir(wd); */
    /* } else { */
    /*     log_error("BUILD_WORKING_DIRECTORY not found. This program must be run from the root directory of a Bazel repo."); */
    /* } */

    /* /\* if (user_luadir) { *\/ */
    /* /\*     if( access( user_luadir, F_OK ) != 0 ) { *\/ */
    /* /\*         log_error("ERROR: user_luadir does not exist %s", user_luadir); *\/ */
    /* /\*         exit(EXIT_FAILURE); *\/ */
    /* /\*     } else { *\/ */
    /* /\*         log_info("user_luadir: %s", user_luadir); *\/ */
    /* /\*     } *\/ */
    /* /\* } *\/ */

    /* /\* /\\* startup lua (luaL_newstate()) *\\/ *\/ */
    /* /\* lua_State *L = luaL_newstate(); *\/ */
    /* /\* luaL_openlibs(L); *\/ */

    /* /\* /\\* set lua load paths *\\/ *\/ */
    /* /\* moonlark_augment_load_path(L, bazel_luadir); *\/ */
    /* /\* moonlark_augment_load_path(L, user_luadir); *\/ */

    /* /\* moonlark_config_moonlark_table(L); *\/ */

    /* /\* moonlark_lua_load_file(L, lua_file); *\/ */

    /* now parse the file using libstarlark */
    struct parse_state_s *parse_state = sealark_parse_file(build_file);
    log_debug("parsed file %s", parse_state->lexer->fname);

    /* convert build file to Lua AST table */
    moonlark_ast2lua(L, parse_state);
    /* L now contains global bazel.build array of ASTs */
    /* log_debug("stack gettop %d", lua_gettop(L)); */

    /* call callback on (Lua) AST - it must serialize to a file*/
    moonlark_lua_call_user_handler(L, callback);

    return EXIT_SUCCESS;
}
