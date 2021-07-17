/*
  starlark_lua.c

  * convert C nodes to Lua tables
 */

#include <errno.h>
/* #include <pthread.h> */
#include <stdarg.h>
#include <sys/stat.h>
#include <unistd.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "log.h"
#include "utstring.h"

UT_string *proj_root;
UT_string *runfiles_root;

/* location of default lua files: <runfiles_root>/moonlark/ */
UT_string *default_handlers_dir;

/* location of user-defined lua files: $cwd/.moonlark.d/ */
UT_string *user_handlers_dir;
/* UT_string *runtime_data_dir; */

char *default_handler_file_name = "handler.lua";
UT_string *user_lua_file;


#include "libmoonlark.h"

int x;

void moonlark_comment2lua(lua_State *L, struct node_s *_comment, int level)
{
    log_debug("moonlark_comment2lua");
    lua_newtable(L);            /* one table per comment node  */

}

void moonlark_comments2lua(lua_State *L, UT_array *_comments, int level)
{
    log_debug("moonlark_comments2lua");
    struct node_s *node=NULL;
    int i = 1;
    while( (node=(struct node_s*)utarray_next(_comments, node))) {
        moonlark_comment2lua(L, node, level); /* puts node tbl on ToS */
        lua_rawseti(L, -2, i);
        i++;
    }
}

void moonlark_nodelist2lua(lua_State *L, UT_array *_nodelist, int level)
{
    /* log_debug("moonlark_nodelist2lua level %d ct: %d", */
    /*           level, utarray_len(_nodelist)); */

    struct node_s *node=NULL;
    int i = 1;
    while( (node=(struct node_s*)utarray_next(_nodelist, node))) {
        /* log_debug("subnode %d", i); */
        moonlark_node2lua(L, node, level); /* puts node tbl on ToS */
        /* log_debug("pushing node %d to subnode list", i); */
        /* lua_newtable(L); */
        /* lua_pushstring(L, "subnode");  /\* key *\/ */
        lua_rawseti(L, -2, i);
        /* lua_settable(L, -3);        /\* add to node table *\/ */
        i++;
    }
    /* log_debug("/moonlark_nodelist2lua level %d", utarray_len(_nodelist)); */
}

EXPORT void moonlark_node2lua(lua_State *L, struct node_s *node, int level)
{
    /* log_debug("moonlark_node2lua %d", level); */
    lua_newtable(L);            /* one table per node  */

    /* log_debug("pushing type %d %s", */
    /*           node->tid, token_name[node->tid][0]); */
    lua_pushstring(L, "type");  /* key */
    lua_pushinteger(L, node->tid);
    lua_settable(L, -3);

    /* debugging only: */
    lua_pushstring(L, "t");  /* key */
    if (token_name[node->tid][0] != NULL) {
        /* log_debug("pushing typestring %s", */
        /*           token_name[node->tid][0]); */
        lua_pushstring(L, token_name[node->tid][0]);
    } else {
        lua_pushstring(L, "FOOBAR");
    }
    lua_settable(L, -3);

    if (node->s != NULL) {
        /* log_debug("pushing string[%d] %s", node->tid, node->s); */
        lua_pushstring(L, "s");  /* key */
        lua_pushstring(L, node->s);
        lua_settable(L, -3);
    }
    if (node->tid == TK_STRING) {
        /* which kind of quote? */
        lua_pushstring(L, "q");
        if (node->qtype & SQUOTE)
            lua_pushstring(L, "'");
        else
            if (node->qtype & DQUOTE)
                lua_pushstring(L, "\"");
        lua_settable(L, -3);

        /* how many quotes? */
        lua_pushstring(L, "qq");
        if (node->qtype & TRIPLE)
            lua_pushinteger(L, 3);
        else
            lua_pushinteger(L, 1);
        lua_settable(L, -3);

        /* raw, binary, or plain? */
        if (node->qtype & BINARY_STR) {
            lua_pushstring(L, "binary");
            lua_pushboolean(L, 1);
            lua_settable(L, -3);
        }
        if (node->qtype & RAW_STR) {
            lua_pushstring(L, "raw");
            lua_pushboolean(L, 1);
            lua_settable(L, -3);
        }

    }

    /* log_debug("pushing line %d", node->line); */
    lua_pushstring(L, "line");
    lua_pushinteger(L, node->line);
    lua_settable(L, -3);

    /* log_debug("pushing col %d", node->col); */
    lua_pushstring(L, "col");
    lua_pushinteger(L, node->col);
    lua_settable(L, -3);

    if (node->comments != NULL) {
        lua_pushstring(L, "comments");  /* key */
        lua_newtable(L);
        moonlark_nodelist2lua(L, node->comments, level + 1);
        lua_settable(L, -3);
    }

    if (node->subnodes != NULL) {
        if (utarray_len(node->subnodes) > 0) {
            lua_pushstring(L, "subnodes");  /* key */
            lua_newtable(L);
            moonlark_nodelist2lua(L, node->subnodes, level + 1);
            lua_settable(L, -3);
        }
    }
    /* log_debug("/moonlark_node2lua %d", level); */
}

EXPORT void moonlark_ast2lua(lua_State *L, struct parse_state_s *parse)
{
    /* log_debug("moonlark_buildfile2lua"); */
    /* log_debug("stack gettop %d", lua_gettop(L)); */

    /* lua_getglobal(L, "bazel"); */
    /* /\* lua_getfield(L, -1, "build"); *\/ */

    /* lua_pushstring(L, "build"); */
    lua_newtable(L);

    lua_pushstring(L, parse->lexer->fname); /* key: build file name */
    lua_setfield(L, -2, "build_file");

    lua_pushstring(L, "type");  /* key */
    lua_pushinteger(L, parse->root->tid); // token_name[parse->root->tid][0]);
    lua_settable(L, -3);

    lua_pushstring(L, "t");  /* key */
    if (token_name[parse->root->tid][0] != NULL) {
        /* log_debug("pushing type string %s", */
        /*           token_name[parse->root->tid][0]); */
        lua_pushstring(L, token_name[parse->root->tid][0]);
    } else {
        /* lua_pushstring(L, "FOOBAR"); */
    }
    lua_settable(L, -3);

    if (parse->root->s != NULL) {
        log_debug("pushing string %s", parse->root->s);
        lua_pushstring(L, "s");  /* key */
        lua_pushstring(L, parse->root->s);
        lua_settable(L, -3);
    }
    lua_pushstring(L, "line");
    lua_pushinteger(L, parse->root->line);
    lua_settable(L, -3);

    lua_pushstring(L, "col");
    lua_pushinteger(L, parse->root->col);
    lua_settable(L, -3);

    if (parse->root->comments != NULL) {
        lua_pushstring(L, "comments");  /* key */
        lua_newtable(L);
        moonlark_nodelist2lua(L, parse->root->comments, 1);
        lua_settable(L, -3);
    }

    if (parse->root->subnodes != NULL) {
        if (utarray_len(parse->root->subnodes) > 0) {
            lua_pushstring(L, "subnodes");  /* key */
            lua_newtable(L);
            moonlark_nodelist2lua(L, parse->root->subnodes, 1);
            lua_settable(L, -3);
        }
    }

    /* lua_settable(L, -3);        /\* add to bazel table *\/ */
    /* log_debug("stack gettop %d", lua_gettop(L)); */

    return;
}

/*
  Top of Stack: parsed lAST
 */
EXPORT void moonlark_call_user_handler(lua_State *L, char *handler)
{
    /* log_debug("moonlark_lua_call_user_handler"); */

    lua_getglobal(L, handler);   /* user-provided function */
    lua_rotate(L, -1, 2); /* swap top 2 elts to put lAST on ToS */

    if (lua_pcall(L, 1, 0, 0) != 0) {
        log_error("Lua: error running handler : %s", handler);
        lerror(L, "Lua error running handler %s: %s\n", handler, lua_tostring(L, -1));
    }

    /* retrieve result */
    /* if (!lua_isstring(L, -1)) { */
    /*     log_error("function `init' must return a string"); */
    /*     lerror(L, "function `init' must return a string"); */
    /* } */
    /* const char *msg = lua_tostring(L, -1); */
    /* lua_pop(L, 1);  /\* pop returned value *\/ */

    /* log_debug("lua user-provided callback '%s' returned", handler); */
}

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
    if (path == NULL) return;

    /* log_debug("moonlark_augment_load_path %s", path); */

    UT_string *load_path;
    utstring_new(load_path);

    /* starlark_bazel_config(L); */

    int t = lua_getglobal(L, "package");
    if (t == LUA_TNIL) {
        log_error("ERROR: Lua table 'package' not found");
        exit(EXIT_FAILURE);
    }
    lua_getfield(L, -1, "path");
    const char *curr_path = lua_tostring(L, -1);
    /* log_debug("current load path: %s", curr_path); */
    lua_pop(L, 1);

    utstring_printf(load_path, "%s/?.lua;%s", path, curr_path);
    /* log_debug("new load_path: %s", utstring_body(load_path)); */

    lua_pushstring(L, utstring_body(load_path));
    lua_setfield(L, -2, "path");
    /* lua_pop(L, 1); */
    lua_setglobal(L, "package");

    utstring_free(load_path);
}

/*
  called by //moonlark:edit, but not //moonlark:repl
 */
EXPORT void moonlark_load_script_file(lua_State *L, char *lua_file)
{
    // log_debug("starlark_lua_load_handlers");

    UT_string * require;
    utstring_new(require);
    utstring_printf(require, "require '%s'", lua_file);
    /* remove .lua to get pkg name for require */
    int end = utstring_findR(require, -1, ".lua'", 5);
    utstring_body(require)[end] = '\'';
    utstring_body(require)[end+1] = '\0';

    if (luaL_dostring(L, utstring_body(require))) {
        lerror(L, "luaL_dostring fail for: %s\n",
               lua_tostring(L, -1));
    }
}

EXPORT void moonlark_create_token_enums(lua_State *L)
{
    /* log_debug("starlark_lua_create_tokens_enum"); */
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
        /* log_debug("%d: printable_token[%d]: %s", */
        /*           i, printable_tokens[i], token_name[printable_tokens[i]][0]); */
        lua_pushinteger(L, printable_tokens[i]);
        lua_pushstring(L, token_name[printable_tokens[i]][1]);
        lua_settable(L, -3);
    }
    lua_settable(L, -3);
}

/*
  called by moonlark:edit, not lmoonlark pkg
 */
EXPORT void moonlark_config_moonlark_table(lua_State *L)
{
    // log_debug("moonlark_config_moonlark_table");

    /* if moonlark exists, we're moonlark:repl; otherwise we're moonlark:edit */
    int t = lua_getglobal(L, "moonlark");
    if (t == LUA_TNIL) {
        /* running moonlark:edit */
        /* log_info("Lua table 'moonlark' not found; creating"); */
        lua_newtable(L);
    } else {
        /* running moonlark:repl */
        /* log_info("Lua table 'moonlark' found"); */
    }
    moonlark_create_token_enums(L);
    lua_setglobal(L, "moonlark");
}
