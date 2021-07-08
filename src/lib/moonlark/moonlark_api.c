/*
  starlark_lua.c

  * convert C nodes to Lua tables
 */

#include <stdarg.h>
#include <pthread.h>
#include <unistd.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "log.h"
#include "utstring.h"

#include "moonlark_api.h"
/* #include "libmoonlark.h" */

int x;

void starlark_comments2lua(lua_State *L, struct node_s *node)
{
    log_debug("starlark_comments2lua");
}

void starlark_nodelist2lua(lua_State *L, UT_array *_nodelist, int level)
{
    /* log_debug("starlark_nodelist2lua level %d ct: %d", */
    /*           level, utarray_len(_nodelist)); */

    struct node_s *node=NULL;
    int i = 1;
    while( (node=(struct node_s*)utarray_next(_nodelist, node))) {
        /* log_debug("subnode %d", i); */
        starlark_node2lua(L, node, level); /* puts node tbl on ToS */
        /* log_debug("pushing node %d to subnode list", i); */
        /* lua_newtable(L); */
        /* lua_pushstring(L, "subnode");  /\* key *\/ */
        lua_rawseti(L, -2, i);
        /* lua_settable(L, -3);        /\* add to node table *\/ */
        i++;
    }
    /* log_debug("/starlark_nodelist2lua level %d", utarray_len(_nodelist)); */
}

EXPORT void starlark_node2lua(lua_State *L, struct node_s *node, int level)
{
    /* log_debug("starlark_node2lua %d", level); */
    lua_newtable(L);            /* one table per node  */

    /* log_debug("pushing type %d %s", */
    /*           node->type, token_name[node->type][0]); */
    lua_pushstring(L, "type");  /* key */
    lua_pushinteger(L, node->type);
    lua_settable(L, -3);

    /* debugging only: */
    lua_pushstring(L, "t");  /* key */
    if (token_name[node->type][0] != NULL) {
        /* log_debug("pushing typestring %s", */
        /*           token_name[node->type][0]); */
        lua_pushstring(L, token_name[node->type][0]);
    } else {
        lua_pushstring(L, "FOOBAR");
    }
    lua_settable(L, -3);

    if (node->s != NULL) {
        /* log_debug("pushing string[%d] %s", node->type, node->s); */
        lua_pushstring(L, "s");  /* key */
        lua_pushstring(L, node->s);
        lua_settable(L, -3);
    }
    if (node->type == TK_STRING) {
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

    /* if (node->comments != NULL) { */
    /*     lua_pushstring(L, "comments");  /\* key *\/ */
    /*     starlark_comments2lua(L, node->comments); */
    /*     lua_settable(L, -3); */
    /* } */

    if (node->subnodes != NULL) {
        if (utarray_len(node->subnodes) > 0) {
            lua_pushstring(L, "subnodes");  /* key */
            lua_newtable(L);
            starlark_nodelist2lua(L, node->subnodes, level + 1);
            lua_settable(L, -3);
        }
    }
    /* log_debug("/starlark_node2lua %d", level); */
}

EXPORT void starlark_ast2lua(lua_State *L, struct parse_state_s *parse)
{
    log_debug("starlark_buildfile2lua");
    log_debug("stack gettop %d", lua_gettop(L));

    /* lua_getglobal(L, "bazel"); */
    /* /\* lua_getfield(L, -1, "build"); *\/ */

    /* lua_pushstring(L, "build"); */
    lua_newtable(L);

    lua_pushstring(L, parse->lexer->fname); /* key: build file name */
    lua_setfield(L, -2, "build_file");

    lua_pushstring(L, "type");  /* key */
    lua_pushinteger(L, parse->root->type); // token_name[parse->root->type][0]);
    lua_settable(L, -3);

    lua_pushstring(L, "t");  /* key */
    if (token_name[parse->root->type][0] != NULL) {
        /* log_debug("pushing type string %s", */
        /*           token_name[parse->root->type][0]); */
        lua_pushstring(L, token_name[parse->root->type][0]);
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

    /* if (node->comments != NULL) { */
    /*     lua_pushstring(L, "comments");  /\* key *\/ */
    /*     starlark_comments2lua(L, node->comments); */
    /*     lua_settable(L, -3); */
    /* } */

    if (parse->root->subnodes != NULL) {
        if (utarray_len(parse->root->subnodes) > 0) {
            lua_pushstring(L, "subnodes");  /* key */
            lua_newtable(L);
            starlark_nodelist2lua(L, parse->root->subnodes, 1);
            lua_settable(L, -3);
        }
    }

    /* lua_settable(L, -3);        /\* add to bazel table *\/ */
    log_debug("stack gettop %d", lua_gettop(L));

    return;
}

/*
  Top of Stack: ast
 */
EXPORT void starlark_lua_call_user_handler(lua_State *L)
{
    log_debug("starlark_lua_call_user_handler");

    lua_getglobal(L, "init");   /* user-provided function */
    lua_rotate(L, -1, 2);       /* swap top 2 elts on stack */

    if (lua_pcall(L, 1, 0, 0) != 0) {
        log_error("error running lua fn: init");
        lerror(L, "error running function `init': %s", lua_tostring(L, -1));
    }

    /* retrieve result */
    /* if (!lua_isstring(L, -1)) { */
    /*     log_error("function `init' must return a string"); */
    /*     lerror(L, "function `init' must return a string"); */
    /* } */
    /* const char *msg = lua_tostring(L, -1); */
    /* lua_pop(L, 1);  /\* pop returned value *\/ */

    log_debug("lua user-provided 'init' returned");
}
