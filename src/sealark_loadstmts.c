#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_loadstmts.h"

/* **************************************************************** */
/*
  :package > :stmt-list > :small-stmt-list > :load-stmt
  :load-stmt
    > :load, :lparen, :string, :comma, :alias > :id, :eq, :string
   **************************************************************** */

/* **************** */
EXPORT
struct node_s *sealark_pkg_loadstmt_for_key(struct node_s *package,
                                            const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_for_key %s", name);
#endif

    assert(package->tid == TK_Package);

    struct node_s *stmt_list = utarray_eltptr(package->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    // :package > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *expr_nd=NULL;

    int name_len = strlen(name);

#if defined(DEBUG_UTARRAYS)
    log_debug("SEARCHING load stmts for src: %s", name);
#endif
    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {
        if (expr_nd->tid == TK_Load_Stmt) {
            struct node_s *loadid =
                utarray_eltptr(expr_nd->subnodes, 2);
#if defined(DEBUG_UTARRAYS)
            log_debug("loadid %d %s %s",
                      loadid->tid, TIDNAME(loadid), loadid->s);
#endif
            if ( (strncmp(loadid->s, name, name_len) == 0)
                 && strlen(loadid->s) == name_len ) {
                return expr_nd;
            }
        }
    }
    errno = ENOT_FOUND;
    return NULL;
}

/* **************** */
EXPORT
struct node_s *sealark_pkg_loadstmt_for_int(struct node_s *package,
                                            int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_pkg_loadstmt_for_int %d", index);
#endif

    struct node_s *stmt_list = utarray_eltptr(package->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    // :package > :stmt-list :smallstmt-list > expr-list > call-expr

    /* normalize index - cannot use util routine since there is no
       args node type */
    int loadstmt_ct = sealark_pkg_loadstmt_count(package);
    log_debug("loadstmt_ct: %d", loadstmt_ct);
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > loadstmt_ct) {
            log_error("abs(%d) > loadstmt_ct", index, loadstmt_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            /* return NULL; */
        } else {
            index = loadstmt_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > loadstmt_ct-1) {
        log_error("index > target count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return NULL;
    }
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("\tnormalized: %d", index);
#endif

    struct node_s *expr_nd=NULL;
    int load_ct = 0;

    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {
        if (expr_nd->tid == TK_Load_Stmt) {
            if (load_ct == index)
                return expr_nd;
            load_ct++;
        }
    }
    return NULL;
}

/* **************** */
EXPORT
UT_array *sealark_loadstmt_arglist(struct node_s *loadstmt)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_arg_list");
#endif

    assert(loadstmt->tid == TK_Load_Stmt);

    struct node_s *arg_nd=NULL;

    UT_array *args;
    utarray_new(args, &node_icd);
    int i;
    while( (arg_nd=(struct node_s*)utarray_next(loadstmt->subnodes,
                                                 arg_nd)) ) {
        if (i == 2) {
            /* omit first (src) arg, after LOAD LPAREN */
            i++; continue;
        }
        if (arg_nd->tid == TK_STRING)
            utarray_push_back(args, arg_nd);
        if (arg_nd->tid == TK_Binding)
            utarray_push_back(args, arg_nd);
        i++;
    }
    return args;
}

/* ****************
 0: TK_Load_Stmt[117] @0:0
  1: TK_LOAD[53] @0:0
  1: TK_LPAREN[54] @0:4
  1: TK_STRING[79] @0:5    "@rules_cc//cc:defs.bzl"
  1: TK_COMMA[15] @0:29
  ...
*/
EXPORT
UT_array *sealark_loadstmt_args(struct node_s *loadstmt)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_args");
#endif

    UT_array *args;
    utarray_new(args, &node_icd);
    int i;
    struct node_s *arg_nd=NULL;

    while( (arg_nd=(struct node_s*)utarray_next(loadstmt->subnodes,
                                                 arg_nd)) ) {
        if (i < 3) { i++; continue; } // omit load("key"

        if (arg_nd->tid == TK_STRING)
            utarray_push_back(args, arg_nd);
        i++;
    }
    log_debug("found %d loadstmt args", utarray_len(args));
    return args;
}

/* FIXME: return deletion count? */
EXPORT
void sealark_loadstmt_rm_args(struct node_s *loadstmt)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_rm_args");
#endif
    sealark_debug_log_ast_outline(loadstmt, 0);
    int subnode_ct = utarray_len(loadstmt->subnodes);
    log_debug("subnode_ct: %d", subnode_ct);
    int span = 2;
    struct node_s *arg_nd=NULL;

    /* work backwards, stopping at key (third subnode) */
    for (int i = subnode_ct-1; i > 2; i--) {
    /* while( (arg_nd=(struct node_s*)utarray_next(loadstmt->subnodes, */
    /*                                              arg_nd)) ) { */
        /* if (i < 3) { continue; } // omit load("key" */
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        log_debug("item %d %s", i, item->s);

        if (item->tid == TK_STRING) {
            log_debug("STRING %d", i);
            /* last subnode is ')' */
            if (i == subnode_ct - 2) span = 1;
            else span = 2;
            log_debug("erasing %d span %d: %s", i, span,
                      item->s);
            utarray_erase(loadstmt->subnodes, i, span);
        }
    }
    return;
}

/* **************************************************************** */
EXPORT int sealark_loadstmt_arg_count(struct node_s *loadstmt)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_arg_count");
#endif

    int subnode_ct = utarray_len(loadstmt->subnodes);
    int arg_ct = 0;

    struct node_s *item;
    for (int i = 4; i < subnode_ct; i++) {
        item = utarray_eltptr(loadstmt->subnodes, i);
        if (item->tid == TK_STRING) {
            arg_ct++;
        }
    }
    return arg_ct;
}

/* **************************************************************** */
//FIXME: for loadstmts use 'alias' instead of attr/binding?
EXPORT int sealark_loadstmt_attr_count(struct node_s *loadstmt)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_arg_count");
#endif

    int subnode_ct = utarray_len(loadstmt->subnodes);
    int arg_ct = 0;

    struct node_s *item;
    for (int i = 4; i < subnode_ct; i++) {
        item = utarray_eltptr(loadstmt->subnodes, i);
        if (item->tid == TK_Binding) {
            arg_ct++;
        }
    }
    return arg_ct;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_rm_arg_at_int(struct node_s *loadstmt,
                                          int index)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_rm_arg_at_int: %d", index);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    /* sealark_debug_log_ast_outline(loadstmt, 0); */

    int subnode_ct = utarray_len(loadstmt->subnodes);
    log_debug("subnode_ct: %d", subnode_ct);

    /* normalize index - cannot use util routine since there is no
       args node type */
    int arg_ct = sealark_loadstmt_arg_count(loadstmt);
    log_debug("arg_ct: %d", arg_ct);
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > arg_ct) {
            log_error("abs(%d) > arg_ct", index, arg_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            return;
        } else {
            index = arg_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > arg_ct-1) {
        log_error("index > target count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return;
    }
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("\tnormalized: %d", index);
#endif

    int span = 2;
    int arg_idx = 0;

    /* start at idx 4, skipping 'load("key",' */
    for (int i = 4; i < subnode_ct; i++) {
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        log_debug("item %d/%d %s", i, arg_idx, item->s);
        if (item->tid == TK_STRING) {
            arg_idx = (i - 4) / 2;
            log_debug("STRING %d, arg %d", i, arg_idx);
            if (arg_idx == index) {
                /* last subnode is ')' */
                if (i == subnode_ct - 2) span = 1;
                else span = 2;
                log_debug("erasing %d span %d: %s", i, span,
                          item->s);
                utarray_erase(loadstmt->subnodes, i, span);
                return;
            }
            i++;
        }
    }
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_rm_arg_at_str(struct node_s *loadstmt,
                                           const char *key)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_rm_arg_at_str: %s", key);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    sealark_debug_log_ast_outline(loadstmt, 0);

    int klen = strlen(key);
    int subnode_ct = utarray_len(loadstmt->subnodes);
    log_debug("subnode_ct: %d", subnode_ct);

    int span = 2;
    int arg_idx = 0;

    /* start at idx 4, skipping 'load("key",' */
    for (int i = 4; i < subnode_ct; i++) {
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        log_debug("item %d/%d %s", i, arg_idx, item->s);
        if (item->tid == TK_STRING) {
            arg_idx = (i - 4) / 2;
            log_debug("STRING %d, arg %d", i, arg_idx);
            if ( (strncmp(item->s, key, klen) == 0)
                 && (strlen(item->s) == klen) ) {
                if (i == subnode_ct - 2) span = 1;
                else span = 2;
                log_debug("erasing %d span %d: %s", i, span,
                          item->s);
                utarray_erase(loadstmt->subnodes, i, span);
                return;
            }
        }
    }
    errno = ENOT_FOUND;
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_rm_attr_at_int(struct node_s *loadstmt,
                                          int index)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_rm_attr_at_int: %d", index);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    sealark_debug_log_ast_outline(loadstmt, 0);

    int subnode_ct = utarray_len(loadstmt->subnodes);
    log_debug("subnode_ct: %d", subnode_ct);

    /* normalize index - cannot use util routine since there is no
       args node type */
    int attr_ct = sealark_loadstmt_attr_count(loadstmt);
    log_debug("attr_ct: %d", attr_ct);
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > attr_ct) {
            log_error("abs(%d) > attr_ct", index, attr_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            return;
        } else {
            index = attr_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > attr_ct-1) {
        log_error("index > tattret count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return;
    }
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("\tidx normalized: %d", index);
#endif

    int span = 2;
    int attr_idx = 0;

    /* start at idx 4, skipping 'load("key",' */
    for (int i = 4; i < subnode_ct; i++) {
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        log_debug("item %d/%d %s", i, attr_idx, item->s);
        if (item->tid == TK_Binding) {
            /* attr_idx = (i - 4) / 2; */
            if (attr_idx == index) {
                /* last subnode is ')' */
                if (i == subnode_ct - 2) span = 1;
                else span = 2;
                log_debug("erasing %d span %d: %s", i, span,
                          item->s);
                utarray_erase(loadstmt->subnodes, i, span);
                return;
            }
            attr_idx++;
        }
    }
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_rm_attr_at_sym(struct node_s *loadstmt,
                                            const char *key)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_rm_attr_at_sym: %s", key);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    sealark_debug_log_ast_outline(loadstmt, 0);

    int klen = strlen(key);
    int subnode_ct = utarray_len(loadstmt->subnodes);
    log_debug("subnode_ct: %d", subnode_ct);

    int span = 2;
    int attr_idx = 0;
    struct node_s *attr_id;

    /* start at idx 4, skipping 'load("key",' */
    for (int i = 4; i < subnode_ct; i++) {
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        log_debug("item %d/%d %s", i, attr_idx, item->s);
        if (item->tid == TK_Binding) {
            attr_id = utarray_eltptr(item->subnodes, 0);
            log_debug("test attr key: %s", attr_id->s);
            if ( (strncmp(attr_id->s, key, klen) == 0)
                 && (strlen(attr_id->s) == klen) ) {
                /* last subnode is ')' */
                if (i == subnode_ct - 2) span = 1;
                else span = 2;
                log_debug("erasing %d span %d: %s", i, span,
                          item->s);
                errno = 0;
                utarray_erase(loadstmt->subnodes, i, span);
                return;
            }
            attr_idx++;
        }
    }
    errno = ENOT_FOUND;
    return;
}

/* **************** */
EXPORT
UT_array *sealark_loadstmt_bindings(struct node_s *loadstmt)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_bindings");
#endif

    struct node_s *arg_nd=NULL;

    UT_array *args;
    utarray_new(args, &node_icd);

    while( (arg_nd=(struct node_s*)utarray_next(loadstmt->subnodes,
                                                 arg_nd)) ) {
        /* log_debug("nd %d %s", arg_nd->tid, TIDNAME(arg_nd)); */

        if (arg_nd->tid == TK_Binding)
            utarray_push_back(args, arg_nd);
    }
    return args;
}

/* **************** */
EXPORT struct node_s *sealark_loadstmt_arg_for_int(struct node_s *loadstmt,
                                                   int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_arg_for_int: %d", index);
#endif

    int ct = 0;
    index++; /* skip first arg (src) */

    struct node_s *arg=NULL;
    while( (arg=(struct node_s*)utarray_next(loadstmt->subnodes,
                                                 arg)) ) {
        log_debug("arg[%d] %d %s",
                  ct, arg->tid, TIDNAME(arg));

        if (arg->tid == TK_STRING) {
            if (ct == index)
                return arg;
            ct++;
        }
    }
    return NULL;
}

/* **************** */
EXPORT struct node_s *sealark_loadstmt_arg_for_string(struct node_s *loadstmt, const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_arg_for_string: %s", name);
#endif

    int len = strlen(name);
    struct node_s *arg=NULL;
    while( (arg=(struct node_s*)utarray_next(loadstmt->subnodes,
                                                 arg)) ) {
        if (arg->tid == TK_STRING) {
            if ( (strncmp(arg->s, name, len) == 0)
                 && (strlen(arg->s) == len)) {
                return arg;
            }
        }
    }
    errno = ENOT_FOUND;
    return NULL;
}

/* **************** */
EXPORT
struct node_s *sealark_loadstmt_binding_for_int(struct node_s *loadstmt,
                                                int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_binding_for_int: %d", index);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);

    int binding_ct = 0;
    struct node_s *binding=NULL;
    while( (binding=(struct node_s*)utarray_next(loadstmt->subnodes,
                                                 binding)) ) {
        if (binding->tid == TK_Binding) {
            if (binding_ct == index) return binding;
            binding_ct++;
        }
    }
    errno = ENOT_FOUND_BINDING;
    return NULL;
}

/* **************** */
EXPORT
struct node_s *sealark_loadstmt_binding_for_sym(struct node_s *loadstmt,
                                                const char *sym)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_binding_for_sym: %s", sym);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);

    int len = strlen(sym);

    struct node_s *key=NULL;
    struct node_s *binding=NULL;
    while( (binding=(struct node_s*)utarray_next(loadstmt->subnodes,
                                                 binding)) ) {
        if (binding->tid == TK_Binding) {
            key = utarray_eltptr(binding->subnodes, 0);
            if ( (strncmp(key->s, sym, len) == 0)
                 && strlen(key->s) == len ) {
                return binding;
            }
        }
    }
    errno = ENOT_FOUND_BINDING;
    return NULL;
}

/* **************** */
EXPORT
char *sealark_loadstmt_src_string(struct node_s *loadstmt)
{
#if defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_src_string");
#endif

    assert(loadstmt->tid == TK_Load_Stmt);

    struct node_s *src = utarray_eltptr(loadstmt->subnodes, 2);
    assert(src->tid == TK_STRING);
    return src->s;
}
