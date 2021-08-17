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

/* **************************************************************** */
/*
  :package > :stmt-list > :small-stmt-list > :load-stmt
  :load-stmt
    > :load, :lparen, :string, :comma, :alias > :id, :eq, :string
   **************************************************************** */

/* **************** */
EXPORT int sealark_pkg_loadstmt_idx_for_key(struct node_s *package,
                                            const char *key)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_for_key %s", key);
#endif

    assert(package->tid == TK_Package);

    struct node_s *stmt_list = utarray_eltptr(package->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    // :package > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *expr_nd=NULL;

    int key_len = strlen(key);

#if defined(DEBUG_UTARRAYS)
    log_debug("SEARCHING load stmts for src: %s", key);
#endif

    int i = 0;
    int loadstmt_ct = 0;

    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {
        if (expr_nd->tid == TK_Load_Stmt) {
            struct node_s *loadid =
                utarray_eltptr(expr_nd->subnodes, 2);
#if defined(DEBUG_UTARRAYS)
            log_debug("loadid %d %s %s",
                      loadid->tid, TIDNAME(loadid), loadid->s);
#endif
            if ( (strncmp(loadid->s, key, key_len) == 0)
                 && strlen(loadid->s) == key_len ) {
                return loadstmt_ct;
            }
            loadstmt_ct++;
        }
    }
    errno = ENOT_FOUND;
    return -1;
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
/* return subnode index for loadstmt idx */
EXPORT int sealark_pkg_subidx_for_loadstmt_idx(struct node_s *package,
                                               int index)
{
#if defined(DEBUG_LOADS)
    log_debug("sealark_pkg_subidx_for_loadstmt_idx %d", index);
#endif

    struct node_s *stmt_list = utarray_eltptr(package->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    // :package > :stmt-list :smallstmt-list > load_stmt

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

    if (index > loadstmt_ct) {
        log_error("index > target count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return -1;
    }
#if defined (DEBUG_LOADS)
    log_debug("\tnormalized: %d", index);
#endif

    struct node_s *expr_nd=NULL;
    int load_ct = 0;
    int load_subct = 0;
    int i = 0;

    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {
        if (expr_nd->tid == TK_Load_Stmt) {
            if (load_ct == index)
                return i;
            load_ct++;
            load_subct = i + 1;
        }
        i++;
    }
    if (index == loadstmt_ct) {
        return load_subct;
    } else {
        errno = EINDEX_OUT_OF_BOUNDS;
        return -1;
    }
}

/* **************************************************************** */
EXPORT struct node_s *sealark_loadstmt_key(struct node_s *loadstmt)
{
#if defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_key");
#endif

    assert(loadstmt->tid == TK_Load_Stmt);
    struct node_s *key_nd=NULL;
    key_nd = utarray_eltptr(loadstmt->subnodes, 2);
    assert(key_nd->tid == TK_STRING);
    return key_nd;
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

/* **************************************************************** */
EXPORT int sealark_loadstmt_arg_count(struct node_s *loadstmt)
{
#if defined(DEBUG_LOADS)
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
EXPORT int sealark_loadstmt_binding_count(struct node_s *loadstmt)
{
#if defined(DEBUG_COUNTS)
    log_debug(">> sealark_loadstmt_binding_count");
#endif

    int subnode_ct = utarray_len(loadstmt->subnodes);
    int binding_ct = 0;

    struct node_s *item;
    for (int i = 4; i < subnode_ct; i++) {
        item = utarray_eltptr(loadstmt->subnodes, i);
        if (item->tid == TK_Binding) {
            binding_ct++;
        }
    }
#if defined(DEBUG_COUNTS)
    log_debug("\tbinding count: %d", binding_ct);
#endif
    return binding_ct;
}

/* **************** */
EXPORT struct node_s *sealark_loadstmt_binding_for_int(struct node_s *loadstmt,
                                                   int index)
{
#if defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_binding_for_int: %d", index);
#endif

    /* subtract 1 for final ')' */
    int subnode_ct = utarray_len(loadstmt->subnodes) - 1;

    int binding_ct = sealark_loadstmt_binding_count(loadstmt);
    log_debug("binding_ct: %d", binding_ct);

    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > binding_ct) {
            log_error("abs(%d) > binding_ct", index, binding_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            /* return NULL; */
        } else {
            index = binding_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > binding_ct-1) {
        log_error("index %d >= binding count %d", index, binding_ct);
        errno = EINDEX_OUT_OF_BOUNDS;
        return NULL;
    }
#if defined (DEBUG_LOADS)
    log_debug("\tnormalized idx: %d", index);
#endif

    int i;
    int attr_idx = 0;
    /* int max = 4 + attr_ct * 2; */
    /* last node may be ')' */
    /* if (max > subnode_ct - 1) max = subnode_ct - 1; */

    struct node_s *attr=NULL;
    // skip first four subnodes 'load("key",'
    for (i = 4; i < subnode_ct; i++) {
    /* while( (attr=(struct node_s*)utarray_next(loadstmt->subnodes, */
    /*                                              attr)) ) { */
        attr = utarray_eltptr(loadstmt->subnodes, i);
        log_debug("attr[%d] %d %s: %s",
                  attr_idx, attr->tid, TIDNAME(attr), attr->s);

        if (attr->tid == TK_Binding) {
            if (attr_idx == index) {
                log_debug("MATCH at %d", index);
                return attr;
            }
            attr_idx++;
        }
    }
    return NULL;
}

/* **************** */
/* return subnode index for loadstmt attr idx */
EXPORT int sealark_loadstmt_binding_subidx_for_idx(struct node_s *loadstmt,
                                                   int index)
{
#if defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_binding_subidx_for_idx %d", index);
#endif

    int subnode_ct = utarray_len(loadstmt->subnodes);

    /* normalize index - cannot use util routine since there is no
       args node type */
    int attr_ct = sealark_loadstmt_binding_count(loadstmt);
    log_debug("  attr_ct: %d", attr_ct);
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > attr_ct) {
            log_error("abs(%d) > attr_ct", index, attr_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            /* return NULL; */
        } else {
            index = attr_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > attr_ct) {
        log_error("index > target count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return -1;
    }
#if defined (DEBUG_LOADS)
    log_debug("  normalized idx: %d", index);
#endif

    struct node_s *expr_nd=NULL;
    int attr_idx = 0;
    int attr_subct = 0;

    for (int i = 4; i < subnode_ct; i++) {
    /* while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes, */
    /*                                              expr_nd)) ) { */
        expr_nd = utarray_eltptr(loadstmt->subnodes, i);
        if (expr_nd->tid == TK_Binding) {
            if (attr_idx == index) {
                return i;
            }
            attr_idx++;
            attr_subct = i + 1;
        }
        i++;
    }
    log_debug("MIISS");
    if (index == attr_ct) {
        return attr_subct;
    } else {
        errno = EINDEX_OUT_OF_BOUNDS;
        return -1;
    }
}

/* **************** */
EXPORT int sealark_loadstmt_attr_idx_for_str(struct node_s *loadstmt,
                                             const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_attr_for_string: %s", name);
#endif

    int len = strlen(name);
    int binding_ct = 0;
    struct node_s *bid;
    struct node_s *attr=NULL;
    while( (attr=(struct node_s*)utarray_next(loadstmt->subnodes,
                                              attr)) ) {
        if (attr->tid == TK_Binding) {
            bid = utarray_eltptr(attr->subnodes, 0);
            if ( (strncmp(bid->s, name, len) == 0)
                 && (strlen(bid->s) == len)) {
                return binding_ct;
            }
            binding_ct++;
        }
    }
    errno = ENOT_FOUND;
    return -1;
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

    int subnode_ct = utarray_len(loadstmt->subnodes);
    /* normalize index - cannot use util routine since there is no
       args node type */
    int arg_ct = sealark_loadstmt_arg_count(loadstmt);
    log_debug("arg_ct: %d", arg_ct);
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > arg_ct) {
            log_error("abs(%d) > arg_ct", index, arg_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            /* return NULL; */
        } else {
            index = arg_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > arg_ct-1) {
        log_error("index > target count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return NULL;
    }
#if defined (DEBUG_LOADS)
    log_debug("\tnormalized idx: %d", index);
#endif

    int i;
    int arg_idx = 0;
    int max = 4 + arg_ct * 2;
    /* last node may be ')' */
    if (max > subnode_ct - 1) max = subnode_ct - 1;

    struct node_s *arg=NULL;
    // skip first four subnodes 'load("key",'
    for (i = 4; i < max; i++) {
    /* while( (arg=(struct node_s*)utarray_next(loadstmt->subnodes, */
    /*                                              arg)) ) { */
        arg = utarray_eltptr(loadstmt->subnodes, i);
        log_debug("arg[%d] %d %s",
                  arg_idx, arg->tid, TIDNAME(arg));

        if (arg->tid == TK_STRING) {
            if (arg_idx == index)
                return arg;
            arg_idx++;
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
EXPORT int sealark_loadstmt_arg_idx_for_str(struct node_s *loadstmt,
                                            const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_arg_idx_for_string: %s", name);
#endif

    int subnode_ct = utarray_len(loadstmt->subnodes);
    log_debug("subnode_ct: %d", subnode_ct);

    int arg_ct = sealark_loadstmt_arg_count(loadstmt);
    log_debug("arg_ct: %d", arg_ct);
    int arg_idx = 0;
    int len = strlen(name);
    struct node_s *arg=NULL;
    for (int i = 4; i < subnode_ct; i++) {
        log_debug("loop i: %d, arg_idx: %d", i, arg_idx);
        arg = utarray_eltptr(loadstmt->subnodes, i);
        if (arg->tid == TK_STRING) {
            log_debug("0 xxxxxxxxxxxxxxxx");

            if ( (strncmp(arg->s, name, len) == 0)
                 && (strlen(arg->s) == len)) {
                    log_debug("HIT xxxxxxxxxxxxxxxx");
                return arg_idx;
            }
            arg_idx++;
        }
    }
    errno = ENOT_FOUND;
    return -1;
}

/* /\* **************** *\/ */
/* EXPORT */
/* struct node_s *sealark_loadstmt_binding_for_int(struct node_s *loadstmt, */
/*                                                 int index) */
/* { */
/* #if defined (DEBUG_TRACE) || defined(DEBUG_LOADS) */
/*     log_debug("sealark_loadstmt_binding_for_int: %d", index); */
/* #endif */
/*     assert(loadstmt->tid == TK_Load_Stmt); */

/*     int attr_ct = sealark_pkg_loadstmt_binding_count(package); */
/*     log_debug("attr_ct: %d", attr_ct); */
/*     /\* reverse indexing *\/ */
/*     if (index < 0) { */
/*         if (abs(index) > attr_ct) { */
/*             log_error("abs(%d) > attr_ct", index, attr_ct); */
/*             errno = EINDEX_OUT_OF_BOUNDS; */
/*             /\* return NULL; *\/ */
/*         } else { */
/*             index = attr_ct + index; */
/*             /\* log_debug("recurring..."); *\/ */
/*             /\* return sealark_vector_item_for_int(node, index); *\/ */
/*         } */
/*     } */

/*     if (index > attr_ct-1) { */
/*         log_error("index > target count"); */
/*         errno = EINDEX_OUT_OF_BOUNDS; */
/*         return NULL; */
/*     } */
/* #if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS) */
/*     log_debug("\tnormalized: %d", index); */
/* #endif */

/*     int binding_ct = 0; */
/*     struct node_s *binding=NULL; */
/*     while( (binding=(struct node_s*)utarray_next(loadstmt->subnodes, */
/*                                                  binding)) ) { */
/*         if (binding->tid == TK_Binding) { */
/*             if (binding_ct == index) return binding; */
/*             binding_ct++; */
/*         } */
/*     } */
/*     errno = ENOT_FOUND_BINDING; */
/*     return NULL; */
/* } */

/* **************** */
//FIXME: use sealark_loadstmt_binding_for_sym
EXPORT struct node_s *sealark_loadstmt_attr_for_string(struct node_s *loadstmt, const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_attr_for_string: %s", name);
#endif

    int len = strlen(name);
    struct node_s *bid;
    struct node_s *attr=NULL;
    while( (attr=(struct node_s*)utarray_next(loadstmt->subnodes,
                                                 attr)) ) {
        if (attr->tid == TK_Binding) {
            bid = utarray_eltptr(attr->subnodes, 0);
            if ( (strncmp(bid->s, name, len) == 0)
                 && (strlen(bid->s) == len)) {
                return attr;
            }
        }
    }
    errno = ENOT_FOUND;
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
    errno = 0;
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
    /* log_debug("sealark_loadstmt_src_string"); */
#endif

    assert(loadstmt->tid == TK_Load_Stmt);

    struct node_s *src = utarray_eltptr(loadstmt->subnodes, 2);
    assert(src->tid == TK_STRING);
    return src->s;
}
