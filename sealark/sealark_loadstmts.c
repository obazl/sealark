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
struct node_s *sealark_pkg_loadstmt_for_src(struct node_s *package,
                                            const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_for_src %s", name);
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
struct node_s *sealark_loadstmt_for_index(struct node_s *package,
                                         int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_for_index %d", index);
#endif

    struct node_s *stmt_list = utarray_eltptr(package->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    // :package > :stmt-list :smallstmt-list > expr-list > call-expr

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

/* **************** */
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
        if (i < 3) { i++; continue; } // omit fst arg (src)

        if (arg_nd->tid == TK_STRING)
            utarray_push_back(args, arg_nd);
        i++;
    }
    return args;
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
        log_debug("nd %d %s", arg_nd->tid, TIDNAME(arg_nd));

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
char *sealark_loadstmt_src_node(struct node_s *loadstmt)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_src_node");
#endif

    assert(loadstmt->tid == TK_Load_Stmt);

    struct node_s *src = utarray_eltptr(loadstmt->subnodes, 2);
    assert(src->tid == TK_STRING);
    return src->s;
}
