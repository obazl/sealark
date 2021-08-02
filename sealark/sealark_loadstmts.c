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
struct node_s *sealark_loadstmt_for_src(struct node_s *package,
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

    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {
        log_debug("node %d %s", expr_nd->tid, TIDNAME(expr_nd));
        if (expr_nd->tid == TK_Load_Stmt) {
            struct node_s *loadid =
                utarray_eltptr(expr_nd->subnodes, 2);
            log_debug("loadid %d %s %s",
                      loadid->tid, TIDNAME(loadid), loadid->s);

            if ( (strncmp(loadid->s, name, name_len) == 0)
                 && strlen(loadid->s) == name_len ) {
                return expr_nd;
            }
        }
    }
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
UT_array *sealark_loadstmt_arglist(struct node_s *load_stmt)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_arg_list");
#endif

    struct node_s *arg_nd=NULL;

    UT_array *args;
    utarray_new(args, &node_icd);

    while( (arg_nd=(struct node_s*)utarray_next(load_stmt->subnodes,
                                                 arg_nd)) ) {
        log_debug("nd %d %s", arg_nd->tid, TIDNAME(arg_nd));

        if (arg_nd->tid == TK_STRING)
            utarray_push_back(args, arg_nd);
        if (arg_nd->tid == TK_Binding)
            utarray_push_back(args, arg_nd);
    }
    return args;
}

/* **************** */
EXPORT
UT_array *sealark_loadstmt_args(struct node_s *load_stmt)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_args");
#endif

    struct node_s *arg_nd=NULL;

    UT_array *args;
    utarray_new(args, &node_icd);

    while( (arg_nd=(struct node_s*)utarray_next(load_stmt->subnodes,
                                                 arg_nd)) ) {
        log_debug("nd %d %s", arg_nd->tid, TIDNAME(arg_nd));

        if (arg_nd->tid == TK_STRING)
            utarray_push_back(args, arg_nd);
    }
    return args;
}

/* **************** */
EXPORT
UT_array *sealark_loadstmt_bindings(struct node_s *load_stmt)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_bindings");
#endif

    struct node_s *arg_nd=NULL;

    UT_array *args;
    utarray_new(args, &node_icd);

    while( (arg_nd=(struct node_s*)utarray_next(load_stmt->subnodes,
                                                 arg_nd)) ) {
        log_debug("nd %d %s", arg_nd->tid, TIDNAME(arg_nd));

        if (arg_nd->tid == TK_Binding)
            utarray_push_back(args, arg_nd);
    }
    return args;
}

/* **************** */
EXPORT
struct node_s *sealark_loadstmt_binding_for_index(struct node_s *load_stmt,
                                                  int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_binding_for_index: %d", index);
#endif

    struct node_s *binding=NULL;

    UT_array *args;
    utarray_new(args, &node_icd);

    int ct = 0;

    while( (binding=(struct node_s*)utarray_next(load_stmt->subnodes,
                                                 binding)) ) {
        log_debug("binding[%d] %d %s",
                  ct, binding->tid, TIDNAME(binding));

        if (binding->tid == TK_Binding) {
            if (ct == index)
                return binding;
            ct++;
        }
    }
    return NULL;
}

/* **************** */
EXPORT
char *sealark_loadstmt_src_node(struct node_s *load_stmt)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_src");
#endif

    assert(load_stmt->tid == TK_Load_Stmt);

    struct node_s *src = utarray_eltptr(load_stmt->subnodes, 2);
    assert(src->tid == TK_STRING);
    return src->s;
}
