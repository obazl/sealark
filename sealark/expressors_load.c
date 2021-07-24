#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "expressors_load.h"

/* **************************************************************** */
/*
  :build-file > :stmt-list > :small-stmt-list
  > :load-stmt
    > :load, :lparen, :string, :comma, :string, :comma, :string, :rparen
  > :load-stmt
    > :load, :lparen, :string, :comma, :alias > :id, :eq, :string
   **************************************************************** */

//FIXME: don't use this, use sealark_procs_for_id(bf_node, "load");
/* EXPORT UT_array *sealark_loadstmts(struct node_s *buildfile_node) */
/* { */
/* #if defined (DEBUG_TRACE) || defined(DEBUG_QUERY) */
/*     log_debug("sunlark_fetch_load_stmts"); */
/* #endif */

/*     if (buildfile_node->tid != TK_Build_File) { */
/*         log_warn("property :loads only valid for :build-file nodes"); */
/*         return NULL; */
/*     } */

/*     struct node_s *stmt_list = utarray_eltptr(buildfile_node->subnodes, 0); */
/*     struct node_s *smalllist = utarray_eltptr(stmt_list->subnodes, 0); */

/*     /\* load_stmts will be freed when gc calls g_destroy_ast_nodelist? *\/ */
/*     UT_array *load_stmts; */
/*     utarray_new(load_stmts, &node_icd); */

/*     struct node_s *nd=NULL; */
/*     while( (nd=(struct node_s*)utarray_next(smalllist->subnodes, nd)) ) { */
/*         if (nd->tid == TK_Load_Stmt) { */
/*             utarray_push_back(load_stmts, nd); */
/*         } */
/*     } */
/*     /\* log_debug("load-stmt ct: %d", utarray_len(load_stmts)); *\/ */

/*     return load_stmts; */
/* } */

/* **************** */
EXPORT
struct node_s *sealark_loadstmt_for_name(struct node_s *build_file,
                                                   const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_loadstmt_by_name %s", name);
#endif

    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    struct node_s *stmt_list = utarray_eltptr(build_file->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    // :build-file > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *expr_nd=NULL;

    int name_len = strlen(name);

    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {
        log_debug("xnode %d %s", expr_nd->tid, TIDNAME(expr_nd));
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
struct node_s *sealark_loadstmt_by_index(struct node_s *build_file,
                                         int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_loadstmt_by_index %d", index);
#endif

    struct node_s *stmt_list = utarray_eltptr(build_file->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    // :build-file > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *expr_nd=NULL;
    int load_ct = 0;

    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {
        log_debug("xnode %d %s", expr_nd->tid, TIDNAME(expr_nd));
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
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
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
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
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
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
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
