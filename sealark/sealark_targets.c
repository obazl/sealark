#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_targets.h"

EXPORT UT_array *sealark_targets_for_buildfile(struct node_s *buildfile_node)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_targets_for_buildfile");
#endif
    // :build-file > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *stmt_list = utarray_eltptr(buildfile_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);
    /* log_debug("small_list child ct: %d", utarray_len(small_list->subnodes)); */

    // each call_expr is wrapped in expr_list

    /* target_exprs will be freed when gc calls g_destroy_ast_nodelist? */
    UT_array *target_list;
    utarray_new(target_list, &node_icd);

    struct node_s *exprs=NULL;
    struct node_s *target;
#if defined(DEBUG_UTARRAYS)
    log_debug("SEARCHING %s (%d), child ct: %d for %s (%d)",
              TIDNAME(small_list),
              small_list->tid,
              utarray_len(small_list->subnodes),
              token_name[TK_Expr_List][0],
              TK_Expr_List);
#endif
    int i = 0;
    while((exprs
           =(struct node_s*)utarray_next(small_list->subnodes, exprs))) {
#if defined(DEBUG_UTARRAYS)
        log_debug("  LOOP (fetch) %d: %s (%d)",
                  i++, TIDNAME(exprs), exprs->tid);
#endif
        if (exprs->tid == TK_Expr_List) {
            target = utarray_eltptr(exprs->subnodes, 0);
            // target is TK_Call_Expr
            node_s *binding = sealark_target_binding_for_key(target, "name");
            if (binding) {
                utarray_push_back(target_list, target);
            }
        } else {
            /* ignore non-targets */
        }
    }

    /* FIXME: put this in a node->subnodes. need new node type?
       TK_Targets? would have to be virtual since targets may be
       interspersed with other productions in a file. */
    return target_list;
}

/* **************************************************************** */
EXPORT
struct node_s *sealark_target_for_name(struct node_s *build_file,
                                      const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PATHS)
    log_debug("sealark_target_for_name: %s", name);
#endif

    struct node_s *stmt_list = utarray_eltptr(build_file->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    // :build-file > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *expr_nd=NULL;
    struct node_s *target_nd=NULL;

    int name_len = strlen(name);

    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {

        while( (target_nd=(struct node_s*)utarray_next(expr_nd->subnodes,
                                                       target_nd)) ) {
            if (target_nd->tid == TK_Call_Expr) {
                /* is this a proper target? */
                bool ok = sealark_target_has_binding(target_nd,
                                                    "name", name);
                if (ok) {
                    /* log_debug("found target for name: %s", name); */
                    return target_nd;
                }
            }
        }
    }
    log_debug("no!");
    return NULL;
}

/* **************************************************************** */
EXPORT
struct node_s *sealark_target_for_index(struct node_s *build_file, int i)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_for_index %d", i);
#endif

    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    struct node_s *stmt_list = utarray_eltptr(build_file->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    // each call_expr is wrapped in expr_list
    /* we cannot just take the ith child, we have to take the ith
       target. toplevel children include load stmts, etc. */
    /* struct node_s *expr_list = utarray_eltptr(small_list->subnodes, i); */

    int target_ct = 0;

    // :build-file > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *expr_nd=NULL;
    struct node_s *target_nd=NULL;

    /*
      if index i < 0, we have no way of knowing target ct without
      iterating. then we have to iterate again to get the item.
     */

    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {

        while( (target_nd=(struct node_s*)utarray_next(expr_nd->subnodes,
                                                       target_nd)) ) {
            if (target_nd->tid == TK_Call_Expr) {
                if (sealark_target_has_binding_key(target_nd, "name")) {
                    log_debug("target_ct: %d, i: %d", target_ct, i);
                    if (target_ct == i) {
                        log_debug("MATCH");
                        return target_nd;
                    }
                    target_ct++;
                }
            }
        }
    }
    log_debug("target_ct: %d", target_ct);

    if (i < 0) {
        if (abs(i) > target_ct) {
            log_error("abs(%d) > target count", i);
            errno = 3;
            return NULL;
        } else {
            i = target_ct + i;
            return sealark_target_for_index(build_file, i);
        }
    }

    if (i > target_ct-1) {
        log_error("index > target count");
        errno = 2;              /* FIXME */
   }

    return NULL;
    /* if (nd) { */
    /* } else { */
    /*     return NULL; */
    /* } */
}
