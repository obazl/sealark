#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "sealark_targets.h"

EXPORT UT_array *sealark_targets_for_pkg(struct node_s *pkg)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_targets_for_pkg");
#endif
    assert(pkg->tid == TK_Package);
    // :package > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
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

/* ********************************* */
EXPORT
struct node_s *sealark_target_for_name(struct node_s *package,
                                      const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PATHS)
    log_debug("sealark_target_for_name: %s", name);
#endif
    struct node_s *t = _target_for_predicate(package, name, -1);
    if (t)
        return t;
    else
        return NULL;
}

/* ********************************** */
//FIXME: rename sealark_pkg_target_for_int
EXPORT
struct node_s *sealark_target_for_index(struct node_s *package, int i)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_for_index %d", i);
#endif
    return _target_for_predicate(package, NULL, i);
}

/* **************************************************************** */
LOCAL struct node_s *_target_for_predicate(struct node_s *package,
                                           const char *name, int i)
{
#if defined (DEBUG_TRACE)
    log_debug("_target_for_predicate: name %s, index %d", name, i);
#endif

    /* if build file starts with comment lines, then the ast will look
       like:
0: TK_Package 96
   1: TK_Stmt_List 131
     2: TK_Stmt 130
       3: TK_COMMENT 16: # comment 1
     2: TK_Stmt 130
       3: TK_COMMENT 16: ## comment 2
     2: TK_Small_Stmt_List 129
       3: TK_Expr_List 109
       ...  # load stmts, targets ...

       otherwise:
 0: TK_Package 96
   1: TK_Stmt_List 131
     2: TK_Small_Stmt_List 129
       3: TK_Expr_List 109   (package directive)
       ...
       3: TK_Load_Stmt 117
       ...
       3: TK_Expr_List 109
         4: TK_Call_Expr 97  (target)
       ...

  so to get targets we need to skip this initial comments (TK_stmt)
    */

    struct node_s *node = NULL;
    struct node_s *expr_nd=NULL;
    struct node_s *target_nd=NULL;

    int name_len;
    if (name) name_len = strlen(name);

    int target_ct = 0;

    struct node_s *stmt_list = utarray_eltptr(package->subnodes, 0);

    /* struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0); */

    // each call_expr is wrapped in expr_list
    /* we cannot just take the ith child, we have to take the ith
       target. toplevel children include load stmts, etc. */

    /*
      indexing by int: if index i < 0, we have no way of knowing
      target ct without iterating. then we have to iterate again to
      get the item.
     */

    while( (node=(struct node_s*)utarray_next(stmt_list->subnodes,
                                              node)) ) {
        /* log_debug("tid: %d %s", node->tid, TIDNAME(node)); */
        if (node->tid == TK_Stmt) { // comment lines at start of file
            continue;
        }
        if (node->tid != TK_Small_Stmt_List) {
            log_error("Unexpected node type %d %s; expected %d %s",
                      node->tid, TIDNAME(node),
                      TK_Small_Stmt_List, token_name[TK_Small_Stmt_List][0]);
            return NULL;
        }

        while( (expr_nd=(struct node_s*)utarray_next(node->subnodes,
                                                     expr_nd)) ) {

            if (expr_nd->tid != TK_Expr_List) {
                continue;
            }
            if (utarray_len(expr_nd->subnodes) > 1) {
                log_error("Unexpected number of subnodes for TK_Expr_List: %d",
                          utarray_len(expr_nd->subnodes));
                exit(EXIT_FAILURE);
            }

            target_nd = utarray_front(expr_nd->subnodes);
            /* while( (target_nd=(struct node_s*)utarray_next(expr_nd->subnodes, */
            /*                                     target_nd)) ) { */

            if (name) {     /* index by name */
                bool ok = sealark_target_has_binding(target_nd,
                                                     "name", name);
                if (ok) {
                    /* log_debug("found target for name: %s", name); */
                    return target_nd;
                }
            } else {        /* index by int */
                /* is this a proper target? */
                if (sealark_target_has_binding_key(target_nd, "name")) {
                    /* log_debug("tgt_ct: %d i: %d", tgt_ct, i); */
                    if (target_ct == i) {
                        /* log_debug("MATCH"); */
                        return target_nd;
                    }

                    target_ct++;
                }
            }
            /* } */
        }
    }
    /* log_debug("target_ct: %d", target_ct); */

    if (name == NULL) {         /* we're indexing by int */
        /* reverse indexing */
        if (i < 0) {
            if (abs(i) > target_ct) {
                log_error("abs(%d) > target count", i);
                errno = EINDEX_OUT_OF_BOUNDS;
                return NULL;
            } else {
                i = target_ct + i;
                return sealark_target_for_index(package, i);
            }
        }

        if (i > target_ct-1) {
            log_error("index > target count");
            errno = EINDEX_OUT_OF_BOUNDS;
        }
    }
    if (name) {                   /* we had a name but could not match it */
        log_warn("target %s not found", name);
        errno = ENOT_FOUND;
        return NULL; /* not found */
    }
    /* should not reach here? */
    log_error("UNEXPECTED FALLTHRU");
}


EXPORT int sealark_target_bindings_count(struct node_s *target)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_bindings_count");
#endif

    UT_array *bindings = sealark_proc_bindings_to_utarray(target);
    int ct = utarray_len(bindings);
    utarray_free(bindings);
    return ct;
}

/* **************** */
EXPORT struct node_s *sealark_target_name(struct node_s *target)
{
/* #if defined (DEBUG_TRACE) || defined(DEBUG_QUERY) */
/*     log_debug("sealark_target_name"); */
/* #endif */

    assert(target->tid == TK_Call_Expr);

    /* :call-expr[1] > :call-sfx[1] > :arg-list */
    /* :arg-list - for targets, list of bindings */
    /*   for ordinary fn application could be anything */

    UT_string *buf;
    utstring_new(buf);
    sealark_display_node(target, buf, 0);
    utstring_free(buf);

    struct node_s *call_sfx = utarray_eltptr(target->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

#if defined(DEBUG_UTARRAYS)
    log_debug("SEARCHING arg_list %d %s, child ct: %d",
              arg_list->tid,
              token_name[arg_list->tid][0],
              utarray_len(arg_list->subnodes));
#endif
    struct node_s *arg_node = NULL;
    struct node_s *id  = NULL;
    struct node_s *val = NULL;
    int i = 0;
    while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  arg_node))) {
#if defined(DEBUG_UTARRAYS)
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++,
                  arg_node->tid, token_name[arg_node->tid][0]);
#endif

        if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes
            /* first subnode is TK_ID */
            id = utarray_eltptr(arg_node->subnodes, 0);
#if defined(DEBUG_UTARRAYS)
            log_debug("testing id[%d]: %d %s", i, id->tid, id->s);
#endif
            if ((strncmp(id->s, "name", 4) == 0)
                && strlen(id->s) == 4 ){
                val = utarray_eltptr(arg_node->subnodes, 2);
                return val;
            }
        } else {
            log_debug("0 xxxxxxxxxxxxxxxx");
        }
    }
    return NULL;
}
