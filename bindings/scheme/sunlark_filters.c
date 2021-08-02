#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_filters.h"

/*
  filter package targets by list
*/
//FIXME: this should be in sealark, but select_list is a list...
// todo: pass it as string array? no, it may contain symbols
UT_array *sunlark_targets_from_filterlist(s7_scheme *s7,
                                             struct node_s *bf_node,
                                             s7_pointer filter_list)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_FILTERS)
    log_debug("sunlark_targets_from_filterlist: %s",
              s7_object_to_c_string(s7, filter_list));
#endif

    struct node_s *stmt_list = utarray_eltptr(bf_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);
    log_debug("small_list child ct: %d",
              utarray_len(small_list->subnodes));

    // each call_expr is wrapped in expr_list

    UT_array *target_list;
    utarray_new(target_list, &node_icd);
    int target_ct = 0;

    struct node_s *exprs=NULL;
    struct node_s *target;
    log_debug("SEARCHING %s (%d), child ct: %d for %s (%d)",
              TIDNAME(small_list),
              small_list->tid,
              utarray_len(small_list->subnodes),
              token_name[TK_Expr_List][0],
              TK_Expr_List);
    int i = 0;
    while((exprs
           =(struct node_s*)utarray_next(small_list->subnodes, exprs))) {
        log_debug("  LOOP (fetch) %d: %s (%d)",
                  i++, TIDNAME(exprs), exprs->tid);
        if (exprs->tid == TK_Expr_List) {
            target = utarray_eltptr(exprs->subnodes, 0);
            // target is TK_Call_Expr
            node_s *binding = sealark_target_binding_for_key(target, "name");
            if (binding) {
                /* this is a target call expr; check against list */
                if (_target_satisfies_filter_criteria(s7,
                                                      target,
                                                      target_ct,
                                                      filter_list)) {
                    utarray_push_back(target_list, target);
                }
                target_ct++;
            }
        } else {
            /* ignore non-targets */
        }
    }
    return target_list;
}

/* ******************************************************* */
//FIXME: most of this code duplicates sunlark_targets_from_filterlist above
struct node_s *sunlark_target_for_index_from_filterlist(s7_scheme *s7,
                                              struct node_s *bf_node,
                                              s7_pointer filter_list,
                                                           int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_FILTERS)
    log_debug("sunlark_target_for_index_from_filterlist: %s",
              s7_object_to_c_string(s7, filter_list));
#endif

    struct node_s *stmt_list = utarray_eltptr(bf_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);
    log_debug("small_list child ct: %d",
              utarray_len(small_list->subnodes));

    // each call_expr is wrapped in expr_list

    UT_array *target_list;
    utarray_new(target_list, &node_icd);
    int target_ct = 0;

    struct node_s *exprs=NULL;
    struct node_s *target;
    log_debug("SEARCHING %s (%d), child ct: %d for %s (%d)",
              TIDNAME(small_list),
              small_list->tid,
              utarray_len(small_list->subnodes),
              token_name[TK_Expr_List][0],
              TK_Expr_List);
    int i = 0;
    while((exprs
           =(struct node_s*)utarray_next(small_list->subnodes, exprs))) {
        log_debug("  LOOP (fetch) %d: %s (%d)",
                  i++, TIDNAME(exprs), exprs->tid);
        if (exprs->tid == TK_Expr_List) {
            target = utarray_eltptr(exprs->subnodes, 0);
            // target is TK_Call_Expr
            node_s *binding = sealark_target_binding_for_key(target, "name");
            if (binding) {
                /* this is a target call expr; check against list */
                if (_target_satisfies_filter_criteria(s7,
                                                      target,
                                                      target_ct,
                                                      filter_list)) {
                    log_debug("PUSHING MATCH");
                    utarray_push_back(target_list, target);
                }
                target_ct++; // no need for this?
            }
        } else {
            /* ignore non-targets */
        }
    }
    log_debug("found %d targets satisfying filter, indexing: %d",
              utarray_len(target_list), index);
    //FIXME: copy the node so we can free target_list
    struct node_s *result = utarray_eltptr(target_list, index);
    return result;
}

/* *************************************************************** */
struct node_s *sunlark_target_for_index_from_filtersym(s7_scheme *s7,
                                             struct node_s *bf_node,
                                              const char *filtersym,
                                                          int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_FILTERS)
    log_debug("sunlark_target_for_index_from_filtersym: %d, %s",
              index, filtersym);
#endif

    /* s7_pointer filter_list = s7_list(s7, 1, */
    /*     op2, s7_nil(s7)); */

    struct node_s *stmt_list = utarray_eltptr(bf_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);
    log_debug("small_list child ct: %d",
              utarray_len(small_list->subnodes));

    // each call_expr is wrapped in expr_list

    /* UT_array *target_list; */
    /* utarray_new(target_list, &node_icd); */
    int target_ct = 0;

    struct node_s *exprs=NULL;
    struct node_s *target;
    log_debug("SEARCHING %s (%d), child ct: %d for %s (%d)",
              TIDNAME(small_list),
              small_list->tid,
              utarray_len(small_list->subnodes),
              token_name[TK_Expr_List][0],
              TK_Expr_List);

    int i = 0;
    while((exprs
           =(struct node_s*)utarray_next(small_list->subnodes, exprs))) {
        log_debug("  LOOP (fetch) %d: %s (%d), target_ct: %d",
                  i++, TIDNAME(exprs), exprs->tid, target_ct);
        if (exprs->tid == TK_Expr_List) {
            target = utarray_eltptr(exprs->subnodes, 0);
            // target is TK_Call_Expr
            node_s *binding = sealark_target_binding_for_key(target, "name");
            if (binding) {
                log_debug("call expr is target");
                /* this is a target call expr; check against filter */
                if (sealark_target_is_rule_kind(target, filtersym)) {
                    log_debug("rule match %s", filtersym);
                    log_debug("target ct: %d, index: %d", target_ct, index);
                    if (target_ct == index) {
                        log_debug("MATCHED index %d", index);
                        return target;
                    }
                    target_ct++;
                }
            }
        } else {
            /* ignore non-targets */
        }
    }
    return NULL;
}

/* *************************************************************** */
struct node_s *sunlark_bindings_for_target_for_index_from_filtersym(s7_scheme *s7,
                                             struct node_s *bf_node,
                                               s7_pointer filtersym,
                                                          int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_FILTERS)
    log_debug("sunlark_target_for_index_from_filtersym: %d, %s",
              index, s7_symbol_name(filtersym));
#endif

    /* s7_pointer filter_list = s7_list(s7, 1, */
    /*     op2, s7_nil(s7)); */

    struct node_s *stmt_list = utarray_eltptr(bf_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);
    log_debug("small_list child ct: %d",
              utarray_len(small_list->subnodes));

    // each call_expr is wrapped in expr_list

    /* UT_array *target_list; */
    /* utarray_new(target_list, &node_icd); */
    int target_ct = 0;

    struct node_s *exprs=NULL;
    struct node_s *target;
    log_debug("SEARCHING %s (%d), child ct: %d for %s (%d)",
              TIDNAME(small_list),
              small_list->tid,
              utarray_len(small_list->subnodes),
              token_name[TK_Expr_List][0],
              TK_Expr_List);

    int i = 0;
    while((exprs
           =(struct node_s*)utarray_next(small_list->subnodes, exprs))) {
        log_debug("  LOOP (fetch) %d: %s (%d), target_ct: %d",
                  i++, TIDNAME(exprs), exprs->tid, target_ct);
        if (exprs->tid == TK_Expr_List) {
            target = utarray_eltptr(exprs->subnodes, 0);
            // target is TK_Call_Expr
            node_s *binding = sealark_target_binding_for_key(target, "name");
            if (binding) {
                log_debug("call expr is target");
                /* this is a target call expr; check against filter */
                if (sealark_target_is_rule_kind(target,
                                                s7_symbol_name(filtersym))) {
                    log_debug("rule match %s", s7_symbol_name(filtersym));
                    log_debug("target ct: %d, index: %d", target_ct, index);
                    if (target_ct == index)
                        return target;
                    target_ct++;
                }
            }
        } else {
            /* ignore non-targets */
        }
    }
    return NULL;
}

/* ***************************************************** */
bool _target_satisfies_filter_criteria(s7_scheme *s7,
                                       struct node_s *target,
                                       int target_ct,
                                       s7_pointer select_list)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_FILTERS)
    log_debug("_target_satisfies_filter_criteria %s",
              s7_object_to_c_string(s7, select_list));
#endif

    s7_pointer predicate, predicates = select_list;

    while ( !s7_is_null(s7, predicates) ) {
        predicate = s7_car(predicates);
        if (s7_is_symbol(predicate)) {
            log_debug("symbol predicate: %s", s7_symbol_name(predicate));
            if (sealark_target_is_rule_kind(target,
                                            s7_symbol_name(predicate)))
                return true;
        }
        if (s7_is_string(predicate)) {
            log_debug("string predicate: %s", s7_string(predicate));
            if (sealark_target_has_name(target,
                                        s7_string(predicate)))
                return true;
        }
        if (s7_is_integer(predicate)) {
            if (target_ct == s7_integer(predicate))
                return true;
        }
        predicates = s7_cdr(predicates);
    }
    return false;
}
