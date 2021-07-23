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
  filter build_file targets by list
*/
//FIXME: this should be in sealark, but select_list is a list...
// todo: pass it as string array? no, it may contain symbols
UT_array *sunlark_filter_bf_targets_by_list(s7_scheme *s7,
                                             struct node_s *bf_node,
                                             s7_pointer select_list)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_FILTERS)
    log_debug("sunlark_bf_targets_by_list: %s",
              s7_object_to_c_string(s7, select_list));
#endif

    struct node_s *stmt_list = utarray_eltptr(bf_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);
    log_debug("small_list child ct: %d",
              utarray_len(small_list->subnodes));

    // each call_expr is wrapped in expr_list

    UT_array *target_list;
    utarray_new(target_list, &node_icd);

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
            node_s *binding = sealark_get_call_binding_by_name(target, "name");
            if (binding) {
                /* this is a target call expr; check against list */
                if (_target_satisfies_filter_criteria(s7,
                                                      target,
                                                      select_list)) {
                    utarray_push_back(target_list, target);
                }
            }
        } else {
            /* ignore non-targets */
        }
    }
    return target_list;
}

bool _target_satisfies_filter_criteria(s7_scheme *s7,
                                       struct node_s *target,
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
            log_debug("predicate: %s", s7_symbol_name(predicate));
            if (sealark_target_is_rule_kind(target,
                                            s7_symbol_name(predicate)))
                return true;
        }
        predicates = s7_cdr(predicates);
    }
    return false;
}
