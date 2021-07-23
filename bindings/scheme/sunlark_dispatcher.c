#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_dispatcher.h"

s7_pointer sunlark_dispatch(s7_scheme *s7,
                            s7_pointer data,
                            s7_pointer path_args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_dispatch: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    /* dispatch on tid; the tid handlers will dispatch on op */
    /* data is either a node or an s7 list of nodes  */

    int data_tid;
    if (s7_is_list(s7, data)) {
        // return sunlark_dispatch_on_list ??
        log_warn("dispatching on s7 list");
        exit(EXIT_FAILURE);     /* FIXME */
    }

    data_tid = sunlark_node_tid(s7, data);

    log_debug("\tdata tid: %d %s",
              data_tid,
              token_name[data_tid][0]);

    /* s7_pointer op = s7_car(path_args); */

    switch( data_tid ) {

    case TK_Build_File: /* build_target */
        log_debug("dispatching on TK_Build_File");
        return sunlark_dispatch_on_buildfile(s7, data, path_args);
        break;

    case TK_Call_Expr: /* build_target */
        log_debug("dispatching on TK_Call_Expr");
        return sunlark_dispatch_on_target(s7, data, path_args);
        break;

    default:
        /* common properties work for any tid */
        return sunlark_dispatch_on_any(s7, data, path_args);
        /* log_error("not implemented: dispatch on tid %d", data_tid); */
        /* exit(EXIT_FAILURE);     /\* FIXME *\/ */
    }
}

/** sunlark_dispatch_on_buildfile

  switch(op count) {
  case 1:
      // must be :target, :targets, :load, :loads, :package, ??
      // OR: :call-expr to get targets and fn applications?
      // OR: :toplevel to get all top-levels? or, compose :loads,
      // :targets, etc.?

      // note that load "stmts" package stmts, and targets are all
      // call exprs.

      // targets have bindings but no args; load stmts may have both

      // also note that args may be vectors, e.g. glob(["**"])

      returns s7_list
      break;
  case 2:
      :targets i (integer)
          returns node at index i of targets list
          sunlark_target_by_index_from_targets

      :targets 'sym
          returns s7 list of targets with rulename sym
          sunlark_targets_by_symbol_from_targets

      :targets '(sym string int)
          returns s7 list of targets filtered by rule/name/index
          sunlark_targets_by_filter_from_targets

      :targets <predicate>
          returns s7 list of targets satisfying predicate (a fn)
          sunlark_targets_by_lambda_from_targets

      :target "string"
          returns node for target with name = "string"
          sunlark_target_for_name_from_targets

      :load "string"
          returns node for load stmt whose first arg is string
          => sunlark_load_for_name
      break;

  case 3:

    filter then select
      :targets <selector> :count
          <selector> :: filter list, rule sym
          sunlark_count_targets_from_filter

      :targets <filter> idx
          sunlark_target_by_index_from_filter

    select then express
      :targets 0 :bindings
          sunlark_bindings_for_target_by_index_from_targets

      :targets 0 :rule
          sunlark_rule_for_target_by_index

      :target "string" :bindings
          returns s7_list of bindings without commas
          sunlark_bindings_for_target_by_name_from_targets

      :target "name" :rule
          sunlark_rule_for_target_by_name

  case 4:
      :target <selector> :bindings 'id
          returns node of binding with name = "id"
          sunlark_binding_from_bindings_from_filter_from_targets

      :target <selector> :bindings :count
      break;

 case 5:
     :target <selector> :bindings 'id :key
         sunlark_key_for_binding_from_bindings_from_filter
     :target <selector> :bindings 'id :value
         sunlark_value_for_binding_from_bindings_from_filter

*/

//FIXME: LOCAL
s7_pointer sunlark_dispatch_on_buildfile(s7_scheme *s7,
                                         s7_pointer data,
                                         s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_dispatch_on_buildfile: %s",
              s7_object_to_c_string(s7, path_args));
#endif
    struct node_s *bf_node = s7_c_object_value(data);

    if (bf_node->tid != TK_Build_File) {
        log_error("Expected node tid %d, got %d %s", TK_Build_File,
                  bf_node->tid, TIDNAME(bf_node));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    int op_count = s7_list_length(s7, path_args);
    log_debug("op count: %d", op_count);

    s7_pointer op = s7_car(path_args);
    //s7_pointer op2, op3, op4, op5;
    if ( !s7_is_keyword(op) ) {
        log_error("Path op %s not supported for :build-file nodes",
                  s7_object_to_c_string(s7, op));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    s7_pointer result_list;

    switch(op_count) {
    case 1:
        if (op == KW(targets)) {
            result_list = sunlark_targets_for_buildfile(s7, bf_node);
            return result_list;
        }
       if (op == KW(loads)) {
            // :build-file > :stmt-list :smallstmt-list > load-expr,...
            result_list = sunlark_fetch_load_stmts(s7, bf_node);
        }
       if (op == KW(package)) {
           /* not yet */
           result_list = s7_unspecified(s7);
        }
        break;
    case 2:
        return buildfile_handle_dyadic_path(s7, bf_node, path_args);
        break;
    case 3:
        return buildfile_handle_triadic_path(s7, bf_node, path_args);
        break;
    case 4:
        /* return buildfile_handle_tetradic_path(s7, bf_node, path_args); */
        break;
    case 5:
        /* return buildfile_handle_pentadic_path(s7, bf_node, path_args); */
        break;
    default:
        ;
    }

    /* /\* predicates *\/ */
    /* s7_pointer sym = s7_keyword_to_symbol(s7, op); */
    /* char *key = (char*)s7_symbol_name(sym); */
    /* if (strrchr(key, '?') - key == strlen(key)-1 ) { */
    /*     result_list = sunlark_is_kw(s7, key, bf_node); */
    /* } */

    /* /\* common props *\/ */
    /* result_list = sunlark_common_property_lookup(s7, bf_node, op); */
    /* if (result_list == NULL) { */
    /*     /\* result_list = s7_unspecified(s7); *\/ */
    /*     result_list =(s7_error(s7, s7_make_symbol(s7, */
    /*                                               "invalid_argument"), */
    /*                            s7_list(s7, 2, s7_make_string(s7, */
    /*                                                          "ast-node-ref arg must be one of :package, :loads, :targets; got ~A"), */
    /*                                    op))); */

    /* } */

    /* s7_pointer next_step = s7_cadr(path_args); */
    /* if (s7_is_null(s7, next_step)) { */
    /*     return result_list; */
    /* } else { */
    /*     return sunlark_dispatch(s7, result_list, s7_cdr(path_args)); */
    /* } */
}

/** sunlark_dispatch_on_target

    datum: node of tid :call-expr

  switch(op count) {
  case 1:
      :bindings -- returns s7_list
      :rulename
      // common properties: :tid, :line, :col, etc.
      break;
  case 2:
      :bindings 'sym -- returns node for binding with name sym
      :bindings :count
 */
//FIXME: LOCAL
s7_pointer sunlark_dispatch_on_target(s7_scheme *s7,
                                      s7_pointer data,
                                      s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_dispatch_on_target: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    struct node_s *target = s7_c_object_value(data);

    if (target->tid != TK_Call_Expr) {
        log_error("Expected node tid %d, got %d %s", TK_Call_Expr,
                  target->tid, TIDNAME(target));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    int op_count = s7_list_length(s7, path_args);
    s7_pointer op = s7_car(path_args);

    s7_pointer result_list;

    switch(op_count) {
    case 0:
        log_error("not enough path steps");
        exit(EXIT_FAILURE);     /* FIXME */
    case 1:
        if (KW(arg-list) == op) {
            struct node_s *arg_list=sealark_arglist_for_target(target);
            return nodelist_to_s7_list(s7, arg_list->subnodes);
        }
        if (KW(bindings) == op) {
            UT_array *bindings = sealark_bindings_for_target(target);
            return nodelist_to_s7_list(s7, bindings);
        }
        if (KW(rule) == op) {
            struct node_s *id=sealark_rulename_for_target(target);
            return sunlark_node_new(s7, id);
        }
        /* common properties */
        s7_pointer result = sunlark_common_property_lookup(s7, target, op);
        if (result) return result;

        log_error("dispatch on %s for target not yet implemented",
                  s7_object_to_c_string(s7, op));
        break;
    case 2:
        // :bindings 'sym -- returns node for binding with name sym
        // :bindings :count
        break;
    default:
        log_error("too many path steps");
        exit(EXIT_FAILURE);     /* FIXME */
    }


    // obsol
    /* tmp = sunlark_target_property_lookup(s7, */
    /*                                      s7_c_object_value(self), */
    /*                                      path_arg); */
    /* if (s7_is_c_object(tmp)) { */
    /*     self = tmp; */
    /*     self_tid = sunlark_node_tid(s7, tmp); */
    /* } else { */
    /*     return tmp; */
    /* } */

}

LOCAL s7_pointer sunlark_dispatch_on_any(s7_scheme *s7,
                                         s7_pointer node,
                                         s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_dispatch_on_any: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    /* should be only one path arg? */
    s7_pointer result = sunlark_common_property_lookup(s7,
                                                       s7_c_object_value(node),
                                                       s7_car(path_args));
    return result;
}

/* **************************************************************** */
/* build file handlers */
s7_pointer buildfile_handle_dyadic_path(s7_scheme *s7,
                                        struct node_s *bf_node,
                                        s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("buildfile_handle_dyadic_path: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    /* int op_count = s7_list_length(s7, path_args); */
    /* log_debug("op count: %d", op_count); */

    s7_pointer op = s7_car(path_args);

    if ( !s7_is_keyword(op) ) {
        log_error("Path op %s not supported for :build-file nodes",
                  s7_object_to_c_string(s7, op));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    s7_pointer result_list;


    s7_pointer op2;
    op2 = s7_cadr(path_args);
    if (op == KW(targets)) {
        if (s7_is_symbol(op2)) {
            s7_pointer filter_list = s7_list(s7, 1,
                                             op2, s7_nil(s7));
            UT_array *l = sunlark_targets_from_filterlist(s7,
                                                            bf_node,
                                                            filter_list);
            return nodelist_to_s7_list(s7, l);
        }
        if (s7_is_integer(op2)) {
            log_debug("target_by_index %d", s7_integer(op2));
            /* struct node_s *n = utarray_eltptr(bf_node->subnodes, */
            /*                                   s7_integer(op)); */
            errno = 0;
            struct node_s *n = sealark_target_by_index(bf_node,
                                                       s7_integer(op2));
            if (n == NULL) {
                switch(errno) {
                case 2:
                    return(s7_error(s7,
                                    s7_make_symbol(s7, "invalid_index"),
                                    s7_list(s7, 2, s7_make_string(s7, "index ~D > target count"),
                                            op2)));
                    break;
                case 3:
                    return(s7_error(s7,
                                    s7_make_symbol(s7, "invalid_index"),
                                    s7_list(s7, 2,
                                            s7_make_string(s7,
                                                           "abs(~D) > target count"),
                                            op2)));
                }
            }
            result_list = sunlark_node_new(s7, n);
            return result_list;
        }
        if (s7_is_list(s7, op2)) {
            log_debug("filtering targets by list %s",
                      s7_object_to_c_string(s7, op2));
            UT_array *tgts = sunlark_targets_from_filterlist(s7,
                                                               bf_node,
                                                               op2);
            return nodelist_to_s7_list(s7, tgts);
        }
        if (s7_is_string(op2)) {
            log_error("String arg after :targets not supported; did you mean ':target'?");
            return(s7_error(s7,
                            s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                                                          "String arg \"~A\" after :targets not supported; did you mean ':target'?"),
                                    op2)));
        }
    }
    /* **************** */
    if (op == KW(target)) {
        if (s7_is_string(op2)) {
            struct node_s *n = sealark_target_for_name(bf_node,
                                                       s7_string(op2));
            /* struct node_s *n = sealark_target_for_name(bf_node, */
            /*                                            s7_string(op2)); */
            return sunlark_node_new(s7, n);
        }
        // only strings after :target
        log_error("Only string arg after :target");
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                      "Only string arg allowed after :target; got ~A"),
                                    op2)));
    }
    /* **************** */
    if (op == KW(load)) {
        if (s7_is_string(op2)) {
            log_debug("load_for_name");
            return NULL;
        }
        // only strings after :target
        log_error("Only string arg after :load");
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                      "Only string arg allowed after :load; got ~A"),
                                    op2)));
    }
    /* **************** */
    if (op == KW(loads)) {
        if (s7_is_integer(op2)) {
            log_debug("load_by_index_from_loads");
            return NULL;
        }
        if (s7_is_list(s7, op2)) {
            log_debug("loads_from_filterlist");
            /* UT_array *tgts = sunlark_targets_from_filterlist(s7, */
            /*                                                    bf_node, */
            /*                                                    op2); */
            /* return nodelist_to_s7_list(s7, tgts); */
            return NULL;
        }
        if (op2 == KW(count)) {
            log_debug("count_loads");
            return NULL;
        }
        // only strings after :target
        log_error("Only integer arg after :loads");
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                      "Only integer arg allowed after :loads; got ~A"),
                                    op2)));
    }
}

/* ************************************************** */
s7_pointer buildfile_handle_triadic_path(s7_scheme *s7,
                                         struct node_s *bf_node,
                                         s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("buildfile_handle_triadic_path: %s",
              s7_object_to_c_string(s7, path_args));
#endif
    /* int op_count = s7_list_length(s7, path_args); */
    /* log_debug("op count: %d", op_count); */

    s7_pointer op = s7_car(path_args);

    if ( !s7_is_keyword(op) ) {
        log_error("Path op %s not supported for :build-file nodes",
                  s7_object_to_c_string(s7, op));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    s7_pointer result_list;


    s7_pointer op2 = s7_cadr(path_args);
    s7_pointer op3 = s7_caddr(path_args);

    /* ******************************** */
    if (op == KW(targets)) {
        if (s7_is_symbol(op2)) {
            if (op3 == KW(count)) {
                /* e.g. :targets 'cc_test :count */
                log_debug("count_of_targets_from_filtersym");
                return NULL;
            }
            if (s7_is_integer(op3)) {
                log_debug("target_by_index_from_filtersym");
                struct node_s *tgt = sunlark_target_by_index_from_filtersym
                    (s7, bf_node, op2, s7_integer(op3));
                     /* s7_pointer select_list); */
                return sunlark_node_new(s7, tgt);
            }
            log_error("Path op %s not supported following :targets ~s",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return NULL;
        }
        /* **************** */
        if (s7_is_list(s7, op2)) {
            if (s7_is_integer(op3)) {
                log_debug("target_by_index_from_filterlist");
                struct node_s *tgt = sunlark_target_by_index_from_filterlist
                    (s7, bf_node, op2, s7_integer(op3));
                     /* s7_pointer select_list); */
                return sunlark_node_new(s7, tgt);
            }
            if (op3 == KW(count)) {
                log_debug("count_of_targets_from_filterlist");
                return NULL;
            }
            log_error("Path op %s not supported following :targets %s",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return(s7_error(s7,
                            s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                  "Path op ~A not supported following :targets and filter"),
                                    op3)));
            return NULL;
        }
        /* **************** */
        if (s7_is_integer(op2)) {
            if (op3 == KW(bindings)) {
                log_debug("bindings_for_target_by_index");
                errno = 0;
                UT_array *bindings = sealark_bindings_for_target_by_index
                    (bf_node, s7_integer(op2));
                result_list = nodelist_to_s7_list(s7, bindings);
                return result_list;

            }
            if (op3 == KW(arg-list)) {
                log_debug("arg_list_for_target_by_index");
                errno = 0;
                struct node_s *arglist = sealark_arglist_for_target_by_index
                    (bf_node, s7_integer(op2));
                result_list = nodelist_to_s7_list(s7, arglist->subnodes);
                return result_list;

            }
            if (op3 == KW(rule)) {
                log_debug("rulename_for_target_by_index");
                return NULL;
            }
            log_error("Not yet supported: %s", s7_object_to_c_string(s7, op3));
            exit(EXIT_FAILURE);
        }
        /* **************** */
        if (s7_is_string(op2)) {
            log_error("String arg after :targets not supported; did you mean ':target'?");
            return(s7_error(s7,
                            s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                                                          "String arg \"~A\" after :targets not supported; did you mean ':target'?"),
                                    op2)));
        }
        /* **************** */
        // op not supported..
    }

    /* ******************************** */
    if (op == KW(target)) {
        if (s7_is_string(op2)) {
            if (op3 == KW(bindings)) {
                log_debug("bindings_for_target_by_name");
                return NULL;
            }
            if (op3 == KW(rule)) {
                log_debug("rule_for_target_by_name");
                return NULL;
            }
        }
        // only strings after :target
        log_error("Only string arg after :target");
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                             "Only string arg allowd after :target; got ~A"),
                                    op2)));
    }

    /* ******************************** */
    if (op == KW(loads)) {
        if (s7_is_list(s7, op2)) {
            if (op3 == KW(count)) {
                log_debug("counting filtered load stmts");
                return NULL;
            }
            log_error("Path op %s not supported following :loads ~s",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return NULL;
        }
        if (s7_is_integer(op2)) {
            if (op3 == KW(key)) {
                log_debug("key_for_load_by_index_from_loads");
                return NULL;
            }
            if (op3 == KW(args)) {
                log_debug("args_for_load_by_index_from_loads");
                return NULL;
            }
            if (op3 == KW(bindings)) {
                log_debug("bindings_for_load_by_index_from_loads");
                return NULL;
            }
            log_error("Path op %s not supported following :loads ~s",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return NULL;
        }
        if (s7_is_string(op2)) {
            log_error("String op %s not supported following :loads",
                      s7_object_to_c_string(s7, op3));
            return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                            "String op not supported following :loads; got ~S"),
                                    op2)));
            return NULL;
        }
    }

    /* ******************************** */
    if (op == KW(load)) {
        if (s7_is_string(op2)) {
            if (op3 == KW(key)) {
                log_debug("key_for_load_by_name");
                return NULL;
            }
            if (op3 == KW(args)) {
                log_debug("args_for_load_by_name");
                return NULL;
            }
            if (op3 == KW(bindings)) {
                log_debug("bindings_for_load_by_name");
                return NULL;
            }
            log_error("Path op %s not supported following :loads ~s",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return NULL;
        }
        if (s7_is_list(s7, op2)) {
            if (op3 == KW(count)) {
                log_debug("counting filtered load stmts");
                return NULL;
            }
            log_error("Path op %s not supported following :loads ~s",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return NULL;
        }
    }
}
