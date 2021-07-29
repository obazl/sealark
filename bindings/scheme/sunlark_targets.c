#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_targets.h"

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
EXPORT s7_pointer sunlark_dispatch_on_target(s7_scheme *s7,
                                      s7_pointer _target,
                                      s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_TARGETS)
    log_debug("sunlark_dispatch_on_target: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    struct node_s *target = s7_c_object_value(_target);

    if (target->tid != TK_Call_Expr) {
        log_error("Expected node tid %d, got %d %s", TK_Call_Expr,
                  target->tid, TIDNAME(target));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    int op_count = s7_list_length(s7, path_args);
    s7_pointer op = s7_car(path_args);
    log_debug("op: %s", s7_object_to_c_string(s7, op));

    s7_pointer rest = s7_cdr(path_args);

    /* s7_pointer result_list; */

    /* switch(op_count) { */
    /* case 0: */
    /*     return _target; */
    /* case 1: */

    if (s7_is_null(s7, op))
        return _target;

    if (op == KW(@) || op == KW(bindings)) {
        if (s7_is_null(s7, rest)) {
            struct node_s *bindings = sealark_bindings_for_target(target);
            return sunlark_node_new(s7, bindings);
            /* return nodelist_to_s7_list(s7, bindings); */
        } else {
           return sunlark_target_binding_for_path(s7, target, rest);
        }
    }
    if (KW(name) == op) {   /* i.e. target "name" attr val */
            struct node_s *id=sealark_target_name(target);
            return sunlark_node_new(s7, id);
        }
        if (KW(rule) == op) {
            struct node_s *id=sealark_ruleid_for_target(target);
            return sunlark_node_new(s7, id);
        }
        if (KW(arg-list) == op) {
            struct node_s *arg_list=sealark_arglist_for_target(target);
            return nodelist_to_s7_list(s7, arg_list->subnodes);
        }
        /* common properties */
        s7_pointer result = sunlark_common_property_lookup(s7, target, op);
        if (result) return result;

        log_error("dispatch on %s for target not yet implemented",
                  s7_object_to_c_string(s7, op));
    /*     break; */
    /* case 2: */
    /*     // :bindings 'sym -- returns node for binding with name sym */
    /*     // :bindings :count */
    /*     break; */
    /* default: */
    /*     log_error("too many path steps"); */
    /*     exit(EXIT_FAILURE);     /\* FIXME *\/ */
    /* } */


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

/* first step of path was :>> (or :targets) */
EXPORT s7_pointer sunlark_forall_targets(s7_scheme *s7,
                                         struct node_s *bf_node,
                                         s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_TARGETS)
    log_debug("sunlark_forall_targets: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    s7_pointer op = s7_car(path_args);

    if (s7_is_null(s7, path_args)) { /* (:>>) */
        UT_array *targets = sealark_targets_for_buildfile(bf_node);
        return nodelist_to_s7_list(s7, targets);
    }

    /* resolved so far: :>> (:targets) */
    if (op == KW(@@) || op == KW(bindings)) {
        /* (:>> :@@ ...) */
        return sunlark_forall_targets_forall_bindings(s7, bf_node, s7_cdr(path_args));
    }

    /* no other kws here */
    if (s7_is_keyword(op)) {
        log_error("Keyword %s not allowed in this context; try :@@ or its synonyms :bindings, :attrs", s7_object_to_c_string(s7, op));
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                            "Keyword ~A not allowed in this context; try :@@ or its synonyms :bindings, :attrs"),
                                    op)));
    }

    /* filter by rule sym */
    if (s7_is_symbol(op)) { // NB: a keyword is a symbol!
        s7_pointer filter_list = s7_list(s7, 1,
                                         op, s7_nil(s7));
        UT_array *l = sunlark_targets_from_filterlist(s7,
                                                      bf_node,
                                                      filter_list);
        return nodelist_to_s7_list(s7, l);
    }

    /* DISALLOW? target for index. use :> instead of :>> */
    if (s7_is_integer(op)) {
        log_error("Bad arg %d in %s; did you mean :target ?",
                  s7_integer(op), s7_object_to_c_string(s7, path_args));
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 3, s7_make_string(s7,
               "Bad arg ~D in ~A: int index may not follow :targets; did you mean :target?"),
                                op, path_args)));


        /* log_debug("target_for_index %d", s7_integer(op)); */
        /* /\* struct node_s *n = utarray_eltptr(bf_node->subnodes, *\/ */
        /* /\*                                   s7_integer(op)); *\/ */
        /* errno = 0; */
        /* struct node_s *n = sealark_target_for_index(bf_node, */
        /*                                             s7_integer(op)); */
        /* if (n == NULL) { */
        /*     switch(errno) { */
        /*     case 2: */
        /*         return(s7_error(s7, */
        /*                         s7_make_symbol(s7, "invalid_index"), */
        /*                         s7_list(s7, 2, s7_make_string(s7, "index ~D > target count"), */
        /*                                 op))); */
        /*         break; */
        /*     case 3: */
        /*         return(s7_error(s7, */
        /*                         s7_make_symbol(s7, "invalid_index"), */
        /*                         s7_list(s7, 2, */
        /*                                 s7_make_string(s7, */
        /*                                                "abs(~D) > target count"), */
        /*                                 op))); */
        /*     } */
        /* } */
        /* s7_pointer result_list = sunlark_node_new(s7, n); */
        /* return result_list; */
    }

    if (s7_is_list(s7, op)) {
        log_debug("filtering targets by list %s",
                  s7_object_to_c_string(s7, op));
        UT_array *tgts = sunlark_targets_from_filterlist(s7,
                                                         bf_node,
                                                         op);
        return nodelist_to_s7_list(s7, tgts);
    }
    if (s7_is_string(op)) {
        log_error("String arg after :targets not supported; did you mean ':target'?");
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                                                      "String arg \"~A\" after :targets not supported; did you mean ':target'?"),
                                op)));
    }
    log_error("NOT SUPPORTED op: %s", s7_object_to_c_string(s7, op));
    return(s7_error(s7,
                    s7_make_symbol(s7, "invalid_argument"),
                    s7_list(s7, 2, s7_make_string(s7,
                                                  "Arg \"~A\" after :targets not supported"),
                            op)));
}

/* path resolved so far: :> or :target
   next step options:
       string - target_for_name
       int    - target_for_index
   (options after :>> or :targets :
       sym    - filter by rule (e.g. 'cc_library)
       list   - filter e.g. '(cc_library cc_test "hello-lib")
 */

EXPORT s7_pointer sunlark_target_select(s7_scheme *s7,
                                        struct node_s *bf_node,
                                        s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_TARGETS)
    log_debug("sunlark_target_select: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    int op_count = s7_list_length(s7, path_args);
    /* log_debug("op count: %d", op_count); */

    s7_pointer op = s7_car(path_args);
    /* s7_pointer op2 = s7_cadr(path_args); */

    s7_pointer result;

    /* resolved so far: :> */
    if (s7_is_string(op)) {
        errno = 0;
        struct node_s *tgt_node = sealark_target_for_name(bf_node,
                                                          s7_string(op));

        if (tgt_node == NULL) {
            return s7_nil(s7);
            /* if (errno == -1) */
            /*     return(s7_error(s7, */
            /*                     s7_make_symbol(s7, "not_found"), */
            /*                 s7_list(s7, 2, s7_make_string(s7, */
            /*         "Target ~S not found"), */
            /*                         op))); */
        }

        if ( s7_is_null(s7, s7_cdr(path_args)) ) { /* e.g. (:> "mylib") */
            return sunlark_node_new(s7, tgt_node);
        } else {
            /* e.g. (:> "mylib" :@ ...), (:> "mylib" :rule), etc. */
            return sunlark_target_1(s7, tgt_node,
                                      s7_cdr(path_args));
        }

    }
    if (s7_is_integer(op)) {
        struct node_s *tgt_node = sealark_target_for_index(bf_node,
                                                         s7_integer(op));
        if ( s7_is_null(s7, s7_cdr(path_args)) ) { /* e.g. (:> 0) */
            return sunlark_node_new(s7, tgt_node);
        } else {
            /* e.g. (:> 1 :@ ...), (:> 1 :rule) */
            return sunlark_target_1(s7, tgt_node,
                                      s7_cdr(path_args));
        }

    }
    log_error("Bad arg: %s after :target", s7_object_to_c_string(s7, op));
    return(s7_error(s7,
                    s7_make_symbol(s7, "invalid_argument"),
                    s7_list(s7, 2, s7_make_string(s7,
                     "Bad arg ~S after target (only string or int allowed)"),
                            op)));
}

/*
  already resolved: 1 target (e.g. (:> "hello"), (:> 1))
  path_args: :@, :rule, :name
 */
EXPORT s7_pointer sunlark_target_1(s7_scheme *s7,
                                   struct node_s *tgt_node,
                                   s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_TARGETS)
    log_debug("sunlark_target_1: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    int op_count = s7_list_length(s7, path_args);
    log_debug("op count: %d", op_count);

    if (op_count == 0)
        return sunlark_node_new(s7, tgt_node);

    s7_pointer op = s7_car(path_args);
    s7_pointer rest = s7_cdr(path_args);
    s7_pointer op2 = s7_car(rest);

    s7_pointer result;

    /* resolved so far: 1 target e.g. (:> "hello"...) */
    /* allowed path op: :@, :@@, :rule, :name */
    if (op==KW(@)||op==KW(binding)||op==KW(attr)) {
        if ( s7_is_null(s7, rest) ) { /* e.g. (:> "mylib" :@) */
            log_error("Missing arg: %s must be followed by a binding selector expression (or use :@@ to select all bindings).", s7_object_to_c_string(s7, op));
            return(s7_error(s7,
                            s7_make_symbol(s7, "missing_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                                                          "Missing arg: ~S must be followed by a binding selector expression (or: use :@@ to select all bindings)."),
                                    op)));
        }
        if (s7_is_symbol(op2)) {
            if (op_count == 2) { // FIXME: eliminate
                errno = 0;
                struct node_s *binding
                    = sealark_target_binding_for_key(tgt_node,
                                                     s7_symbol_name(op2));
                if (binding)
                    return sunlark_node_new(s7, binding);
                else {
                    /* log_warn("Binding %s not found", */
                    /*          s7_object_to_c_string(s7, path_args)); */
                    return s7_f(s7);
                }
            } else {
                return sunlark_target_binding_for_path(s7,
                                                       tgt_node,
                                                       rest);
            }
        }
        if (s7_is_integer(op2)) {
            if (op_count == 2) {
                struct node_s *binding
                    = sealark_target_binding_for_index(tgt_node,
                                                       s7_integer(op2));
                if (binding)
                    return sunlark_node_new(s7, binding);
                else
                    /* r7rs: error */
                    return s7_f(s7); /* same as binding_for_name  */
            } else {
                return sunlark_target_binding_for_path(s7,
                                                       tgt_node,
                                                       rest);
            }
        }
        /* error: only sym or int may follow :@ */
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 1, s7_make_string(s7,
                                                      "Only sym or int allowed here, to select attribute"))));

    }
    if (op==KW(@@)||op==KW(bindings)||op==KW(attrs)) {
        if ( s7_is_null(s7, rest) ) { /* e.g. (:> "mylib" :@@) */
            struct node_s *n = sealark_bindings_for_target(tgt_node);
            return sunlark_node_new(s7, n);
        } else {
            log_error("kw %s must come last in path expression",
                      s7_object_to_c_string(s7, op));
            return(s7_error(s7,
                            s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                            "Keyword ~A must come last in path expression."),
                                    op)));
        }
    }
    log_error("Allowed ops in this context: :@@, :bindings, :attrs; :@, :binding, :attr");
    return(s7_error(s7,
                    s7_make_symbol(s7, "invalid_argument"),
                    s7_list(s7, 2, s7_make_string(s7,
      "Allowed ops in this context: :@@, :bindings, :attrs; :@, :binding, :attr"))));
}
