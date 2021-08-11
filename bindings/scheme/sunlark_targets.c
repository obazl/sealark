#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_targets.h"

/** sunlark_target_dispatcher

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
EXPORT s7_pointer sunlark_target_dispatcher(s7_scheme *s7,
                                            struct node_s *target,
                                            /* s7_pointer _target, */
                                            s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_TARGETS)
    log_debug("sunlark_target_dispatcher: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    assert(target->tid == TK_Call_Expr);
    /* struct node_s *target = s7_c_object_value(_target); */

    if (target->tid != TK_Call_Expr) {
        log_error("Expected node tid %d, got %d %s", TK_Call_Expr,
                  target->tid, TIDNAME(target));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    int op_count = s7_list_length(s7, path_args);
    s7_pointer op = s7_car(path_args);
    /* log_debug("op: %s", s7_object_to_c_string(s7, op)); */

    s7_pointer rest = s7_cdr(path_args);

    /* s7_pointer result_list; */

    /* switch(op_count) { */
    /* case 0: */
    /*     return _target; */
    /* case 1: */

    if (s7_is_null(s7, op))
        return sunlark_new_node(s7,target); //_target;

    if (op == KW(@@) || op == KW(bindings) || op == KW(attrs)) {
        if (s7_is_null(s7, rest)) {
            struct node_s *bindings = sealark_bindings_for_target(target);
            return sunlark_new_node(s7, bindings);
            /* return nodelist_to_s7_list(s7, bindings); */
        } else {
            /* IF we decide to treat :@@ as synonym of :@ */
           /* return _target_binding_dispatcher(s7, target, rest); */
        }
    }

    if (op == KW(@) || op == KW(binding) || op == KW(attr)) {
        if (s7_is_null(s7, rest)) {
            /* treat :@ same as :@@ if no following args */
            struct node_s *bindings = sealark_bindings_for_target(target);
            return sunlark_new_node(s7, bindings);
        } else {
            return _target_binding_dispatcher(s7, target, rest);
        }
        /* if (s7_is_null(s7, rest)) { */

        /* } else { */
        /*    return sunlark_resolve_binding_path_on_target(s7, target, rest); */
        /* } */
    }

    if (KW(name) == op) {   /* i.e. target "name" attr val */
            struct node_s *id=sealark_target_name(target);
            return sunlark_new_node(s7, id);
    }
    if (KW(key) == op) {   /* :key synonym for :name */
            struct node_s *id=sealark_target_name(target);
            return sunlark_new_node(s7, id);
    }
    if (KW(rule) == op) {
        struct node_s *id=sealark_ruleid_for_target(target);
        return sunlark_new_node(s7, id);
    }
    if (KW(arg-list) == op) {
        struct node_s *arg_list=sealark_arglist_for_target(target);
        return nodelist_to_s7_list(s7, arg_list->subnodes);
    }

    /* target length == count of bindings, including 'name' */
    if (KW(length) == op) {
        int ct = sealark_target_bindings_count(target);
        return s7_make_integer(s7, ct);
    }

    /* common properties */
    s7_pointer result = sunlark_common_property_lookup(s7, target, op);
    if (result) return result;

    log_error("dispatch on %s for target not yet implemented",
              s7_object_to_c_string(s7, op));
}

/* ******************************** */
/* resolved path: (:> x :@) */
/* EXPORT s7_pointer sunlark_resolve_binding_path_on_target(s7_scheme *s7, */
//FIXME return struct node_s *
LOCAL s7_pointer _target_binding_dispatcher(s7_scheme *s7,
                                            struct node_s *target,
                                            s7_pointer path_args)
{
#ifdef DEBUG_TRACE
    log_debug("_target_binding_dispatcher: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    assert(target->tid == TK_Call_Expr);

    int op_count = s7_list_length(s7, path_args);
    s7_pointer op = s7_car(path_args);
    /* log_debug("op: %s", s7_object_to_c_string(s7, op)); */

    s7_pointer rest = s7_cdr(path_args);
    s7_pointer op2 = s7_car(rest);

    /* expected args: <int> or <sym <key>? idx?>, where <key> = :key | :value */
    struct node_s *binding;
    if (s7_is_keyword(op)) { /* kws are symbols, so we catch here */
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 3, s7_make_string(s7,
                        "Bad arg: ~S in ~S; expected symbol or int"),
                                op, path_args)));
    }

    if (s7_is_symbol(op)) {
        errno = 0;
        binding = sealark_target_binding_for_key(target, s7_symbol_name(op));
        if (binding) {
            if (s7_is_null(s7, rest)) {
                return sunlark_new_node(s7, binding);
            } else {
                /* if (s7_list_length(s7, rest) == 1) */
                return sunlark_binding_component(s7, binding, rest);
                /* else { */
                /*     log_error("Too many args: %s", */
                /*               s7_object_to_c_string(s7, path_args)); */
                /*     return(s7_error(s7, */
                /*                     s7_make_symbol(s7, "invalid_argument"), */
                /*                     s7_list(s7, 2, s7_make_string(s7, */
                /*                                                   "too many args: ~A"), path_args))); */
                /* } */
            }
        } else {
            return handle_errno(s7, errno, path_args);
           /* if (errno = -1) { */
           /*      log_error("Binding not found for key: %s", s7_symbol_name(op)); */
           /*      return(s7_error(s7, */
           /*                      s7_make_symbol(s7, "not_found"), */
           /*                      s7_list(s7, 2, s7_make_string(s7, */
           /*                      "Binding not found for key: ~A"), op))); */
           /*  } else { */
           /*      log_error("wtf 2 ????????????????"); */
           /*      return NULL; */
           /*  } */
        }
    }

    if (s7_is_integer(op)) {
        binding = sealark_target_binding_for_index(target, s7_integer(op));
        if (binding) {
            if (s7_is_null(s7, rest)) {
                return sunlark_new_node(s7, binding);
            } else {
                return sunlark_binding_component(s7, binding, rest);
                /* else { */
                /*     log_error("Too many args: %s", */
                /*               s7_object_to_c_string(s7, path_args)); */
                /*     return(s7_error(s7, */
                /*                     s7_make_symbol(s7, "invalid_argument"), */
                /*                     s7_list(s7, 2, s7_make_string(s7, */
                /*                                                   "too many args: ~A"), path_args))); */
                /* } */
            }
        } else {
            switch(errno) {
            case -1:
                log_error("Binding not found for key: %s", s7_symbol_name(op));
                return(s7_error(s7,
                                s7_make_symbol(s7, "not_found"),
                                s7_list(s7, 2, s7_make_string(s7,
                                                              "Binding not found for key: ~A"), op)));
                break;
            case EINDEX_TOO_BIG:
                return(s7_error(s7,
                                s7_make_symbol(s7, "invalid_argument"),
                                s7_list(s7, 2, s7_make_string(s7,
                                "Index too big: ~A"), op)));
                break;
            default:
                log_error("wtf 2 ????????????????");
            }
        }


        /* if (s7_is_null(s7, rest)) { */
        /*     return sunlark_new_node(s7, binding); */
        /* } else { */
        /*     if (s7_list_length(s7, rest) == 1) */
        /*         return _binding_component(s7, binding, rest); */
        /*     else { */
        /*         log_error("too many args: %s", */
        /*                   s7_object_to_c_string(s7, path_args)); */
        /*         return(s7_error(s7, */
        /*                         s7_make_symbol(s7, "invalid_argument"), */
        /*                 s7_list(s7, 2, s7_make_string(s7, */
        /*                 "too many args: ~A"), path_args))); */
        /*     } */
        /* } */
    }

    return(s7_error(s7,
                    s7_make_symbol(s7, "invalid_argument"),
                    s7_list(s7, 2, s7_make_string(s7,
                                                  "Bad arg: ~S; expected symbol or int"),
                            op)));
}

/* **************************************************************** */
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
        UT_array *targets = sealark_targets_for_pkg(bf_node);
        return nodelist_to_s7_list(s7, targets);
    }

    /* resolved so far: :>> (:targets) */
    if (op == KW(@@) || op == KW(bindings) || op == KW(attrs)) {
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
        /* s7_pointer result_list = sunlark_new_node(s7, n); */
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

/*
  already resolved: 1 target (e.g. (:> "hello"), (:> 1))
  path_args: :@, :rule, :name
 */
/* EXPORT s7_pointer sunlark_target_1(s7_scheme *s7, */
/*                                    struct node_s *tgt_node, */
/*                                    s7_pointer path_args) */
/* { */
/* #if defined (DEBUG_TRACE) || defined(DEBUG_TARGETS) */
/*     log_debug("sunlark_target_1: %s", */
/*               s7_object_to_c_string(s7, path_args)); */
/* #endif */

/*     int op_count = s7_list_length(s7, path_args); */
/*     /\* log_debug("op count: %d", op_count); *\/ */

/*     s7_pointer op = s7_car(path_args); */

/*     if (op_count == 0) */
/*         return sunlark_new_node(s7, tgt_node); */

/*     if (op_count < 2) */
/*         if (op != KW(@@) && op != KW(bindings) && op != KW(attrs)) { */
/*             /\* if ( s7_is_null(s7, rest) ) { /\\* e.g. (:> "mylib" :@) *\\/ *\/ */
/*             log_error("Missing arg: %s must be followed by a binding selector expression (or use :@@ to select all bindings).", s7_object_to_c_string(s7, s7_car(path_args))); */
/*             return(s7_error(s7, */
/*                             s7_make_symbol(s7, "missing_argument"), */
/*                             s7_list(s7, 2, s7_make_string(s7, */
/*                                                           "Missing arg: ~S must be followed by a binding selector expression (or: use :@@ to select all bindings)."), */
/*                                     s7_car(path_args)))); */
/*             /\* } *\/ */
/*         } */

/*     s7_pointer rest = s7_cdr(path_args); */
/*     s7_pointer op2 = s7_car(rest); */

/*     s7_pointer result; */

/*     /\* resolved so far: 1 target e.g. (:> "hello"...) *\/ */
/*     /\* allowed path op: :@, :@@, :rule, :name *\/ */
/*     if (op==KW(@)||op==KW(binding)||op==KW(attr)) { */
/*         if (s7_is_symbol(op2)) { */
/*             if (op_count == 2) { // FIXME: eliminate */
/*                 errno = 0; */
/*                 struct node_s *binding */
/*                     = sealark_target_binding_for_key(tgt_node, */
/*                                                      s7_symbol_name(op2)); */
/*                 if (binding) { */
/*                     return sunlark_new_node(s7, binding); */
/*                 } else { */
/*                     if (errno = -1) { */
/*                         log_error("Binding not found for key: %s", s7_symbol_name(op2)); */
/*                         return(s7_error(s7, */
/*                                         s7_make_symbol(s7, "not_found"), */
/*                                         s7_list(s7, 2, s7_make_string(s7, */
/*                                                                       "Binding not found for key: ~A"), op2))); */
/*                     } else { */
/*                         log_error("wtf 1 ????????????????"); */
/*                     } */
/*                 } */
/*             } else { */
/*                 return sunlark_resolve_binding_path_on_target(s7, */
/*                                                        tgt_node, */
/*                                                        rest); */
/*             } */
/*         } */
/*         if (s7_is_integer(op2)) { */
/*             s7_pointer binding = sunlark_resolve_binding_path_on_target(s7, tgt_node, rest); */
/*             return binding; */
/*                 /\* = sealark_target_binding_for_index(tgt_node, s7_integer(op2)); *\/ */

/*             /\* if (op_count == 2) { *\/ */
/*             /\*     if (binding) *\/ */
/*             /\*         return sunlark_new_node(s7, binding); *\/ */
/*             /\*     else *\/ */
/*             /\*         /\\* r7rs: error *\\/ *\/ */
/*             /\*         return s7_f(s7); /\\* same as binding_for_name  *\\/ *\/ */
/*             /\* } else { *\/ */
/*             /\*     return sunlark_resolve_binding_path_on_target(s7, *\/ */
/*             /\*                                            tgt_node, *\/ */
/*             /\*                                            rest); *\/ */
/*             /\* } *\/ */
/*         } */
/*         /\* error: only sym or int may follow :@ *\/ */
/*         return(s7_error(s7, */
/*                         s7_make_symbol(s7, "invalid_argument"), */
/*                         s7_list(s7, 1, s7_make_string(s7, */
/*                                                       "Only sym or int allowed here, to select attribute")))); */

/*     } */
/*     if (op==KW(@@)||op==KW(bindings)||op==KW(attrs)) { */
/*         if ( s7_is_null(s7, rest) ) { /\* e.g. (:> "mylib" :@@) *\/ */
/*             struct node_s *n = sealark_bindings_for_target(tgt_node); */
/*             return sunlark_new_node(s7, n); */
/*         } else { */
/*             log_error("kw %s must come last in path expression; did you mean :binding ?", */
/*                       s7_object_to_c_string(s7, op)); */
/*             return(s7_error(s7, */
/*                             s7_make_symbol(s7, "invalid_argument"), */
/*                             s7_list(s7, 2, s7_make_string(s7, */
/*                             "Keyword ~A must come last in path expression; did you mean :target (or :>)?"), */
/*                                     op))); */
/*         } */
/*     } */
/*     log_error("Allowed ops in this context: :@@, :bindings, :attrs; :@, :binding, :attr"); */
/*     return(s7_error(s7, */
/*                     s7_make_symbol(s7, "invalid_argument"), */
/*                     s7_list(s7, 2, s7_make_string(s7, */
/*       "Allowed ops in this context: :@@, :bindings, :attrs; :@, :binding, :attr")))); */
/* } */
