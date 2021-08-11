#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"

#include "sunlark_bindings.h"

/* docs at bottom */

/* ******************************** */
/* binding accepts :key, :value
       :value optionally followed by int index
   returns: node
 */
//s7_pointer
/* s7_pointer sunlark_binding_dispatcher(s7_scheme *s7, */
/* struct node_s * */
s7_pointer sunlark_binding_dispatcher(s7_scheme *s7,
                                      struct node_s *binding,
                                      s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PATHS)
    log_debug("sunlark_binding_dispatcher: %s",
              s7_object_to_c_string(s7, path_args));
#endif
#if defined(DEBUG_AST)
    sealark_debug_log_ast_outline(binding, 0);
#endif

    /* struct node_s *binding = s7_c_object_value(_binding); */
    assert(binding->tid == TK_Binding);

    int op_count = s7_list_length(s7, path_args);
    if (op_count == 0) {
        return sunlark_new_node(s7, binding);
        /* log_error("not enough path steps"); */
        /* return(s7_wrong_type_arg_error(s7, "node type :binding applicator:", */
        /*                                1, path_args, ":key or :value")); */
    }
    s7_pointer op = s7_car(path_args);

    struct node_s *tmp_node;

    s7_pointer result_list;

    if (KW(key) == op) {
        struct node_s *r = utarray_eltptr(binding->subnodes, 0);
        return sunlark_new_node(s7, r);
        //return sunlark_new_node(s7, utarray_eltptr(binding->subnodes, 0));
        /* tmp_node = utarray_eltptr(binding->subnodes, 0); */
        /* return sunlark_new_node(s7, tmp_node); */
    }

    if (op == s7_make_keyword(s7, "$") || op == KW(value)) {
        struct node_s *bval = utarray_eltptr(binding->subnodes, 2);
        if (s7_is_null(s7, s7_cdr(path_args)))
            return sunlark_new_node(s7, bval);

        /* next step depends on type of value */
        switch(bval->tid) {
        case TK_List_Expr:
            /* return sunlark_vector_dispatcher(); */
            ;
            break;
        case TK_Dict_Expr: {
            s7_pointer x
                = sunlark_dict_expr_dispatcher(s7, bval, s7_cdr(path_args) );
            return x; //sunlark_new_node(s7, x);
        }
            break;
        default:
            ;
        }

        /* buildfile dicts indexed by strings */
        if (s7_is_string(s7_cadr(path_args))) {
            log_debug("FIXME: unexpected string arg");
        }
        /* vectors are indexed by number keyword */
        if (s7_is_keyword(s7_cadr(path_args))) {
            s7_pointer lval = s7_cadr(path_args);
            int idx = sunlark_kwindex_to_int(s7, lval);
            if (errno != 0) { // not an int
                log_error("FIXME: non-kw path op: %s",
                          s7_object_to_c_string(s7, path_args));
                exit(EXIT_FAILURE);
            } else {// we got an int

                /* implies: val is a vector */
                struct node_s *x;
                if (bval->tid == TK_List_Expr) {
                    x = sealark_vector_item_for_int(bval, idx);
                    return sunlark_new_node(s7, x);
                    //  return sunlark_new_node(s7, sealark_vector_item_for_int(bval, idx));
                }
                if (bval->tid == TK_Dict_Expr) {
                    x = sealark_dict_entry_for_int(bval, idx);
                    return sunlark_new_node(s7, x);
                }
                log_error("trying to index a non-list of type %d %s",
                          bval->tid, TIDNAME(bval));
                errno = EINDEX_TYPE_ERR;
                return NULL;
            }
        } else {
            log_error("FIXME: invalid path op: %s",
                      s7_object_to_c_string(s7, path_args));
            exit(EXIT_FAILURE);
        }
    }

    if (s7_is_integer(op)) {
        log_error("Integer indexing of bindings (0 == :key, 1 == :value) not (yet?) supported", s7_object_to_c_string(s7, op));
        return handle_errno(s7, EINVALID_INDEX, path_args);
        /* return NULL; */
        /* return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"), */
        /*                 s7_list(s7, 2, s7_make_string(s7, */
        /*                 "Invalid arg ~D (integer indexing of bindings (0 == :key, 1 == :value) not (yet?) supported)"), op))); */
    }

    if (s7_is_keyword(op)) {
        return handle_errno(s7, EUNKNOWN_KW, path_args);
        /* errno = EUNKNOWN_KW; */
        /* return NULL; */
        /* return sunlark_common_property_lookup(s7, binding, op); */
    }

    log_error("Invalid op: %s", s7_object_to_c_string(s7, op));
    return handle_errno(s7, EINVALID_ARG, path_args);
    /* errno = EINVALID_ARG; */
    /* return NULL; */
    /*    return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"), */
    /*                    s7_list(s7, 2, s7_make_string(s7, */
    /* "Invalid arg: ~A (a binding can be indexed by :key or :value only)"), op))); */
}

/* ******************************** */
/*
  args: type predicate (e.g. :bindings?), ??
  returns: bool or node?
 */
//FIXME: rename arglist_dispatcher (bindings a subset of args)
s7_pointer sunlark_bindings_list_dispatcher(s7_scheme *s7,
                                                 s7_pointer _bindings,
                                                 s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PATHS)
    log_debug("sunlark_binding_dispatchers_list: %s",
              s7_object_to_c_string(s7, path_args));
    /* sealark_debug_log_ast_outline(s7_c_object_value(_bindings), 0); */
#endif

    struct node_s *bindings_node = s7_c_object_value(_bindings);

    assert(bindings_node->tid == TK_Arg_List);

    int op_count = s7_list_length(s7, path_args);
    s7_pointer op = s7_car(path_args);

    struct node_s *tmp_node;

    s7_pointer result_list;

    /* switch(op_count) { */
    /* /\* case 0: *\/ */
    /* /\*     return bindings; *\/ */
    /* /\*     break; *\/ */
    /* case 1: */

    if (s7_is_keyword(op)) {
        log_debug("on keyword");
        if (op == KW(bindings?)) {
            return s7_t(s7);
        } else {
            return sunlark_common_property_lookup(s7, bindings_node, op);
        }
    }

    if (s7_is_symbol(op)) {
        /* log_debug("index bindings by key"); */
        errno = 0;
        tmp_node = sealark_bindings_binding_for_key(bindings_node->subnodes,
                                                    s7_symbol_name(op));
        s7_pointer r = sunlark_binding_dispatcher(s7, tmp_node,
                                    /* sunlark_new_node(s7, tmp_node), */
                                           s7_cdr(path_args));
        if (r)
            return r; //sunlark_new_node(s7, r);
        else
            return NULL; // errno already set
    }

    if (s7_is_integer(op)) {
        log_debug("indexing bindings by int");
        int index = s7_integer(op);
        struct node_s *binding
            = sealark_bindings_binding_for_index(bindings_node, index);
        errno = 0;
        s7_pointer r = sunlark_binding_dispatcher(s7, binding,
                                     /* sunlark_new_node(s7, binding), */
                                           s7_cdr(path_args));
        if (r)
            return r; //sunlark_new_node(s7, r);
        else
            return NULL;
    }
        /* s7_pointer result = sunlark_common_property_lookup(s7, binding, op); */
        /* if (result) return result; */

    log_error("dispatch on %s for binding not yet implemented",
              s7_object_to_c_string(s7, op));
    exit(EXIT_FAILURE);     /* FIXME */
}

/* **************************************************************** */
static s7_pointer _list_expr_to_s7_vector(s7_scheme *s7,
                                          struct node_s *list_expr)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_BINDINGS)
    log_debug("_list_expr_to_s7_vector, tid: %d %s",
              list_expr->tid, TIDNAME(list_expr));
    /* sealark_debug_log_ast_outline(binding, 0); */
#endif
    /*
      :list-expr > :lbrack, :expr-list, :rbrack
      :expr-list > :string, :comma, ... :string
     */

    struct node_s *expr_list = utarray_eltptr(list_expr->subnodes, 1);
    int val_ct = (utarray_len(expr_list->subnodes) + 1) / 2;

    /* what kind of vector? int or string */
    struct node_s *vectype = utarray_eltptr(expr_list->subnodes, 0);

    s7_pointer vec = s7_make_vector(s7, val_ct);
    struct node_s *node = NULL;
    int i = 0;
    while( (node=(struct node_s*)utarray_next(expr_list->subnodes, node)) ) {
        if (node->tid == vectype->tid) {
            log_debug("setting item %d", i);
            s7_pointer elt = sunlark_new_node(s7, node);
            s7_vector_set(s7, vec, i, elt);
            i++;
        }
    }
    return vec;
}

/* **************** */
s7_pointer deprecate_sunlark_value_for_binding(s7_scheme *s7,
                                     struct node_s *binding)
{
#if defined(DEBUG_PATHS)
    log_debug("sunlark_value_for_binding, tid: %d %s",
              binding->tid, token_name[binding->tid][0]);
#endif
#if defined(DEBUG_BINDINGS)
    sealark_debug_log_ast_outline(binding, 0);
#endif
    /* :binding > :id, :eq, (:list-expr | :string | ...) */
    struct node_s *valnode = utarray_eltptr(binding->subnodes, 2);

    switch(valnode->tid) {
    case TK_List_Expr:
        return _list_expr_to_s7_vector(s7, valnode);
        break;
    case TK_STRING:
        break;
    default:
        log_warn("FALLTHROUGH");
        return s7_unspecified(s7);
    }
}

/* **************************************************************** */
struct node_s *sunlark_get_attrs_list(s7_scheme *s7,
                                      struct node_s *target_node)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("sunlark_get_attrs_list, tid: %d %s",
              target_node->tid, token_name[target_node->tid][0]);
#endif
    /* :call-expr[1] => :call-sfx[1] => :arg-list */
    struct node_s *call_sfx = utarray_eltptr(target_node->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);
    return arg_list;
}

/* **************** */
//FIXME: bindings_binding_for_key
s7_pointer sunlark_attr_list_kw_lookup(s7_scheme *s7,
                                    struct node_s *attrs,
                                    s7_pointer kw)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_attr_list_kw_lookup: %d %s",
              attrs->tid,       /* :arg_list == 88 */
              s7_object_to_c_string(s7, kw));
#endif
    /* s7_pointer key = s7_car(kw); */
    if (s7_is_keyword(kw)) {
        s7_pointer sym = s7_keyword_to_symbol(s7, kw);
        const char *attr_name = s7_symbol_name(sym);

        /* struct node_s *expr_list = utarray_eltptr(attrs->subnodes, 0); */
        /* log_debug("expr_list tid: %d", attrs->tid); */

        struct node_s *node=NULL;
        int i = 1;
        int len = strlen(attr_name);
        log_debug("looking up attr '%s'", attr_name);

        /* first handle real props, e.g. :tid */
        s7_pointer result = sunlark_common_property_lookup(s7,
                                                           attrs,
                                                           kw);
        if (result == NULL) {
            return(s7_wrong_type_arg_error(s7,
                                           "attr prop lookup",
                                           2, kw,
                                           "'name or 'value"));
        } else {
            return result;
        }

        /* then search subnodes */
        while((node=(struct node_s*)utarray_next(attrs->subnodes, node))) {
            if (node->tid == TK_Binding) {
                struct node_s *attrname_node = utarray_eltptr(node->subnodes, 0);
                    log_debug("attr name: %s", attrname_node->s);
                if ( strncmp(attr_name, attrname_node->s, len) == 0 ) {
                    log_debug("FOUND %s", attr_name);
                    break;
                }
            }
        }
        if (node) {
            return sunlark_new_node(s7, node);
        } else {
            /* not found */
            return s7_nil(s7);
        }
    } else {
        return(s7_wrong_type_arg_error(s7, "attrs lookup key",
                                       2, kw, "a keyword"));
    }
}

/* **************** */
bool sunlark_new_binding_is_valid(s7_scheme *s7,
                                  s7_pointer new_binding)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_binding_list_valid: %s",
              s7_object_to_c_string(s7, new_binding));
#endif
    if ( s7_is_keyword(s7_car(new_binding)) ) {
        log_error("Keyword not allowed for binding key");
        return false;
    }

    if ( s7_is_symbol(s7_car(new_binding))
         || s7_is_string(s7_car(new_binding)) ) {

        if (s7_is_string(s7_cadr(new_binding))
            || s7_is_symbol(s7_cadr(new_binding))
            || s7_is_integer(s7_cadr(new_binding))
            || s7_is_boolean(s7_cadr(new_binding))) {
            return true;
        } else {
            if(s7_is_pair(s7_cadr(new_binding))) {
                return _list_is_valid(s7, s7_cadr(new_binding));
            } else {
                log_error("Second element of binding list must be string,int, or list of same, or bool; got: %s",
                          s7_object_to_c_string(s7, s7_car(new_binding)));
                return false;
            }
        }
    } else {
        log_error("First element of binding list must be string or symbol; got: %s",
                  s7_object_to_c_string(s7, s7_car(new_binding)));
        return false;
    }
    return true;
}

bool _list_is_valid(s7_scheme *s7, s7_pointer list)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_list_is_valid: %s",
              s7_object_to_c_string(s7, list));
#endif

    /* lists must be heterogenous int or string, but symbols
       (variables) allowed in either */

    if (s7_is_string(s7_car(list))) {
        while( !s7_is_null(s7, list) ) {
            if ( ! s7_is_string(s7_car(list)) ) {
                if ( ! s7_is_symbol(s7_car(list)) ) {
                    log_error("List is not heterogenous");
                    return false;
                }
            }
            list = s7_cdr(list);
        }
        return true;
    } else {
        if (s7_is_integer(s7_car(list))) {
            while( !s7_is_null(s7, list) ) {
                if ( ! s7_is_integer(s7_car(list)) ) {
                    if ( ! s7_is_symbol(s7_car(list)) ) {
                        log_error("List is not heterogenous");
                        return false;
                    }
                }
                list = s7_cdr(list);
            }
            return true;
        } else {
            log_error("Must be int or string list");
            return false;
        }
    }
}

/* **************************************************************** */
_merge_binding_list(UT_array *bindings, struct node_s *bindings_list)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PATHS)
    log_debug("_merge_binding_list"); /* = TK_Arg_List */
    /* sealark_debug_log_ast_outline(bindings_list, true); */
#endif

    struct node_s *binding = NULL;
    while( (binding=(struct node_s*)utarray_next(bindings_list->subnodes,
                                              binding)) ) {
        if (binding->tid == TK_Binding) {
            utarray_push_back(bindings, binding);
        }
    }
}

s7_pointer sunlark_forall_targets_forall_bindings(s7_scheme *s7,
                                                  struct node_s *pkg,
                                                  s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PATHS)
    log_debug("sunlark_forall_targets_forall_bindings: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    assert(pkg->tid == TK_Package);

    s7_pointer result;

    UT_array *targets = sealark_targets_for_pkg(pkg);
    struct node_s *target = NULL;
    UT_array *bindings;
    utarray_new(bindings, &node_icd);
    struct node_s *binding_list;

    if (s7_is_null(s7, path_args)) {
        /* return list of all bindings for all targets */
        target = NULL;
        while( (target=(struct node_s*)utarray_next(targets, target)) ) {
            binding_list = sealark_bindings_for_target(target);
            log_debug("blist: %d %s", binding_list->tid,
                      TIDNAME(binding_list));
            /* utarray_push_back(bindings, binding_list); */
            _merge_binding_list(bindings, binding_list);
        }
        result = nodelist_to_s7_list(s7, bindings);
        return result;
    }

    s7_pointer op = s7_car(path_args);
    bool deref_key = false;
    bool deref_value = false;

    s7_pointer op2;
    if ( !s7_is_null(s7, s7_cdr(path_args)) ) {
        log_debug("0 xxxxxxxxxxxxxxxx");
        if (s7_list_length(s7, path_args) > 2) {
            log_error("Invalid arg(s): last arg must be :key or :value (or :$)");
            return s7_unspecified(s7);
        }
        op2 = s7_cadr(path_args);
    }
    /* if (op2) { */
    /*     log_debug("op2: %s", s7_object_to_c_string(s7, op2)); */
    /* } */

    struct node_s *binding;

    if (s7_is_symbol(op)) { // binding key
        log_debug("tgt ct: %d", utarray_len(targets));

        /* if (s7_is_null(s7, s7_cdr(path_args))) { */

            const char *key = s7_symbol_name(op);

            /* return all bindings with key = op for all targets */
            binding = NULL;
            int i=0;
            target = NULL;
            while( (target=(struct node_s*)utarray_next(targets, target)) ) {
                log_debug("tgt[%d]: %d %s", i++, target->tid, TIDNAME(target));
                binding = sealark_target_binding_for_key(target, key);
                if (op2 == KW(key)) {
                    if (binding) {
                        struct node_s *v
                            = utarray_eltptr(binding->subnodes, 0);
                        utarray_push_back(bindings, v);
                    }
                } else {
                    if (op2 == s7_make_keyword(s7,"$") || op2 == KW(value)) {
                        if (binding) {
                            struct node_s *v
                                = utarray_eltptr(binding->subnodes, 2);
                            utarray_push_back(bindings, v);
                        }
                    } else {
                        if (binding) {
                            log_debug("2 xxxxxxxxxxxxxxxx");
                            utarray_push_back(bindings, binding);
                        }
                    }
                }
            }
            result = nodelist_to_s7_list(s7, bindings);
            return result;
        /* } */
        // so far: :>> :@@ sym, next is :key or :value
    }
    if (s7_is_integer(op)) {
        /* return all bindings at index op for all targets */

    }

    return s7_unspecified(s7);
}

/* component: :key or :val (idx | fld)? */
//FIXME return struct node_s *, let caller handler final :$ op
s7_pointer sunlark_binding_component(s7_scheme *s7, struct node_s *binding,
                                     s7_pointer path_args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_binding_component: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    assert(binding->tid == TK_Binding);

    int op_count = s7_list_length(s7, path_args);
    /* log_debug("op_count: %d", op_count); */
    /* if (op_count > 2) { */
    /*     log_error("Too many args: %s", s7_object_to_c_string(s7, path_args)); */
    /*     return(s7_error(s7, */
    /*                     s7_make_symbol(s7, "invalid_argument"), */
    /*                     s7_list(s7, 2, s7_make_string(s7, */
    /*                        "Too many args: ~S"), path_args))); */
    /* } */

    s7_pointer op = s7_car(path_args);

    if (op == KW(key)) {
        if (op_count > 1) {
            log_error("too many args, :key must be the last arg");
            return NULL;
        } else {
            struct node_s *k = utarray_eltptr(binding->subnodes, 0);
            return sunlark_new_node(s7, k);
        }
    }

    if (op == s7_make_keyword(s7,"$") || op == KW(value)) {
        struct node_s *val = utarray_eltptr(binding->subnodes, 2);
        if (op_count == 1)
            return sunlark_new_node(s7, val);

        if (val->tid == TK_Dict_Expr) {
            s7_pointer r
                = sunlark_dict_expr_dispatcher(s7, val, s7_cdr(path_args));
            if (r)
                return r; // sunlark_new_node(s7, r);
            else
                return NULL;
        }

        if (val->tid == TK_List_Expr) {
            // return sunlark_vector_dispatch(s7, val, s7_cdr(path_args));
            s7_pointer idx = s7_cadr(path_args);
            int ix = sunlark_is_nbr_kw(s7,idx);
            if (errno == 0) {
                log_debug("indexing on %d", ix);
                struct node_s *item
                    = sealark_vector_item_for_int(val, ix);
                return sunlark_new_node(s7,item);
            }
            log_debug("indexing by value: %s",
                      s7_object_to_c_string(s7, idx));
            if (s7_is_string(idx)) {
                UT_array *items     /* list of item nodes */
                    = sealark_vector_items_for_string(val, s7_string(idx));
                return nodelist_to_s7_list(s7, items);
            }
            if (s7_is_integer(idx)) {
                /* sealark_debug_log_ast_outline(val, 0); */
                UT_array *items     /* list of item nodes */
                    = sealark_vector_items_for_int_val(val, s7_integer(idx));
                return nodelist_to_s7_list(s7, items);
            }
            return(s7_error(s7,
                            s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 3, s7_make_string(s7,
                                                          "Bad arg ~S (of type ~A); :value may only be followed by int or string"),
                                    idx, s7_type_of(s7, idx))));
        }

        log_error("Trying to index into a non-list: %d %s s=%s",
                  val->tid, TIDNAME(val), val->s);
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 4, s7_make_string(s7,
                                                      "Cannot index into non-list value ~A[~D]: ~A"),
                                s7_make_string(s7, TIDNAME(val)),
                                s7_make_integer(s7, val->tid),
                                s7_make_string(s7, val->s))));
    }
    log_error("Bad arg %s; only :key or :value valid in this context",
              s7_object_to_c_string(s7, op));
    return(s7_error(s7,
                    s7_make_symbol(s7, "invalid_argument"),
                    s7_list(s7, 2, s7_make_string(s7,
           "Bad arg \"~A\"; only :key or :value valid in this context"),
                            op)));
}
