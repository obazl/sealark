#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_bindings.h"

/* docs at bottom */

#define ESUNLARK_INVALID_ARG -1
#define ESUNLARK_ARG_TYPE_ERR -2
#define ESUNLARK_LOCN_ARG_ERR -3

/* ******************************** */
/* binding accepts :key, :value
       :value optionally followed by int index
   returns: node
 */
//s7_pointer
struct node_s *sunlark_dispatch_on_binding(s7_scheme *s7,
                                      s7_pointer _binding,
                                      s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_dispatch_on_binding: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    sealark_debug_print_ast_outline(s7_c_object_value(_binding), 0);

    struct node_s *binding = s7_c_object_value(_binding);

    if (binding->tid != TK_Binding) {
        log_error("Expected node tid %d, got %d %s", TK_Binding,
                  binding->tid, TIDNAME(binding));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    int op_count = s7_list_length(s7, path_args);
    s7_pointer op = s7_car(path_args);

    struct node_s *tmp_node;

    s7_pointer result_list;

    switch(op_count) {
    case 0:
        return binding;
        /* log_error("not enough path steps"); */
        /* return(s7_wrong_type_arg_error(s7, "node type :binding applicator:", */
        /*                                1, path_args, ":key or :value")); */
    case 1:
        if (KW(key) == op) {
            return utarray_eltptr(binding->subnodes, 0);
            /* tmp_node = utarray_eltptr(binding->subnodes, 0); */
            /* return sunlark_node_new(s7, tmp_node); */
        }
        if (KW(value) == op) {
            return utarray_eltptr(binding->subnodes, 2);
            /* tmp_node = utarray_eltptr(binding->subnodes, 2); */
            /* return sunlark_node_new(s7, tmp_node); */
        }
        /* common properties */
        /* FIXME: this returns string, int, etc, not node */
        /* return sunlark_common_property_lookup(s7, binding, op); */
        return NULL;

        /* s7_pointer result = sunlark_common_property_lookup(s7, binding, op); */
        /* if (result) return result; */

        log_error("dispatch on %s for binding not yet implemented",
                  s7_object_to_c_string(s7, op));
        break;
    case 2:
        // :bindings :key, :value
        // :bindings <int>
        if (KW(key) == op) {
            return utarray_eltptr(binding->subnodes, 0);
            /* tmp_node = utarray_eltptr(binding->subnodes, 0); */
            /* return sunlark_node_new(s7, tmp_node); */
        }
        if (KW(value) == op) {
            /* bval : list or string or int or ?? */
            struct node_s *bval = utarray_eltptr(binding->subnodes, 2);
            if (s7_is_integer(s7_cadr(path_args))) {
                int idx = s7_integer(s7_cadr(path_args));
                /* implies: val is a vector */
                if (bval->tid == TK_List_Expr) {
                    return sealark_vector_index(bval, idx);
                    /* struct node_s *item = sealark_vector_index(bval, idx); */
                    /* return sunlark_node_new(s7, item); */
                } else {
                    log_error("trying to index a non-list of type %d %s",
                              bval->tid, TIDNAME(bval));
                    exit(EXIT_FAILURE);
                }
            } else {
                log_error("invalid path op: %s",
                          s7_object_to_c_string(s7, path_args));
                exit(EXIT_FAILURE);
            }
        }
        if (s7_is_integer(op)) {
        }
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

/* **************************************************************** */
static s7_pointer _list_expr_to_s7_vector(s7_scheme *s7,
                                          struct node_s *list_expr)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("_list_expr_to_s7_vector, tid: %d %s",
              list_expr->tid, TIDNAME(list_expr));
    /* sealark_debug_print_ast_outline(binding, 0); */
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
            s7_pointer elt = sunlark_node_new(s7, node);
            s7_vector_set(s7, vec, i, elt);
            i++;
        }
    }
    return vec;
}

s7_pointer sunlark_value_for_binding(s7_scheme *s7,
                                     struct node_s *binding)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("sunlark_value_for_binding, tid: %d %s",
              binding->tid, token_name[binding->tid][0]);
    /* sealark_debug_print_ast_outline(binding, 0); */
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
            return sunlark_node_new(s7, node);
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
