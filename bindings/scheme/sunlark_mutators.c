#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_mutators.h"

/* called for (set! (sunlark-node ...) val) */
s7_pointer sunlark_node_set_generic(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug(">>>>>>>>>>>>>>>> sunlark_node_set_generic <<<<<<<<<<<<<<<<");
    /* debug_print_s7(s7, "set_generic spec: ", args); */
#endif
    return sunlark_set_bang(s7, args);
}

/* sunlark_node_set_specialized

   implements (ast_node_set! sunlark-node key val)

 */
s7_pointer sunlark_node_set_specialized(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug(">>>>>>>>>>>>>>>> sunlark_node_set_specialized <<<<<<<<<<<<<<<<");
#endif

    return sunlark_set_bang(s7, args);
}

s7_pointer sunlark_set_bang(s7_scheme *s7, s7_pointer set_args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_set_bang");
#endif

    s7_pointer self = s7_car(set_args);
    struct node_s *self_node = s7_c_object_value(self);

    /* last arg: new value */
    s7_pointer args = s7_reverse(s7, s7_cdr(set_args));
    s7_pointer update_val = s7_car(args);
    args = s7_reverse(s7, s7_cdr(args));

    log_debug("set! self: %d %s; args: %s; new val: %s",
              sunlark_node_tid(s7, self),
              token_name[sunlark_node_tid(s7, self)][0],
              s7_object_to_c_string(s7, args),
              s7_object_to_c_string(s7, update_val));

    struct node_s *node;
    s7_int typ;
    s7_pointer key;

    /* qua procedure, check type of first arg */
    typ = s7_c_object_type(self);
    if (typ != ast_node_t)
        return(s7_wrong_type_arg_error(s7, "ast-node-set!", 1, self, "a ast_node"));

    if (s7_is_immutable(self))
        return(s7_wrong_type_arg_error(s7, "ast-node-set!", 1, self, "a mutable ast_node"));

    if (s7_list_length(s7, args) > 1) {
        log_error("Too many args? %s", s7_object_to_c_string(s7, args));
        /* exit(EXIT_FAILURE); */
    }

    log_debug("self_node %d %s", self_node->tid, TIDNAME(self_node));

    switch(self_node->tid) {
    case TK_STRING:
         if (s7_is_null(s7, args)) {
            /* update s field */
            /* FIXME: update_val could be string, number, or ? */
            int len;
            const char *new_s;
            if (s7_is_string(update_val)) {
                new_s = s7_string(update_val);
                len = strlen(new_s);
            }
            free(self_node->s);
            self_node->s = calloc(len, sizeof(char));
            strncpy(self_node->s, new_s, len);
            return self;
        }
        break;
    case TK_Binding:
        log_debug("set! Binding");

        struct node_s *result = sunlark_dispatch_on_binding(s7, self, args);
        /* FIXME: resolved_path may be: string; :list-expr (binding), dict */
        /* struct node_s *result = s7_c_object_value(resolved_path); */

        if (result->tid == TK_Binding) {
            log_debug("REPLACING BINDING");
            struct node_s *r = _mutate_binding(s7, result, update_val);
            if (r)
                return s7_unspecified(s7);
            else
                return s7_unspecified(s7);
        } else {
            if (result->tid == TK_STRING) {
                result->s = (char*)s7_string(update_val);
                return sunlark_node_new(s7, result);
            } else {
                if (result->tid == TK_List_Expr) {
                    log_debug("REPLACING LIST_EXPR");
                    return s7_unspecified(s7);
                }
            }
        }
        break;
    case TK_List_Expr:
        log_error("FIXME: setting list-expr (vector)");
        return s7_unspecified(s7);
        break;
    default:
        log_error("NOT IMPLEMENTED");
        exit(EXIT_FAILURE);
        ;
    }

    /* s7_pointer resolved_path = sunlark_dispatch(s7, self, args); */
    /* log_debug("resoved path: %s", s7_object_to_c_string(s7, resolved_path)); */
    /* return resolved_path; */

    /* log_debug("set_target: %s", s7_object_to_c_string(s7, set_target)); */
    /* log_debug("update_val: %s", s7_object_to_c_string(s7, update_val)); */

    // now update set_target

    /* return sunlark_update_binding_name(s7, node_s7, key, val); */
    /* return sunlark_update_binding_value(s7, node_s7, key, val); */

    /* _update_ast_node_property(s7, node, key, s7_caddr(args)); */

    /* _update_starlark(s7, self, s7_symbol_name(key), s7_caddr(args)); */

    //FIXME: r7rs says result of set! is unspecified. does that mean
    //implementation-specified?
    return s7_unspecified(s7);

    /* return sunlark_node_new(set_target); */
}

LOCAL struct node_s *_mutate_binding(s7_scheme *s7, struct node_s *binding, s7_pointer new_binding)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("_mutate_binding, new_binding: %s",
              s7_object_to_c_string(s7, new_binding));
#endif
    if (s7_is_list(s7, new_binding)) {
        log_debug("replacing binding");
        if ( !sunlark_new_binding_is_valid(s7, new_binding) )
            exit(EXIT_FAILURE); /* FIXME: s7_error */
        if (binding->tid != TK_Binding) {
            log_error("Expected TK_Binding node, got %d  %s",
                      binding->tid, TIDNAME(binding));
            exit(EXIT_FAILURE); /* FIXME: s7_error */
        }
        /* new_binding is valid */

        /* mutate key */
        struct node_s *bid = utarray_eltptr(binding->subnodes, 0);
        const char *new_key;
        if (s7_is_string(s7_car(new_binding))) {
            new_key = s7_string(s7_car(new_binding));
        } else {
            if (s7_is_symbol(s7_car(new_binding))) {
                new_key = s7_symbol_name(s7_car(new_binding));
            } else {
                // should not happen, has been validated above
            }
        }
        int len = strlen(new_key);
        free(bid->s);
        bid->s = calloc(len, sizeof(char));
        strncpy(bid->s, new_key, len);

        /* mutate value */
        struct node_s *old_value = utarray_eltptr(binding->subnodes, 2);
        s7_pointer new_value = s7_cadr(new_binding);

        if (s7_is_list(s7, new_value)) {
            if (old_value->tid == TK_List_Expr) {
                /* update existing vector */
                _update_old_vector(s7, old_value, new_value);
            } else {
                /* replace */
                sealark_node_free(old_value);
                old_value = sealark_node_new();
            }
        }
        return binding;
    } else {
        if (s7_is_string(new_binding)) {
            log_error("Cannot replace binding with string");
            exit(EXIT_FAILURE); /* FIXME: s7_error */
        }
    }
}

/* **************** */
LOCAL struct node_s *_update_old_vector(s7_scheme *s7,
                                        struct node_s *old_vec,
                                        s7_pointer new_vec)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("_update_old_vector");
#endif

    int new_ct = s7_list_length(s7, new_vec);

    /* :list_expr > :lbrack, :expr_list, :rbrack */
    struct node_s *old_items = utarray_eltptr(old_vec->subnodes, 1);
    int old_ct = (utarray_len(old_items->subnodes) + 1) / 2;
    log_debug("old_ct: %d", old_ct);

    struct node_s *old_item;

    s7_pointer new_item;
    /* need both, since an int/string list can contain syms (vars) */
    int new_vec_type;
    int new_item_type;

    /* first element sets element type */
    /* BUT: what if first elt is sym? e.g. (myvar 8 9) */
    s7_pointer proto = s7_car(new_vec);
    if (s7_is_string(proto)) {
        new_vec_type = TK_STRING;
    } else {
        if (s7_is_integer(proto)) {
        new_vec_type = TK_INT;
        }
    }

    const char *new_str;
    int new_str_len;

    /* FIXME: handle case where new item ct > old item ct */

    int i = 0;
    while( ! s7_is_null(s7, new_vec) ) {
        new_item = s7_car(new_vec);
        if (s7_is_string(new_item)) {
            new_item_type = TK_STRING;
            new_str = s7_string(new_item);
        }
        if (s7_is_symbol(new_item)) {
            new_item_type = TK_ID;
            new_str = s7_symbol_name(new_item);
        }
        if (s7_is_integer(new_item)) {
            new_item_type = TK_INT;
            int new_int = s7_integer(new_item);
            char ibuf[64];
            snprintf(ibuf, 64, "%d", new_int);
            new_str = (char*)ibuf;
        }
        new_str_len = strlen(new_str);
        log_debug("new_str: %s (%d %s)",
                  new_str, new_item_type,
                  token_name[new_item_type][0]);

        /* if (new_item_idx > old_items_ct) { */
        /*     malloc, init, add new item */
        /* } else { */
        old_item = utarray_eltptr(old_items->subnodes, i);
        log_debug("old item tid: %d %s",
                  old_item->tid, TIDNAME(old_item));

        /* skip commas but include symbols */
        while( (old_item->tid != new_vec_type)
               && (old_item->tid != TK_ID)) {
            log_debug("\told item tid: %d %s",
                      old_item->tid, TIDNAME(old_item));
            i++;
            old_item = utarray_eltptr(old_items->subnodes, i);
        }
        i++;

        /* now the update */
        if (old_item->tid == new_vec_type) {
            log_debug("updating old: %s", old_item->s);
            free(old_item->s);
            old_item->s = calloc(new_str_len, sizeof(char));
            strncpy(old_item->s, new_str, new_str_len);
            old_item->tid = new_item_type; // in case new item is sym
        } else {
            if (old_item->tid == TK_ID) {
                log_warn("FIXME: update sym/ID");
            }
        }
        i++;
        new_vec = s7_cdr(new_vec);
    }

    if (new_ct < old_ct) {
        log_debug("removing extras");
        utarray_resize(old_items->subnodes, new_ct * 2 - 1);
    }
    /* old_vec was updated-in-place */
    return old_vec;
}
