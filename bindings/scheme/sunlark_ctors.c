#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"

#include "s7.h"

#include "sunlark_ctors.h"

/* **************************************************************** */
#if INTERFACE
#define SUNLARK_MAKE_TARGET_HELP "(make-target) returns a new node of type :call-expr"

#define SUNLARK_MAKE_TARGET_FORMAL_PARAMS "(rule) (name) (attrs)"

/* make-target takes a symbol as key; as val: bool, string, int, vector of ints or strings (which may include symbols); returns a node */
#define SUNLARK_MAKE_TARGET_SIG s7_make_signature(s7, 3, s7_make_symbol(s7, "node?"), s7_make_symbol(s7, "symbol?"),s7_make_symbol(s7, "symbol?"))
#endif
// NB: the sig does not seem to have any effect

EXPORT s7_pointer sunlark_make_target(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_make_target: %s",
              s7_object_to_c_string(s7, s7_car(args)));
#endif

    /* log_debug("sunlark-make-target args: %s", */
    /*           s7_object_to_c_string(s7, args)); */

    s7_pointer rule = s7_car(args);
    s7_pointer name = s7_cadr(args);
    s7_pointer rest = s7_caddr(args);

    int line = 0;
    int col  = 0;
    int indent = 4;

    if ( !s7_is_symbol(rule)) {
        log_error("Target :rule must be a symbol: %s",
                  s7_object_to_c_string(s7, rule));
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 3, s7_make_string(s7,
                  "Target :rule must be a symbol: ~A satisfies ~A"),
                                rule,
                                s7_type_of(s7, rule))));
    }

    /* log_debug("rule: %s; type: %s", */
    /*           s7_object_to_c_string(s7, rule), */
    /*           s7_object_to_c_string(s7, s7_type_of(s7, rule))); */
    const char *rule_str = s7_symbol_name(rule);
    int rule_len = strlen(rule_str);

    if ( !s7_is_string(name) ) {
        log_error("Target :name must be a string: %s",
                  s7_object_to_c_string(s7, name));
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 3, s7_make_string(s7,
                  "Target :name must be a string: ~A satisfies ~A"),
                                name,
                                s7_type_of(s7, name))));
    }

    struct node_s *target = sealark_new_node(TK_Call_Expr, with_subnodes);
    struct node_s *node = sealark_new_node_s(TK_ID, rule_str);
    utarray_push_back(target->subnodes, node);

    struct node_s *call_sfx = sealark_new_node(TK_Call_Sfx, with_subnodes);
    utarray_push_back(target->subnodes, call_sfx);

    node = sealark_new_node(TK_LPAREN, without_subnodes);
    utarray_push_back(call_sfx->subnodes, node);

    struct node_s *arg_list = sealark_new_node(TK_Arg_List, with_subnodes);
    utarray_push_back(call_sfx->subnodes, arg_list);

    struct node_s *binding = sealark_new_node(TK_Binding, with_subnodes);
    node = sealark_new_node_s(TK_ID, "name");
    node->line = ++line; node->col = indent;
    utarray_push_back(binding->subnodes, node);
    node = sealark_new_node(TK_EQ, without_subnodes);
    utarray_push_back(binding->subnodes, node);
    node = sealark_new_node_s(TK_STRING, s7_string(name));
    utarray_push_back(binding->subnodes, node);
    utarray_push_back(arg_list->subnodes, binding);

    /* log_debug("rest: %s", s7_object_to_c_string(s7, rest)); */
    int rest_len = s7_list_length(s7, rest) - 1;
    /* log_debug("rest len: %d", rest_len+1); */

    struct node_s *comma;
    if (rest_len > 0) {
        comma = sealark_new_node(TK_COMMA, without_subnodes);
        utarray_push_back(arg_list->subnodes, comma);
    }
    struct node_s *barg;
    s7_pointer arg = NULL;
    int i = 0;
    while ( !s7_is_null(s7,rest) ) {
        arg = s7_car(rest);
        if ( s7_is_c_object(arg) ) {
            if (sunlark_node_tid(s7,arg) == TK_Binding) {
                barg = s7_c_object_value(arg);
                barg->line = ++line; barg->col = indent;
                utarray_push_back(arg_list->subnodes, barg);
                if (rest_len > i) {
                    comma = sealark_new_node(TK_COMMA, without_subnodes);
                    utarray_push_back(arg_list->subnodes, comma);
                }
            } else {
                log_error(":args must be list or vector of :binding nodes");
            }
        } else {
            log_error(":args must be a list or vector if :binding nodes; got:", s7_object_to_c_string(s7, s7_type_of(s7, arg)));
        }
        i++;
        rest = s7_cdr(rest);
    }

    node = sealark_new_node(TK_RPAREN, without_subnodes);
    node->line = ++line;
    utarray_push_back(call_sfx->subnodes, node);

 resume:
    ;
    /* log_debug("new target: %d %s", */
    /*           target->tid, TIDNAME(target)); */
    /* sealark_debug_log_ast_outline(target, 0); */

    s7_pointer new_ast_node_s7 = s7_make_c_object(s7, ast_node_t,
                                                  (void *)target);

    sunlark_register_c_object_methods(s7, new_ast_node_s7);

    return new_ast_node_s7;
}

/* **************************************************************** */
#if INTERFACE
#define SUNLARK_MAKE_BINDING_HELP "(make-binding) returns a new node of type :binding"

#define SUNLARK_MAKE_BINDING_FORMAL_PARAMS "key value"

/* make-binding takes a symbol as key; as val: bool, string, int, vector of ints or strings (which may include symbols); returns a node */
#define SUNLARK_MAKE_BINDING_SIG s7_make_signature(s7, 3, s7_make_symbol(s7, "node?"), s7_make_symbol(s7, "symbol?"),s7_make_symbol(s7, "symbol?"))
#endif
// NB: the sig does not seem to have any effect

//FIXME: migrate C stuff to sealark
EXPORT s7_pointer sunlark_make_binding(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_make_binding: %s",
              s7_object_to_c_string(s7, s7_car(args)));
    log_debug("sunlark-make-binding args: %s",
              s7_object_to_c_string(s7, args));

#endif

    s7_pointer key = s7_car(args);
    if ( !s7_is_symbol(key)) {
        log_error("Key of binding must be a symbol: %s",
                  s7_object_to_c_string(s7, key));
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 3, s7_make_string(s7,
                  "Key of binding must be a symbol: ~A satisfies ~A"),
                                key,
                                s7_type_of(s7, key))));
    }

    /* log_debug("key: %s; type: %s", s7_object_to_c_string(s7, key), */
    /*           s7_object_to_c_string(s7, s7_type_of(s7, key))); */
    const char *keystr = s7_symbol_name(key);
    int len = strlen(keystr);

    struct node_s *binding = sealark_new_node(TK_Binding, with_subnodes);
    struct node_s *keynode = sealark_new_node(TK_ID, without_subnodes);
    keynode->s = calloc(len, sizeof(char));
    strncpy(keynode->s, keystr, len+1);
    utarray_push_back(binding->subnodes, keynode);

    struct node_s *eq = sealark_new_node(TK_EQ, without_subnodes);
    utarray_push_back(binding->subnodes, eq);

    s7_pointer val = s7_cadr(args);
    /* log_debug("val %s", s7_object_to_c_string(s7, val)); */
    struct node_s *nd;

    if (s7_is_list(s7, val)) {
        binding = _make_list_from_list(s7, binding, val);
        goto resume;
    }

    if (s7_is_vector(val)) {
        binding = _make_list_from_vector(s7, binding, val);
        goto resume;
    }

    if (s7_is_integer(val)) {
        /* log_debug("int %d", s7_integer(val)); */
        binding = sealark_make_binding_int(keystr, s7_integer(val));
        goto resume;
    }
    if (s7_is_string(val)) {
        /* log_debug("string %s", s7_string(val)); */
        nd = sealark_new_node(TK_STRING,
                              with_subnodes);
        const char *s = s7_string(val);
        int len = strlen(s);
        nd->s = calloc(len+1, sizeof(char));
        snprintf(nd->s, len+1, "%s", s);
        utarray_push_back(binding->subnodes, nd);
        goto resume;
    }
    if (s7_is_symbol(val)) {
        /* log_debug("symbol %s", s7_symbol_name(val)); */
        nd = sealark_new_node(TK_ID,
                              without_subnodes);
        const char *s = s7_symbol_name(val);
        int len = strlen(s);
        nd->s = calloc(len+1, sizeof(char));
        snprintf(nd->s, len+1, "%s", s);
        utarray_push_back(binding->subnodes, nd);
        goto resume;
    }

    if (s7_is_boolean(val)) {
        /* log_debug("boolean %d", s7_boolean(s7, val)); */
        nd = sealark_new_node(TK_ID,
                              without_subnodes);
        if (val == s7_t(s7)) {
            nd->s = calloc(5, sizeof(char));
            snprintf(nd->s, 5, "%s", "True");
            utarray_push_back(binding->subnodes, nd);
        } else {
            nd->s = calloc(6, sizeof(char));
            snprintf(nd->s, 6, "%s", "False");
            utarray_push_back(binding->subnodes, nd);
        }
        goto resume;
    }
    if (s7_is_c_object(val)) {
        /* log_debug("cobject %s", s7_object_to_c_string(s7, val)); */
        struct node_s *nd = s7_c_object_value(val);
        utarray_push_back(binding->subnodes, nd);
        goto resume;
    }
    log_error("UNCAUGHT binding val %s, satisfies %s",
              s7_object_to_c_string(s7, val),
              s7_object_to_c_string(s7, s7_type_of(s7, val)));
 resume:
    ;
    /* log_debug("new binding: %d %s, %s", */
    /*           binding->tid, TIDNAME(binding), binding->s); */
    /* sealark_debug_log_ast_outline(binding, 0); */

    s7_pointer new_ast_node_s7;
    new_ast_node_s7 = s7_make_c_object(s7, ast_node_t,
                                                  (void *)binding);

    sunlark_register_c_object_methods(s7, new_ast_node_s7);

    return new_ast_node_s7;
}

/* **************************************************************** */
//FIXME: migrate C stuff to sealark
struct node_s *_make_list_from_list(s7_scheme *s7,
                                         struct node_s *binding,
                                         s7_pointer val)
{
#ifdef DEBUG_TRACE
    log_debug("_make_list_from_list");
#endif

    int list_len = s7_list_length(s7, val);
    s7_pointer vals = val;
    /* log_debug("xxxx %s", s7_object_to_c_string(s7, vals)); */
    s7_pointer v;

    struct node_s *list_expr
        = sealark_new_node(TK_List_Expr, with_subnodes);
    utarray_push_back(binding->subnodes, list_expr);

    struct node_s *nd;
    nd = sealark_new_node(TK_LBRACK, without_subnodes);
    utarray_push_back(list_expr->subnodes, nd);

    struct node_s *expr_list
        = sealark_new_node(TK_Expr_List, with_subnodes);
    utarray_push_back(list_expr->subnodes, expr_list);

    char buf[64];

    int i = 0;
    while ( !s7_is_null(s7, vals) ) {
        v = s7_car(vals);
        /* log_debug("0 val %s", s7_object_to_c_string(s7, v)); */
        if (s7_is_integer(v)) {
            nd = sealark_new_node(TK_INT, without_subnodes);
            buf[0] = '\0';
            /* int ival = s7_integer(v); */
            /* log_debug("int: %d", ival); */
            snprintf(buf, 32, "%d", s7_integer(v));
            /* log_debug("buf: %s, i: %d", buf, i); */

            nd->s = calloc(strlen(buf)+1, sizeof(char));
            snprintf(nd->s, strlen(buf)+1, "%s", buf);
            utarray_push_back(expr_list->subnodes, nd);
            /* log_debug("list_len: %d, i: %d", list_len, i); */
            if ((list_len - i) > 1) {
                nd = sealark_new_node(TK_COMMA, without_subnodes);
                utarray_push_back(expr_list->subnodes, nd);
            }
        }
        if (s7_is_string(v)) {
            nd = sealark_new_node(TK_STRING, without_subnodes);
            const char *buf = s7_string(v);
            nd->s = calloc(strlen(buf)+1, sizeof(char));
            snprintf(nd->s, strlen(buf)+1, "%s", buf);
            utarray_push_back(expr_list->subnodes, nd);
            /* log_debug("list_len: %d, i: %d", list_len, i); */
            if ((list_len - i) > 1) {
                nd = sealark_new_node(TK_COMMA, without_subnodes);
                utarray_push_back(expr_list->subnodes, nd);
            }
        }
        if (s7_is_symbol(v)) {
            nd = sealark_new_node(TK_ID, without_subnodes);
            buf[0] = '\0';
            snprintf(buf, 64, "%s", s7_symbol_name(v));
            /* log_debug("buf: %s, i: %d", buf, i); */

            nd->s = calloc(strlen(buf)+1, sizeof(char));
            snprintf(nd->s, strlen(buf)+1, "%s", buf);
            utarray_push_back(expr_list->subnodes, nd);
            /* log_debug("list_len: %d, i: %d", list_len, i); */
            if ((list_len - i) > 1) {
                nd = sealark_new_node(TK_COMMA, without_subnodes);
                utarray_push_back(expr_list->subnodes, nd);
            }
        }
        i++;
        vals = s7_cdr(vals);
    }
    nd = sealark_new_node(TK_RBRACK, without_subnodes);
    utarray_push_back(list_expr->subnodes, nd);
    return binding;
}

/* **************************************************************** */
//FIXME: migrate C stuff to sealark
struct node_s *_make_list_from_vector(s7_scheme *s7,
                                      struct node_s *binding,
                                      s7_pointer val)
{
#ifdef DEBUG_TRACE
    log_debug("_make_list_from_vector");
#endif

    int vector_len = s7_vector_length(val);
    s7_pointer vals = val;
    /* log_debug("xxxx %s", s7_object_to_c_string(s7, vals)); */
    s7_pointer v;

    struct node_s *list_expr
        = sealark_new_node(TK_List_Expr, with_subnodes);
    utarray_push_back(binding->subnodes, list_expr);

    struct node_s *nd;
    nd = sealark_new_node(TK_LBRACK, without_subnodes);
    utarray_push_back(list_expr->subnodes, nd);

    struct node_s *expr_list
        = sealark_new_node(TK_Expr_List, with_subnodes);
    utarray_push_back(list_expr->subnodes, expr_list);

    char buf[64];

    for (int i = 0; i < vector_len; i++) {
        v = s7_vector_ref(s7, vals, i);
        /* log_debug("0 val %s", s7_object_to_c_string(s7, v)); */
        if (s7_is_integer(v)) {
            nd = sealark_new_node(TK_INT, without_subnodes);
            buf[0] = '\0';
            /* int ival = s7_integer(v); */
            /* log_debug("int: %d", ival); */
            snprintf(buf, 32, "%d", s7_integer(v));
            /* log_debug("buf: %s, i: %d", buf, i); */

            nd->s = calloc(strlen(buf)+1, sizeof(char));
            snprintf(nd->s, strlen(buf)+1, "%s", buf);
            utarray_push_back(expr_list->subnodes, nd);
            /* log_debug("vector_len: %d, i: %d", vector_len, i); */
            if ((vector_len - i) > 1) {
                nd = sealark_new_node(TK_COMMA, without_subnodes);
                utarray_push_back(expr_list->subnodes, nd);
            }
        }
        if (s7_is_string(v)) {
            nd = sealark_new_node(TK_STRING, without_subnodes);
            const char *buf = s7_string(v);
            nd->s = calloc(strlen(buf)+1, sizeof(char));
            snprintf(nd->s, strlen(buf)+1, "%s", buf);
            utarray_push_back(expr_list->subnodes, nd);
            /* log_debug("vector_len: %d, i: %d", vector_len, i); */
            if ((vector_len - i) > 1) {
                nd = sealark_new_node(TK_COMMA, without_subnodes);
                utarray_push_back(expr_list->subnodes, nd);
            }
        }
        if (s7_is_symbol(v)) {
            nd = sealark_new_node(TK_ID, without_subnodes);
            buf[0] = '\0';
            snprintf(buf, 64, "%s", s7_symbol_name(v));
            /* log_debug("buf: %s, i: %d", buf, i); */

            nd->s = calloc(strlen(buf)+1, sizeof(char));
            snprintf(nd->s, strlen(buf)+1, "%s", buf);
            utarray_push_back(expr_list->subnodes, nd);
            /* log_debug("vector_len: %d, i: %d", vector_len, i); */
            if ((vector_len - i) > 1) {
                nd = sealark_new_node(TK_COMMA, without_subnodes);
                utarray_push_back(expr_list->subnodes, nd);
            }
        }
        if (s7_is_boolean(v)) {
            nd = sealark_new_node(TK_ID, without_subnodes);
            buf[0] = '\0';
            if (v == s7_t(s7))
                snprintf(buf, 5, "%s", "True");
            else
                snprintf(buf, 6, "%s", "False");

            nd->s = calloc(strlen(buf)+1, sizeof(char));
            snprintf(nd->s, strlen(buf)+1, "%s", buf);
            utarray_push_back(expr_list->subnodes, nd);
            /* log_debug("vector_len: %d, i: %d", vector_len, i); */
            if ((vector_len - i) > 1) {
                nd = sealark_new_node(TK_COMMA, without_subnodes);
                utarray_push_back(expr_list->subnodes, nd);
            }
        }
    }
    nd = sealark_new_node(TK_RBRACK, without_subnodes);
    utarray_push_back(list_expr->subnodes, nd);
    return binding;
}

