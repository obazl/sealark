#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"

#include "s7.h"

#include "sunlark_functions.h"

/* **************** */
/** sunlark_node_ref_specialized

    (ast-node-ref obj key)
    takes two args, a ast_node object and a keyword to look up in the object.
 */
#define SUNLARK_NODE_REF_SPECIALIZED_HELP "(ast-node-ref nd k) returns the value for property k (a keyword) of ast-node nd."

/* sig: takes a node (satisfies node?) and an in (satisfies integer?),
   returns ... what does s7_t mean here? "something"? i.e not void? */
#define SUNLARK_NODE_REF_SPECIALIZED_SIG s7_make_signature(s7, 3, s7_t(s7), s7_make_symbol(s7, "node?"), s7_make_symbol(s7, "integer?"))

/** sunlark_node_ref_specialized

    Looks up node properties, whose names are keywords. Each field in
    the node_s struct has a property whose name is formed by prefixing
    a colon: :tid, :line, :col, :trailing_newline, :qtype, :s,
    :comments, :subnodes.

    In addition the following pseudo-properties are supported:
        :print - returns string for printable nodes, with correct quoting.

        :@<attr> - only for nodes of type :call_expr. returns binding
        (i.e. :binding node) whose :id is <attr>. E.g. (rulenode :deps)
        would return the 'deps' binding of the rulenode.

 */
static s7_pointer sunlark_node_ref_specialized(s7_scheme *s7, s7_pointer args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("sunlark_node_ref_specialized");
    debug_print_s7(s7, "sunlark_node_ref_specialized args: ", s7_cdr(args));
#endif

    struct node_s *self;
    /* size_t index; */
    s7_int typ;
    s7_pointer self_s7;

    if (s7_list_length(s7, s7_cdr(args)) < 1)
        return(s7_wrong_number_of_args_error(s7, "ast-node-ref takes 1 or  more arguments: ~S", s7_cdr(args)));

    self_s7 = s7_car(args);
    typ = s7_c_object_type(self_s7);
    if (typ != ast_node_t)
        return(s7_wrong_type_arg_error(s7, "ast-node-ref", 1, self_s7, "a ast_node"));
    self  = (struct node_s *)s7_c_object_value(self_s7);

    if (s7_is_null(s7, s7_cdr(args)))
        return(s7_wrong_type_arg_error(s7, "ast-node-ref", 1, self_s7, "missing ref arg"));
        /* return(s7_make_integer(s7, 32)); */

    s7_pointer params = s7_cdr(args);

    log_debug("get_target");
    /* may return c-objects (node, nodelist) or primitives (s7_integer) */
    /* s7_pointer get_target = sunlark_resolve_path(s7, self_s7, params); */
    /* if (s7_is_c_object(get_target)) { */
    /*     log_debug("get_target tid: %d", sunlark_node_tid(s7, get_target)); */
    /* } */
    /* return get_target; */
}

/* **************************************************************** */
/** sunlark_make_ast_node
 */
/* docstring passed to the s7_define_.. used to register the fn in Scheme */
#define SUNLARK_MAKE_AST_NODE_HELP "(make-ast-node) returns a new ast_node with randome data"

#define SUNLARK_MAKE_AST_NODE_FORMAL_PARAMS "(type 0) (line 0) (col 0) (trailing_newline #f) (qtype 0) (s NULL) (comments NULL) (subnodes NULL)"

LOCAL s7_pointer sunlark_make_ast_node(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_make_ast_node");
#endif

    /* struct node_s *new_ast_node = (struct node_s *) */
    /*     calloc(1, sizeof(struct node_s)); */
    /* new_ast_node = ast_node_init_default(new_ast_node); */

    struct node_s *n = sealark_new_node(0, /* will be initialized */
                                        without_subnodes);

    if (sunlark_node_init_from_s7(s7, n, args) != NULL) {
        log_debug("OOPS");
    }

    s7_pointer new_ast_node_s7 = s7_make_c_object(s7, ast_node_t,
                                                  (void *)n);

    sunlark_register_c_object_methods(s7, new_ast_node_s7);

    return(new_ast_node_s7);
}

#if INTERFACE
#define SUNLARK_IS_NODE_HELP "(node? obj) returns #t if obj is a node."
#define SUNLARK_IS_NODE_SIG s7_make_signature(s7, 2, s7_make_symbol(s7, "boolean?"), s7_t(s7))
#endif

/* called by Scheme 'node?'; internally, use c_is_sunlark_node (bool) */
s7_pointer sunlark_is_node(s7_scheme *s7, s7_pointer args)
{
#if defined(DEBUG_S7_API)
    log_debug(">>>>>>>>>>>>>>>> sunlark_is_node <<<<<<<<<<<<<<<<");
#endif
    /* log_debug("obj t %d, node t %d", */
    /*           s7_c_object_type(args), ast_node_t); */

    /* log_debug("car obj t %d, node t %d", */
    /*           s7_c_object_type(args), ast_node_t); */

    //NB: args is always a list

    return s7_make_boolean(s7, c_is_sunlark_node(s7, s7_car(args)));
}

/* **************************************************************** */
#if INTERFACE
#define SUNLARK_MAKE_STRING_HELP "(sunlark-make-string) returns a new :string node"

/* NB: we need to escape #\" in C... */
#define SUNLARK_MAKE_STRING_FORMAL_PARAMS "s (t :plain) (q #\\\") (qqq #f)"
#endif

EXPORT s7_pointer sunlark_make_string(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_make_string");
#endif

    log_debug("make-string args: %s", s7_object_to_c_string(s7, args));

    struct node_s *nd = sealark_new_node(TK_STRING, without_subnodes);

    if ( s7_is_string(s7_car(args)) ) {
        const char *s = s7_string(s7_car(args));
        nd->s = calloc(strlen(s), sizeof(char));
        strncpy(nd->s, s, strlen(s));
    } else {
        return(s7_error(s7,
                        s7_make_symbol(s7, "missing_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                                                      "First arg to sunlark-make-string must be a string: ~A"), args)));
    }

    /* nd->tid = TK_STRING; */
    s7_pointer type = s7_cadr(args);

    if (type == KW(plain)) {
        nd->qtype = 0;
    } else {
        if (type == KW(r)) {
            nd->qtype = RAW_STR;
        } else {
            if (type == KW(b)) {
                nd->qtype = BINARY_STR;
            } else {
                if (type == KW(br)) {
                    nd->qtype = (BINARY_STR | RAW_STR);
                } else {
                    if (type == KW(rb)) {
                        nd->qtype = (BINARY_STR | RAW_STR);
                    } else {
                        return(s7_error(s7,
                                        s7_make_symbol(s7, "invalid_argument"),
                                        s7_list(s7, 2, s7_make_string(s7,
                                                                      "val for key :t must be one of :plain (default), :r, :b, :br or :rb; got: ~A"),
                                                type)));
                    }
                }
            }
        }
    }

    s7_pointer qtype = s7_caddr(args);
    if ( !s7_is_character(qtype) ) {
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
              "val for key :qtype must char literal #\\\" (default) or #\\', got: ~A"),
                                qtype)));

    }
    if (qtype == s7_make_character(s7, '"')) {
        ; /* nd->qtype |= DQUOTE; */
    } else {
        if (qtype == s7_make_character(s7, '\'')) {
            nd->qtype |= SQUOTE;
        } else {
            return(s7_error(s7,
                            s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
     "val for key :q must be :dquote (default) or :squote; got: ~A"),
                                    qtype)));
        }
    }

    s7_pointer qqq   = s7_cadddr(args);
    if (!s7_is_boolean(qqq)) {
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
             "Val for arg :qqq must be #t or #f; got ~A"),
                                qqq)));
    }
    if (qqq == s7_f(s7) == 1) {
        ; // default
    } else {
        if (qqq == s7_t(s7)) {
            nd->qtype |= TRIPLE;
        } else {
            return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
     "val for key :qqq must be #t or #f (default); got: ~A"),
                                    qqq)));
        }
    }

    s7_pointer new_ast_node_s7 = s7_make_c_object(s7, ast_node_t,
                                                  (void *)nd);

    sunlark_register_c_object_methods(s7, new_ast_node_s7);

    return(new_ast_node_s7);
}

/* **************************************************************** */
struct node_s *_make_list_valued_binding(s7_scheme *s7,
                                         struct node_s *binding,
                                         s7_pointer val)
{
#ifdef DEBUG_TRACE
    log_debug("_make_list_valued_binding");
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
        log_debug("0 val %s", s7_object_to_c_string(s7, v));
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
            log_debug("list_len: %d, i: %d", list_len, i);
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
            log_debug("list_len: %d, i: %d", list_len, i);
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
            log_debug("list_len: %d, i: %d", list_len, i);
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
#if INTERFACE
#define SUNLARK_MAKE_BINDING_HELP "(make-binding) returns a new node of type :binding"

#define SUNLARK_MAKE_BINDING_FORMAL_PARAMS "key value"

/* make-binding takes a symbol as key; as val: bool, string, int, vector of ints or strings (which may include symbols); returns a node */
#define SUNLARK_MAKE_BINDING_SIG s7_make_signature(s7, 3, s7_make_symbol(s7, "node?"), s7_make_symbol(s7, "symbol?"),s7_make_symbol(s7, "symbol?"))
#endif
// NB: the sig does not seem to have any effect

EXPORT s7_pointer sunlark_make_binding(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_make_binding: %s",
              s7_object_to_c_string(s7, s7_car(args)));
#endif

    log_debug("sunlark-make-binding args: %s",
              s7_object_to_c_string(s7, args));

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

    log_debug("key: %s; type: %s", s7_object_to_c_string(s7, key),
              s7_object_to_c_string(s7, s7_type_of(s7, key)));
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
    log_debug("val %s", s7_object_to_c_string(s7, val));
    struct node_s *nd;
    if (s7_is_list(s7, val)) {
        binding = _make_list_valued_binding(s7, binding, val);
        goto resume;
    }
    if (s7_is_integer(val)) {
        log_debug("int %d", s7_integer(val));
        nd = sealark_new_node(TK_INT, without_subnodes);
        char buf[32];
        snprintf(buf, 32, "%d", s7_integer(val));
        nd->s = calloc(strlen(buf)+1, sizeof(char));
        snprintf(nd->s, strlen(buf)+1, buf);
        utarray_push_back(binding->subnodes, nd);
        goto resume;
    }
    if (s7_is_string(val)) {
        log_debug("string %s", s7_string(val));
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
        log_debug("symbol %s", s7_symbol_name(val));
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
        log_debug("boolean %d", s7_boolean(s7, val));
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
    log_error("UNCAUGHT binding val %s, satisfies %s",
              s7_object_to_c_string(s7, val),
              s7_object_to_c_string(s7, s7_type_of(s7, val)));
 resume:

    log_debug("new binding: %d %s, %s",
              binding->tid, TIDNAME(binding), binding->s);
    sealark_debug_print_ast_outline(binding, 0);

    s7_pointer new_ast_node_s7 = s7_make_c_object(s7, ast_node_t,
                                                  (void *)binding);

    sunlark_register_c_object_methods(s7, new_ast_node_s7);

    return new_ast_node_s7;
}

/* **************************************************************** */
void sunlark_register_ast_node_fns(s7_scheme *s7)
{
#if defined(DEBUG_CONFIG)
    log_debug("_register_ast_node_fns");
#endif
    /* s7_define_safe_function(s7, "ast-node", g_to_ast_node, 0, 0, true, sunlark_node_help); */

    /* constructors */
    s7_define_safe_function_star(s7, "make-ast-node",
                                 sunlark_make_ast_node,
                                 SUNLARK_MAKE_AST_NODE_FORMAL_PARAMS,
                                 SUNLARK_MAKE_AST_NODE_HELP);

    s7_define_safe_function_star(s7, "sunlark-make-string",
                                 sunlark_make_string,
                                 SUNLARK_MAKE_STRING_FORMAL_PARAMS,
                                 SUNLARK_MAKE_STRING_HELP);

    s7_define_typed_function(s7, "make-binding",
                             sunlark_make_binding,
                             2, 0, false,
                                  /* SUNLARK_MAKE_BINDING_FORMAL_PARAMS, */
                             SUNLARK_MAKE_BINDING_HELP,
                             SUNLARK_MAKE_BINDING_SIG);

    /* predicates */
    s7_define_typed_function(s7, "sunlark-node?", sunlark_is_node,
                             1, 0, false,
                             SUNLARK_IS_NODE_HELP,
                             SUNLARK_IS_NODE_SIG);

    /* specialized get/set! */
    s7_define_typed_function(s7, "ast-node-ref",
                             sunlark_node_ref_specialized,
                             2, 1, true,
                             SUNLARK_NODE_REF_SPECIALIZED_HELP,
                             SUNLARK_NODE_REF_SPECIALIZED_SIG);

    s7_define_typed_function(s7, "ast-node-set!",
                             sunlark_node_set_specialized,
                             3, 0, true,
                             SUNLARK_NODE_SET_SPECIALIZED_HELP,
                             SUNLARK_NODE_SET_SPECIALIZED_SIG);

    /* s7_define_typed_function(s7, "remove!", */
    s7_define_typed_function(s7, "remove!",
                             sunlark_remove_bang,
                             2, 0, true,
                             SUNLARK_REMOVE_BANG_HELP,
                             SUNLARK_REMOVE_BANG_SIG);

    /* serialization */
    s7_define_safe_function(s7, "sunlark->starlark",
                            sunlark_to_starlark,
                            1, 1, false,
                            SUNLARK_TO_STARLARK_HELP);

    // ast_node-let => s7_c_object_let, a let for the instance not the type
    /* s7_define_safe_function(s7, "ast-node-let", */
    /*                         sunlark_node_let, */
    /*                         1, 0, false, */
    /*                         sunlark_node_let_help); */

    /* parsing */
    s7_define_safe_function(s7,
                            "sunlark-parse-build-file",
                            sunlark_parse_build_file,
                            1, 0, false,
                            SUNLARK_PARSE_BUILD_FILE_HELP);

    s7_define_safe_function(s7,
                            "sunlark-parse-bzl-file",
                            sunlark_parse_bzl_file,
                            1, 0, false,
                            SUNLARK_PARSE_BZL_FILE_HELP);

    s7_define_safe_function(s7,
                            "sunlark-parse-string",
                            sunlark_parse_string,
                            1, 0, false,
                            SUNLARK_PARSE_STRING_HELP);


}
