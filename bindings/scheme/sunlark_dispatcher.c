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
#if defined(DEBUG_TRACE)
    log_debug("sunlark_dispatch: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    /* dispatch on tid; the tid handlers will dispatch on op */
    /* data is either a node or an s7 list of nodes  */

    //FIXME: handle final :$ here, so that subroutines can return
    //struct node_s *
    s7_pointer rev = s7_reverse(s7, path_args);
    s7_pointer last = s7_car(rev);
    s7_pointer but_last = s7_reverse(s7, s7_cdr(rev));
    if (last == s7_make_keyword(s7, "$")) {
        path_args = but_last;
    /* } else { */
    }

    int data_tid;
    if (s7_is_list(s7, data)) {
        // return sunlark_dispatch_on_list ??
        log_warn("dispatching on s7 list");
        exit(EXIT_FAILURE);     /* FIXME */
    }

    data_tid = sunlark_node_tid(s7, data);

#if defined(DEBUG_TRACE)
    log_debug("\tdata tid: %d %s",
              data_tid,
              token_name[data_tid][0]);
#endif

    s7_pointer op = s7_car(path_args);
    struct node_s *result_node;

    switch( data_tid ) {

    case TK_Package: /* build_target */
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_Package");
#endif
        return sunlark_package_dispatcher(s7, data, path_args);
        break;

    case TK_Load_Stmt:
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_Load_Stmt");
#endif
        return sunlark_loadstmt_dispatch(s7, data, path_args);
        break;

    case TK_Call_Expr: /* build_target */
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_Call_Expr");
#endif
        s7_pointer r = sunlark_target_dispatcher(s7,
                                                 s7_c_object_value(data),
                                                 /* data, */
                                                 path_args);
        if (r)
            return r;
        else {
            /* r = sunlark_common_property_lookup(s7, data, op); */
            /* return r; */
        }
        break;

    case TK_Arg_List: { /* bindings list */
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_Arg_List");
#endif
        s7_pointer x = sunlark_bindings_list_dispatcher(s7, data, path_args);
        return x;
    }
        break;

    case TK_Binding: {
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_Binding");
#endif
        bool p = sunlark_op_is_predicate(s7, op);
        if (p) {
            return sunlark_node_satisfies_kw_pred(s7, op,
                                                  s7_c_object_value(data));
        } else {
            errno = 0;
            struct node_s *r = sunlark_binding_dispatcher(s7,
                                              s7_c_object_value(data),
                                              path_args);
            if (r)
                return sunlark_node_new(s7, r);
            else
                if (errno == EUNKNOWN_KW) {
                    log_debug("unknown kw");
                    return sunlark_common_property_lookup(s7,
                                                          s7_c_object_value(data), op);
                } else {
                    return handle_errno(s7, errno,
                                        s7_list(s7, 2, data, path_args));
                }
        }
        /* FIXME: type of result? */
        return sunlark_node_new(s7, result_node);
    }
        break;

    case TK_ID:
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_ID");
#endif
        return _dispatch_on_id(s7, data, path_args);
        break;

    case TK_STRING:
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_STRING");
#endif
        return _dispatch_on_string(s7, data, path_args);
        break;

    case TK_INT:
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_INT");
#endif
        return _dispatch_on_int(s7, data, path_args);
        break;

    case TK_List_Expr: /* vector, e.g. as binding value */
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_List_Expr");
#endif
        return sunlark_dispatch_on_list_expr(s7, data, path_args);
        break;

    default:
        /* common properties work for any tid */
        return sunlark_dispatch_on_any(s7, data, path_args);
        /* log_error("not implemented: dispatch on tid %d", data_tid); */
        /* exit(EXIT_FAILURE);     /\* FIXME *\/ */
    }
}

/* **************** */
LOCAL s7_pointer _dispatch_on_string(s7_scheme *s7,
                                         s7_pointer node,
                                         s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("_dispatch_on_string: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    int op_count = s7_list_length(s7, path_args);
    /* log_debug("op count: %d", op_count); */

    if (op_count > 1) {
        //error, :s, :tid etc. allowed
        log_error("FIXME, only one op allowed here");
    }

    s7_pointer kw = s7_car(path_args);

    if (kw == KW(q)) {
        struct node_s *s = s7_c_object_value(node);
        if (s->qtype & SQUOTE)
            return s7_make_character(s7, '\'');
        else
            return s7_make_character(s7, '"');
    } else {
        if (kw == KW(qqq)) {
            struct node_s *s = s7_c_object_value(node);
            log_debug("qtype: %#04x", s->qtype);
            log_debug("TRIPLE: %#04x", TRIPLE);
            if (s->qtype & TRIPLE)
                return s7_t(s7);
            else
                return s7_f(s7);
        } else {
            if (kw == KW(t)) {
                struct node_s *s = s7_c_object_value(node);
                log_debug("qtype: %#4x", s->qtype);
                if (s->qtype & (BINARY_STR & RAW_STR))
                    return s7_make_keyword(s7, "br");
                else
                    if (s->qtype & RAW_STR)
                        return s7_make_keyword(s7, "r");
                    else
                        if (s->qtype & BINARY_STR)
                            return s7_make_keyword(s7, "b");
                        else {
                            return s7_make_keyword(s7, "plain");
                        }
            }
        }
    }
    s7_pointer result = sunlark_common_property_lookup(s7,
                                                       s7_c_object_value(node),
                                                       kw);
    return result;
}

/* **************** */
LOCAL s7_pointer _dispatch_on_int(s7_scheme *s7,
                                  s7_pointer node,
                                  s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("_dispatch_on_int: %s", s7_object_to_c_string(s7, path_args));
#endif

    int op_count = s7_list_length(s7, path_args);
    /* log_debug("op count: %d", op_count); */

    if (op_count > 1) {
        //error, :s, :tid etc. allowed
        log_error("FIXME, only one op allowed here");
    }

    s7_pointer kw = s7_car(path_args);

    s7_pointer result = sunlark_common_property_lookup(s7,
                                                       s7_c_object_value(node),
                                                       kw);
    return result;
}

/* **************** */
LOCAL s7_pointer _dispatch_on_id(s7_scheme *s7,
                                 s7_pointer node,
                                 s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("_dispatch_on_id: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    int op_count = s7_list_length(s7, path_args);
    log_debug("op count: %d", op_count);

    if (op_count > 1) {
        //error, :s, :tid etc. allowed
        log_error("FIXME, only one op allowed here");
    }

    s7_pointer kw = s7_car(path_args);

    s7_pointer result = sunlark_common_property_lookup(s7,
                                                       s7_c_object_value(node),
                                                       kw);
    return result;
}

/* **************** */
/*
  vectors (a/k/a starlark "lists") in the ast contain metadata like
  punctuation (commas, etc.) and delimiters. to index we need to skip
  those.
 */
LOCAL s7_pointer sunlark_dispatch_on_list_expr(s7_scheme *s7,
                                            s7_pointer data,
                                            s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_dispatch_on_list_expr: %s",
              s7_object_to_c_string(s7, path_args));
#endif
#if defined(DEBUG_AST)
    sealark_debug_log_ast_outline(s7_c_object_value(data), 0);
#endif

    int op_count = s7_list_length(s7, path_args);
    log_debug("op count: %d", op_count);

    if (op_count == 0)
        return data;

    /* vector ops:  int index, ? :print, :tid, etc. */
    if (op_count > 1) {
        //error, :s, :tid etc. allowed
        log_error("FIXME, only one op allowed here");
    }
    s7_pointer op;
    if (s7_is_list(s7,path_args))
        op = s7_car(path_args);
    else
        op = path_args;

    // :list-expr > :lbrack, :expr-list, :rbrack
    //  :expr-list > :string, :comma, etc.
    struct node_s *list_expr = s7_c_object_value(data);
    struct node_s *vector = utarray_eltptr(list_expr->subnodes, 1);

    /* tid: TK_Expr-List */
    log_debug("vector tid %d %s", vector->tid, TIDNAME(vector));

    // Let Bazel enforce list homogeneity in BUILD files
    /* treat first non-sym item as prototype, giving list type */
    /* struct node_s *prototype = utarray_eltptr(vector->subnodes, 0); */
    /* int item_type = prototype->tid; */
    /* log_debug("item_type: %d", item_type); */

    int item_ct = 0;

    if (s7_is_integer(op)) {
        int idx = s7_integer(op);
        log_debug("indexing on %d", idx);
        int len = utarray_len(vector->subnodes);
        if (idx > len) {
            log_error("index out of bounds: % > %", idx, len);
            return NULL;
        }
        /* index by (semantic) items, skipping metadata */
        struct node_s *node = NULL;
        while( (node
                =(struct node_s*)utarray_next(vector->subnodes, node)) ) {
            /* if (node->tid == item_type) { */
                if (item_ct == idx)
                    return sunlark_node_new(s7, node);
                item_ct++;
            /* } */
        }
        //FIXME: handle index out of bounds
        return s7_unspecified(s7);
    } else {
        s7_pointer result
            = sunlark_common_property_lookup(s7, list_expr, op);
        return result;
    }
}

/* **************** */
LOCAL s7_pointer sunlark_dispatch_on_any(s7_scheme *s7,
                                         s7_pointer node,
                                         s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_dispatch_on_any: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    log_debug("path args is list? %d", s7_is_list(s7, path_args));
    log_debug("path args is kw? %d", s7_is_keyword(path_args));
    log_debug("path args is sym? %d", s7_is_symbol(path_args));
    log_debug("path args is string? %d", s7_is_string(path_args));

    //FIXME: constraints on path_args

    /* should be only one path arg? */
    s7_pointer result = sunlark_common_property_lookup(s7,
                                                       s7_c_object_value(node),
                                                       s7_car(path_args));
    return result;
}
