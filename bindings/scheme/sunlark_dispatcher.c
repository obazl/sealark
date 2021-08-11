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
    struct node_s *resnode;

    /* log_debug("path adjusted: %s", s7_object_to_c_string(s7, path_args)); */
    /* log_debug("last arg: %s", s7_object_to_c_string(s7, last)); */

    if (s7_is_null(s7, path_args)) {
        /* if adjusted path is () then we should have last == :$ */
        /* (we cannot arrive here with () args) */
        if (last == s7_make_keyword(s7, "$")) {
            /* struct node_s *ob = s7_c_object_value(data); */
            /* switch(ob->tid) { */
            /* case TK_List_Expr: { */
            /*     struct node_s *vr */
            /*         = sunlark_vector_dispatcher(s7, s7_c_object_value(data), */
            /*                                     s7_list(s7, 1, last)); */
            /*     /\* if (vr) *\/ */
            /*     return sunlark_new_node(s7, vr); */
            /* } */
            /*     break; */
            /* case TK_Dict_Expr: */
            /*     break; */
            /* default: */
                return node_to_scheme(s7, s7_c_object_value(data));
            /* } */
        }else {
            log_error("Expected :$, got %s", s7_object_to_c_string(s7, last));
            return handle_errno(s7, EUNEXPECTED_STATE, path_args);
        }
    }

    switch( data_tid ) {

    case TK_Package: /* build_target */
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_Package");
#endif
        {
            s7_pointer p = sunlark_package_dispatcher(s7, data, path_args);
            if (last == s7_make_keyword(s7, "$")) {
                return node_to_scheme(s7, s7_c_object_value(p));
            } else {
                return p;
            }
        }
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
        {
            bool p = sunlark_op_is_predicate(s7, op);
            if (p) {
                return sunlark_node_satisfies_kw_pred(s7, op,
                                                      s7_c_object_value(data));
            } else {
                errno = 0;
                s7_pointer r = sunlark_target_dispatcher(s7,
                                                         s7_c_object_value(data),
                                                         /* data, */
                                                         path_args);
                if (last == s7_make_keyword(s7, "$")) {
                    return node_to_scheme(s7, s7_c_object_value(r));
                } else {
                    if (r)
                        return r;
                    else {
                        if (errno == EUNKNOWN_KW) {
                            log_debug("unknown kw");
                            return sunlark_common_property_lookup(s7,
                                                                  s7_c_object_value(data), op);
                        } else {
                            return handle_errno(s7, errno,
                                                s7_list(s7, 2, data, path_args));
                        }
                    }
                }
                /* FIXME: type of result? */
                log_error("wtf ????????????????");
                return sunlark_new_node(s7, resnode);
            }
            /* if (r) */
            /*     return r; */
            /* else { */
            /*     /\* r = sunlark_common_property_lookup(s7, data, op); *\/ */
            /*     /\* return r; *\/ */
            /* } */
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

    case TK_Binding:
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_Binding");
#endif
        {
        bool p = sunlark_op_is_predicate(s7, op);
        if (p) {
            return sunlark_node_satisfies_kw_pred(s7, op,
                                                  s7_c_object_value(data));
        } else {
            errno = 0;
            s7_pointer res = sunlark_binding_dispatcher(s7,
                                              s7_c_object_value(data),
                                              path_args);
            if (res)
                return res; //sunlark_new_node(s7, res);
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
        log_error("wtf ????????????????");
        return sunlark_new_node(s7, resnode);
    }
        break;

    case TK_Dict_Entry:
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_Dict_Entry");
#endif
        errno = 0;
        s7_pointer d
            = sunlark_dict_entry_dispatcher(s7, s7_c_object_value(data),
                                            path_args);
        if (d)
            return d; //sunlark_new_node(s7, resnode);
        else
            if (errno == 0) {
                s7_pointer op;
                if (s7_is_list(s7,path_args))
                    op = s7_car(path_args);
                else
                    op = path_args;
                errno = 0;
                s7_pointer result
                    = sunlark_common_property_lookup(s7,
                                                     s7_c_object_value(data),
                                                     op);
                if (result)
                    return result;
                else
                    return handle_errno(s7, errno, path_args);
            } else {
                return handle_errno(s7, errno, path_args);
            }
        break;

    case TK_Dict_Expr:
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_Dict_Expr");
#endif
        errno = 0;
        /* resnode */
        return sunlark_dict_expr_dispatcher(s7, s7_c_object_value(data),
                                           path_args);
        /* if (resnode) */
        /*     return sunlark_new_node(s7, resnode); */
        /* else */
        /*     return handle_errno(s7, errno, path_args); */
        break;

    case TK_ID:
#if defined(DEBUG_TRACE)
        log_debug("dispatching on TK_ID, path: %s",
                  s7_object_to_c_string(s7, path_args));
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
        {
        s7_pointer vec = sunlark_vector_dispatcher(s7,
                                   s7_c_object_value(data), path_args);
        if (vec)
            return vec; // sunlark_new_node(s7, resnode);
        else
            if (errno == 0) {
                s7_pointer op;
                if (s7_is_list(s7,path_args))
                    op = s7_car(path_args);
                else
                    op = path_args;
                errno = 0;
                s7_pointer result
                    = sunlark_common_property_lookup(s7,
                                                     s7_c_object_value(data),
                                                     op);
                if (result)
                    return result;
                else
                    return handle_errno(s7, errno, path_args);
            } else {
                return handle_errno(s7, errno, path_args);
            }
        /* return sunlark_dispatch_on_list_expr(s7, data, path_args); */
        }
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

    s7_pointer result
        = sunlark_common_property_lookup(s7, s7_c_object_value(node), kw);
    return result;
}

/* **************** */
/*
  vectors (a/k/a starlark "lists") in the ast contain metadata like
  punctuation (commas, etc.) and delimiters. to index we need to skip
  those.
 */
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
