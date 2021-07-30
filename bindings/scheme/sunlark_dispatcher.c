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

    s7_pointer op = s7_car(path_args);
    struct node_s *result_node;

    switch( data_tid ) {

    case TK_Build_File: /* build_target */
        log_debug("dispatching on TK_Build_File");
        return sunlark_dispatch_on_buildfile(s7, data, path_args);
        break;

    case TK_Call_Expr: /* build_target */
        log_debug("dispatching on TK_Call_Expr");
        return sunlark_dispatch_on_target(s7, data, path_args);
        break;

    case TK_Arg_List: /* bindings list */
        log_debug("dispatching on TK_Arg_List");
        s7_pointer x = sunlark_dispatch_on_bindings_list(s7, data, path_args);
        return x;
        break;

    case TK_Binding:
        log_debug("dispatching on TK_Binding");
        bool p = sunlark_op_is_predicate(s7, op);
        if (p) {
            return sunlark_node_is_kw_pred(s7, op, s7_c_object_value(data));
        } else {
            return sunlark_dispatch_on_binding(s7, data, path_args);
        }
        /* FIXME: type of result? */
        return sunlark_node_new(s7, result_node);
        break;

    case TK_ID:
        log_debug("dispatching on TK_ID");
        return _dispatch_on_id(s7, data, path_args);
        break;

    case TK_STRING:
        log_debug("dispatching on TK_STRING");
        return _dispatch_on_string(s7, data, path_args);
        break;

    case TK_INT:
        log_debug("dispatching on TK_INT");
        return _dispatch_on_int(s7, data, path_args);
        break;

    case TK_List_Expr: /* vector, e.g. as binding value */
        log_debug("dispatching on TK_List_Expr");
        return sunlark_dispatch_on_list_expr(s7, data, path_args);
        break;

    default:
        /* common properties work for any tid */
        return sunlark_dispatch_on_any(s7, data, path_args);
        /* log_error("not implemented: dispatch on tid %d", data_tid); */
        /* exit(EXIT_FAILURE);     /\* FIXME *\/ */
    }
}

//FIXME: LOCAL
s7_pointer sunlark_dispatch_on_buildfile(s7_scheme *s7,
                                         s7_pointer data,
                                         s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PATHS)
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
    /* log_debug("op count: %d", op_count); */

    s7_pointer op = s7_car(path_args);
    //s7_pointer op2, op3, op4, op5;
    if ( !s7_is_keyword(op) ) {
        log_error("Path op %s not supported for :build-file nodes",
                  s7_object_to_c_string(s7, op));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    s7_pointer result_list;

    if (op == KW(>>) || op == KW(targets)) {
        s7_pointer r
            = sunlark_forall_targets(s7, bf_node, s7_cdr(path_args));
        return r;
    }

    if (op == KW(>) || op == KW(target)) {
        result_list //= sunlark_targets_for_buildfile(s7, bf_node);
            = sunlark_target_select(s7, bf_node, s7_cdr(path_args));
            return result_list;
    }


    switch(op_count) {
    case 1:
        if (op == KW(>>) || op == KW(>) || op == KW(targets)) {
            result_list = sunlark_targets_for_buildfile(s7, bf_node);
            //FIXME: switch to:
            /* UT_array *loads = sealark_procs_for_id(bf_node, */
            /*                                       "target"); */
            return result_list;
        }
       if (op == KW(loads)) {
            // :build-file > :stmt-list :smallstmt-list > load-expr,...
           /* result_list = sunlark_fetch_load_stmts(s7, bf_node); */

           UT_array *loads = sealark_procs_for_id(bf_node,
                                                  "load");
           /* UT_array *loads = sealark_loadstmts(bf_node); */
           if (loads)
               return nodelist_to_s7_list(s7, loads);
           else
               log_debug("ERROR: ...fixme...");
        }
       if (op == KW(package)) {
           UT_array *procs = sealark_procs_for_id(bf_node,
                                                  "package");
           if (utarray_len(procs) == 1)
               return sunlark_node_new(s7,
                                       utarray_eltptr(procs, 0));
           else
               return s7_nil(s7);
        }
        if (op == KW(directives)) {
            /* all procs and definitions */
            UT_array *directives = sealark_directives(bf_node);
            return nodelist_to_s7_list(s7, directives);
            return NULL;
        }
        if (op == KW(definitions)) {
            /* sealark_debug_print_ast_outline(bf_node, 0); */
            UT_array *defns = sealark_definitions(bf_node);
            return nodelist_to_s7_list(s7, defns);
        }
        if (op == KW(vardefs)) {
            /* result_list = */
            UT_array *vardefs = sealark_vardefs(bf_node);
            return nodelist_to_s7_list(s7, vardefs);
        }
        if (op == KW(procedures)) {
            UT_array *procs = sealark_procs(bf_node);
            return nodelist_to_s7_list(s7, procs);
        }
        /* common properties */
        s7_pointer result = sunlark_common_property_lookup(s7, bf_node, op);
        if (result) return result;
        break;
    case 2:
        return buildfile_handle_dyadic_path(s7, bf_node, path_args);
        break;
    case 3:
        return buildfile_handle_triadic_path(s7, bf_node, path_args);
        break;
    case 4:
        return buildfile_handle_tetradic_path(s7, bf_node, path_args);
        break;
    case 5:
        return buildfile_handle_pentadic_path(s7, bf_node, path_args);
        break;
    default:
        return buildfile_handle_pentadic_path(s7, bf_node, path_args);
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
    sealark_debug_print_ast_outline(s7_c_object_value(data), 0);
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

    /* treat first item as prototype, giving list type */
    struct node_s *prototype = utarray_eltptr(vector->subnodes, 0);
    int item_type = prototype->tid;
    log_debug("item_type: %d", item_type);
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
            if (node->tid == item_type) {
                if (item_ct == idx)
                    return sunlark_node_new(s7, node);
                item_ct++;
            }
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
