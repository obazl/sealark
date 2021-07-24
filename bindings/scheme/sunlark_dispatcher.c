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

    /* s7_pointer op = s7_car(path_args); */

    switch( data_tid ) {

    case TK_Build_File: /* build_target */
        log_debug("dispatching on TK_Build_File");
        return sunlark_dispatch_on_buildfile(s7, data, path_args);
        break;

    case TK_Call_Expr: /* build_target */
        log_debug("dispatching on TK_Call_Expr");
        return sunlark_dispatch_on_target(s7, data, path_args);
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
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
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
    log_debug("op count: %d", op_count);

    s7_pointer op = s7_car(path_args);
    //s7_pointer op2, op3, op4, op5;
    if ( !s7_is_keyword(op) ) {
        log_error("Path op %s not supported for :build-file nodes",
                  s7_object_to_c_string(s7, op));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    s7_pointer result_list;

    switch(op_count) {
    case 1:
        /* if (op == KW(all)) { */
        /*     result_list = sunlark_toplevel(s7, bf_node); */
        /*     return result_list; */
        /* } */
        if (op == KW(targets)) {
            result_list = sunlark_targets_for_buildfile(s7, bf_node);
            return result_list;
        }
       if (op == KW(loads)) {
            // :build-file > :stmt-list :smallstmt-list > load-expr,...
           /* result_list = sunlark_fetch_load_stmts(s7, bf_node); */
           UT_array *loads = sealark_loadstmts(bf_node);
           if (loads)
               return nodelist_to_s7_list(s7, loads);
           else
               log_debug("ERROR: ...fixme...");
        }
       if (op == KW(package)) {
           /* not yet */
           result_list = s7_unspecified(s7);
        }
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
        ;
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
//FIXME: LOCAL
s7_pointer sunlark_dispatch_on_target(s7_scheme *s7,
                                      s7_pointer data,
                                      s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_dispatch_on_target: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    struct node_s *target = s7_c_object_value(data);

    if (target->tid != TK_Call_Expr) {
        log_error("Expected node tid %d, got %d %s", TK_Call_Expr,
                  target->tid, TIDNAME(target));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    int op_count = s7_list_length(s7, path_args);
    s7_pointer op = s7_car(path_args);

    s7_pointer result_list;

    switch(op_count) {
    case 0:
        log_error("not enough path steps");
        exit(EXIT_FAILURE);     /* FIXME */
    case 1:
        if (KW(arg-list) == op) {
            struct node_s *arg_list=sealark_arglist_for_target(target);
            return nodelist_to_s7_list(s7, arg_list->subnodes);
        }
        if (KW(bindings) == op) {
            UT_array *bindings = sealark_bindings_for_target(target);
            return nodelist_to_s7_list(s7, bindings);
        }
        if (KW(rule) == op) {
            struct node_s *id=sealark_ruleid_for_target(target);
            return sunlark_node_new(s7, id);
        }
        /* common properties */
        s7_pointer result = sunlark_common_property_lookup(s7, target, op);
        if (result) return result;

        log_error("dispatch on %s for target not yet implemented",
                  s7_object_to_c_string(s7, op));
        break;
    case 2:
        // :bindings 'sym -- returns node for binding with name sym
        // :bindings :count
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

LOCAL s7_pointer sunlark_dispatch_on_any(s7_scheme *s7,
                                         s7_pointer node,
                                         s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_dispatch_on_any: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    /* should be only one path arg? */
    s7_pointer result = sunlark_common_property_lookup(s7,
                                                       s7_c_object_value(node),
                                                       s7_car(path_args));
    return result;
}
