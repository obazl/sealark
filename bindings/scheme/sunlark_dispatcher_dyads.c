#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_dispatcher_dyads.h"

/* **************************************************************** */
/* build file handlers */
s7_pointer buildfile_handle_dyadic_path(s7_scheme *s7,
                                        struct node_s *bf_node,
                                        s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("buildfile_handle_dyadic_path: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    /* int op_count = s7_list_length(s7, path_args); */
    /* log_debug("op count: %d", op_count); */

    s7_pointer op = s7_car(path_args);

    if ( !s7_is_keyword(op) ) {
        log_error("Path op %s not supported for :build-file nodes",
                  s7_object_to_c_string(s7, op));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    s7_pointer result_list;


    s7_pointer op2;
    op2 = s7_cadr(path_args);
    if (op == KW(targets)) {
        if (s7_is_symbol(op2)) {
            s7_pointer filter_list = s7_list(s7, 1,
                                             op2, s7_nil(s7));
            UT_array *l = sunlark_targets_from_filterlist(s7,
                                                            bf_node,
                                                            filter_list);
            return nodelist_to_s7_list(s7, l);
        }
        if (s7_is_integer(op2)) {
            log_debug("target_by_index %d", s7_integer(op2));
            /* struct node_s *n = utarray_eltptr(bf_node->subnodes, */
            /*                                   s7_integer(op)); */
            errno = 0;
            struct node_s *n = sealark_target_by_index(bf_node,
                                                       s7_integer(op2));
            if (n == NULL) {
                switch(errno) {
                case 2:
                    return(s7_error(s7,
                                    s7_make_symbol(s7, "invalid_index"),
                                    s7_list(s7, 2, s7_make_string(s7, "index ~D > target count"),
                                            op2)));
                    break;
                case 3:
                    return(s7_error(s7,
                                    s7_make_symbol(s7, "invalid_index"),
                                    s7_list(s7, 2,
                                            s7_make_string(s7,
                                                           "abs(~D) > target count"),
                                            op2)));
                }
            }
            result_list = sunlark_node_new(s7, n);
            return result_list;
        }
        if (s7_is_list(s7, op2)) {
            log_debug("filtering targets by list %s",
                      s7_object_to_c_string(s7, op2));
            UT_array *tgts = sunlark_targets_from_filterlist(s7,
                                                               bf_node,
                                                               op2);
            return nodelist_to_s7_list(s7, tgts);
        }
        if (s7_is_string(op2)) {
            log_error("String arg after :targets not supported; did you mean ':target'?");
            return(s7_error(s7,
                            s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                                                          "String arg \"~A\" after :targets not supported; did you mean ':target'?"),
                                    op2)));
        }
    }
    /* **************** */
    if (op == KW(target)) {
        if (s7_is_string(op2)) {
            struct node_s *n = sealark_target_for_name(bf_node,
                                                       s7_string(op2));
            /* struct node_s *n = sealark_target_for_name(bf_node, */
            /*                                            s7_string(op2)); */
            return sunlark_node_new(s7, n);
        }
        // only strings after :target
        log_error("Only string arg after :target");
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                      "Only string arg allowed after :target; got ~A"),
                                    op2)));
    }
    /* **************** */
    if (op == KW(load)) {
        if (s7_is_string(op2)) {
            struct node_s *loadstmt
                = sealark_loadstmt_for_name(bf_node, s7_string(op2));
            if (loadstmt)
                return sunlark_node_new(s7, loadstmt);
            else
                log_debug("ERROR: ...fixme...");
        }
        // only strings after :target
        log_error("Only string arg after :load");
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                      "Only string arg allowed after :load; got ~A"),
                                    op2)));
    }
    /* **************** */
    if (op == KW(loads)) {
        if (s7_is_integer(op2)) {
            log_debug("dyad_int_loads");
            struct node_s *loadstmt
                = sealark_loadstmt_by_index(bf_node, s7_integer(op2));
            return sunlark_node_new(s7, loadstmt);
        }
        if (s7_is_list(s7, op2)) {
            log_debug("loads_from_filterlist");
            /* UT_array *tgts = sunlark_targets_from_filterlist(s7, */
            /*                                                    bf_node, */
            /*                                                    op2); */
            /* return nodelist_to_s7_list(s7, tgts); */
            return NULL;
        }
        if (op2 == KW(count)) {
            log_debug("count_loads");
            return NULL;
        }
        // only strings after :target
        log_error("Only integer arg after :loads");
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                      "Only integer arg allowed after :loads; got ~A"),
                                    op2)));
    }
}

