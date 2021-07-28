#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_dispatcher_triads.h"


s7_pointer buildfile_handle_triadic_path(s7_scheme *s7,
                                         struct node_s *bf_node,
                                         s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("buildfile_handle_triadic_path: %s",
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


    s7_pointer op2 = s7_cadr(path_args);
    s7_pointer op3 = s7_caddr(path_args);

    /* ******************************** */
    if (op == KW(targets)) {
        if (s7_is_symbol(op2)) {
            if (s7_is_integer(op3)) {
                log_debug("target_for_index_from_filtersym");
                struct node_s *tgt = sunlark_target_for_index_from_filtersym
                    (s7, bf_node, s7_symbol_name(op2), s7_integer(op3));
                     /* s7_pointer select_list); */
                return sunlark_node_new(s7, tgt);
            }
            log_error("Path op %s not supported following :targets ~s",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return NULL;
        }
        /* **************** */
        if (s7_is_list(s7, op2)) {
            if (s7_is_integer(op3)) {
                log_debug("target_for_index_from_filterlist");
                struct node_s *tgt = sunlark_target_for_index_from_filterlist
                    (s7, bf_node, op2, s7_integer(op3));
                     /* s7_pointer select_list); */
                return sunlark_node_new(s7, tgt);
            }
            if (op3 == KW(count)) {
                log_debug("count_of_targets_from_filterlist");
                return NULL;
            }
            log_error("Path op %s not supported following :targets %s",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return(s7_error(s7,
                            s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                  "Path op ~A not supported following :targets and filter"),
                                    op3)));
            return NULL;
        }
        /* **************** */
        if (s7_is_integer(op2)) {
            if (op3 == KW(bindings)) { /* plurual */
                log_debug("bindings_for_target_for_index");
                errno = 0;
                /* UT_array *bindings = sealark_bindings_for_target_for_index */
                /*     (bf_node, s7_integer(op2)); */
                /* result_list = nodelist_to_s7_list(s7, bindings); */
                struct node_s *bindings = sealark_bindings_for_target_for_index
                    (bf_node, s7_integer(op2));
                return sunlark_node_new(s7, bindings);
            }
            if (op3 == KW(binding)) { /* singular */
                log_error("Not yet supported: %s",
                          s7_object_to_c_string(s7, op3));
                return(s7_error(s7,
                                s7_make_symbol(s7, "invalid_argument"),
                                s7_list(s7, 3,
                                        s7_make_string(s7,
                     "Bad arg '~A' in ~A; did you mean ':binding'?"),
                                        op3, path_args)));
            }
            if (op3 == KW(arg-list)) {
                log_debug("arglist_for_target_for_index");
                errno = 0;
                struct node_s *arglist = sealark_arglist_for_target_for_index
                    (bf_node, s7_integer(op2));
                result_list = nodelist_to_s7_list(s7, arglist->subnodes);
                return result_list;
            }
            if (op3 == KW(rule)) {
                log_debug("ruleid_for_target_for_index");
                struct node_s *ruleid = sealark_ruleid_for_target_for_index
                    (bf_node, s7_integer(op2));
                /* log_debug("ruleid: %d %s", ruleid->tid, TIDNAME(ruleid)); */
                return sunlark_node_new(s7, ruleid);
            }
            log_error("Not yet supported: %s", s7_object_to_c_string(s7, op3));
            exit(EXIT_FAILURE);
        }
        /* **************** */
        if (s7_is_string(op2)) {
            log_error("String arg after :targets not supported; did you mean ':target'?");
            return(s7_error(s7,
                            s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                                                          "String arg \"~A\" after :targets not supported; did you mean ':target'?"),
                                    op2)));
        }
        /* **************** */
        // op not supported..
    }

    /* ******************************** */
    if (op == KW(target)) {
        if (s7_is_string(op2)) {
            if (op3 == KW(bindings)) {
                log_debug("bindings_for_target_for_name");
                errno = 0;
                /* UT_array *bindings = sealark_bindings_for_target_for_name */
                /*     (bf_node, s7_string(op2)); */
                /* result_list = nodelist_to_s7_list(s7, bindings); */
                /* return result_list; */
                struct node_s *bindings = sealark_bindings_for_target_for_name
                    (bf_node, s7_string(op2));
                return sunlark_node_new(s7, bindings);
            }
            if (op3 == KW(arg-list)) {
                log_debug("arglist_for_target_for_name");
                errno = 0;
                struct node_s *arglist = sealark_arglist_for_target_for_name
                    (bf_node, s7_string(op2));
                result_list = nodelist_to_s7_list(s7, arglist->subnodes);
                return result_list;
            }
            if (op3 == KW(rule)) {
                /* log_debug("ruleid_for_target_for_name"); */
                struct node_s * tgt = sealark_ruleid_for_target_for_name
                    (bf_node, s7_string(op2));
                return sunlark_node_new(s7, tgt);
            }
        }
        if (op == KW(target)) {
            log_error("String arg not allowed after :target; did you mean \":targets %d\" ?", s7_integer(op2));
            return(s7_error(s7,
                            s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                                                          "String arg not allowed after :target; did you mean \":targets ~A\" ?"),
                                    op2)));

        } else {
            // only strings after :target
            log_error("Only string arg after :target");
            return(s7_error(s7,
                            s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                                                          "Only string arg allowd after :target; got ~A"),
                                    op2)));
        }
    }
    /* ******************************** */
    if (op == KW(loads)) {
        if (s7_is_list(s7, op2)) {
            if (op3 == KW(count)) {
                log_debug("counting filtered load stmts");
                return NULL;
            }
            log_error("Path op %s not supported following :loads ~s",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return NULL;
        }
        if (s7_is_integer(op2)) {
            if (op3 == KW(key)) {
                log_debug("key_for_load_for_index_from_loads");
                return NULL;
            }
            if (op3 == KW(args)) {
                log_debug("triad_args_int_loads");
                struct node_s *load =
                    sealark_loadstmt_for_index(bf_node, s7_integer(op2));
                UT_array *args =
                    sealark_loadstmt_args(load);
                return nodelist_to_s7_list(s7, args);
            }
            if (op3 == KW(bindings)) {
                log_debug("triad_bindings_int_loads");
                return NULL;
            }
            log_error("Path op %s not supported following :loads ~s",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return NULL;
        }
        if (s7_is_string(op2)) {
            log_error("String op %s not supported following :loads",
                      s7_object_to_c_string(s7, op3));
            return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                            "String op not supported following :loads; got ~S"),
                                    op2)));
            return NULL;
        }
    }

    /* ******************************** */
    if (op == KW(load)) {
        if (s7_is_string(op2)) {
            if (op3 == KW(args)) {
                log_debug("triad_args_string_load");
                struct node_s *load =
                    sealark_loadstmt_for_name(bf_node, s7_string(op2));
                UT_array *args =
                    sealark_loadstmt_args(load);
                return nodelist_to_s7_list(s7, args);
            }
            if (op3 == KW(bindings)) {
                log_debug("triad_bindings_string_load");
                struct node_s *load =
                    sealark_loadstmt_for_name(bf_node, s7_string(op2));
                UT_array *args =
                    sealark_loadstmt_bindings(load);
                return nodelist_to_s7_list(s7, args);
            }
            if (op3 == KW(arg-list)) {
                log_debug("triad_arglist_string_load");
                struct node_s *load =
                    sealark_loadstmt_for_name(bf_node, s7_string(op2));
                UT_array *args =
                    sealark_loadstmt_arglist(load);
                return nodelist_to_s7_list(s7, args);
            }
            log_error("Third path op %s not supported here.",
                      s7_object_to_c_string(s7, op3));
            return NULL;
        }
        log_error("Second path op %s not supported here.",
                  s7_object_to_c_string(s7, op2));
        if (s7_is_integer(op2))
            log_error("Did you mean :loads %d ?",
                  s7_integer(op2));
        return NULL;
    }
}

