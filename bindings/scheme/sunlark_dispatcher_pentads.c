#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_dispatcher_pentads.h"

/* ************************************************** */
s7_pointer buildfile_handle_pentadic_path(s7_scheme *s7,
                                         struct node_s *bf_node,
                                         s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("buildfile_handle_pentadic_path: %s",
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
    s7_pointer op4 = s7_cadddr(path_args);
    s7_pointer op5 = s7_car(s7_cddddr(path_args));

    /* ******************************** */
    if (op == KW(targets)) {
        if (s7_is_symbol(op2)) { /* filtersym, i.e. rule kind */
            if (s7_is_integer(op3)) {
                if (op4 = KW(bindings)) {
                    if (s7_is_symbol(op5)) {
                        log_debug("pentadic_sym_bindings_int_sym_targets");
                        /* struct node_s val */
                        /*     = sealark_pentadic_sym_bindings_int_sym_targets */
                        /*     (bf_node, op2, op3); */
                        return NULL;
                    }
                    if (s7_is_integer(op5)) {
                        log_debug("pentadic_int_bindings_int_sym_targets");
                        /* struct node_s val */
                        /*     = sealark_pentadic_int_bindings_int_sym_targets */
                        /*     (bf_node, op2, op3); */
                        return NULL;
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                log_error("Fourth path op %s not supported here.",
                          s7_object_to_c_string(s7, op4));
                exit(EXIT_FAILURE);
            }
            log_error("Third path op %s not supported here.",
                          s7_object_to_c_string(s7, op3));
            exit(EXIT_FAILURE);
        }
        /* **************** */
        if (s7_is_list(s7, op2)) { /* filter list */
            if (s7_is_integer(op3)) {
                if (op4 = KW(bindings)) {
                    if (s7_is_symbol(op5)) {
                        log_debug("pentad_sym_bindings_int_filter_targets");
                        return NULL;
                    }
                    if (s7_is_integer(op5)) {
                        log_debug("pentad_int_bindings_int_filter_targets");
                        return NULL;
                    }
                    log_error("Fifth path op %s not supported here.",
                          s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                log_error("Fourth path op %s not supported here.",
                          s7_object_to_c_string(s7, op4));
                exit(EXIT_FAILURE);
            }
            log_error("Third path op %s not supported here.",
                          s7_object_to_c_string(s7, op3));
            exit(EXIT_FAILURE);
        }
        /* **************** */
        if (s7_is_integer(op2)) { /* index */
            if (op3 == KW(bindings)) {
                if (s7_is_symbol(op4)) {
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_sym_binding_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_key_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_sym_binding_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_val_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                if (s7_is_integer(op4)) {
                    /* same as prev sym op4 */
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_sym_binding_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_key_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_sym_binding_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_val_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                log_error("Fourth path op %s not supported here.",
                          s7_object_to_c_string(s7, op4));
                exit(EXIT_FAILURE);
            }
            if (op3 == KW(arg-list)) {
                /* same as for op3 = :bindings */
                log_error("not yet implemented %s",
                              s7_object_to_c_string(s7, path_args));
                    exit(EXIT_FAILURE);
            }
            log_error("Third path op %s not supported here.",
                      s7_object_to_c_string(s7, op3));
            exit(EXIT_FAILURE);
        }
        log_error("Second path op %s not supported here.",
                  s7_object_to_c_string(s7, op2));
        exit(EXIT_FAILURE);
    }

    /* ******************************** */
    if (op == KW(target)) {
        if (s7_is_string(op2)) {
            if (op3 == KW(bindings)) {
                if (s7_is_symbol(op4)) {
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_sym_bindings_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_key_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_sym_bindings_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_val_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                if (s7_is_integer(op4)) {
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_int_bindings_string_target");
                        struct node_s *load
                            = sealark_loadstmt_for_name(bf_node, s7_string(op2));
                        UT_array *bindings
                            = sealark_loadstmt_bindings(load);
                        struct node_s *binding
                            = utarray_eltptr(bindings, s7_integer(op4));
                        struct node_s *key = utarray_eltptr(binding->subnodes, 0);
                        return sunlark_node_new(s7, key);
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_int_bindings_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_val_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                    log_error("Fourth path op %s not supported here.",
                              s7_object_to_c_string(s7, op4));
                    exit(EXIT_FAILURE);
            }
            if (op3 == KW(arg-list)) {
                // same options as for :binding
                if (s7_is_symbol(op4)) {
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_sym_arglist_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_key_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_sym_arglist_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_val_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                if (s7_is_integer(op4)) {
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_int_arglist_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_key_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_int_arglist_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_val_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                log_error("Fourth path op %s not supported here.",
                          s7_object_to_c_string(s7, op4));
                exit(EXIT_FAILURE);
            }
            log_error("Third path op %s not supported here.",
                      s7_object_to_c_string(s7, op3));
            exit(EXIT_FAILURE);
        }
        log_error("Second path op %s not supported here.",
                  s7_object_to_c_string(s7, op2));
        exit(EXIT_FAILURE);
    }

    /* ******************************** */
    if (op == KW(loads)) {
        if (s7_is_integer(op2)) {
            if (op3 == KW(bindings)) {
                if (s7_is_symbol(op4)) {
                    if (op5 == KW(key)) {
                        // (:loads 0 :bindings mysym :key)
                        log_debug("pentad_key_sym_bindings_int_loads");
                        return NULL;
                    }
                    if (op5 == KW(value)) {
                        // (:loads 0 :bindings mysym :value)
                        log_debug("pentad_val_sym_bindings_int_loads");
                        return NULL;
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                if (s7_is_integer(op4)) {
                    if (op5 == KW(key)) {
                        // (:loads 0 :bindings 0 :key)
                        log_debug("pentad_key_int_bindings_int_load");
                        return NULL;
                    }
                    if (op5 == KW(value)) {
                        // (:loads 0 :bindings 0 :value)
                        log_debug("pentad_val_int_bindings_int_load");
                        return NULL;
                    }
                    log_error("Fifth path op '%s' not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                log_error("Fourth path op %s not supported here.",
                          s7_object_to_c_string(s7, op4));
                exit(EXIT_FAILURE);
            }
            log_error("Third path op %s not supported here",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return NULL;
        }
        log_error("Second path op %s not supported here.",
                  s7_object_to_c_string(s7, op2));
        exit(EXIT_FAILURE);
    }

    /* ******************************** */
    if (op == KW(load)) {
        if (s7_is_string(op2)) {
            if (op3 == KW(bindings)) {
                if (s7_is_symbol(op4)) {
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_sym_bindings_string_load");
                        /* struct node_s *binding */
                        /*     = sealark_key_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_sym_bindings_string_load");
                        /* struct node_s *binding */
                        /*     = sealark_val_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                if (s7_is_integer(op4)) {
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_int_bindings_string_load");
                        struct node_s *load
                            = sealark_loadstmt_for_name(bf_node, s7_string(op2));
                        UT_array *bindings
                            = sealark_loadstmt_bindings(load);
                        struct node_s *binding
                            = utarray_eltptr(bindings, s7_integer(op4));
                        struct node_s *key = utarray_eltptr(binding->subnodes, 0);
                        return sunlark_node_new(s7, key);
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_int_bindings_string_load");
                        struct node_s *load
                            = sealark_loadstmt_for_name(bf_node, s7_string(op2));
                        UT_array *bindings
                            = sealark_loadstmt_bindings(load);
                        struct node_s *binding
                            = utarray_eltptr(bindings, s7_integer(op4));
                        struct node_s *key = utarray_eltptr(binding->subnodes, 2);
                        return sunlark_node_new(s7, key);
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                log_error("Fourth path op %s not supported here.",
                          s7_object_to_c_string(s7, op4));
                exit(EXIT_FAILURE);

                return NULL;
            }
            if (op3 == KW(arg-list)) {
                log_debug("arglist_for_load_by_name");
                if (s7_is_integer(op4)) {
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_int_arglist_string_load");
                        /* struct node_s *binding */
                        /*     = sealark_key_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_int_arglist_string_target");
                        /* struct node_s *binding */
                        /*     = sealark_val_sym_bindings_string_target(bf_node, s7_string(op2), s7_symbol_name(op4)); */
                        /* return sunlark_node_new(s7, binding); */
                        return NULL;
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                log_error("Fourth path op %s not supported here.",
                          s7_object_to_c_string(s7, op4));
                exit(EXIT_FAILURE);
            }
            /* NB: :args not used in pentads */
            log_error("Path op %s not supported following :loads ~s",
                      s7_object_to_c_string(s7, op3),
                      s7_symbol_name(op));
            return NULL;
        }
    }
}
