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
    s7_pointer op6 = s7_cadr(s7_cddddr(path_args));

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
        if (s7_is_integer(op2)) { /* :targets index */
            if (op3 == KW(bindings)) {
                if (s7_is_symbol(op4)) {
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_sym_binding_int_target");
                        struct node_s *target
                            = sealark_target_for_index(bf_node,
                                                       s7_integer(op2));
                        struct node_s *binding
                            = sealark_target_binding_for_key(target,
                                                             s7_symbol_name(op4));
                        struct node_s *key
                            = utarray_eltptr(binding->subnodes, 0);
                        return sunlark_node_new(s7, key);
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_sym_bindings_int_target");
                        struct node_s *target
                            = sealark_target_for_index(bf_node,
                                                       s7_integer(op2));
                        struct node_s *binding
                            = sealark_target_binding_for_key(target,
                                                             s7_symbol_name(op4));
                        struct node_s *val
                            = utarray_eltptr(binding->subnodes, 2);
                        if (s7_is_list(s7, op6)) {
                            log_debug("op6: list");
                            return NULL;
                        }
                        if (s7_is_integer(op6)) {
                            if (val->tid == TK_List_Expr) {
                                struct node_s *item
                                    = sealark_list_item_for_index(val,
                                                                  s7_integer(op6));
                                return sunlark_node_new(s7, item);
                            }
                            /* if (val->tid == TK_STRING) { */
                            /*     //FIXME index into string? */
                            /*     return s7_make_character(s7, */
                            /*                              val->s[s7_integer(op6)]); */
                            /*     /\* return sunlark_node_new(s7, item); *\/ */
                            /* } */
                            log_error("Trying to index into node type %d %s", val->tid, TIDNAME(val));
                            exit(EXIT_FAILURE); //FIXME
                        } else {
                            return sunlark_node_new(s7, val);
                        }
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                if (s7_is_integer(op4)) {
                    /* same as prev sym op4 */
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_sym_binding_string_target");
                        return NULL;
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_sym_binding_string_target");
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
        log_error("Second path op %s not supported here; did you mean ':target %s'?",
                  s7_object_to_c_string(s7, op2),
                  s7_object_to_c_string(s7, op2)
                  );
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 4, s7_make_string(s7,
                        "~S: ~S not supported after :targets; did you mean (:target ~S...)?"),
                                path_args, op2, op2)));

        exit(EXIT_FAILURE);
    }

    /* ******************************** */
    if (op == KW(target)) {
        if (s7_is_string(op2)) {
            if (op3 == KW(bindings)) {
                if (s7_is_symbol(op4)) {
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_sym_bindings_string_target");
                        struct node_s *target
                            = sealark_target_for_name(bf_node, s7_string(op2));
                        struct node_s *binding
                            = sealark_target_binding_for_key(target,
                                                             s7_symbol_name(op4));
                        struct node_s *key
                            = utarray_eltptr(binding->subnodes, 0);
                        return sunlark_node_new(s7, key);
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_sym_bindings_string_target");
                        struct node_s *target
                            = sealark_target_for_name(bf_node, s7_string(op2));
                        struct node_s *binding
                            = sealark_target_binding_for_key(target,
                                                             s7_symbol_name(op4));
                        return sunlark_value_for_binding(s7, binding);
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                if (s7_is_integer(op4)) {
                    if (op5 == KW(key)) {
                        log_debug("pentad_key_int_bindings_string_target");
                        struct node_s *target
                            = sealark_target_for_name(bf_node, s7_string(op2));
                        struct node_s *binding
                            = sealark_target_binding_for_index(target,
                                                             s7_integer(op4));
                        struct node_s *key
                            = utarray_eltptr(binding->subnodes, 0);
                        return sunlark_node_new(s7, key);
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_int_bindings_string_target");
                        struct node_s *target
                            = sealark_target_for_name(bf_node, s7_string(op2));
                        struct node_s *binding
                            = sealark_target_binding_for_index(target,
                                                             s7_integer(op4));
                        struct node_s *val
                            = utarray_eltptr(binding->subnodes, 2);
                        return sunlark_node_new(s7, val);
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
                        struct node_s *load
                            = sealark_loadstmt_for_index(bf_node, s7_integer(op2));
                        UT_array *bindings
                            = sealark_loadstmt_bindings(load);
                        struct node_s *binding
                            = sealark_bindings_binding_for_key(bindings,
                                                      s7_symbol_name(op4));
                        struct node_s *key = utarray_eltptr(binding->subnodes, 0);
                        return sunlark_node_new(s7, key);
                    }
                    if (op5 == KW(value)) {
                        // (:loads 0 :bindings mysym :value)
                        log_debug("pentad_val_sym_bindings_int_loads");
                        struct node_s *load
                            = sealark_loadstmt_for_index(bf_node, s7_integer(op2));
                        UT_array *bindings
                            = sealark_loadstmt_bindings(load);
                        struct node_s *binding
                            = sealark_bindings_binding_for_key(bindings,
                                                      s7_symbol_name(op4));
                        struct node_s *val = utarray_eltptr(binding->subnodes, 2);
                        return sunlark_node_new(s7, val);
                    }
                    log_error("Fifth path op %s not supported here.",
                              s7_object_to_c_string(s7, op5));
                    exit(EXIT_FAILURE);
                }
                if (s7_is_integer(op4)) {
                    if (op5 == KW(key)) {
                        // (:loads 0 :bindings 0 :key)
                        log_debug("pentad_key_int_bindings_int_load");
                        struct node_s *load
                            = sealark_loadstmt_for_index(bf_node, s7_integer(op2));
                        UT_array *bindings
                            = sealark_loadstmt_bindings(load);
                        struct node_s *binding
                            = utarray_eltptr(bindings, s7_integer(op4));
                        struct node_s *key = utarray_eltptr(binding->subnodes, 0);
                        return sunlark_node_new(s7, key);
                    }
                    if (op5 == KW(value)) {
                        // (:loads 0 :bindings 0 :value)
                        log_debug("pentad_val_int_bindings_int_load");
                        struct node_s *load
                            = sealark_loadstmt_for_index(bf_node, s7_integer(op2));
                        struct node_s *binding
                            = sealark_loadstmt_binding_for_index(load,
                                                             s7_integer(op4));
                        /* UT_array *bindings */
                        /*     = sealark_loadstmt_bindings(load); */
                        /* struct node_s *binding */
                        /*     = utarray_eltptr(bindings, s7_integer(op4)); */
                        struct node_s *val = utarray_eltptr(binding->subnodes, 2);
                        return sunlark_node_new(s7, val);
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
                        return NULL;
                    }
                    if (op5 == KW(value)) {
                        log_debug("pentad_val_sym_bindings_string_load");
                        struct node_s *load
                            = sealark_loadstmt_for_name(bf_node, s7_string(op2));
                        UT_array *bindings
                            = sealark_loadstmt_bindings(load);
                        struct node_s *binding
                            = sealark_bindings_binding_for_key(bindings,
                                                               s7_symbol_name(op4));
                        struct node_s *val = utarray_eltptr(binding->subnodes, 2);
                        return sunlark_node_new(s7, val);
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
                log_debug("arglist_for_load_for_name");
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
    log_error("Path op %s not supported",
              s7_object_to_c_string(s7, op));
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 3, s7_make_string(s7,
                                      "~S: path op ~S not supported"),
                                path_args, op)));

}
