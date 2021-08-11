#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_loadstmts.h"

/* **************************************************************** */
EXPORT s7_pointer sunlark_loadstmt_dispatch(s7_scheme *s7,
                                               s7_pointer _loadstmt,
                                               s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sunlark_dispatch_on_loadstmt: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    struct node_s *loadstmt = s7_c_object_value(_loadstmt);

    assert(loadstmt->tid == TK_Load_Stmt);

    return _loadstmt_dispatch(s7, loadstmt, path_args);

}

/* **************************************************************** */
/* selector: :args OR :bindings OR :arg <sel> OR :binding <sel> */
LOCAL s7_pointer _loadstmt_dispatch(s7_scheme *s7,
                                      struct node_s *loadstmt,
                                      s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("_loadstmt_dispatch %s",
              s7_object_to_c_string(s7, path_args));
#endif

    assert(loadstmt->tid == TK_Load_Stmt);

    s7_pointer op = s7_car(path_args);

    /* **************** */
    if (op == KW(arg)) { /* must be followed by selector */
        if (s7_is_null(s7, s7_cdr(path_args))) {
            log_debug(":arg must be followed by selector");
            errno = EMISSING_ARG_SELECTOR;
            return NULL;
        } else {
            errno = 0;
            s7_pointer arg
                = sunlark_loadstmt_arg_dispatcher(s7, loadstmt,
                                                   s7_cdr(path_args));
            if (arg) {
                return arg;
            } else {
                /* errno already set */
                return NULL;
            }
        }
    }

    if (op == KW(args)) {
        if (s7_is_null(s7, s7_cdr(path_args))) {
            UT_array *args
                = sealark_loadstmt_args(loadstmt);
            if (args) {
                return nodelist_to_s7_list(s7, args);
            } else {
                log_debug("no args found");
                errno = ENOT_FOUND;
                return NULL;
            }
        } else {
            log_error(":args may not be followed by another arg: %s",
                      s7_object_to_c_string(s7, path_args));
            errno = ETOO_MANY_ARGS;
            return NULL;
        }
    }

    if (op == KW(bindings) || op == KW(@@) || op == KW(attrs)) {
        if (s7_is_null(s7, s7_cdr(path_args))) {
            sealark_debug_log_ast_outline(loadstmt, 0);
            UT_array *bindings
                = sealark_loadstmt_bindings(loadstmt);
            if (bindings) {
                return nodelist_to_s7_list(s7, bindings);
            } else {
                log_debug("no bindings found");
                errno = ENOT_FOUND;
                return NULL;
            }
        } else {
            log_error(":bindings may not be followed by another arg: %s",
                      s7_object_to_c_string(s7, path_args));
            errno = ETOO_MANY_ARGS_BINDINGS;
            return NULL;
        }
    }

    if (op == KW(@) || op == KW(binding) || op == KW(attr)) {
        if (s7_is_null(s7, s7_cdr(path_args))) {
            log_error(":binding must be followed by another arg: %s",
                      s7_object_to_c_string(s7, path_args));
            errno = EMISSING_ARG_BINDING;
            return NULL;
        } else {
            s7_pointer binding
                = _loadstmt_binding_dispatcher(s7, loadstmt,
                                                      s7_cdr(path_args));
            if (binding) {
                return binding;
            } else {
                /* errno already set */
                return NULL;
            }
        }
    }

    if (s7_is_keyword(op)) {
        s7_pointer result
            = sunlark_common_property_lookup(s7, loadstmt, op);
        return result;
    }

    log_error("NOT YET: sunlark_loadstmt_select for %s",
              s7_object_to_c_string(s7, path_args));
    errno = ENOT_IMPLEMENTED;
    return NULL;
}

/* **************************************************************** */
/* selector: int OR string (name) OR common property */
s7_pointer sunlark_loadstmt_arg_dispatcher(s7_scheme *s7,
                                          struct node_s *loadstmt,
                                          s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sunlark_loadstmt_arg_dispatcher %s",
              s7_object_to_c_string(s7, path_args));
#endif

    assert(loadstmt->tid == TK_Load_Stmt);

    s7_pointer op = s7_car(path_args);

    /* **************** */
    if (s7_is_integer(op)) {
        struct node_s *arg
            = sealark_loadstmt_arg_for_int(loadstmt, s7_integer(op));
        if (s7_is_null(s7, s7_cdr(path_args))) {
            if (arg) {
                return sunlark_new_node(s7, arg);
            } else {
                /* errno set by sealark */
                return NULL;
            }
        } else {
            if (s7_list_length(s7, path_args) > 2) {
                log_error("Too many args: %s",
                          s7_object_to_c_string(s7, path_args));
                errno = ETOO_MANY_ARGS;
                return NULL;
            } else {
                s7_pointer sel = s7_cadr(path_args);
                if (s7_is_keyword(sel)) {
                    s7_pointer result
                        = sunlark_common_property_lookup(s7, arg, sel);
                    return result;
                } else {
                    log_debug("Invalid arg: %s",
                              s7_object_to_c_string(s7, sel));
                    errno = EINVALID_ARG;
                    return NULL;
                }
            }
        }
    }

    if (s7_is_string(op)) {
        struct node_s *arg
            = sealark_loadstmt_arg_for_string(loadstmt, s7_string(op));
        if (s7_is_null(s7, s7_cdr(path_args))) {
            if (arg) {
                return sunlark_new_node(s7, arg);
            } else {
                /* errno set by sealark */
                return NULL;
            }
        } else {
            if (s7_list_length(s7, path_args) > 2) {
                log_error("Too many args: %s",
                          s7_object_to_c_string(s7, path_args));
                errno = ETOO_MANY_ARGS;
                return NULL;
            } else {
                s7_pointer sel = s7_cadr(path_args);
                if (s7_is_keyword(sel)) {
                    s7_pointer result
                        = sunlark_common_property_lookup(s7, arg, sel);
                    return result;
                } else {
                    log_debug("Invalid arg: %s",
                              s7_object_to_c_string(s7, sel));
                    errno = EINVALID_ARG;
                    return NULL;
                }
            }
        }
    }

    /* if (op == KW(bindings) || op == KW(@@) || op == KW(attrs)) { */
    /* } */

    /* if (op == KW(binding) || op == KW(@) || op == KW(attr)) { */
    /* } */

    if (s7_is_keyword(op)) {
        s7_pointer result
            = sunlark_common_property_lookup(s7, loadstmt, op);
        return result;
    }

    log_error("NOT YET: sunlark_loadstmt_select for %s",
              s7_object_to_c_string(s7, path_args));
    errno = ENOT_IMPLEMENTED;
    return NULL;
}

/* **************************************************************** */
/* so far: <tgt> :@ */
LOCAL s7_pointer _loadstmt_binding_dispatcher(s7_scheme *s7,
                                               struct node_s *loadstmt,
                                               s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sunlark_loadstmt_binding_dispatcher %s",
              s7_object_to_c_string(s7, path_args));
#endif

    assert(loadstmt->tid == TK_Load_Stmt);

    s7_pointer op = s7_car(path_args);

    /* **************** */
    if (s7_is_integer(op)) {
        struct node_s *binding
            = sealark_loadstmt_binding_for_int(loadstmt, s7_integer(op));
        if (binding) {
            if (s7_is_null(s7, s7_cdr(path_args))) {
                return sunlark_new_node(s7, binding);
            } else {
                if (s7_list_length(s7, path_args) > 2) {
                    log_error("Too many args: %s",
                              s7_object_to_c_string(s7, path_args));
                    errno = ETOO_MANY_ARGS;
                    return NULL;
                } else {
                    s7_pointer sel = s7_cadr(path_args);
                    if (sel == KW(key)) {
                        log_debug("project key");
                        /* struct node_s *k */
                        s7_pointer k
                            = sunlark_binding_dispatcher(s7, binding,
                                                         s7_cdr(path_args));
                        return k; //sunlark_new_node(s7, k);
                    }
                    if (sel == KW(value)) {
                        log_debug("project value");
                    }
                    if (s7_is_keyword(sel)) {
                        s7_pointer result
                            = sunlark_common_property_lookup(s7, binding, sel);
                        return result;
                    }
                    log_debug("Invalid arg: %s",
                              s7_object_to_c_string(s7, sel));
                    errno = EINVALID_ARG;
                    return NULL;
                }
            }
        } else {
            /* errno set by sealark */
            return NULL;
        }
    }

    if (s7_is_symbol(op)) {
        struct node_s *binding
            = sealark_loadstmt_binding_for_sym(loadstmt, s7_symbol_name(op));
        if (binding) {
            if (s7_is_null(s7, s7_cdr(path_args))) {
                return sunlark_new_node(s7, binding);
            } else {
                if (s7_list_length(s7, path_args) > 2) {
                    log_error("Too many args: %s",
                              s7_object_to_c_string(s7, path_args));
                    errno = ETOO_MANY_ARGS;
                    return NULL;
                } else {
                    s7_pointer sel = s7_cadr(path_args);
                    if (sel == KW(key)) {
                        log_debug("projecting key");
                        /* struct node_s *k */
                        s7_pointer k
                            = sunlark_binding_dispatcher(s7, binding, sel);
                        return k;
                    }
                    if (sel == KW(value)) {
                        log_debug("project value");
                    }
                    if (s7_is_keyword(sel)) {
                        s7_pointer result
                            = sunlark_common_property_lookup(s7, binding, sel);
                        return result;
                    } else {
                        log_debug("Invalid arg: %s",
                                  s7_object_to_c_string(s7, sel));
                        errno = EINVALID_ARG;
                        return NULL;
                    }
                }
            }
        } else {
            /* errno set by sealark */
            return NULL;
        }
    }

    if (s7_is_keyword(op)) {
        s7_pointer result
            = sunlark_common_property_lookup(s7, loadstmt, op);
        return result;
    }

    log_error("NOT IMPLEMENTED: sunlark_loadstmt_select for %s (%s)",
              s7_object_to_c_string(s7, path_args),
              s7_object_to_c_string(s7, s7_type_of(s7, op)));
    errno = ENOT_IMPLEMENTED;
    return NULL;
}

/* **************************************************************** */
/* (car path_args): string (= src); int (index)  */
/* return: node or list of nodes */
s7_pointer sunlark_pkg_loadstmt_dispatch(s7_scheme *s7,
                                                 struct node_s *pkg,
                                                 s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug(">> sunlark_pkg_loadstmt_dispatch %s",
              s7_object_to_c_string(s7, path_args));
#endif

    assert(pkg->tid == TK_Package);

    s7_pointer op = s7_car(path_args);

    /* **************** */
    if (s7_is_string(op)) {
        struct node_s *loadstmt
            = sealark_pkg_loadstmt_for_src(pkg, s7_string(op));
        if (loadstmt) {
            if (s7_is_null(s7, s7_cdr(path_args))) {
                return sunlark_new_node(s7, loadstmt);
            } else {
                s7_pointer result
                    = _loadstmt_dispatch(s7, loadstmt, s7_cdr(path_args));
                return result;
            }
        } else {
            return NULL;
        }
    }

    /* **************** */
    if (s7_is_integer(op)) {
        struct node_s *loadstmt
            = sealark_pkg_loadstmt_for_int(pkg, s7_integer(op));
        if (loadstmt)
            if (s7_is_null(s7, s7_cdr(path_args))) {
                return sunlark_new_node(s7, loadstmt);
            } else {
                s7_pointer result
                    = _loadstmt_dispatch(s7, loadstmt, s7_cdr(path_args));
                return result;
            }
        else
            return NULL;
    }
    log_error("Invalid arg: %s", s7_object_to_c_string(s7, path_args));
    errno = EINVALID_ARG_LOAD;
    return NULL;
}
