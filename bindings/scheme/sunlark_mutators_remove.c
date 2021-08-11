#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_mutators_remove.h"

s7_pointer sunlark_remove(s7_scheme *s7, s7_pointer self,
                          s7_pointer get_path, s7_pointer selector)
{
#ifdef DEBUG_TRACE
    log_debug(">> sunlark_remove path: %s, selector %s",
              s7_object_to_c_string(s7, get_path),
              s7_object_to_c_string(s7, selector));
#endif

    int path_len = s7_list_length(s7, get_path);

    struct node_s *self_node = s7_c_object_value(self);

    struct node_s *result;

    /* if self == TK_Package */
    /*   call applicator only if len(get_path) > 1 */
    switch(self_node->tid) {
    case TK_Package:
    /* if (self_node->tid == TK_Package) { */
        /* getter (pkg :>>): get_path (), selector :>> */
        /* case: (set! (pkg :>>) :null) - delete all targets */
        /* case: (set! (pkg :>>) #>(mytgt ...)) - illegal replace */
        /* case: (set! (pkg :>>) '(tgt1 tgt2 ...)) - illegal splice */
        /* case: (set! (pkg :>>) #(tgt1 tgt2 ...)) - illegal splice */
        if (get_path == s7_nil(s7)) {
            if (selector == KW(>>)) {
                /* only allowed set action: :null */
                /* if (update_val == KW(null)) { */
                    result = sealark_pkg_remove_all_targets(self_node);
                    return sunlark_new_node(s7, result);
                /* } else { */
                /*     log_error("Only action for selector :>> is :null"); */
                /*     return handle_errno(s7, EINVALID_ACTION, update_val); */
                /* } */
            }
            if (selector == KW(loads)) {
                /* if (update_val == KW(null)) { */
                    result = sealark_pkg_remove_all_loadstmts(self_node);
                    return sunlark_new_node(s7, result);
                /* } else { */
                /*     log_error("Only action for selector :loads is :null"); */
                /*     return handle_errno(s7, EINVALID_ACTION, update_val); */
                /* } */
            }

            if (selector == KW(>)) {
                log_error(":> may not be selector in getter");
                return handle_errno(s7, EINVALID_GET_SELECTOR, selector);
            }
            if (selector == KW(load)) {
                log_error(":load may not be selector in getter");
                return handle_errno(s7, EINVALID_GET_SELECTOR,
                                    selector);
            }
            log_error("UNHANDLED selector: %s",
                      s7_object_to_c_string(s7, selector));
            return NULL;

            /* other singleton selectors on pkg: :loadstmts, etc. */
        }
        /* special cases */
        if (path_len == 1) {
            if (s7_car(get_path) == KW(>)) {
                if (selector == s7_make_keyword(s7, "*")) {
                    /* (set! (pkg :> :*) :null) */
                    /* if (update_val == KW(null)) { */
                        result = sealark_pkg_remove_all_targets(self_node);
                        return sunlark_new_node(s7, result);
                    /* } else { */
                    /*     log_error("Only action for selector :* is :null"); */
                    /*     return handle_errno(s7, EINVALID_ACTION, update_val); */
                    /* } */
                } else {
                    /* if selector is :n */
                    log_warn("special case: path = :> sel");
                    errno = 0;
                    int idx = sunlark_kwindex_to_int(s7, selector);
                    if (errno == 0) {
                        result = sealark_pkg_rm_target_at_int(self_node,
                                                                  idx);
                        if (result)
                            return sunlark_new_node(s7, result);
                        return handle_errno(s7, errno, selector);
                    } else {
                        return handle_errno(s7, errno, s7_nil(s7));
                    }
                }
            }
            /* (set! (pkg :load :*) :null) */
            if (s7_car(get_path) == KW(load)) {
                if (selector == s7_make_keyword(s7, "*")) {
                    /* if (update_val == KW(null)) { */
                        result = sealark_pkg_remove_all_loadstmts(self_node);
                        return sunlark_new_node(s7, result);
                    /* } else { */
                    /*     log_error("Only action in this context for selector :load is :null"); */
                    /*     return handle_errno(s7, EINVALID_ACTION, update_val); */
                    /* } */
                } else {
                    errno = 0;
                    int idx = sunlark_kwindex_to_int(s7, selector);
                    if (errno == 0) {
                        result = sealark_pkg_remove_loadstmt_at_int(self_node,
                                                                    idx);
                        if (result)
                            return sunlark_new_node(s7, result);
                        else
                            return handle_errno(s7, errno, selector);
                    } else {
                        /* selector not a kw int, must be a string */
                        if (s7_is_string(selector)) {
                            result
                                = sealark_pkg_remove_loadstmt_at_key(
                                                     self_node,
                                                     s7_string(selector));
                            if (result)
                                return sunlark_new_node(s7, result);
                            else
                                return handle_errno(s7, errno, selector);
                        } else {
                            log_error("In this context :load must be followe by an int (or kw int) or string key");
                            return handle_errno(s7, EINVALID_ARG,
                                                selector);
                        }
                    }
                    log_error("special case: path == :load");
                    return NULL;
                }
            }
            log_error("special case len(get_path) == 1 ...");
        }
        break;
    case TK_List_Expr:
        log_warn("TK_List_Expr");
        return NULL;
        break;
    default:
        log_error("uncaught tid");
        return NULL;
    }

}
