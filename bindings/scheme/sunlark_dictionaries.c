#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"

#include "sunlark_dictionaries.h"

/*
 4: TK_Dict_Expr[105] @11:12
   5: TK_LBRACE[48] @11:12
   5: TK_Dict_Entry_List[104] @12:8
     6: TK_Dict_Entry[103] @12:8
       7: TK_STRING[79] @12:8    "ckey1"
       7: TK_COLON[14] @12:15
       7: TK_STRING[79] @12:17    "cval1"
     6: TK_COMMA[15] @12:24
     6: TK_Dict_Entry[103] @13:8
       7: TK_STRING[79] @13:8    "ckey2"
       7: TK_COLON[14] @13:15
       7: TK_INT[79] @13:17       7
     6: TK_COMMA[15] @13:24
     ...etc...
   5: TK_RBRACE[68] @15:4
 */

s7_pointer sunlark_dict_expr_dispatcher(s7_scheme *s7,
                                        struct node_s *dict,
                                        s7_pointer path_args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_dict_expr_dispatcher %d %s: %s",
              dict->tid, TIDNAME(dict),
              s7_object_to_c_string(s7, path_args));
#endif

    assert(dict->tid == TK_Dict_Expr);

    int arg_ct = s7_list_length(s7, path_args);

    /* path len min: 1? */
    /* path len max: 4? e.g. for vec val: "akey" :value :0 :$ */
    int path_len = s7_list_length(s7, path_args);
    s7_pointer idx = s7_car(path_args);

    if (s7_is_integer(idx)) {
        log_error("Invalid index: %d - did you mean :%d ?",
                  s7_integer(idx), s7_integer(idx));
        errno = EINVALID_INT_INDEX;
        return NULL;
    }

    if (s7_is_string(idx)) {
        /* string key lookup */
        errno = 0;
        struct node_s *entry;
        entry = sealark_dict_entry_for_string_key(dict, s7_string(idx));
        if (entry == NULL)
            return handle_errno(s7, errno, path_args);
        assert(entry->tid == TK_Dict_Entry);

        if (path_len == 0) {
            log_error("Unexpected: empty path args");
            errno = EUNEXPECTED_STATE;
            return NULL;
        }

        if (path_len == 1) {    /* e.g. ("akey") */
            return sunlark_new_node(s7, entry);
        }

        /* path_len must be > 1 */
        s7_pointer arg2 = s7_cadr(path_args);

        struct node_s *val;

        if (arg2 == KW(key)) {
            if (path_len > 2) {
                log_error("Arg :key must come last in this context, got %s",
                          s7_object_to_c_string(s7, path_args));
                errno = ETOO_MANY_ARGS;
                return NULL;
            }
            struct node_s *nd = utarray_eltptr(entry->subnodes, 0);
            return sunlark_new_node(s7, nd);
        } else {
            if (arg2 == KW(value) || arg2 == s7_make_keyword(s7, "$")) {
                val = utarray_eltptr(entry->subnodes, 2);
            } else {
                log_error("Second arg must be :key or :value, got: %s",
                          s7_object_to_c_string(s7, arg2));
                errno = EINVALID_ARG;
                return NULL;
            }
        }

        if (path_len == 2) return sunlark_new_node(s7, val);

        if (path_len == 3) {    /* e.g. ("akey" :value :0) */
            assert(val->tid = TK_List_Expr);
            log_debug("0 xxxxxxxxxxxxxxxx");
            s7_pointer arg3 = s7_caddr(path_args);
            if (s7_is_string(arg3)) {

            } else {
                errno = 0;
                int idx = sunlark_is_nbr_kw(s7, arg3);
                if (errno != 0) {
                    errno = EINVALID_ARG;
                    return NULL;
                } else {
                    struct node_s *item =
                        sealark_vector_item_for_int(val, idx);
                    if (item)
                        return sunlark_new_node(s7, item);
                    else
                        return NULL;
                }
            }
        }
        errno = ETOO_MANY_ARGS;
        return NULL;
    }

    /* not string, not int */
    int i = sunlark_kwindex_to_int(s7, idx);
    if (errno == 0) {// we got an int
        log_debug("indexing at %d", i);
        errno = 0;
        struct node_s *dict_entry
            = sealark_dict_entry_for_int(dict, i);
        if (arg_ct == 1) {
            if (dict_entry)
                return sunlark_new_node(s7, dict_entry);
            else
                return NULL;
        }

        s7_pointer entry_ref = s7_cadr(path_args);
        if (entry_ref == KW(key)) {
            struct node_s *k = utarray_eltptr(dict_entry->subnodes, 0);
            return sunlark_new_node(s7, k);
        }
        if (entry_ref == KW(value) || entry_ref == s7_make_keyword(s7,"$")) {
            struct node_s *v = utarray_eltptr(dict_entry->subnodes, 2);
            return sunlark_new_node(s7, v);
        }

    } else {
        if (idx == KW(length)) {
            int l  = sealark_dict_expr_length(dict);
            return s7_make_integer(s7, l);
        } else {
            s7_pointer result
                = sunlark_common_property_lookup(s7, dict, idx);
            log_debug("0 xxxxxxxxxxxxxxxx %s", s7_object_to_c_string(s7, result));

            if (result) {
                errno = 0;
                return result;
            } else
                return handle_errno(s7, errno, path_args);
        }
    }
    return NULL;
}

/* **************************************************************** */
/* struct node_s * */
s7_pointer sunlark_dict_entry_dispatcher(s7_scheme *s7,
                                            struct node_s *dentry,
                                            s7_pointer path_args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_dict_entry_dispatcher %d %s: %s",
              dentry->tid, TIDNAME(dentry),
              s7_object_to_c_string(s7, path_args));
#endif

    assert(dentry->tid == TK_Dict_Entry);

    int path_len = s7_list_length(s7, path_args);
    s7_pointer op = s7_car(path_args);

    if (op == KW(key)) {
        struct node_s *r = utarray_eltptr(dentry->subnodes, 0);
        return sunlark_new_node(s7, r);
    }

    if (op == KW(value) || op == s7_make_keyword(s7, "$")) {
        struct node_s *v = utarray_eltptr(dentry->subnodes, 2);
        if (path_len == 1)
            return sunlark_new_node(s7, v);
        else {
            /* next arg must be an index e.g. :0 */
            if (path_len == 2) {
                s7_pointer idx = s7_cadr(path_args);
                switch(v->tid) {
                case TK_List_Expr:
                    return sunlark_vector_dispatcher(s7, v,
                                                     s7_list(s7,1,idx));
                    break;
                case TK_Dict_Expr:
                    /* should not happen for BUILD files */
                    log_error("Found dict value in dict");
                    break;
                default:
                    log_error("Trying to index a non-list: %d %s",
                              v->tid, TIDNAME(v));
                    errno = EINVALID_ARG;
                    return NULL;
                }
            } else {
                log_error("Too many args: %s",
                          s7_object_to_c_string(s7, path_args));
                errno = ETOO_MANY_ARGS;
                return NULL;
            }
        }
    }

    /* log_error("Invalid arg for dict-entry: %s", */
    /*           s7_object_to_c_string(s7, path_args)); */
    errno = 0; //EINVALID_ARG;
    return NULL;
}

/* **************************************************************** */
struct node_s *sunlark_mutate_dict_expr(s7_scheme *s7,
                                        struct node_s *dict_expr,
                                        s7_pointer lval,
                                        s7_pointer update_val)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_mutate_dict_expr");
    log_debug("lval %s; update: %s",
              s7_object_to_c_string(s7, lval),
              s7_object_to_c_string(s7, update_val));
#endif
    assert(dict_expr->tid == TK_Dict_Expr);

    if (update_val == KW(null)) {
        int idx = sunlark_kwindex_to_int(s7, lval);
        if (errno == 0) // we got an int
            return sealark_dexpr_rm_entry_for_int(dict_expr, idx);
        else {
            if (s7_is_string(lval)) {
                return sealark_dexpr_rm_entry_for_string(dict_expr,
                                                         s7_string(lval));
            }
            if (s7_is_symbol(lval)) {
                return sealark_dexpr_rm_entry_for_string(dict_expr,
                                                  s7_symbol_name(lval));
            }
            log_error("Invalid dict index: %s",
                      s7_object_to_c_string(s7, lval));
            errno = EINVALID_INDEX;
            return NULL;
        }
    }
    log_debug("FIXME");
    return dict_expr;
}
