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

struct node_s *sunlark_dict_value_dispatch(s7_scheme *s7,
                                       struct node_s *dict,
                                       s7_pointer path_args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_dict_value_dispatch %d %s: %s",
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
            return NULL;
        assert(entry->tid == TK_Dict_Entry);

        if (path_len == 0) {
            log_error("Unexpected: empty path args");
            errno = EUNEXPECTED_STATE;
            return NULL;
        }

        if (path_len == 1) {    /* e.g. ("akey") */
            return entry;
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
            return utarray_eltptr(entry->subnodes, 0);
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

        if (path_len == 2) return val;

        if (path_len == 3) {    /* e.g. ("akey" :value :0) */
            s7_pointer arg3 = s7_caddr(path_args);
            errno = 0;
            int idx = sunlark_is_nbr_kw(s7, arg3);
            if (errno != 0) {
                errno = EINVALID_ARG;
                return NULL;
            } else {
                assert(val->tid = TK_List_Expr);
                struct node_s *item =
                    sealark_vector_item_for_int(val, idx);
                if (item)
                    return item;
                else
                    return NULL;
            }
        }
        errno = ETOO_MANY_ARGS;
        return NULL;
    }

    /* e.g. (:0 :value), (:1 :key), etc. */
    /* if (s7_is_keyword(idx)) { */
    int i;
    if ((i = sunlark_is_nbr_kw(s7, idx)) < 0) {
        log_error("bad kw arg");
    } else {
        log_debug("indexing at %d", i);
        errno = 0;
        struct node_s *dict_entry
            = sealark_dict_entry_for_int(dict, i);
        if (arg_ct == 1) {
            if (dict_entry)
                return dict_entry;
            else
                return NULL;
        }

        s7_pointer entry_ref = s7_cadr(path_args);
        if (entry_ref == KW(key)) {
            struct node_s *k = utarray_eltptr(dict_entry->subnodes, 0);
            return k;
        }
        if (entry_ref == KW(value) || entry_ref == s7_make_keyword(s7,"$")) {
            struct node_s *v = utarray_eltptr(dict_entry->subnodes, 2);
            return v;
        }
    }

    if (s7_is_string(idx)) {
        /* sealark_debug_print_ast_outline(dict, 0); */
        UT_array *items     /* list of item nodes */
            = sealark_vector_items_for_string(dict, s7_string(idx));
        /* s7_pointer ilist = intlist_to_s7_list(s7, items); */
        /* utarray_free(items); */
        /* s7_pointer ilist = vec_entries_to_s7_list(s7, items); */
        // utarray_free(items); /* FIXME: leak */
        log_error("FIXME!!!!!!!!!!!!!!!!");
        /* return nodelist_to_s7_list(s7, items); */
        return NULL;
    }
    log_error("Invalid arg: %s",
              s7_object_to_c_string(s7, path_args));
    errno = EINVALID_ARG;
    return NULL;
}

/* **************************************************************** */
struct node_s *sunlark_mutate_dict_expr(s7_scheme *s7, struct node_s *node,
                                        s7_pointer lval,
                                        s7_pointer update_val)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_mutate_dict_expr; lval %s; update: %s",
              node->tid, TIDNAME(node),
              s7_object_to_c_string(s7, lval),
              s7_object_to_c_string(s7, update_val));
#endif

    assert(node->tid == TK_Dict_Expr);

    return node;
}
