#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_dictionaries.h"

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

/*
*/
EXPORT struct node_s *sealark_dict_entry_for_int(struct node_s *dict_expr,
                                                  int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("sealark_dict_entry_for_int: %d", index);
#endif

    assert(dict_expr->tid == TK_Dict_Expr);

    struct node_s *entry_list = utarray_eltptr(dict_expr->subnodes, 1);

    int list_ct = utarray_len(entry_list->subnodes);
    int item_ct = (list_ct + 1) / 2;

    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > item_ct) {
            log_error("abs(%d) > item_ct", index, item_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            return NULL;
        } else {
            index = item_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_dict_entry_for_int(dict_expr, index); */
        }
    }

    if (index > item_ct-1) {
        log_error("index > target count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return NULL;
    }

    /* struct node_s *entry_list = utarray_eltptr(dict_expr->subnodes, 1); */
    /* item_ct = utarray_len(entry_list->subnodes); */
    /* if (index*2 > item_ct) { */
    /*     log_error("index out of bounds: %d", index); */
    /*     return NULL; */
    /* } */

    //FIXME: support negative index

    return utarray_eltptr(entry_list->subnodes, index*2);

    log_error("UNEXPECTED");
    exit(EXIT_FAILURE);
}

EXPORT struct node_s *sealark_dict_entry_for_string_key(struct node_s *dict_expr,
                                                        const char *key)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("sealark_dict_entry_for_string_key: %s", key);
#endif

    assert(dict_expr->tid == TK_Dict_Expr);

    struct node_s *entry_list = utarray_eltptr(dict_expr->subnodes, 1);

    int key_len = strlen(key);

    int list_ct = utarray_len(entry_list->subnodes);
    int item_ct = (list_ct + 1) / 2;

    struct node_s *ekey;
    struct node_s *sub = NULL;
    int i = 0;
    while( (sub=(struct node_s*)utarray_next(entry_list->subnodes, sub)) ) {
        if (sub->tid == TK_Dict_Entry) {
            ekey = utarray_eltptr(sub->subnodes, 0);

            if (ekey->tid == TK_STRING) {
                if ( (strncmp(ekey->s, key, key_len) == 0)
                     && (strlen(ekey->s) == key_len) ) {
                    /* match */
                    return sub;
                }
            }
        }
        i++;
    }

    log_error("UNEXPECTED");
    exit(EXIT_FAILURE);
}

/* **************************************************************** */
/* NB: returns list of mapentries (idx .item) */
EXPORT UT_array *sealark_dict_items_for_string(struct node_s *vector,
                                                 const char *selector)
{
#if defined(DEBUG_TRACE)
    log_debug("sunlark_dict_items_for_string: %s", selector);
#endif
    log_debug("vector tid: %d %s", vector->tid, TIDNAME(vector));
    assert(vector->tid == TK_Dict_Expr);

    int selector_len = strlen(selector);

    /* struct node_s *new_list = sealark_new_dict_expr(); */
    /* assert(new_list->tid == TK_Dict_Expr); */
    /* new_list->index = 1; */
    /* struct node_s *new_items = utarray_eltptr(new_list->subnodes, 1); */

    /* UT_array *nodemap; */
    /* utarray_new(nodemap, &mapentry_icd); // client deletes */
    /* struct mapentry_s *mapentry; */

    UT_array *items;
    utarray_new(items, &node_icd);
    /* struct node_s *comma; */

    struct node_s *expr_list = utarray_eltptr(vector->subnodes, 1);
    int item_ct = utarray_len(expr_list->subnodes);

    struct node_s *sub = NULL;
    int i = 0;
    while( (sub=(struct node_s*)utarray_next(expr_list->subnodes, sub)) ) {
        if (sub->tid == TK_ID) i++;
        if (sub->tid == TK_STRING) {
            if ((strncmp(sub->s, selector, selector_len) == 0)
                && strlen(sub->s) == selector_len ){
                utarray_push_back(items, sub);

                /* mapentry = calloc(1, sizeof(struct mapentry_s)); // client deletes */
                /* mapentry->index = i; */
                /* mapentry->node = sub; */
                /* utarray_push_back(nodemap, mapentry); */
                /* free(mapentry); // push_back copies, so we can free? */
                /* fixme: omit trailing comma */
                /* comma = sealark_new_node(TK_COMMA, without_subnodes); */
                /* utarray_push_back(new_items->subnodes, comma); */
            }
            i++;
        }
    }
    return items;
}

/* **************************************************************** */
EXPORT void sealark_update_dict_items(UT_array *items,
                                         const char *newval)
{
#if defined(DEBUG_TRACE)
    log_debug("sealark_update_dict_items: %s", newval);
#endif

    log_debug("found %d items", utarray_len(items));

    int newval_len = strlen(newval);

    int i = 0;
    struct node_s *sub = NULL;
    while( (sub=(struct node_s*)utarray_next(items, sub)) ) {
            free(sub->s);
            sub->s = calloc(newval_len + 1, sizeof(char));
            strncpy(sub->s, newval, newval_len + 1);
    }

    /* struct mapentry_s *mentry = NULL; */
    /* int i = 0; */
    /* while( (mentry=(struct mapentry_s*)utarray_next(items, mentry)) ) { */
    /*     /\* assert(mentry->node->tid == TK_STRING); *\/ */
    /*     if (mentry->node->tid == TK_ID) { */
    /*     } */
    /*     if (mentry->node->tid == TK_STRING) { */
    /*         free(mentry->node->s); */
    /*         mentry->node->s = calloc(newval_len + 1, sizeof(char)); */
    /*         strncpy(mentry->node->s, newval, newval_len + 1); */
    /*     } */
    /* } */
}

/* **************************************************************** */
EXPORT struct node_s *sealark_dict_remove_item(struct node_s *vector,
                                            int index)
{
#if defined(DEBUG_TRACE)
    log_debug("sealark_dict_remove_item: %d", index);
#endif

    assert(vector->tid == TK_Expr_List);

    int subnode_ct = utarray_len(vector->subnodes);
    /* each item except the last followed by comma */
    int item_ct = (subnode_ct + 1) / 2;

    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > item_ct) {
            log_error("abs(%d) > item_ct", index, item_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            return NULL;
        } else {
            index = item_ct + index;
            // do we need to recur?
        }
    }

    if (index > item_ct-1) {
        log_error("desired index %d > item count %d", index, item_ct);
        errno = EINDEX_OUT_OF_BOUNDS;
        return NULL;
    }

    int i = 0;
    int item_idx = 0;
    struct node_s *sub = NULL;
    while( (sub=(struct node_s*)utarray_next(vector->subnodes, sub)) ) {
        if (sub->tid == TK_COMMA) {
            i++;
            continue;
        }
        if (index == item_idx) break;
        item_idx++;
        i++;
    }
    /* we should always match desired index */
    log_debug("removing item at %d, subnode %d", index, i);

    /* if last item, back up one to remove preceding comma */
    i = ((subnode_ct - i) > 1)? i : i-1;
    utarray_erase(vector->subnodes, i, 2);
    return vector;
}

