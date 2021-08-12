#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_vectors.h"

/*
node0 88 TK_Binding
node1 37 TK_ID: string_vecb
node1 26 TK_EQ
node1 116 TK_List_Expr    <==== vector
  node2 49 TK_LBRACK
  node2 109 TK_Expr_List
    node3 79 TK_STRING: b1
    node3 15 TK_COMMA
    node3 79 TK_STRING: b2
    node3 15 TK_COMMA
    node3 79 TK_STRING: b3
  node2 69 TK_RBRACK
*/
/* prereq: index is normalized */
EXPORT void sealark_vector_insert_int_at_index(struct node_s *list_expr,
                                                  int val,
                                                  int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("sealark_vector_insert_int_at_index: %d @ %d", val, index);
#endif

    assert(list_expr->tid == TK_List_Expr);

    struct node_s *expr_list = utarray_eltptr(list_expr->subnodes, 1);

    int list_ct = utarray_len(expr_list->subnodes);
    int item_ct = (list_ct + 1) / 2;

    /* NB: when appending, index will be > length of current list */

    log_debug("index: %d", index);
    struct node_s *new = sealark_new_int_node(val);
    struct node_s *comma = sealark_new_node(TK_COMMA, without_subnodes);

    if ( index == item_ct ) {
        /* appending after end */
        utarray_push_back(expr_list->subnodes, comma);
        utarray_push_back(expr_list->subnodes, new);
    } else {
        utarray_insert(expr_list->subnodes, comma, index*2);
        utarray_insert(expr_list->subnodes, new, index*2);
    }
}

EXPORT struct node_s *sealark_vector_item_for_int(struct node_s *list_expr,
                                                  int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("sealark_vector_item_for_int: %d", index);
#endif

    assert(list_expr->tid == TK_List_Expr);

    struct node_s *expr_list = utarray_eltptr(list_expr->subnodes, 1);

    int list_ct = utarray_len(expr_list->subnodes);
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
            return sealark_vector_item_for_int(list_expr, index);
        }
    }

    if (index > item_ct-1) {
        log_error("index > target count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return NULL;
    }

    //FIXME: support negative index
    /* log_debug("indexing %d", index); */

    struct node_s *item = utarray_eltptr(expr_list->subnodes, index*2);
    /* log_debug("found: %s", item->s); */
    return item;

    log_error("UNEXPECTED");
    exit(EXIT_FAILURE);
}

/* **************************************************************** */
/* NB: returns list of mapentries (idx .item) */
EXPORT UT_array *sealark_vector_items_for_string(struct node_s *vector,
                                                 const char *selector)
{
#if defined(DEBUG_TRACE)
    log_debug("sealark_vector_items_for_string: %s", selector);
#endif
    /* log_debug("vector tid: %d %s", vector->tid, TIDNAME(vector)); */
    assert(vector->tid == TK_List_Expr);

    int selector_len = strlen(selector);

    /* struct node_s *new_list = sealark_new_list_expr(); */
    /* assert(new_list->tid == TK_List_Expr); */
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
    /* log_debug("matched items: %d", utarray_len(items)); */
    return items;
}

/* **************************************************************** */
/* NB: returns list of mapentries (idx .item) */
EXPORT UT_array *sealark_vector_items_for_int_val(struct node_s *vector,
                                                  int val)
{
#if defined(DEBUG_TRACE)
    log_debug("sealark_vector_items_for_int_val: %d", val);
#endif
    assert(vector->tid == TK_List_Expr);

    UT_array *items;
    utarray_new(items, &node_icd);
    /* struct node_s *comma; */

    struct node_s *expr_list = utarray_eltptr(vector->subnodes, 1);
    int item_ct = utarray_len(expr_list->subnodes);

    int item_val;
    struct node_s *sub = NULL;
    int i = 0;
    while( (sub=(struct node_s*)utarray_next(expr_list->subnodes, sub)) ) {
        if (sub->tid == TK_ID) i++;
        if (sub->tid == TK_INT) {
            item_val = atoi(sub->s);
            if (item_val == val)
                utarray_push_back(items, sub);
            i++;
        }
    }
    int ct = utarray_len(items);
    /* log_debug("matched items: %d", ct); */
    return items;
}

/* **************************************************************** */
EXPORT void sealark_update_vector_items(UT_array *items,
                                         const char *newval)
{
#if defined(DEBUG_TRACE)
    log_debug("sealark_update_vector_items: %s", newval);
#endif

    /* log_debug("found %d items", utarray_len(items)); */

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
EXPORT struct node_s *sealark_vector_remove_item(struct node_s *vector,
                                                 int index)
{
#if defined(DEBUG_TRACE)
    log_debug("sealark_vector_remove_item: %d", index);
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
    /* log_debug("removing item at %d, subnode %d", index, i); */

    /* if last item, back up one to remove preceding comma */
    i = ((subnode_ct - i) > 1)? i : i-1;
    utarray_erase(vector->subnodes, i, 2);
    return vector;
}

