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
EXPORT struct node_s *sealark_vector_item_for_int(struct node_s *list_expr,
                                                  int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("sealark_vector_item_for_int: %d", index);
#endif

    if (index < 0) {
        log_error("index out of bounds: %d", index);
        return NULL;
    }

    struct node_s *expr_list = utarray_eltptr(list_expr->subnodes, 1);
    int item_ct = utarray_len(expr_list->subnodes);
    if (index*2 > item_ct) {
        log_error("index out of bounds: %d", index);
        return NULL;
    }

    //FIXME: support negative index

    return utarray_eltptr(expr_list->subnodes, index*2);

    log_error("UNEXPECTED");
    exit(EXIT_FAILURE);
}

/* **************************************************************** */
/* NB: returns list of (idx .item) */
EXPORT struct node_s *sealark_vector_items_for_string(struct node_s *vector,
                                                 const char *selector)
{
#if defined(DEBUG_TRACE)
    log_debug("sunlark_vector_items_for_string: %s", selector);
#endif
    log_debug("vector tid: %d %s", vector->tid, TIDNAME(vector));
    assert(vector->tid == TK_List_Expr);

    int selector_len = strlen(selector);

    struct node_s *new_list = sealark_new_list_expr();
    assert(new_list->tid == TK_List_Expr);
    new_list->index = 1;
    struct node_s *new_items = utarray_eltptr(new_list->subnodes, 1);
    struct node_s *comma;

    struct node_s *expr_list = utarray_eltptr(vector->subnodes, 1);
    int item_ct = utarray_len(expr_list->subnodes);

    struct node_s *sub = NULL;
    int i = 0;
    while( (sub=(struct node_s*)utarray_next(expr_list->subnodes, sub)) ) {
        if (sub->tid == TK_ID) i++;
        if (sub->tid == TK_STRING) {
            if ((strncmp(sub->s, selector, selector_len) == 0)
                && strlen(sub->s) == selector_len ){
                sub->index = i;
                utarray_push_back(new_items->subnodes, sub);
                /* fixme: omit trailing comma */
                comma = sealark_new_node(TK_COMMA, without_subnodes);
                utarray_push_back(new_items->subnodes, comma);
            }
            i++;
        }
    }
    return new_list;
}

/* **************************************************************** */
EXPORT struct node_s *sealark_update_vector_mapentries(struct node_s *vector,
                                                       const char *newval)
{
#if defined(DEBUG_TRACE)
    log_debug("sealark_update_vector_mapentries");
    log_debug("vector tid: %d %s", vector->tid, TIDNAME(vector));
#endif

    assert(vector->tid == TK_List_Expr);

    int newval_len = strlen(newval);

    struct node_s *expr_list = utarray_eltptr(vector->subnodes, 1);
    int item_ct = utarray_len(expr_list->subnodes);

    struct node_s *sub = NULL;
    int i = 0;
    while( (sub=(struct node_s*)utarray_next(expr_list->subnodes, sub)) ) {
        if (sub->tid == TK_ID) {
        }
        if (sub->tid == TK_STRING) {
            strcpy(sub->s, newval);
        }
    }
    return vector;
}

