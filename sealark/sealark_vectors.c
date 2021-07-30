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
EXPORT struct node_s *sealark_vector_item_for_string(struct node_s *vector,
                                                     const char *selector)
{
#if defined(DEBUG_TRACE)
    log_debug("sunlark_vector_item_for_string: %s", selector);
#endif

}
