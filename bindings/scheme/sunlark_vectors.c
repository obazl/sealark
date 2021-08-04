#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_vectors.h"

struct node_s *sunlark_vector_dispatcher(s7_scheme *s7,
                                           s7_pointer self,
                                           s7_pointer path_args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_vector_dispatcher: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    struct node_s *vec = s7_c_object_value(self);
    assert(vec->tid == TK_List_Expr);

    if ( !s7_is_list(s7, path_args) ) {
        log_error("Expected list of path_args, %s",
                  s7_object_to_c_string(s7, path_args));
        return NULL;
    }
    int path_ct = s7_list_length(s7, path_args);
    switch(path_ct) {
    case 0:
        return s7_c_object_value(self);
        break;
    default:
        log_error("Not yet implemented");
        return NULL;
    }
}

/* **************** */
int sunlark_infer_vector_type_from_list(s7_scheme *s7, s7_pointer new_vec)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("_infer_vector_type_from_list");
#endif

    s7_pointer proto = s7_car(new_vec);
    int new_vec_type;

    if (s7_is_string(proto)) {
        return TK_STRING;
    } else {
        if (s7_is_integer(proto)) {
            return TK_INT;
        } else {
            if (s7_is_symbol(proto)) {
                return sunlark_infer_vector_type_from_list(s7, s7_cdr(new_vec));
            } else {
                log_error("Bad value for vector item: %s. Allowed types: int, string, symbol",
                          s7_object_to_c_string(s7, proto));
                return -1;
            }
        }
    }

}
/* **************** */
int sunlark_infer_expr_list_type(struct node_s *expr_list)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("_infer_expr_list_type");
#endif

    /* first item that is not an ID determines type */
    struct node_s *subnode = NULL;
    while( (subnode=(struct node_s*)utarray_next(expr_list->subnodes, subnode)) ) {

        if (subnode->tid == TK_STRING) return TK_STRING;
        if (subnode->tid == TK_INT) return TK_INT;
    }
    return -1;
}

/* **************************************************************** */
