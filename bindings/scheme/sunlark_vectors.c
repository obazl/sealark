#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_vectors.h"

/* struct node_s * */
s7_pointer sunlark_vector_dispatcher(s7_scheme *s7,
                                         struct node_s *datum,
                                           s7_pointer path_args)
/* LOCAL s7_pointer sunlark_dispatch_on_list_expr(s7_scheme *s7, */
/*                                             s7_pointer datum, */
/*                                             s7_pointer path_args) */
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_vector_dispatcher %s",
              s7_object_to_c_string(s7, path_args));
#endif
#if defined(DEBUG_AST)
    sealark_debug_log_ast_outline(datum, 0);
#endif

    assert(datum->tid == TK_List_Expr);

    int op_count = s7_list_length(s7, path_args);
    log_debug("op count: %d", op_count);

    if (op_count == 0)
        return sunlark_new_node(s7, datum);

    /* vector ops:  int index, ? :print, :tid, etc. */
    if (op_count > 1) {
        //error, :s, :tid etc. allowed
        log_error("FIXME, only one op allowed here");
    }
    s7_pointer op;
    if (s7_is_list(s7,path_args))
        op = s7_car(path_args);
    else
        op = path_args;

    if (op == s7_make_keyword(s7, "list-expr?"))
        return s7_t(s7);

    // :list-expr > :lbrack, :expr-list, :rbrack
    //  :expr-list > :string, :comma, etc.
    /* struct node_s *list_expr = s7_c_object_value(datum); */
    struct node_s *vector = utarray_eltptr(datum->subnodes, 1);

    /* tid: TK_Expr-List */
    log_debug("vector tid %d %s", vector->tid, TIDNAME(vector));

    // Let Bazel enforce list homogeneity in BUILD files
    /* treat first non-sym item as prototype, giving list type */
    /* struct node_s *prototype = utarray_eltptr(vector->subnodes, 0); */
    /* int item_type = prototype->tid; */
    /* log_debug("item_type: %d", item_type); */

    int item_ct = 0;

    /* sunlark uses keywordized numbers to index, but s7 uses ints for
       thingslike iterations, so we need to support both. */
    bool int_idx = false;
log_debug("68 xxxxxxxxxxxxxxxx");
    int idx = sunlark_is_nbr_kw(s7,op);
    if (errno == 0) { // op is a keywordized number
        int_idx = true;
    } else {
        if (s7_is_integer(op)) {
            idx = s7_integer(op);
            int_idx = true;
        }
    }
    if (int_idx) {

        /* int idx = s7_integer(op); */
        log_debug("indexing on %d", idx);
        struct node_s *nd = sealark_vector_item_for_int(datum, idx);
        return sunlark_new_node(s7, nd);
        /* int len = utarray_len(vector->subnodes); */
        /* if (idx > len) { */
        /*     log_error("index out of bounds: % > %", idx, len); */
        /*     return NULL; */
        /* } */
        /* /\* index by (semantic) items, skipping metadata *\/ */
        /* struct node_s *node = NULL; */
        /* while( (node */
        /*         =(struct node_s*)utarray_next(vector->subnodes, node)) ) { */
        /*     /\* if (node->tid == item_type) { *\/ */
        /*         if (item_ct == idx) */
        /*             return node; // sunlark_new_node(s7, node); */
        /*         item_ct++; */
        /*     /\* } *\/ */
        /* } */
        /* errno = ENOT_FOUND; */
        /* return NULL; */
    }
    errno = 0;
    return NULL;
}


/* { */
/* #ifdef DEBUG_TRACE */
/*     log_debug("sunlark_vector_dispatcher: %s", */
/*               s7_object_to_c_string(s7, path_args)); */
/* #endif */

/*     /\* struct node_s *vec = s7_c_object_value(self); *\/ */
/*     assert(vec->tid == TK_List_Expr); */

/*     if ( !s7_is_list(s7, path_args) ) { */
/*         log_error("Expected list of path_args, %s", */
/*                   s7_object_to_c_string(s7, path_args)); */
/*         return NULL; */
/*     } */
/*     int path_ct = s7_list_length(s7, path_args); */
/*     switch(path_ct) { */
/*     case 0: */
/*         return vec; //s7_c_object_value(self); */
/*         break; */
/*     default: */
/*         log_error("Not yet implemented"); */
/*         return NULL; */
/*     } */
/* } */

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
