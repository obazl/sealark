#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_vectors.h"

struct node_s *sunlark_vector_resolve_path(s7_scheme *s7,
                                           s7_pointer self,
                                           s7_pointer path_args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_vector_resolve_path: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    struct node_s *vec = s7_c_object_value(self);
    /* validate */
    if (vec->tid != TK_List_Expr) {
        log_error("Expected TK_List_Expr, got: %d %s",
                  vec->tid, TIDNAME(vec));
        return NULL;
    }
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
int infer_vector_type(s7_scheme *s7, s7_pointer new_vec)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("_infer_vector_type");
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
                return infer_vector_type(s7, s7_cdr(new_vec));
            } else {
                log_error("Bad value for vector item: %s. Allowed types: int, string, symbol",
                          s7_object_to_c_string(s7, proto));
                return -1;
            }
        }
    }

}

/* **************** */
//FIXME: also update rbrack position?
struct node_s *sunlark_set_vector(s7_scheme *s7,
                                        struct node_s *old_vec,
                                        s7_pointer new_vec)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATORS)
    log_debug("sunlark_set_vector => %s",
              s7_object_to_c_string(s7, new_vec));
#endif
    sealark_debug_print_ast_outline(old_vec, 4);

    int new_ct = s7_list_length(s7, new_vec);

    /* :list_expr > :lbrack, :expr_list, :rbrack */
    struct node_s *old_items = utarray_eltptr(old_vec->subnodes, 1);
    int old_ct = (utarray_len(old_items->subnodes) + 1) / 2;
    log_debug("old_ct: %d", old_ct);

    struct node_s *old_item;

    s7_pointer new_item;
    /* need both, since an int/string list can contain syms (vars) */
    int new_vec_type;
    int new_item_type;

    if (s7_is_null(s7, new_vec)) {
        log_debug("emptying vector");
        utarray_clear(old_items->subnodes);
        struct node_s *lbrack = utarray_eltptr(old_vec->subnodes, 0);
        struct node_s *rbrack = utarray_eltptr(old_vec->subnodes, 2);
        rbrack->line = lbrack->line;
        rbrack->col = lbrack->col + 1;
        utarray_erase(old_vec->subnodes, 1,1);
        return old_vec;
    }

    /* first element sets element type */
    /* BUT: what if first elt is sym? e.g. (myvar 8 9) */
    new_vec_type = infer_vector_type(s7, new_vec);
    if (new_vec_type < 0) {
        //FIXME: throw s7 error
        log_error("bad vec type");
        return NULL;
    }
    log_debug("new_vec_type: %d %s",
              new_vec_type, token_name[new_vec_type][0]);

    const char *new_str;
    int new_str_len;

    /* FIXME: handle case where new item ct > old item ct */

    int i = 0;
    int new_idx = 0;
    while( ! s7_is_null(s7, new_vec) ) {
        log_debug("new_idx: %d", new_idx);
        new_item = s7_car(new_vec);
        if (s7_is_string(new_item)) {
            new_item_type = TK_STRING;
            new_str = s7_string(new_item);
        }
        if (s7_is_symbol(new_item)) {
            new_item_type = TK_ID;
            new_str = s7_symbol_name(new_item);
        }
        if (s7_is_integer(new_item)) {
            new_item_type = TK_INT;
            int new_int = s7_integer(new_item);
            char ibuf[64];
            snprintf(ibuf, 64, "%d", new_int);
            new_str = (char*)ibuf;
        }
        new_str_len = strlen(new_str);
        log_debug("new_str: %s (%d %s)",
                  new_str, new_item_type,
                  token_name[new_item_type][0]);

        /* if (new_item_idx > old_items_ct) { */
        /*     malloc, init, add new item */
        /* } else { */

        if (new_idx > old_ct-1) {
            log_debug("extending vec");
            struct node_s *newnode = sealark_node_new();
            newnode->tid = TK_COMMA;
            //FIXME: set line, col
            utarray_push_back(old_items->subnodes, newnode);

            newnode = sealark_node_new();
            newnode->tid = new_item_type;
            if (new_item_type == TK_STRING) {
                newnode->qtype = DQUOTE;
            }
            newnode->s = calloc(new_str_len, sizeof(char));
            strncpy(newnode->s, new_str, new_str_len);
            //FIXME: set line, col
            utarray_push_back(old_items->subnodes, newnode);
        } else {

            old_item = utarray_eltptr(old_items->subnodes, i);
            log_debug("old item tid: %d %s",
                      old_item->tid, TIDNAME(old_item));

            /* skip commas but include symbols */
            if (old_item->tid == TK_COMMA) {
                i++;
                old_item = utarray_eltptr(old_items->subnodes, i);
            }

            /* now the update */
            log_debug("replacing old: %s", old_item->s);
            free(old_item->s);
            old_item->s = calloc(new_str_len, sizeof(char));
            strncpy(old_item->s, new_str, new_str_len);
            old_item->tid = new_item_type; // in case new item is sym
        }
        /* } else { */
        /*     if (old_item->tid == TK_ID) { */
        /*         log_warn("FIXME: update sym/ID"); */
        /*     } */
        /* } */

        /* FIXME: this logic is for updating items, not replacing vec */
        /* skip commas but include symbols */
        /* while( (old_item->tid != new_vec_type) */
        /*        && (old_item->tid != TK_ID)) { */
        /*     log_debug("\told item tid: %d %s", */
        /*               old_item->tid, TIDNAME(old_item)); */
        /*     i++; */
        /*     old_item = utarray_eltptr(old_items->subnodes, i); */
        /* } */
        /* i++; */

        /* if (old_item->tid == new_vec_type) { */
        /*     log_debug("updating old: %s", old_item->s); */
        /*     free(old_item->s); */
        /*     old_item->s = calloc(new_str_len, sizeof(char)); */
        /*     strncpy(old_item->s, new_str, new_str_len); */
        /*     old_item->tid = new_item_type; // in case new item is sym */
        /* } else { */
        /*     if (old_item->tid == TK_ID) { */
        /*         log_warn("FIXME: update sym/ID"); */
        /*     } */
        /* } */

        i++;
        new_idx++;
        new_vec = s7_cdr(new_vec);
    }

    if (new_ct < old_ct) {
        log_debug("removing extras");
        utarray_resize(old_items->subnodes, new_ct * 2 - 1);
    }
    /* old_vec was updated-in-place */
    return old_vec;
}

