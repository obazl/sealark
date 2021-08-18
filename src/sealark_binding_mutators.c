#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_binding_mutators.h"

/* **************** */
//FIXME: do not remove "name" binding
EXPORT
struct node_s *sealark_target_bindings_rm_all(struct node_s *arglist)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sealark_target_bindings_rm_all");
#endif

    assert(arglist->tid == TK_Arg_List);

    int len = utarray_len(arglist->subnodes);
    /* log_debug("bindings ct: %d", len); */

    int name_idx = sealark_binding_index_for_key(arglist, "name");
    name_idx = name_idx * 2;
    /* log_debug("name idx: %d", name_idx); */

    /* erase after */
    /* log_debug("len to erase: %d", len - name_idx - 1); */
    utarray_erase(arglist->subnodes, name_idx+1, len - name_idx - 1);

    /* erase before */
    if (name_idx > 0) {
        /* log_debug("len to erase: %d", name_idx); */
        utarray_erase(arglist->subnodes, 0, name_idx);
    }
    return arglist;
}

/* ******************************** */
EXPORT
struct node_s *sealark_remove_binding_at_index(struct node_s *bindings,
                                               int index)
{
#if defined(DEBUG_BINDINGS) || defined(DEBUG_SET)
    log_debug("sealark_remove_binding_at_index: %d", index);
#endif

    assert(bindings->tid == TK_Arg_List);

    int subnode_ct = utarray_len(bindings->subnodes);

    index = 2 * index; // translate item idx to subnode idx

    if ( (subnode_ct - 1) == index) {
        /* remove last */
        utarray_erase(bindings->subnodes, index - 1, 2);
    } else {
        utarray_erase(bindings->subnodes, index, 2);
    }
    return bindings;
}

/* **************************************************************** */
/* EXPORT sealark_arglist_set_binding_for_int(struct node_s *arg_list, */
/*                                            int index, */
/*                                            char *newval) */
/* { */
/* } */

/* **************************************************************** */
/*
 0: TK_Call_Expr[97] @0:0
  1: TK_ID[37] @0:0    sunlark_test
  1: TK_Call_Sfx[98] @0:12
    2: TK_LPAREN[54] @0:12
    2: TK_Arg_List[87] @1:4
       3: TK_Binding[88] @1:4
*/
EXPORT
struct node_s *sealark_target_rm_binding_for_int(struct node_s *target,
                                                 int index)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sealark_target_rm_binding_for_int: %d", index);
#endif

    assert(target->tid == TK_Call_Expr);

    struct node_s *call_sfx = utarray_eltptr(target->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    int subnode_ct = utarray_len(arg_list->subnodes);
    int item_ct = (subnode_ct + 1) / 2;

    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > item_ct) {
            /* log_error("abs(%d) > item count %d", index, item_ct); */
            errno = EINDEX_OUT_OF_BOUNDS;
            return NULL;
        } else {
            index = item_ct + index;
            /* return sealark_target_for_index(package, i); */
        }
    }

    if (index > item_ct-1) {
        log_error("index %d > item count %d", index, item_ct);
        errno = 2;  //FIXME: should never happen after above adjustment?
        return NULL;
    }
    /* log_debug("index %d, item_ct %d, subnode_ct %d", */
    /*           index, item_ct, subnode_ct); */

    utarray_erase(arg_list->subnodes, index*2,
                  /* also rm comma unless at last item */
                  ((item_ct - index) > 1)? 2 : 1);

    return target;
}

/* **************************************************************** */
/*
 0: TK_Arg_List[87] @1:4
  1: TK_Binding[88] @1:4
     2 ...
  1: TK_COMMA[15] @1:27
  ...
 */
EXPORT
struct node_s *sealark_arglist_rm_binding_for_int(struct node_s *arg_list,
                                                  int index)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sealark_arglist_rm_binding_for_int: %d", index);
#endif

    assert(arg_list->tid == TK_Arg_List);

    int subnode_ct = utarray_len(arg_list->subnodes);
    int item_ct = (subnode_ct + 1) / 2;

    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > item_ct) {
            log_error("abs(%d) > item count %d", index, item_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            return NULL;
        } else {
            index = item_ct + index;
            /* return sealark_target_for_index(package, i); */
        }
    }

    //FIXME: should never happen after above adjustment?
    if (index > item_ct-1) {
        log_error("index %d > item count %d", index, item_ct);
        errno = EINDEX_OUT_OF_BOUNDS;
        return NULL;
    }
    /* log_debug("index %d, item_ct %d, subnode_ct %d", */
    /*           index, item_ct, subnode_ct); */

    utarray_erase(arg_list->subnodes, index*2,
                  /* also rm comma unless at last item */
                  ((item_ct - index) > 1)? 2 : 1);

    return arg_list;
}

/* **************************************************************** */
EXPORT
struct node_s *sealark_arglist_replace_binding_at_int(struct node_s *arglist,
                                                      int index,
                                                      struct node_s *newb)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_SET)
    log_debug("sealark_arglist_replace_binding_at_int %d", index);
#endif

    assert(arglist->tid == TK_Arg_List);
    assert(newb->tid    == TK_Binding);

    /* sealark_debug_log_ast_outline(arglist, 0); */
    int subnodes_ct = utarray_len(arglist->subnodes);
    int bindings_ct = (subnodes_ct + 1) / 2;

    int idx = index * 2;
    /* log_debug("IDX: %d", idx); */
    struct node_s *old = utarray_eltptr(arglist->subnodes, idx);

    newb->line = old->line;
    newb->col = old->col;
    /* log_debug("new binding:"); */
    /* sealark_debug_log_ast_outline(newb, 0); */

    int del;
    if (idx == subnodes_ct - 1) {
        del = 1;
    } else {
        del = 2;
    }

    /* log_debug("del: %d", del); */
    utarray_erase(arglist->subnodes, idx, del);

    /* log_debug("after erase :"); */
    /* sealark_debug_log_ast_outline(arglist, 0); */

    utarray_insert(arglist->subnodes, newb, idx);
    struct node_s *comma = sealark_new_node(TK_COMMA, without_subnodes);
    utarray_insert(arglist->subnodes, comma, idx+1);

    return arglist;
}

/* **************************************************************** */
EXPORT
struct node_s *sealark_arglist_insert_binding_at_int(struct node_s *arglist,
                                                     int index,
                                                     struct node_s *newb)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_SET)
    log_debug("sealark_arglist_replace_binding_at_int %d", index);
#endif

    assert(arglist->tid == TK_Arg_List);

    /* sealark_debug_log_ast_outline(arglist, 0); */

    int subnode_ct = utarray_len(arglist->subnodes);
    int item_ct = (subnode_ct + 1) / 2;
    /* log_debug("subnode ct: %d, item ct: %d", */
    /*           subnode_ct, item_ct); */

    /* DO NOT set line/col; leave that for formatting routines */
    /* int line = 0; */
    /* int col  = 0; */
    if (index == item_ct) {
        struct node_s *old = utarray_eltptr(arglist->subnodes, subnode_ct-1);
        struct node_s *comma = sealark_new_node(TK_COMMA, without_subnodes);
        utarray_push_back(arglist->subnodes, comma);
        utarray_push_back(arglist->subnodes, newb);
    } else {
        struct node_s *old = utarray_eltptr(arglist->subnodes, index*2);
        utarray_insert(arglist->subnodes, newb, index*2);
        struct node_s *comma = sealark_new_node(TK_COMMA, without_subnodes);
        utarray_insert(arglist->subnodes, comma, index*2+1);
    }
    /* log_debug("after insert :"); */
    /* sealark_debug_log_ast_outline(arglist, 0); */

    return arglist;
}
/* **************************************************************** */
/* EXPORT */
/* struct node_s *sealark_arglist_insert_binding_after_int(struct node_s *arglist, */
/*                                                      int index, */
/*                                                      struct node_s *newb) */
/* { */
/* #if defined(DEBUG_TRACE) || defined(DEBUG_SET) */
/*     log_debug("sealark_arglist_replace_binding_after_int %d", index); */
/* #endif */

/*     assert(arglist->tid == TK_Arg_List); */

/*     struct node_s *old = utarray_eltptr(arglist->subnodes, index*2); */
/*     newb->line = old->line; */
/*     old->line += 1; */
/*     newb->col = old->col; */

/*     //FIXME: increment line for all following bindings */

/*     /\* log_debug("new binding:"); *\/ */
/*     /\* sealark_debug_log_ast_outline(newb, 0); *\/ */

/*     /\* log_debug("after erase :"); *\/ */
/*     /\* sealark_debug_log_ast_outline(arglist, 0); *\/ */

/*     utarray_insert(arglist->subnodes, newb, index*2); */
/*     struct node_s *comma = sealark_new_node(TK_COMMA, without_subnodes); */
/*     utarray_insert(arglist->subnodes, comma, index*2+1); */

/*     /\* log_debug("after insert :"); *\/ */
/*     /\* sealark_debug_log_ast_outline(arglist, 0); *\/ */

/*     return arglist; */
/* } */

/* **************************************************************** */
EXPORT void sealark_bindings_incr_lines_from_int(struct node_s *arglist,
                                                 int index)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_SET)
    log_debug("sealark_bindings_incr_lines_from_int %d", index);
#endif

    assert(arglist->tid == TK_Arg_List);

    int subnode_ct = utarray_len(arglist->subnodes);
    int item_ct = (subnode_ct + 1) / 2;

    /* log_debug("subnode ct: %d, item ct: %d", subnode_ct, item_ct); */

    struct node_s *start = utarray_eltptr(arglist->subnodes,
                                          (index-1)*2);
    int line = start->line;

    /* binding := id, eq, val */
    struct node_s *binding;
    struct node_s *nd;
    for (int i = index; i < item_ct; i++) {
        binding = utarray_eltptr(arglist->subnodes, i*2);
        binding->line = line++;
        nd = utarray_eltptr(binding->subnodes, 0);
        nd->line = line;
        nd = utarray_eltptr(binding->subnodes, 1);
        nd->line = line;
        nd = utarray_eltptr(binding->subnodes, 2);
        nd->line = line;
    }
}
