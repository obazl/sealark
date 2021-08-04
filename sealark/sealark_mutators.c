#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_mutators.h"

/* **************** */
EXPORT struct node_s *sealark_set_int(struct node_s *node,
                                      const int newint)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sealark_set_int: %d", newint);
#endif

    assert(node->tid ==  TK_INT);

    /* int d = s7_integer(newint); */
    char buf[128];
    snprintf(buf, 128, "%d", newint);
    int len = strlen(buf);
    log_debug("new val: %d", newint);
    node->tid = TK_INT;
    node->s = calloc(len, sizeof(char));
    strncpy(node->s, buf, len);
    return node;
}

/* **************** */
/* string flags in sealark_nodes.c */
EXPORT struct node_s *sealark_set_string(struct node_s *node,
                                         int qtype,
                                         const char *newstr)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sealark_set_string: %s", newstr);
#endif

    if (node->tid != TK_STRING) {
        if (node->subnodes) {
            utarray_free(node->subnodes);
            node->subnodes = NULL;
        }
    }
    node->tid = TK_STRING;
    node->qtype = qtype; //(qmark | qqq | qtype);
    log_debug("qtype: %#04x", node->qtype);

    int len = strlen(newstr);
    node->s = calloc(len, sizeof(char));
    strncpy(node->s, newstr, len);
    return node;
}

EXPORT struct node_s *sealark_set_string_c_object(struct node_s *node,
                                                  struct node_s *newstr)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sealark_set_string_c_object");
#endif

    if (node->tid != TK_STRING) {
        if (node->subnodes) {
            utarray_free(node->subnodes);
            node->subnodes = NULL;
        }
    }
    node->tid = TK_STRING;
    node->qtype = newstr->qtype;
    log_debug("qtype: %#04x", node->qtype);

    int len = strlen(newstr->s);
    node->s = calloc(len, sizeof(char));
    strncpy(node->s, newstr->s, len);
    return node;
}

/* **************** */
EXPORT struct node_s *sealark_set_symbol(struct node_s *node,
                                         const char *newstr)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sealark_set_symbol: %s", newstr);
#endif

    if (node->tid != TK_ID) {
        if (node->subnodes) {
            utarray_free(node->subnodes);
            node->subnodes = NULL;
        }
    }
    node->qtype = 0;
    node->tid = TK_ID;
    int len = strlen(newstr);
    node->s = calloc(len, sizeof(char));
    strncpy(node->s, newstr, len);
    return node;
}

/* **************************************************************** */
EXPORT sealark_arglist_set_binding_for_int(struct node_s *arg_list,
                                           int index,
                                           char *newval)
{
}

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
            log_error("abs(%d) > item count %d", index, item_ct);
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
    log_debug("index %d, item_ct %d, subnode_ct %d",
              index, item_ct, subnode_ct);

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
    log_debug("index %d, item_ct %d, subnode_ct %d",
              index, item_ct, subnode_ct);

    utarray_erase(arg_list->subnodes, index*2,
                  /* also rm comma unless at last item */
                  ((item_ct - index) > 1)? 2 : 1);

    return arg_list;
}
