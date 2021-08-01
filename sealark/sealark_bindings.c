#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_bindings.h"


/* ******************************** */
/* returns only bindings (TK_Named_Arg) in a new UT_array */
/* **************************************************************** */
/* EXPORT struct node_s *sealark_target_bindings(struct node_s *target) */
/* { */
/* #if defined (DEBUG_TRACE) || defined(DEBUG_QUERY) */
/*     log_debug("sealark_target_bindings"); */
/* #endif */

/*     log_debug("target tid: %d %s", target->tid, TIDNAME(target)); */

/*     /\* :call-expr[1] => :call-sfx[1] => :arg-list *\/ */
/*     struct node_s *call_sfx = utarray_eltptr(target->subnodes, 1); */
/*     struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1); */

/*     return arg_list; */
/* } */

/* **************************************************************** */
EXPORT struct node_s *sealark_bindings_for_target(struct node_s *target)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_bindings_for_target");
#endif

    /* log_debug("target tid: %d %s", target->tid, TIDNAME(target)); */

    /* :call-expr[1] => :call-sfx[1] => :arg-list */
    struct node_s *call_sfx = utarray_eltptr(target->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    return arg_list;
}

/* **************************************************************** */
EXPORT struct node_s *sealark_bindings_for_target_for_name
(struct node_s *build_file, const char *tgt_name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_bindings_for_target_for_name %s", tgt_name);
#endif

    struct node_s *target = sealark_target_for_name(build_file, tgt_name);

    /* UT_array *bindings = sealark_bindings_for_target(target); */
    struct node_s *bindings = sealark_bindings_for_target(target);
    return bindings;
}

/* **************************************************************** */
/* EXPORT UT_array *sealark_bindings_for_target_for_index */
EXPORT struct node_s *sealark_bindings_for_target_for_index
(struct node_s *build_file, int i)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_bindings_for_target_for_index %d", i);
#endif

    struct node_s *target = sealark_target_for_index(build_file, i);

    /* UT_array *bindings = sealark_bindings_for_target(target); */
    struct node_s *bindings = sealark_bindings_for_target(target);
    return bindings;
}

EXPORT struct node_s *sealark_value_for_binding(struct node_s *binding)
{
#if defined(DEBUG_PATHS)
    log_debug("sealark_value_for_binding, tid: %d %s",
              binding->tid, token_name[binding->tid][0]);
#endif
#if defined(DEBUG_BINDINGS)
    sealark_debug_print_ast_outline(binding, 0);
#endif
    /* :binding > :id, :eq, (:list-expr | :string | ...) */
    struct node_s *valnode = utarray_eltptr(binding->subnodes, 2);
    return valnode;
}

/* ******************************** */
EXPORT struct node_s *sealark_target_binding_for_key(struct node_s *call_expr, const char *key)
{
/* #if defined(DEBUG_TRACE) */
/*     log_debug("sealark_target_binding_for_key: %s", key); */
/* #endif */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    struct node_s *id;
    int key_len = strlen(key);
    struct node_s *arg_node = NULL;
    int i = 0;

    while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  arg_node))) {
#if defined(DEBUG_UTARRAYS)
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++, arg_node->tid,
                  token_name[arg_node->tid][0]);
#endif
        if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes
            id = utarray_eltptr(arg_node->subnodes, 0);

#if defined(DEBUG_UTARRAYS)
            log_debug("testing id[%d]: %d %s", i, id->tid, id->s);
#endif
            if ((strncmp(id->s, key, key_len) == 0)
                && strlen(id->s) == key_len ){

#if defined(DEBUG_UTARRAYS)
                log_debug("MATCHED key: %s", key);
#endif
                return arg_node;

            }
        } else {
        }
    }
#if defined(DEBUG_UTARRAYS)
                log_debug("no match: %s", key);
#endif
    errno = -1;
    return NULL;
}

/* ******************************** */
EXPORT struct node_s *sealark_target_binding_for_index(struct node_s *call_expr, int index)
{
#if defined(DEBUG_TRACE)
    log_debug("sealark_target_binding_for_index: %d", index);
#endif
    /* :call-expr[1] > :call-sfx[1] > :arg-list */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);
    int arg_list_ct = utarray_len(arg_list->subnodes);
    int args_item_ct = (arg_list_ct + 1) / 2;
    log_debug("arg_list_ct: %d; item ct: %d", arg_list_ct, args_item_ct);

    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > args_item_ct) {
            log_error("abs(%d) > args_item_ct", index, args_item_ct);
            errno = 3;
            return NULL;
        } else {
            index = args_item_ct + index;
            log_debug("recurring...");
            return sealark_target_binding_for_index(call_expr, index);
        }
    }

    if (index > args_item_ct-1) {
        log_error("index > target count");
        errno = 2;              /* FIXME */
        return NULL;
    }

#if defined(DEBUG_UTARRAYS)
    log_debug("SEARCHING arg_list %d %s, child ct: %d, item ct: %d",
              arg_list->tid,
              token_name[arg_list->tid][0],
              arg_list_ct, args_item_ct);
#endif

    /* struct node_s *id; */
    struct node_s *arg_node = NULL;

    /* for target nodes, only bindings (attrs) are allowed in arglist,
       so no need to account for non-attribute args
     */
    int binding_ct = 0;
    while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  arg_node))) {
#if defined(DEBUG_UTARRAYS)
        log_debug(" LOOP arg_list[%d] tid: %d %s",
                  binding_ct, arg_node->tid,
                  token_name[arg_node->tid][0]);
#endif
        if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes
            if (binding_ct == index) {
#if defined(DEBUG_UTARRAYS)
                log_debug("MATCHED index: %d", binding_ct);
#endif
                return arg_node;
            }
            binding_ct++;
        }
    }
    return NULL;
}

/* ******************************** */
EXPORT struct node_s *sealark_bindings_binding_for_index(struct node_s *bindings, int index)
{
#if defined(DEBUG_BINDINGS)
    log_debug("sealark_bindings_binding_for_index: %d", index);
    sealark_debug_print_ast_outline(bindings, 0);
#endif

    if (bindings->tid != TK_Arg_List) {
        log_error("Node type error. Expected TK_Arg_List, got %d %s",
                  bindings->tid, TIDNAME(bindings));
    }

    /* struct node_s *node, *binding; */

#if defined(DEBUG_UTARRAYS)
    log_debug("SEARCHING bindings %d %s, child ct: %d",
              bindings->tid,
              token_name[bindings->tid][0],
              utarray_len(bindings->subnodes));
#endif
    struct node_s *binding_node = NULL;

    /* for target nodes, only bindings (attrs) are allowed in arglist,
       so no need to account for non-attribute args
     */
    int binding_ct = 0;
    while((binding_node=(struct node_s*)utarray_next(bindings->subnodes,
                                                  binding_node))) {
#if defined(DEBUG_UTARRAYS)
        log_debug(" LOOP bindings[%d] tid: %d %s",
                  binding_ct, binding_node->tid,
                  token_name[binding_node->tid][0]);
#endif
        if (binding_node->tid == TK_Binding) { // skip TK_COMMA nodes
            if (binding_ct == index) {
#if defined(DEBUG_UTARRAYS)
                log_debug("MATCHED index: %d", binding_ct);
#endif
                return binding_node;
            }
            binding_ct++;
        }
    }
    return NULL;
}
