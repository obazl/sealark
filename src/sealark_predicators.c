#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"

#include "sealark_predicators.h"

EXPORT bool sealark_is_printable(struct node_s *ast_node)
{
    /* log_debug("tid %d printable?", ast_node->tid); */
    for (int i = 0; printable_tokens[i] != 0; i++) {
        /* log_debug("tok %d,  printable? %d", i, printable_tokens[i]); */
        if (ast_node->tid == printable_tokens[i]) {
            return true;
        }
    }
    return false;
}

EXPORT bool sealark_is_name_attr(struct node_s *node)
{
#ifdef DEBUG_PREDICATES
    log_debug("sealark_is_name_attr %d %s", node->tid, TIDNAME(node));
#endif

    if (node->tid != TK_Binding) return false;
    struct node_s *key = utarray_eltptr(node->subnodes, 0);
    /* log_debug("keystr: %s", key->s); */
    if ((strncmp(key->s, "name", 4) == 0) && strlen(key->s) == 4) {
        return true;
    }
    return false;
}

EXPORT bool sealark_call_expr_is_target(struct node_s *call_expr)
{
#ifdef DEBUG_PREDICATES
    log_debug("sealark_call_expr_is_target");
#endif
    if (call_expr->tid != TK_Call_Expr) return false;

    // find name attrib
    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */
    /* struct node_s *stmt_list = utarray_eltptr(call_expr->subnodes, 0); */

    node_s *binding = sealark_target_binding_for_key(call_expr, "name");
    if (binding) {
        return true;
    } else
        return false;

}

/* ******************************** */
/* matches key and value */
//FIXME: this is an expressor, not a predicate
/* EXPORT struct node_s *sealark_binding_for_target(struct node_s *call_expr, */
EXPORT struct node_s *sealark_target_has_binding(struct node_s *call_expr,
                                                 const char *key,
                                                 const char *val)
{
#if defined(DEBUG_PREDICATORS)
    log_debug("sealark_target_has_binding: %s = %s", key, val);
#endif
    /* :call-expr[1] > :call-sfx[1] > :arg-list */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    /* struct node_s *node, *binding; */
#if defined(DEBUG_UTARRAYS)
    log_debug("SEARCHING arg_list %d %s, child ct: %d",
              arg_list->tid,
              token_name[arg_list->tid][0],
              utarray_len(arg_list->subnodes));
#endif
    struct node_s *key_node, *val_node;
    int key_len = strlen(key);
    int val_len = strlen(val);

    struct node_s *binding_node = NULL;
    int i = 0;

    while((binding_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  binding_node))) {
#if defined(DEBUG_UTARRAYS)
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++, binding_node->tid,
                  token_name[binding_node->tid][0]);
#endif
        if (binding_node->tid == TK_Binding) { // skip TK_COMMA nodes
            key_node = utarray_eltptr(binding_node->subnodes, 0);
#if defined(DEBUG_UTARRAYS)
            log_debug("testing key_node[%d]: %d %s", i, key_node->tid, key_node->s);
#endif

            if ((strncmp(key_node->s, key, key_len) == 0)
                && strlen(key_node->s) == key_len ){
#if defined(DEBUG_UTARRAYS)
                log_debug("MATCHED KEY %s", key);
#endif
                /* key matches, now test value */
                val_node = utarray_eltptr(binding_node->subnodes, 2);
                if (val_node->tid == TK_STRING) {
                    /* log_debug("\tbinding: %s = %s", key_node->s, val_node->s); */

                    if ( (strncmp(val_node->s, val, val_len) == 0)
                         && strlen(val_node->s) == val_len ) {
                        return binding_node;
                    }
                } else {
                    log_debug("binding val not string: %d %s",
                              val_node->tid, TIDNAME(val_node));
                }
            }
        } else {
        }
    }
    return NULL;
}

/* **************** */
/* matches key only */
EXPORT bool sealark_target_has_binding_key(struct node_s *call_expr,
                                           const char *key)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("_sealark_target_has_binding_key: %s", key);
#endif

    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    /* log_debug("call_expr %d %s", call_expr->tid, */
    /*           token_name[call_expr->tid][0]); */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    /* struct node_s *id; */
    int key_len = strlen(key);

    //FIXME: call _get_binding_for_name_unique

#if defined(DEBUG_UTARRAYS)
    log_debug("SEARCHING arg_list %d %s, child ct: %d",
              arg_list->tid,
              token_name[arg_list->tid][0],
              utarray_len(arg_list->subnodes));
#endif
    struct node_s *arg_node = NULL;
    struct node_s *id = NULL;
    int i = 0;
    while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  arg_node))) {
#if defined(DEBUG_UTARRAYS)
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++, arg_node->tid,
                  token_name[arg_node->tid][0]);
#endif
        if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes
            /* first subnode is TK_ID */
            id = utarray_eltptr(arg_node->subnodes, 0);
            /* log_debug("testing id[%d]: %d %s", i, id->tid, id->s); */

            if ((strncmp(id->s, key, key_len) == 0)
                && strlen(id->s) == key_len ){
                /* log_debug("true"); */
                return true;
            }
        }
    }
    /* log_debug("false"); */
    return false;
}

EXPORT bool sealark_target_has_name(struct node_s *call_expr,
                                       const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("_sealark_target_has_name %s", name);
#endif

    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    /* log_debug("call_expr %d %s", call_expr->tid, */
    /*           token_name[call_expr->tid][0]); */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    /* struct node_s *id; */
    int name_len = strlen(name);

#if defined(DEBUG_QUERY)
    log_debug("SEARCHING arg_list %d %s, child ct: %d",
              arg_list->tid,
              token_name[arg_list->tid][0],
              utarray_len(arg_list->subnodes));
#endif
    struct node_s *arg_node = NULL;
    struct node_s *id  = NULL;
    struct node_s *val = NULL;
    int i = 0;
    while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  arg_node))) {
#if defined(DEBUG_QUERY)
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++, arg_node->tid,
                  token_name[arg_node->tid][0]);
#endif
        if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes
            /* first subnode is TK_ID */
            id = utarray_eltptr(arg_node->subnodes, 0);
            /* log_debug("testing id[%d]: %d %s", i, id->tid, id->s); */

            if ((strncmp(id->s, "name", 4) == 0)
                && strlen(id->s) == 4 ){
                val = utarray_eltptr(arg_node->subnodes, 2);
                if ((strncmp(val->s, name, name_len) == 0)
                    && strlen(val->s) == name_len ){
                    return true;
                }
            }
        }
    }
    log_debug("false");
    return false;
}

/* rule may contain final glob, e.g. 'cc_*' */
EXPORT bool sealark_target_is_rule_kind(struct node_s *call_expr,
                                        const char *rule_kind)
{
#if defined(DEBUG_QUERY)
    /* log_debug("sealark_target_is_rule_kind: %s", rule_kind); */
#endif

    char *rule;

    int rule_len = strlen(rule_kind);
    bool globbing = false;

    if (rule_kind[rule_len-1] == '*') {
        globbing = true;
        rule = calloc(rule_len - 1, sizeof(char));
        strncpy(rule, rule_kind, rule_len-1);
        rule[rule_len] = '\0';
        rule_len--;
    } else {
        rule = (char*)rule_kind;
    }

    struct node_s *id = utarray_eltptr(call_expr->subnodes, 0);
    /* log_debug("call expr rule: %s", id->s); */

    if (globbing) {
        if (strncmp(id->s, rule, rule_len) == 0) {
            free(rule);
            return true;
        } else {
            return false;
        }
    } else {
        if ( (strncmp(id->s, rule, rule_len) == 0)
             && (strlen(id->s) == rule_len) ) {
            /* log_debug("target has kind %s", rule); */
            return true;
        } else {
            /* log_debug("target does note have kind %s", rule); */
            return false;
        }
    }
}
