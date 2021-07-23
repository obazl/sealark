#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"

#include "predicators.h"

EXPORT bool sealark_is_printable(struct node_s *ast_node)
{
    log_debug("tid %d printable?", ast_node->tid);
    for (int i = 0; printable_tokens[i] != 0; i++) {
        if (ast_node->tid == printable_tokens[i])
            return true;
    }
    return false;
}

EXPORT bool sealark_is_target(struct node_s *call_expr)
{
#ifdef DEBUG_PREDICATES
    log_debug("sunlark_is_target");
#endif
    if (call_expr->tid != TK_Call_Expr) return false;

    // find name attrib
    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */
    /* struct node_s *stmt_list = utarray_eltptr(call_expr->subnodes, 0); */

    node_s *attr = sealark_get_call_attr_by_name(call_expr, "name");
    if (attr) {
        return true;
    } else
        return false;

}

EXPORT bool sealark_target_has_attribute(struct node_s *call_expr,
                                         const char *attr_name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("_sealark_target_has_attribute %s", attr_name);
#endif

    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    /* log_debug("call_expr %d %s", call_expr->tid, */
    /*           token_name[call_expr->tid][0]); */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    /* struct node_s *id; */
    int name_len = strlen(attr_name);

    //FIXME: call _get_attr_by_name_unique

#if defined(DEBUG_QUERY)
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
#if defined(DEBUG_QUERY)
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++, arg_node->tid,
                  token_name[arg_node->tid][0]);
#endif
        if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes
            /* first subnode is TK_ID */
            id = utarray_eltptr(arg_node->subnodes, 0);
            /* log_debug("testing id[%d]: %d %s", i, id->tid, id->s); */

            if ((strncmp(id->s, attr_name, name_len) == 0)
                && strlen(id->s) == name_len ){
                log_debug("true");
                return true;
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
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_is_rule_kind: %s", rule_kind);
#endif

    char *rule;

    int rule_len = strlen(rule_kind);
    bool globbing;

    if (rule_kind[rule_len-1] == '*') {
        globbing = true;
        rule = calloc(rule_len - 1, sizeof(char));
        strncpy(rule, rule_kind, rule_len-1);
        rule[rule_len] = '\0';
        rule_len--;
        log_debug("0 xxxxxxxxxxxxxxxx %s", rule);
    } else {
        rule = (char*)rule_kind;
    }

    struct node_s *id = utarray_eltptr(call_expr->subnodes, 0);
    log_debug("call expr rule: %s", id->s);

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
            log_debug("target has kind %s", rule);
            return true;
        } else {
            log_debug("target does note have kind %s", rule);
            return false;
        }
    }
}
