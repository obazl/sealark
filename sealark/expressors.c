#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"

#include "expressors.h"

EXPORT char *sealark_get_target_rule(struct node_s *call_expr)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_get_target_rule");
#endif

    /* :call-expr[0] > :id */

    struct node_s *rule_id = utarray_eltptr(call_expr->subnodes, 0);
    /* no need to copy, s7 will copy it */
    char *rule_name = rule_id->s;
    return rule_name;
}

/* ******************************** */
EXPORT struct node_s
*sealark_get_target_by_attribute(struct node_s *call_expr,
                                 const char *attr_name,
                                 const char *attr_val)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("_get_target_attribute %s = %s", attr_name, attr_val);
#endif

    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    /* log_debug("call_expr %d %s", call_expr->tid, */
    /*           token_name[call_expr->tid][0]); */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    struct node_s *id, *val;
    int name_len = strlen(attr_name);
    int attr_val_len = strlen(attr_val);

    //FIXME: call _get_attr_by_name_unique

#if defined(DEBUG_QUERY)
    log_debug("SEARCHING arg_list %d %s, child ct: %d",
              arg_list->tid,
              token_name[arg_list->tid][0],
              utarray_len(arg_list->subnodes));
#endif
    struct node_s *arg_node = NULL;
    int i = 0;
    while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  arg_node))) {
#if defined(DEBUG_QUERY)
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++, arg_node->tid,
                  token_name[arg_node->tid][0]);
#endif
        if (arg_node->tid == TK_Arg_Named) { // skip TK_COMMA nodes
            id = utarray_eltptr(arg_node->subnodes, 0);
            /* log_debug("testing id[%d]: %d %s", i, id->tid, id->s); */

            if ((strncmp(id->s, attr_name, name_len) == 0)
                && strlen(id->s) == name_len ){

                /* name matches, now test value */
                val = utarray_eltptr(arg_node->subnodes, 2);
                /* log_debug("\tattr: %s = %s", id->s, val->s); */

                if ( (strncmp(val->s, attr_val, attr_val_len) == 0)
                     && strlen(val->s) == attr_val_len ) {
                    /* log_debug("3 xxxxxxxxxxxxxxxx MATCH %d %s == %s", */
                    /*           attr_val_len, val->s, attr_val); */
                    return arg_node;
                }
            }
        }
    }
    /* log_debug("NOT FOUND"); */
    return NULL;
}

/* ******************************** */
EXPORT struct node_s *sealark_get_call_attr_by_name_val(struct node_s *call_expr,
                                                         char *name,
                                                         char *val)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_get_call_attr_by_name");
#endif
}

/* ******************************** */
EXPORT struct node_s *sealark_get_call_attr_by_name(struct node_s *call_expr,
                                                    char *attr_name)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_get_call_attr_by_name");
#endif
    /* :call-expr[1] > :call-sfx[1] > :arg-list */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    /* struct node_s *node, *attr; */

    log_debug("SEARCHING arg_list %d %s, child ct: %d",
              arg_list->tid,
              token_name[arg_list->tid][0],
              utarray_len(arg_list->subnodes));

    struct node_s *id;
    int name_len = strlen(attr_name);
    struct node_s *arg_node = NULL;
    int i = 0;

    while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  arg_node))) {
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++, arg_node->tid,
                  token_name[arg_node->tid][0]);

        if (arg_node->tid == TK_Arg_Named) { // skip TK_COMMA nodes
            id = utarray_eltptr(arg_node->subnodes, 0);
            log_debug("testing id[%d]: %d %s", i, id->tid, id->s);

            if ((strncmp(id->s, attr_name, name_len) == 0)
                && strlen(id->s) == name_len ){

                log_debug("MATCH");
                return arg_node;

                /* name matches, now test value */
                /* val = utarray_eltptr(arg_node->subnodes, 2); */
                /* log_debug("\tattr: %s = %s", id->s, val->s); */

                /* if ( (strncmp(val->s, attr_val, attr_val_len) == 0) */
                /*      && strlen(val->s) == attr_val_len ) { */
                /*     /\* log_debug("3 xxxxxxxxxxxxxxxx MATCH %d %s == %s", *\/ */
                /*     /\*           attr_val_len, val->s, attr_val); *\/ */
                /*     return arg_node; */
                /* } */
            }
        }
    }
}
