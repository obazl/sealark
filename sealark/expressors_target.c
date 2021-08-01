#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "expressors_target.h"

EXPORT UT_array *sealark_target_bindings_to_utarray(struct node_s *target)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_bindings");
#endif

    log_debug("target tid: %d %s", target->tid, TIDNAME(target));

    /* :call-expr[1] => :call-sfx[1] => :arg-list */
    struct node_s *call_sfx = utarray_eltptr(target->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

#if defined(DEBUG_AST)
    sealark_debug_print_ast_outline(arg_list, 0);
#endif
    UT_array *attribs;
    utarray_new(attribs, &node_icd);

    struct node_s *nd=NULL;
    while( (nd=(struct node_s*)utarray_next(arg_list->subnodes, nd)) ) {
        if (nd->tid == TK_Binding)
            utarray_push_back(attribs, nd);
    }
    log_debug("found %d bindings (named args)", utarray_len(attribs));
    return attribs;
}

EXPORT int sealark_target_bindings_count(struct node_s *target)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_bindings_count");
#endif

    UT_array *bindings = sealark_target_bindings_to_utarray(target);
    int ct = utarray_len(bindings);
    utarray_free(bindings);
    return ct;
}

/* **************** */
EXPORT struct node_s *sealark_target_name(struct node_s *target)
{
/* #if defined (DEBUG_TRACE) || defined(DEBUG_QUERY) */
/*     log_debug("sealark_target_name"); */
/* #endif */

    assert(target->tid == TK_Call_Expr);

    /* :call-expr[1] > :call-sfx[1] > :arg-list */
    /* :arg-list - for targets, list of bindings */
    /*   for ordinary fn application could be anything */

    UT_string *buf;
    utstring_new(buf);
    sealark_display_node(target, buf, 0);
    utstring_free(buf);

    struct node_s *call_sfx = utarray_eltptr(target->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

#if defined(DEBUG_UTARRAYS)
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
#if defined(DEBUG_UTARRAYS)
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++,
                  arg_node->tid, token_name[arg_node->tid][0]);
#endif

        if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes
            /* first subnode is TK_ID */
            id = utarray_eltptr(arg_node->subnodes, 0);
#if defined(DEBUG_UTARRAYS)
            log_debug("testing id[%d]: %d %s", i, id->tid, id->s);
#endif
            if ((strncmp(id->s, "name", 4) == 0)
                && strlen(id->s) == 4 ){
                val = utarray_eltptr(arg_node->subnodes, 2);
                return val;
            }
        } else {
            log_debug("0 xxxxxxxxxxxxxxxx");
        }
    }
    return NULL;
}
