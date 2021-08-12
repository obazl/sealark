#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "sealark_procs.h"

EXPORT UT_array *sealark_proc_bindings_to_utarray(struct node_s *target)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_bindings");
#endif
    assert(target->tid == TK_Call_Expr);
    log_debug("target tid: %d %s", target->tid, TIDNAME(target));

    /* :call-expr[1] => :call-sfx[1] => :arg-list */
    struct node_s *call_sfx = utarray_eltptr(target->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

#if defined(DEBUG_AST)
    sealark_debug_log_ast_outline(arg_list, 0);
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

