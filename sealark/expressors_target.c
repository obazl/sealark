#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "expressors.h"

/* ******************************** */
/* returns only bindings (TK_Named_Arg) in a new UT_array */
/* **************************************************************** */
EXPORT UT_array *sealark_target_bindings(struct node_s *target)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_bindings");
#endif

    log_debug("target tid: %d %s", target->tid, TIDNAME(target));

    /* :call-expr[1] => :call-sfx[1] => :arg-list */
    struct node_s *call_sfx = utarray_eltptr(target->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

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

EXPORT int sealark_target_bindings_ct(struct node_s *target)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_bindings_ct");
#endif

    UT_array *bindings = sealark_target_bindings(target);
    return utarray_len(bindings);
}
