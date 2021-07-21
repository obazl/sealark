#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"

#include "predicators.h"

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
