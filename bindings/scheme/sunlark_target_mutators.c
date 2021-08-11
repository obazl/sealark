#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_target_mutators.h"

struct node_s *sunlark_target_mutate(s7_scheme *s7,
                                     struct node_s *target,
                                     s7_pointer lval,
                                     s7_pointer update_val)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sunlark_target_mutate: %s => %s",
              s7_object_to_c_string(s7, lval),
              s7_object_to_c_string(s7, update_val));
#endif
    /* sealark_debug_log_ast_outline(target, 0); */

    assert(target->tid == TK_Call_Expr);

    if (lval == KW(@@)) {
        struct node_s *call_sfx = utarray_eltptr(target->subnodes, 1);
        struct node_s *arglist = utarray_eltptr(call_sfx->subnodes, 1);
        struct node_s *r
            = sealark_target_bindings_rm_all(arglist);
        if (r)
            return r;
        else
            return NULL;
    }
    if (lval == KW(rule)) {
        log_error("mutate :rule, under construction ...");
        errno = ENOT_IMPLEMENTED;
        return NULL;
    }
}
