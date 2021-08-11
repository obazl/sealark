#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"

#include "s7.h"

#include "sunlark_predicators.h"

/*
  WARNING: for Scheme predicates we use paired implementations. For
  internal use c_pred returns C bool; for Scheme, sunlark_pred returns
  Scheme boolean.
*/

/* **************************************************************** */

/* for internal C use (returns bool); Scheme 'node?' calls sunlark_is_node */
bool c_is_sunlark_node(s7_scheme *s7, s7_pointer node_s7)
{
#if defined(DEBUG_PREDICATES)
    log_debug("c_is_sunlark_node %s",
              s7_object_to_c_string(s7, node_s7));
#endif

    if (s7_is_c_object(node_s7)) {
        bool eq = (s7_c_object_type(node_s7) == ast_node_t);
        return eq;
    } else {
        return false;
    }
}

/* **************************************************************** */
#if INTERFACE
#define SUNLARK_IS_NODELIST_HELP "(ast-nodelist? obj) returns #t if obj is a ast_nodelist."
#define SUNLARK_IS_NODELIST_SIG s7_make_signature(s7, 2, s7_make_symbol(s7, "boolean?"), s7_t(s7))
#endif

/* **************************************************************** */
s7_pointer sunlark_node_satisfies_kw_pred(s7_scheme *s7, s7_pointer kw, struct node_s *self)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_node_satisfies_kw_pred: %s", kw);
#endif

    s7_pointer sym = s7_keyword_to_symbol(s7, kw);
    const char *pred = s7_symbol_name(sym);

    if ( (strncmp("node?", pred, 5) == 0) && (strlen(pred) == 5) ) {
        s7_pointer n = sunlark_new_node(s7, self);
        return sunlark_is_node(s7,
                               s7_list(s7, 1, n));
    }

    if ( (strncmp("printable?", pred, 10) == 0) && (strlen(pred) == 10) ) {
        bool is_printable = sunlark_new_node(s7, self);
        return s7_make_boolean(s7, is_printable);
    }

    if ( (strncmp("target?", pred, 7) == 0) && (strlen(pred) == 7) ) {
        /* until we get TK_Target implemented */
        bool is_target = sealark_call_expr_is_target(self);
        return s7_make_boolean(s7, is_target);
    }

    if ( (strncmp("symbol?", pred, 7) == 0) && (strlen(pred) == 7) ) {
        /* until we get TK_Symbol implemented */
        if (self->tid == TK_ID)
            return s7_t(s7);
        else
            return s7_f(s7);
    }

    /* token type predication */
    char buf[128];
    strncpy(buf, pred, strlen(pred) + 1);

    buf[strlen(buf) - 1] = '\0'; /* remove final ?  */

    int tokid = sealark_kw_to_tid(buf);
    log_debug("kw to tid: %d", tokid);
    if (tokid < 0) {
        return s7_f(s7);
    }
    /* s7_pointer obj = s7_car(args); */
    /* struct node_s *n = s7_c_object_value(obj); */
    log_debug("self tid: %d %s, tokid %d %s",
              self->tid, TIDNAME(self),
              tokid, token_name[tokid][0]);

    return s7_make_boolean(s7, self->tid == tokid);
}

/* **************************************************************** */
bool sunlark_op_is_predicate(s7_scheme *s7, s7_pointer op)
{
#ifdef DEBUG_PREDICATORS
    log_debug("sunlark_op_is_predicate: %s",
              s7_object_to_c_string(s7, op));
#endif

    if (s7_is_keyword(op)) {
        s7_pointer sym = s7_keyword_to_symbol(s7, op);
        const char *kw = s7_symbol_name(sym);
        return kw[strlen(kw) - 1] == '?';
    } else {
        return false;
    }

}
