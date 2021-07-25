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

#if INTERFACE
#define SUNLARK_IS_NODE_HELP "(node? obj) returns #t if obj is a node."
#define SUNLARK_IS_NODE_SIG s7_make_signature(s7, 2, s7_make_symbol(s7, "boolean?"), s7_t(s7))
#endif

/* called by Scheme 'node?'; internally, use c_is_sunlark_node (bool) */
s7_pointer sunlark_is_node(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_is_node");
#endif
    log_debug("obj t %d, node t %d",
              s7_c_object_type(args), ast_node_t);

    log_debug("car obj t %d, node t %d",
              s7_c_object_type(args), ast_node_t);

    return s7_make_boolean(s7, c_is_sunlark_node(s7, args));
}

/* for internal C use (returns bool); Scheme 'node?' calls sunlark_is_node */
bool c_is_sunlark_node(s7_scheme *s7, s7_pointer node_s7)
{
#if defined(DEBUG_SEALARK_PREDICATES)
    log_debug("c_is_sunlark_node X");
#endif

    if (s7_is_c_object(node_s7)) {
        bool eq = s7_c_object_type(node_s7) == ast_node_t;
        log_debug("5 is node? %d", eq);
        return eq;
    } else {
        return s7_f(s7);
    }
}

/* **************************************************************** */
#if INTERFACE
#define SUNLARK_IS_NODELIST_HELP "(ast-nodelist? obj) returns #t if obj is a ast_nodelist."
#define SUNLARK_IS_NODELIST_SIG s7_make_signature(s7, 2, s7_make_symbol(s7, "boolean?"), s7_t(s7))
#endif

/* **************************************************************** */
s7_pointer sunlark_is_kw(s7_scheme *s7, char *kw, struct node_s *self)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_is_kw: %s", kw);
#endif

    char buf[128];
    strncpy(buf, kw, strlen(kw) + 1);

    buf[strlen(buf) - 1] = '\0'; /* remove final ?  */

    if ( (strncmp("node?", kw, 5) == 0) && (strlen(kw) == 5) ) {
        s7_pointer n = sunlark_node_new(s7, self);
        return sunlark_is_node(s7,
                               s7_list(s7, 1, n));
    }

    if ( (strncmp("printable?", kw, 10) == 0) && (strlen(kw) == 10) ) {
        bool is_printable = sunlark_node_new(s7, self);
        return s7_make_boolean(s7, is_printable);
    }

    if ( (strncmp("target?", kw, 7) == 0) && (strlen(kw) == 7) ) {
        /* until we get TK_Target implemented */
        bool is_target =  sealark_is_target(self);
        return s7_make_boolean(s7, is_target);
    }

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
