#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"

#include "s7.h"

#include "sunlark_methods.h"

static s7_pointer sunlark_node_methods_let;

/* section: extension methods */
/* extension methods extend standard Scheme procedures like 'length'
   and 'equals' to support custom c-types.

   unsupported methods return #f (?)
 */

#define METHODS_PREFIX   "(openlet (immutable! (inlet "
#define METHODS_POSTFIX  ")))"

#define OBJECT_METHODS \
    "'float-vector? (lambda (p) (display \"foo \") #t) " \
    "'foo (lambda (self) \"hello from foo method!\") " \
    "'memq (lambda (self arg) \"hello from perverse memq method!\") " \
    "'cdr (lambda (self) \"hello from perverse cdr method!\") " \
    "'arity (lambda (p) (cons 1 1)) " \
    "'aritable? (lambda (p args) (= args 1)) " \
    "'vector-dimensions (lambda (p) (list (length p))) " \
    "'empty (lambda (p) (zero? (length p))) " \
    "'ref ast-node-ref " \
    "'vector-ref ast-node-ref " \
    "'vector-set! ast-node-set! "
    /* "'reverse! ast-node-reverse! " \ */
    /* "'subsequence subast_node " \ */
    /* "'append ast-node-append " */
    /* "'signature (lambda (p) (list '#t 'node? 'integer?)) " \ */
    /* "'type nodex? " \ */


/* object methods: registered on each object, not type */
void sunlark_register_c_object_methods(s7_scheme *s7, s7_pointer ast_node)
{
/* #ifdef DEBUG_TRACE */
/*     log_debug("_register_c_object_methods"); */
/* #endif */
    static bool initialized = false;
    if (!initialized) {
        sunlark_node_methods_let = s7_eval_c_string(s7, METHODS_PREFIX OBJECT_METHODS METHODS_POSTFIX);
        s7_gc_protect(s7, sunlark_node_methods_let);
        initialized = true;
    }

    s7_c_object_set_let(s7, ast_node, sunlark_node_methods_let);
    s7_openlet(s7, ast_node);
}

