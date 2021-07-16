#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "s7.h"

#include "ast_node_s7.h"

/* exported s7 c-types */
s7_int ast_node_t = 0;

static s7_pointer g_ast_node_methods_let;

/* forward decls */
/* section: identity */
static s7_pointer g_is_ast_node(s7_scheme *s7, s7_pointer args);

/* section: equality */
static bool _ast_nodes_are_value_equal(struct node_s *val1,
                                      struct node_s *val2);
static s7_pointer g_ast_nodes_are_equal(s7_scheme *s7, s7_pointer args);
static s7_pointer g_ast_nodes_are_equivalent(s7_scheme *s7, s7_pointer args);

/* section: getters and setters */
static s7_pointer g_ast_node_ref_specialized(s7_scheme *s7, s7_pointer args);
static s7_pointer g_ast_node_set_specialized(s7_scheme *s7, s7_pointer args);

static s7_pointer g_ast_node_object_applicator(s7_scheme *s7, s7_pointer args);

static void _register_get_and_set(s7_scheme *s7);

/* section: serialization */
/* we use a global buffer for serialization to avoid local allocs */
#if INTERFACE
#define SZDISPLAY_BUF (4096 * 4)
#endif
char *display_buf; // [SZDISPLAY_BUF];
char *display_ptr;

#define INC_PTR (workptr = workbuf + strlen(workbuf))

char *g_ast_node_display(s7_scheme *s7, void *value);
char *g_ast_node_display_readably(s7_scheme *s7, void *value);
static s7_pointer g_ast_node_to_string(s7_scheme *s7, s7_pointer args);

/* section: c-object construction */
static s7_pointer g_ast_node_copy(s7_scheme *s7, s7_pointer args);
static s7_pointer g_ast_node_init_from_s7(s7_scheme *s7, struct node_s *cs, s7_pointer args);
static s7_pointer g_new_ast_node(s7_scheme *s7, s7_pointer args);
enum formals_e {
    FLD_TYPE = 0,
    FLD_LINE, FLD_COL,
    FLD_TRAILING_NEWLINE,
    FLD_QTYPE,
    FLD_STR,
    FLD_COMMENTS,
    FLD_SUBNODES
};

/* section: c-object destruction */
static s7_pointer g_destroy_ast_node(s7_scheme *s7, s7_pointer obj);

/* section: extension methods */
static void _register_c_type_methods(s7_scheme *s7, s7_int ast_node_t);
static void _register_c_object_methods(s7_scheme *s7, s7_pointer ast_node);

/* section: c-type configuration */
static void   _register_ast_node_fns(s7_scheme *s7);

/* this is the public API that clients call: */
int configure_s7_ast_node_type(s7_scheme *s7); /* public */

/* section: gc */
static s7_pointer g_ast_node_gc_mark(s7_scheme *s7, s7_pointer p);

/* section: debugging */
void debug_print_s7(s7_scheme *s7, char *label, s7_pointer obj);

/* **************************************************************** */

/* **************************************************************** */
/* section: identity */
#define g_is_ast_node_help "(ast_node? obj) returns #t if obj is a ast_node."
#define g_is_ast_node_sig s7_make_signature(s7, 2, s7_make_symbol(s7, "boolean?"), s7_t(s7))
static s7_pointer g_is_ast_node(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("g_is_ast_node");
#endif
    return(s7_make_boolean(s7,
                           s7_c_object_type(s7_car(args)) == ast_node_t));
}
/* /section: identity */

/* **************************************************************** */
/* section: equality

   eqv? "returns #t if obj1 and obj2 are normally regarded as the same
   object." (r7rs)

   eq? "On symbols, booleans, the empty list, pairs, and records, and
also on non-empty strings, vectors, and bytevectors, eq? and eqv? are
guaranteed to have the same behavior. On procedures, eq? must return
true if the arguments’ location tags are equal. On numbers and
characters, eq?’s behavior is implementation-dependent, but it will
always return either true or false. On empty strings, empty vectors,
and empty bytevectors, eq? may also behave differently from eqv?."
(r7rs)

   equal? "The equal? procedure, when applied to pairs, vectors,
strings and bytevectors, recursively compares them, returning #t when
the unfoldings of its arguments into (possibly infinite) trees are
equal (in the sense of equal?) as ordered trees, and #f otherwise. It
returns the same as eqv? when applied to booleans, symbols, numbers,
characters, ports, procedures, and the empty list. If two objects are
eqv?, they must be equal? as well. In all other cases, equal? may
return either #t or #f." (r7rs)

   equivalent? s7 only? same as Scheme 'equal?' ?

 */

/** _ast_nodes_are_value_equal

    true if (possibly distinct) ast_nodes are value-equal
 */
static bool _ast_nodelists_are_value_equal(UT_array *nl1,
                                           UT_array *nl2)
{
#ifdef DEBUG_TRACE
    log_debug("_ast_nodes_are_value_equal");
#endif
    if (nl1 == nl2) return true;

    int len1 = utarray_len(nl1);
    int len2 = utarray_len(nl2);
    if (len1 != len2) return false;

    struct node_s *n1=NULL, *n2=NULL;
    int i = 1;
    while( (n1=(struct node_s*)utarray_next(nl1, n1))
           && (n2=(struct node_s*)utarray_next(nl2, n2)) ) {
        if ( !_ast_nodes_are_value_equal(n1, n2) )
            return false;
        i++;
    }

    return(true);
}

static bool _ast_nodes_are_value_equal(struct node_s *val1,
                                      struct node_s *val2)
{
#ifdef DEBUG_TRACE
    log_debug("_ast_nodes_are_value_equal");
#endif
    if (val1 == val2) return true;

    if (val1->type != val2->type) return false;
    if (val1->line != val2->line) return false;
    if (val1->col  != val2->col) return false;
    if (val1->trailing_newline  != val2->trailing_newline) return false;
    if (val1->qtype  != val2->qtype) return false;
    if (val1->s != val2->s) {
        if (strncmp(val1->s, val2->s, strlen(val1->s)) != 0)
            return false;
    }

    if (val1->comments == NULL) {
        if (val2->comments != NULL) {
            return false;
        /* } else { */
        }
    } else {
        if (val2->comments == NULL)
            return false;
    }
    if ( ! _ast_nodelists_are_value_equal(val1->comments, val2->comments) )
        return false;

    if (val1->subnodes == NULL) {
        if (val2->subnodes != NULL) {
            return false;
        /* } else { */
        }
    } else {
        if (val2->subnodes == NULL)
            return false;
    }
    if ( ! _ast_nodelists_are_value_equal(val1->subnodes, val2->subnodes) )
        return false;

    return(true);
}

/** _ast_nodes_are_c_eql

    c implementation of Scheme 'eqv?' ?

    numbers: both exact and numerically equal (=)
    pairs, vectors, bytevectors, records, strings: same mem address
 */

/** g_ast_nodes_are_equal

    callback for Scheme 'eqv?' ?
    wrapper on _ast_nodes_are_c_eql
 */
static s7_pointer g_ast_nodes_are_equal(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("g_ast_nodes_are_equal");
#endif
    return(s7_make_boolean(s7,
          /* _ast_nodes_are_c_eql((void *)s7_c_object_value(s7_car(args)), */
          /*                      (void *)s7_c_object_value(s7_cadr(args))) */
          _ast_nodes_are_value_equal((void *)s7_c_object_value(s7_car(args)),
                                    (void *)s7_c_object_value(s7_cadr(args)))
                           )
           );
}

/** g_ast_nodes_are_equivalent

    implementation for Scheme 'equal?' 'eq?' ?
    true if same values
 */
#define G_AST_NODES_ARE_EQUIVALENT_HELP "(equivalent? ast_node1 ast_node2)"
static s7_pointer g_ast_nodes_are_equivalent(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("g_ast_nodes_are_equivalent");
#endif

    s7_pointer arg1, arg2;

    arg1 = s7_car(args);
    arg2 = s7_cadr(args);
    if (!s7_is_c_object(arg2))
        return(s7_f(s7));

    if (s7_is_let(arg1))             /* (ast_node-let (ast_node)) */
        return(s7_make_boolean(s7, false));    /* checked == above */

    /* same type? type of arg1 known to be ast_node */
    if (s7_c_object_type(arg2) != ast_node_t)
        return(s7_make_boolean(s7, false));

    if (arg1 == arg2) { /* same c-object? */
        return(s7_t(s7));
    }

    /* compare c objects for equivalence */
    struct node_s *g1, *g2;
    g1 = (struct node_s *)s7_c_object_value(arg1);
    g2 = (struct node_s *)s7_c_object_value(arg2);

    if (g1 == g2) /* same ast_node */
        return s7_t(s7);

    bool eq = _ast_nodes_are_value_equal(g1, g2);
    if (eq)
        return(s7_t(s7));
        /* return(s7_make_boolean(s7, true)); */
    else
        return(s7_f(s7));
        /* return(s7_make_boolean(s7, false)); */
}
/* /section: equality */

/* **************************************************************** */
/* section: getters and setters */
/* get and set are special, since there are two ways to do each:
   * 1. generic get and set! (c-type methods)
   * 2. specialized ast_node-get and ast_node-set! (Scheme procedures)
 */

/* **************** */
/* helper fn */
static s7_pointer _ast_node_lookup_kw(s7_scheme *s7,
                             struct node_s *ast_node, s7_pointer kw)
{
#ifdef DEBUG_TRACE
    log_debug("ast_node_lookup_kw");
#endif
    if (kw == s7_make_keyword(s7, "type"))
        return s7_make_integer(s7, ast_node->type);

    if (kw == s7_make_keyword(s7, "line"))
        return s7_make_integer(s7, ast_node->line);

    if (kw == s7_make_keyword(s7, "col"))
        return s7_make_integer(s7, ast_node->col);

    if (kw == s7_make_keyword(s7, "trailing_newine"))
        return s7_make_boolean(s7, ast_node->trailing_newline);

    if (kw == s7_make_keyword(s7, "qtype"))
        return s7_make_integer(s7, ast_node->qtype);

    if (kw == s7_make_keyword(s7, "s"))
        return s7_make_string(s7, ast_node->s);

    //FIXME: implement
    /* if (kw == s7_make_keyword(s7, "subnodes")) */
    /*     construct and return nodelist */

    //FIXME: implement
    /* if (kw == s7_make_keyword(s7, "comments")) */
    /*     construct and return nodelist */

    return(s7_wrong_type_arg_error(s7, "ast-node-ref",
                                       1, kw, "one of :c, :str, :i, etc."));
}

/* **************** */
/** g_ast_node_ref_specialized

    (ast_node-ref obj key)
    takes two args, a ast_node object and a keyword to look up in the object.
 */
#define G_AST_NODE_REF_SPECIALIZED_HELP "(ast_node-ref b i) returns the ast_node value at index i."
#define G_AST_NODE_REF_SPECIALIZED_SIG s7_make_signature(s7, 3, s7_t(s7), s7_make_symbol(s7, "ast-node?"), s7_make_symbol(s7, "integer?"))

static s7_pointer g_ast_node_ref_specialized(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("g_ast_node_ref_specialized");
    /* debug_print_s7(s7, "g_ast_node_ref_specialized args: ", args); */
#endif

    struct node_s *g;
    /* size_t index; */
    s7_int typ;
    s7_pointer obj;
    if (s7_list_length(s7, args) != 2)
        return(s7_wrong_number_of_args_error(s7, "ast-node-ref takes 2 arguments: ~~S", args));

    obj = s7_car(args);
    typ = s7_c_object_type(obj);
    if (typ != ast_node_t)
        return(s7_wrong_type_arg_error(s7, "ast-node-ref", 1, obj, "a ast_node"));
    g  = (struct node_s *)s7_c_object_value(obj);

    if (s7_is_null(s7, s7_cdr(args))) /* this is for an (obj) test */
        return(s7_wrong_type_arg_error(s7, "ast-node-ref", 1, obj, "missing keyword arg"));
        /* return(s7_make_integer(s7, 32)); */

    /* kw arg = name of field, find it field in the object */
    /* symbol arg = name of method, find it in object's method table */
    s7_pointer arg = s7_cadr(args);
    if (s7_is_keyword(arg))
        return _ast_node_lookup_kw(s7, g, arg);
    else {
        return(s7_wrong_type_arg_error(s7, "ast-node-ref",
                                       2, arg, "a keyword"));
    }
}

/** g_ast_node_object_applicator

    (more accurate: object_applicator, not essentially tied to ref)

    registered by s7_c_type_set_ref

    first arg is "self"

    function called when objects of type ast_node are evaluated as
    functions, i.e. when they occur in function position (car of a
    list).

    not to be confused with generic ref of SRFI 123, e.g. (ref vec i),
    which s7 does not support.(?)

    by convention, same as ref_specialized (i.e ast_node-ref) but this
    is not a requirement. could be used for anything, not just
    reference. example: (o :child-count) == (ast_node-child-count o)
    or (o :fullname) concats (o :fname) and (o :lname)

    iow, it's a generic generic function, whereas generic ref is just
    a generic ref function. "meta generic?"

    in practice, its a dispatcher. sorta. its job is to inspect the args and
    decide what to do with them.
 */
static s7_pointer g_ast_node_object_applicator(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("g_ast_node_object_applicator");
    debug_print_s7(s7, "APPLICATOR ARGS: ", s7_cdr(args));
#endif

    /* no need to check arg1, it's the "self" ast_node obj */
    /* s7_pointer g  = (struct node_s *)s7_c_object_value(obj); */

    s7_pointer rest = s7_cdr(args);
    if (s7_is_null(s7, rest))
        return(s7_wrong_type_arg_error(s7, "ast-node-fn",
                                       1, rest, "missing keyword arg"));

    s7_pointer op = s7_car(rest);

    /* Currently s7 does not make a distinction between keywords and symbols; (keyword? :a) and (symbol? :a) both report true, and s7_is_ */
    if (s7_is_keyword(op)) {
        if (op == s7_make_keyword(s7, "subnodes_ct")) {
            log_debug("running :subnodes_ct");
            s7_pointer obj = s7_car(args);
            struct node_s *cs  = (struct node_s *)s7_c_object_value(obj);
            int ct = utarray_len(cs->subnodes);
            return s7_make_integer(s7, ct);
        } else {
            return g_ast_node_ref_specialized(s7, args);
        }
    } else {
        if (s7_is_symbol(op)) { /* method access ((ast_node 'foo) b) etc */
            s7_pointer val;
            val = s7_symbol_local_value(s7, op, g_ast_node_methods_let);
            if (val != op)
                return(val);
            else {
                /* found self-referring method? method not found? methods table corrupt */
                /* return(s7_wrong_type_arg_error(s7, "ast-node-ref", */
                /*                                2, arg, "a kw or sym")); */
                log_error("ERROR: corrupt object-methods-let?");
                return NULL;
            }
	} else {
            return(s7_wrong_type_arg_error(s7, "ast-node-ref",
                                           2, op, "a keyword or symbol"));
        }
    }
}

/* **************** */
/** g_ast_node_set_specialized

    registered twice: as a c-type generalize set! (s7_c_type_set_set()) and
    as procedure "ast-node-set!" (s7_define_typed_function())

    generalized set: (set! (c-obj :k) v)

    in this case set! will call the set method registered with the
    c-obj's c-type, passing the c-obj, key :k, and value v.

    note that outside of this set! context, (c-obj :k) will lookup the
    value bound to :k in c-obj (using g_struct_get).
 */
#define G_AST_NODE_SET_SPECIALIZED_HELP "(ast_node-set! b i x) sets the ast_node value at index i to x."

#define G_AST_NODE_SET_SPECIALIZED_SIG s7_make_signature(s7, 4, s7_make_symbol(s7, "float?"), s7_make_symbol(s7, "ast-node?"), s7_make_symbol(s7, "integer?"), s7_make_symbol(s7, "float?"))

static s7_pointer _update_ast_node(s7_scheme *s7,
                                  struct node_s *ast_node,
                                  s7_pointer key, s7_pointer val)
{
#ifdef DEBUG_TRACE
    log_debug("_update_ast_node");
#endif

    if (key == s7_make_keyword(s7, "type")) {
        if (!s7_is_integer(val))
            return(s7_wrong_type_arg_error(s7, "ast-node-set!",
                                           3, val, "an integer"));
        ast_node->type = s7_integer(val);
        return val;
    }

    if (key == s7_make_keyword(s7, "line")) {
        if (!s7_is_integer(val))
            return(s7_wrong_type_arg_error(s7, "ast-node-set!",
                                           3, val, "an integer"));
        ast_node->line = s7_integer(val);
        return val;
    }

    if (key == s7_make_keyword(s7, "col")) {
        if (!s7_is_integer(val))
            return(s7_wrong_type_arg_error(s7, "ast-node-set!",
                                           3, val, "an integer"));
        ast_node->col = s7_integer(val);
        return val;
    }

    if (key == s7_make_keyword(s7, "trailing_newline")) {
        if (!s7_is_boolean(val))
            return(s7_wrong_type_arg_error(s7, "ast-node-set!",
                                           3, val, "a boolean"));
        ast_node->trailing_newline = s7_boolean(s7, val);
        return val;
    }

    if (key == s7_make_keyword(s7, "qtype")) {
        if (!s7_is_integer(val))
            return(s7_wrong_type_arg_error(s7, "ast-node-set!",
                                           3, val, "an integer"));
        ast_node->qtype = s7_integer(val);
        return val;
    }

    if (key == s7_make_keyword(s7, "s")) {
        if (!s7_is_string(val))
            return(s7_wrong_type_arg_error(s7, "ast-node-set!",
                                           3, val, "a string"));
        free(ast_node->s);
        /* s7_string() doc says "do not free the string", so we need
           to copy it? */
        int n = s7_string_length(val);
        ast_node->s = calloc(1, n);
        strncpy(ast_node->s, s7_string(val), n);
        return val;
    }

    //FIXME: implement for ast_nodelist?
    // we need this if we want to be able to replace subnodes/comments
    // e.g. replace a deps list

    /* if (key == s7_make_keyword(s7, "comments")) { */
    /*     if (s7_c_object_type(val) != ast_node_t) { */
    /*         return(s7_wrong_type_arg_error(s7, "ast-node-set!", */
    /*                                        3, val, "a ast_node")); */
    /*     } else { */
    /*         log_debug("2 xxxxxxxxxxxxxxxx"); */
    /*         ast_node->substruct = s7_c_object_value(val); */
    /*         return val; */
    /*     } */
    /* } */

    /* if (key == s7_make_keyword(s7, "subnodes")) { */
    /*     if (s7_c_object_type(val) != ast_node_t) { */
    /*         return(s7_wrong_type_arg_error(s7, "ast-node-set!", */
    /*                                        3, val, "a ast_node")); */
    /*     } else { */
    /*         log_debug("2 xxxxxxxxxxxxxxxx"); */
    /*         ast_node->substruct = s7_c_object_value(val); */
    /*         return val; */
    /*     } */
    /* } */

  return(s7_error(s7, s7_make_symbol(s7, "key not found"),
                  s7_list(s7, 2, s7_make_string(s7, "key not found: ~S"),
                          key)));
}

/* g_ast_node_set_specialized

   implements (ast_node_set! ...)

   what about (set! (node :subnodes) nodelist)? we need ctype for nodelist
 */
static s7_pointer g_ast_node_set_specialized(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("g_ast_node_set_specialized");
#endif
    struct node_s *g;
    s7_int typ;
    s7_pointer obj, kw;

    /* set! methods/procedures need to check that they have
       been passed the correct number of arguments: 3 */

    if (s7_list_length(s7, args) != 3)
        return(s7_wrong_number_of_args_error(s7, "ast-node-set! takes 3 arguments: ~~S", args));

    obj = s7_car(args);
    /* qua procedure, check type of first arg */
    typ = s7_c_object_type(obj);
    if (typ != ast_node_t)
        return(s7_wrong_type_arg_error(s7, "ast-node-set!", 1, obj, "a ast_node"));

    if (s7_is_immutable(obj))
        return(s7_wrong_type_arg_error(s7, "ast-node-set!", 1, obj, "a mutable ast_node"));

    /* validate lookup key type - in this case, a keyword */
    kw = s7_cadr(args);
    if (!s7_is_keyword(kw))
        return(s7_wrong_type_arg_error(s7, "ast-node-set!",
                                       2, kw, "a keyword"));

    /* mutate to object: */
    g = (struct node_s *)s7_c_object_value(obj);
    _update_ast_node(s7, g, kw, s7_caddr(args));
    //FIXME: r7rs says result of set! is unspecified. does that mean
    //implementation-specified?
    return s7_unspecified(s7);
}

static s7_pointer g_ast_node_set_generic(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("g_ast_node_set_generic");
    /* debug_print_s7(s7, "set_generic spec: ", args); */
#endif
    return g_ast_node_set_specialized(s7, args);
}

static void _register_get_and_set(s7_scheme *s7)
{
    /* generic (SRFI 17) */
    s7_c_type_set_getter(s7, ast_node_t, s7_name_to_value(s7, "ast-node-ref"));
    s7_c_type_set_setter(s7, ast_node_t, s7_name_to_value(s7, "ast-node-set!"));
    // only for var setters, not fn setters?
    /* s7_set_setter(s7, */
    /*               s7_name_to_value(s7, "ast-node-ref"), */
    /*               /\* s7_make_symbol(s7, "ast-node-ref"), *\/ */
    /*               s7_name_to_value(s7, "ast-node-set!")); */

    /* set_ref should be called set_applicator or some such */
    s7_c_type_set_ref(s7, ast_node_t, g_ast_node_object_applicator);
    s7_c_type_set_set(s7, ast_node_t, g_ast_node_set_generic);
}

/* /section: getters and setters */

/* **************************************************************** */
/* section: serialization */
//FIXME: handle large files. use dynamic alloc
char *g_ast_node_display(s7_scheme *s7, void *value)
{
#ifdef DEBUG_TRACE
    log_debug("g_ast_node_display");
#endif

    struct node_s *nd = (struct node_s *)value;

    // check display_buf size, expand if needed

    char buf[128];
    int len;

    sprintf(buf, "#ast_node<\n");
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " tid  = %d,\n", nd->type);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " tnm  = %s,\n", token_name[nd->type][0]);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " line  = %d,\n", nd->line);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " col   = %d,\n", nd->col);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " trailing_newline = %d,\n", nd->trailing_newline);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    if (nd->type == TK_STRING) {
        sprintf(buf, " qtype = #x%#X,\n", nd->qtype);
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    if (nd->s) {
        sprintf(buf, " s     = %s,\n", nd->s);
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    if (nd->comments) {
        sprintf(buf, " comments = ");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;

        /* updates global display_buf */
        g_ast_nodelist_display(s7, (UT_array*)nd->comments);

        sprintf(buf, ",\n");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    if (nd->subnodes) {
        sprintf(buf, " subnodes =\n\t");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;

        /* updates global display_buf */
        g_ast_nodelist_display(s7, (UT_array*)nd->subnodes);

        sprintf(buf, ",\n");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    sprintf(display_ptr - 2, ">,\n");
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    display_ptr++; // -= 1;

    return display_buf;
}

/** g_ast_node_display_readably

    produces a "roundtrippable" string, one that when read by the reader
    results in an object equal to the original.
 */
char *g_ast_node_display_readably(s7_scheme *s7, void *value)
{
#ifdef DEBUG_TRACE
    log_debug("g_ast_node_display_readably");
#endif

    struct node_s *nd = (struct node_s *)value;

    // check display_buf size, expand if needed

    char buf[128];
    int len;

    sprintf(display_ptr++, "(");

    sprintf(buf, "(tid %d) ;; %s\n", nd->type, token_name[nd->type][0]);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " (line %d)\n", nd->line);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " (col  %d)\n", nd->col);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " (trailing_newline %d)\n", nd->trailing_newline);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    if (nd->type == TK_STRING) {
        sprintf(buf, " (qtype #x%02X)\n", nd->qtype);
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    if (nd->s) {
        sprintf(buf, " (s %s)\n", nd->s);
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    if (nd->comments) {
        sprintf(buf, " (comments ");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;

        /* updates global display_buf */
        g_ast_nodelist_display_readably(s7, (UT_array*)nd->comments);

        /* sprintf(buf, ") ;; end comments\n\n"); */
        sprintf(buf, ")\n");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    if (nd->subnodes) {
        sprintf(buf, " (subnodes\n\t");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;

        /* updates global display_buf */
        g_ast_nodelist_display_readably(s7, (UT_array*)nd->subnodes);

        sprintf(buf, ")\n");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    sprintf(display_ptr - 1, ")\n");  /* backup to rem last newline */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    display_ptr++;

    return display_buf; // workbuffer;
}

static s7_pointer g_ast_node_to_string(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("g_ast_node_to_string");
    /* debug_print_s7(s7, "to_string cdr: ", s7_cdr(args)); */
#endif
    display_buf = calloc(1, SZDISPLAY_BUF);
    display_ptr = display_buf;

    s7_pointer obj, choice;
    char *descr;
    obj = s7_car(args);
    if (s7_is_pair(s7_cdr(args)))
        choice = s7_cadr(args);
    else choice = s7_t(s7);
    if (choice == s7_make_keyword(s7, "readable")) {
        memset(display_buf, '\0', SZDISPLAY_BUF);
        display_ptr = (char*)display_buf;
        descr = g_ast_node_display_readably(s7, s7_c_object_value(obj));
    }
    else descr = g_ast_node_display(s7, s7_c_object_value(obj));

    /* log_debug("TO_STRING LEN: %d", strlen(descr)); */
    obj = s7_make_string(s7, descr);

    free(descr); //BUG? FIXME free substruct strings
    return(obj);
}
/* /section: serialization */

/* **************************************************************** */
/* section: c-object construction */

/** g_ast_node_copy

    registered as c-type method using s7_c_type_set_copy

    this extends the predefined (copy ...) proc. to handle our custom
    c-type.

    in s7, 'copy' is generic, but it seems to be limited to sequences;
    maybe copy-seq would be better. OTOH it does work with e.g. a
    single int, as in (let* ((a 1) (b (copy a))) ...). But the two-arg
    version (copy a b) only works with sequences?

    But it's also not strictly copy; it may copy only part of the
    source, for example. And it's destructive. A better name might be
    update! or similar. (idiomatic english: "copy a to b" means "make
    b distinct from but identical to a".)

    if arg1 has ast_node type
        if cdr(args) is pair
            arg2 = cadr(args)
            check for mutability
            if arg2.type is ast_node_t
                ok to copy
                if optional 3rd arg ('start-cp-from' index)
                    ...
                else
                    copy arg1 data to arg1
            else ;; arg2.type is NOT ast_node_t
                g_block_copy assumes arg2 is a vector and copies data to it?
        else ;; only one arg, copy it
            make a new ast_node
            copy arg1 data to new ast_node
            return new ast_node
    else ;;  arg1 not ast_node, so arg2 must be

 */
static s7_pointer g_ast_node_copy(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("g_ast_node_copy");
    debug_print_s7(s7, "COPY ARGS: ", args);
    /* debug_print_s7(s7, "COPY ARGS len: ", s7_list_length(s7, args)); */
#endif

    s7_pointer arg1, arg2;
    struct node_s *n1, *n2;

    arg1 = s7_car(args);
    if (s7_c_object_type(arg1) == ast_node_t) {
        log_debug("copy ast_node");
        n1 = (struct node_s*)s7_c_object_value(arg1);
        if (s7_is_pair(s7_cdr(args))) {
            log_debug("copy a b");
            arg2 = s7_cadr(args);
            if (s7_is_immutable(arg2))
                return(s7_wrong_type_arg_error(s7, "ast-node-copy!",
                                               0, arg2, "a mutable ast_node"));
            if (s7_c_object_type(arg2) == ast_node_t) {
                log_debug("copy ast_node to ast_node");
                n2 = (struct node_s*)s7_c_object_value(arg2);
                ast_node_copy(n1, n2);
                return arg2;
            } else {
                return(s7_wrong_type_arg_error(s7, "ast-node-copy!",
                                               0, arg2, "a mutable ast_node"));
            }
        } else {
            log_debug("copy one");
            /* only one arg, copy it to new ast_node */
            /* struct node_s *n2 = (struct node_s *) */
            /*     calloc(1, sizeof(struct node_s)); */
            /* n2 = ast_node_init_default(n2); */
            n2 = ast_node_new();
            ast_node_copy(n2, n1);
            return s7_make_c_object(s7, ast_node_t,
                                    (void *)n2);
        }
    } else {
        //FIXME: implement
        log_debug("copy non-node");
        /* arg1 type != ast_node_t */
    }
}

/*
  g_ast_node_init_from_s7
  initialize a C struct from s7-scheme arg-list (compare
  'ast_node_init', initialize a ast_node from C args)
 */
static s7_pointer g_ast_node_init_from_s7(s7_scheme *s7, struct node_s *cs, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("g_ast_node_init_from_s7");
    /* debug_print_s7(s7, "INIT ARGS: ", args); */
#endif

    //FIXME: is this the right way to cdr over a list?
    s7_pointer arg = s7_car(args);
    s7_pointer cdr  = s7_cdr(args);
    int i = 0;
    while ( !s7_is_unspecified(s7, arg) ) {
        int len;
        switch(i) {
        case FLD_TYPE:
            if (!s7_is_integer(arg)) {
                return(s7_wrong_type_arg_error(s7, "make-ast-node",
                                               1, arg, "an integer"));
            }
            cs->type = s7_integer(arg);
            break;
        case FLD_LINE:
            if (!s7_is_integer(arg)) {
                return(s7_wrong_type_arg_error(s7, "make-ast-node",
                                               1, arg, "an integer"));
            }
            cs->line = s7_integer(arg);
            break;
        case FLD_COL:
            if (!s7_is_integer(arg)) {
                return(s7_wrong_type_arg_error(s7, "make-ast-node",
                                               1, arg, "an integer"));
            }
            cs->col = s7_integer(arg);
            break;

        case FLD_TRAILING_NEWLINE:
            if (!s7_is_integer(arg)) {
                return(s7_wrong_type_arg_error(s7, "make-ast-node",
                                               1, arg, "an integer"));
            }
            cs->trailing_newline = s7_integer(arg);
            break;

        case FLD_QTYPE:
            if (!s7_is_integer(arg)) {
                return(s7_wrong_type_arg_error(s7, "make-ast-node",
                                               1, arg, "an integer"));
            }
            cs->qtype = s7_integer(arg);
            break;

        case FLD_STR:
            len = s7_string_length(arg);
            cs->s = calloc(1, len);
            strncpy(cs->s, s7_string(arg), len);
            break;

        case FLD_COMMENTS:
            //FIXME: implement
            break;

        case FLD_SUBNODES:
            //FIXME: implement
            break;
        }
        arg = s7_car(cdr);
        cdr  = s7_cdr(cdr);
        i++;
    }
    return NULL;
}

/** g_new_ast_node
 */
/* docstring passed to the s7_define_.. used to register the fn in Scheme */
#define g_new_ast_node_help "(make-ast-node) returns a new ast_node with randome data"

#define MAKE_AST_NODE_FORMAL_PARAMS "(type 0) (line 0) (col 0) (trailing_newline #f) (qtype 0) (s NULL) (comments NULL) (subnodes NULL)"

static s7_pointer g_new_ast_node(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("g_new_ast_node");
#endif

    /* struct node_s *new_ast_node = (struct node_s *) */
    /*     calloc(1, sizeof(struct node_s)); */
    /* new_ast_node = ast_node_init_default(new_ast_node); */

    struct node_s *n = ast_node_new();

    if (g_ast_node_init_from_s7(s7, n, args) != NULL) {
        log_debug("OOPS");
    }

    s7_pointer new_ast_node_s7 = s7_make_c_object(s7, ast_node_t,
                                                  (void *)n);

    _register_c_object_methods(s7, new_ast_node_s7);

    return(new_ast_node_s7);
}
/* /section: c-object construction */

/* **************************************************************** */
/* section: c-object destruction */
static s7_pointer g_destroy_ast_node(s7_scheme *s7, s7_pointer obj)
{
#ifdef DEBUG_TRACE
    log_debug("g_destroy_ast_node");
#endif
    struct node_s *cs = (struct node_s*)s7_c_object_value(obj);
    ast_node_free(cs);
    return NULL;
}
/* /section: c-object destruction */

/* section: extension methods */
/* extension methods extend standard Scheme procedures like 'length'
   and 'equals' to support custom c-types.

   unsupported methods return #f (?)
 */

#define METHODS_PREFIX   "(openlet (immutable! (inlet "
#define METHODS_POSTFIX  ")))"

#define OBJECT_METHODS \
    "'float-vector? (lambda (p) (display \"foo \") #t) " \
    "'signature (lambda (p) (list '#t 'ast_node? 'integer?)) " \
    "'type ast_node? " \
    "'foo (lambda (self) \"hello from foo method!\") " \
    "'memq (lambda (self arg) \"hello from perverse memq method!\") " \
    "'arity (lambda (p) (cons 1 1)) " \
    "'aritable? (lambda (p args) (= args 1)) " \
    "'vector-dimensions (lambda (p) (list (length p))) " \
    "'empty (lambda (p) (zero? (length p))) " \
    "'ref ast_node-ref " \
    "'vector-ref ast_node-ref " \
    "'vector-set! ast_node-set! "
    /* "'reverse! ast_node-reverse! " \ */
    /* "'subsequence subast_node " \ */
    /* "'append ast_node-append " */

/* object methods: registered on each object, not type */
static void _register_c_object_methods(s7_scheme *s7, s7_pointer ast_node)
{
#ifdef DEBUG_TRACE
    log_debug("_register_c_object_methods");
#endif
    static bool initialized = false;
    if (!initialized) {
        g_ast_node_methods_let = s7_eval_c_string(s7, METHODS_PREFIX OBJECT_METHODS METHODS_POSTFIX);
        s7_gc_protect(s7, g_ast_node_methods_let);
        initialized = true;
    }
    s7_c_object_set_let(s7, ast_node, g_ast_node_methods_let);
    s7_openlet(s7, ast_node);
}

static void _register_c_type_methods(s7_scheme *s7, s7_int ast_node_t)
{
#ifdef DEBUG_TRACE
    log_debug("_register_c_type_methods");
#endif
    s7_c_type_set_gc_free(s7, ast_node_t, g_destroy_ast_node);
    s7_c_type_set_gc_mark(s7, ast_node_t, g_ast_node_gc_mark);

    /* s7_c_type_set_equal(s7, ast_node_t, _ast_nodes_are_c_eql); */
    s7_c_type_set_is_equal(s7, ast_node_t, g_ast_nodes_are_equal);
    s7_c_type_set_is_equivalent(s7, ast_node_t, g_ast_nodes_are_equivalent);

    s7_c_type_set_copy(s7, ast_node_t, g_ast_node_copy);
    /* s7_c_type_set_length(s7, ast_node_t, g_ast_node_length); */
    /* s7_c_type_set_reverse(s7, ast_node_t, g_ast_node_reverse); */
    /* s7_c_type_set_fill(s7, ast_node_t, g_ast_node_fill); */

    s7_c_type_set_to_string(s7, ast_node_t, g_ast_node_to_string);

}

/* /section: extension methods */


/* **************************************************************** */
/* section: c-type configuration */

static void _register_ast_node_fns(s7_scheme *s7)
{
#ifdef DEBUG_TRACE
    log_debug("_register_ast_node_fns");
#endif
    /* s7_define_safe_function(s7, "ast-node", g_to_ast_node, 0, 0, true, g_ast_node_help); */
    s7_define_safe_function_star(s7, "make-ast-node", g_new_ast_node,
                                 MAKE_AST_NODE_FORMAL_PARAMS,
                                 g_new_ast_node_help);

    s7_define_typed_function(s7, "ast-node?", g_is_ast_node, 1, 0, false, g_is_ast_node_help, g_is_ast_node_sig);

    /* specialized get/set! */
    s7_define_typed_function(s7, "ast-node-ref", g_ast_node_ref_specialized, 2, 0, false, G_AST_NODE_REF_SPECIALIZED_HELP, G_AST_NODE_REF_SPECIALIZED_SIG);
    s7_define_typed_function(s7, "ast-node-set!", g_ast_node_set_specialized, 3, 0, false, G_AST_NODE_SET_SPECIALIZED_HELP, G_AST_NODE_SET_SPECIALIZED_SIG);

    // ast_node-let => s7_c_object_let, a let for the instance not the type
    /* s7_define_safe_function(s7, "ast-node-let", g_ast_node_let, 1, 0, false, g_ast_node_let_help); */

    /* s7_define_safe_function(s7, "subast_node", g_subast_node, 1, 0, true, g_subast_node_help); */
    /* s7_define_safe_function(s7, "ast-node-append", g_ast_node_append, 0, 0, true, g_ast_node_append_help); */
    /* s7_define_safe_function(s7, "ast-node-reverse!", g_ast_node_reverse_in_place, 1, 0, false, g_ast_node_reverse_in_place_help); */
}

/* **************** */
/** configure_s7_ast_node_type(s7_scheme *s7)

    public
*/
//FIXME: return int rc?
EXPORT int configure_s7_ast_node_type(s7_scheme *s7)
{
#ifdef DEBUG_TRACE
    log_debug("configure_s7_ast_node_type");
#endif
    /* s7_int t = _make_c_type(s7); */
    ast_node_t = s7_make_c_type(s7, "<ast_node>");
    _register_c_type_methods(s7, ast_node_t);
    _register_get_and_set(s7);
    _register_ast_node_fns(s7);
    s7_provide(s7, "ast-node");
    return ast_node_t;
}
/* section: c-type configuration */

/* **************************************************************** */
/* section: gc */
static s7_pointer g_ast_node_gc_mark(s7_scheme *s7, s7_pointer p)
{
    /* nothing to mark because we protect g_ast_node_methods_let, and all ast_node objects get the same let */
    return(p);
}

/* static s7_pointer g_ast_node_gc_free(s7_scheme *s7, s7_pointer obj) */
/* { */
/* #ifdef DEBUG_TRACE */
/*     log_debug("g_ast_node_gc_free"); */
/* #endif */
/*     struct node_s *cs = (struct node_s*)s7_c_object_value(obj); */
/*     ast_node_free(cs); */
/*     return(NULL); */
/* } */

/* /section: gc */

/* **************************************************************** */
// FIXME: put this in debug.c
/* section: debugging */
void debug_print_s7(s7_scheme *s7, char *label, s7_pointer obj)
{
    log_debug("debug_print_s7: ");
    /* s7_pointer p = s7_current_output_port(s7); */
    /* s7_display(s7, s7_make_string(s7, label), p); */
    log_debug("label: %s", label);
    log_debug("%s", s7_object_to_c_string(s7, obj));
    /* s7_display(s7, obj, p); */
    /* s7_newline(s7, p); */
}

/* /section: debugging */
