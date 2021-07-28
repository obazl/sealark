#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"

#include "s7.h"

#include "sunlark_node.h"

/* exported s7 c-types */

s7_int ast_node_t;

#if EXPORT_INTERFACE
#define AST_NODE_T ast_node_t
#endif

static s7_pointer sunlark_node_methods_let;

/* forward decls */
/* section: identity */
/* section: equality */
static bool _ast_nodes_are_value_equal(struct node_s *val1,
                                      struct node_s *val2);
static s7_pointer sunlark_nodes_are_equal(s7_scheme *s7, s7_pointer args);
static s7_pointer sunlark_nodes_are_equivalent(s7_scheme *s7, s7_pointer args);

/* section: getters and setters */
static s7_pointer sunlark_node_ref_specialized(s7_scheme *s7, s7_pointer args);
s7_pointer sunlark_node_object_applicator(s7_scheme *s7, s7_pointer args);

static void _register_get_and_set(s7_scheme *s7);

/* section: c-object construction */
static s7_pointer sunlark_node_copy(s7_scheme *s7, s7_pointer args);
static s7_pointer sunlark_node_init_from_s7(s7_scheme *s7, struct node_s *cs, s7_pointer args);
static s7_pointer sunlark_make_ast_node(s7_scheme *s7, s7_pointer args);
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
static s7_pointer sunlark_destroy_ast_node(s7_scheme *s7, s7_pointer obj);

/* section: extension methods */
static void _register_c_type_methods(s7_scheme *s7, s7_int ast_node_t);
static void _register_c_object_methods(s7_scheme *s7, s7_pointer ast_node);

/* section: c-type configuration */
static void   _register_ast_node_fns(s7_scheme *s7);

/* this is the public API that clients call: */
int configure_s7_ast_node_type(s7_scheme *s7); /* public */

/* section: gc */
static s7_pointer sunlark_node_gc_mark(s7_scheme *s7, s7_pointer p);

/* section: debugging */
void debug_print_s7(s7_scheme *s7, char *label, s7_pointer obj);

/* **************************************************************** */

#define NODE_TID (NODE_S7) ((s7_c_object_value(s7, NODE_S7)->tid;))

/* arg may be a node or a nodelist */
EXPORT int sunlark_node_tid(s7_scheme *s7, s7_pointer node_s7)
{
    if (s7_is_c_object(node_s7)) {
#ifdef DEBUG_TRACE
        /* struct node_s *n = s7_c_object_value(node_s7); */
        /* log_debug("sunlark_node_tid %d %s", */
        /*           n->tid, TIDNAME(n)); */
              /* s7_object_to_c_string(s7, node_s7)); */
#endif
    }
    if (s7_is_c_object(node_s7)) {
        /* if (c_is_sunlark_node(s7, s7_list(s7, 1, node_s7))) { */
        if (c_is_sunlark_node(s7, node_s7)) {
            struct node_s *n = s7_c_object_value(node_s7);
            return n->tid;
        /* } else { */
        /*     if (c_is_sunlark_nodelist(s7, node_s7)) { */
        /*         return TK_Node_List; */
        /*     } else { */
        /*         return TK_Unspecified; */
        /*     } */
        /* } */
        } else {
            //FIXME
            log_error("Found c object that is  not sunlark node");
            exit(EXIT_FAILURE);
        }
        log_error("Applying tid to non-c-object");
            exit(EXIT_FAILURE);
    }
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

    if (val1->tid != val2->tid) return false;
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

/** sunlark_nodes_are_equal

    callback for Scheme 'eqv?' ?
    wrapper on _ast_nodes_are_c_eql
 */
static s7_pointer sunlark_nodes_are_equal(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_nodes_are_equal");
#endif
    return(s7_make_boolean(s7,
          /* _ast_nodes_are_c_eql((void *)s7_c_object_value(s7_car(args)), */
          /*                      (void *)s7_c_object_value(s7_cadr(args))) */
          _ast_nodes_are_value_equal((void *)s7_c_object_value(s7_car(args)),
                                    (void *)s7_c_object_value(s7_cadr(args)))
                           )
           );
}

/** sunlark_nodes_are_equivalent

    implementation for Scheme 'equal?' 'eq?' ?
    true if same values
 */
#define SUNLARK_NODES_ARE_EQUIVALENT_HELP "(equivalent? ast_node1 ast_node2)"
static s7_pointer sunlark_nodes_are_equivalent(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_nodes_are_equivalent");
#endif

    s7_pointer arg1, arg2;

    arg1 = s7_car(args);
    arg2 = s7_cadr(args);
    if (!s7_is_c_object(arg2))
        return(s7_f(s7));

    if (s7_is_let(arg1))             /* (ast-node-let (ast_node)) */
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

/* **************** */
/* 'length' implementation */
/* IMPORTANT: this count may be used by iterators (for-each, map), so
   do not count meta data (delims, punctuation) */
s7_pointer sunlark_node_subnode_count(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_S7_API
    log_debug(">>>>>>>>>>>>>>> sunlark_node_subnode_count <<<<<<<<<<<<<<<");
#endif
    /* called by Scheme, so args is a list */
    struct node_s *node = s7_c_object_value(s7_car(args));
    //FIXME: count depends on node type
    if (node->subnodes) {
        int ct = 0;
        struct node_s *subnode = NULL;
        while( (subnode=(struct node_s*)utarray_next(node->subnodes, subnode)) ) {
            if (subnode->tid == TK_COMMA) continue;
            if (subnode->tid == TK_LBRACK) continue;
            if (subnode->tid == TK_RBRACK) continue;
            ct++;
        }
        return s7_make_integer(s7, ct);
    } else
        return s7_make_integer(s7, 0);
}

/* **************** */
s7_pointer sunlark_node_to_list(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_S7_API
    /* log_debug(">>>>>>>>>>>>>>>> sunlark_node_to_list <<<<<<<<<<<<<<<<"); */
#endif
    /* called by Scheme, so args is a list */
    struct node_s *node = s7_c_object_value(s7_car(args));
    /* log_debug("tid: %d %s", node->tid, TIDNAME(node)); */

    if (node->subnodes)
        return nodelist_to_s7_list(s7, node->subnodes);
    else
        return s7_nil(s7);
}


/* **************************************************************** */
/* section: getters and setters */
/* get and set are special, since there are two ways to do each:
   * 1. generic get and set! (c-type methods)
   * 2. specialized ast-node-get and ast-node-set! (Scheme procedures)
 */

/* **************** */
/** sunlark_node_ref_specialized

    (ast-node-ref obj key)
    takes two args, a ast_node object and a keyword to look up in the object.
 */
#define SUNLARK_NODE_REF_SPECIALIZED_HELP "(ast-node-ref nd k) returns the value for property k (a keyword) of ast-node nd."

/* sig: takes a node (satisfies node?) and an in (satisfies integer?),
   returns ... what does s7_t mean here? "something"? i.e not void? */
#define SUNLARK_NODE_REF_SPECIALIZED_SIG s7_make_signature(s7, 3, s7_t(s7), s7_make_symbol(s7, "node?"), s7_make_symbol(s7, "integer?"))

/** sunlark_node_ref_specialized

    Looks up node properties, whose names are keywords. Each field in
    the node_s struct has a property whose name is formed by prefixing
    a colon: :tid, :line, :col, :trailing_newline, :qtype, :s,
    :comments, :subnodes.

    In addition the following pseudo-properties are supported:
        :print - returns string for printable nodes, with correct quoting.

        :@<attr> - only for nodes of type :call_expr. returns binding
        (i.e. :binding node) whose :id is <attr>. E.g. (rulenode :deps)
        would return the 'deps' binding of the rulenode.

 */
static s7_pointer sunlark_node_ref_specialized(s7_scheme *s7, s7_pointer args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("sunlark_node_ref_specialized");
    debug_print_s7(s7, "sunlark_node_ref_specialized args: ", s7_cdr(args));
#endif

    struct node_s *self;
    /* size_t index; */
    s7_int typ;
    s7_pointer self_s7;

    if (s7_list_length(s7, s7_cdr(args)) < 1)
        return(s7_wrong_number_of_args_error(s7, "ast-node-ref takes 1 or  more arguments: ~S", s7_cdr(args)));

    self_s7 = s7_car(args);
    typ = s7_c_object_type(self_s7);
    if (typ != ast_node_t)
        return(s7_wrong_type_arg_error(s7, "ast-node-ref", 1, self_s7, "a ast_node"));
    self  = (struct node_s *)s7_c_object_value(self_s7);

    if (s7_is_null(s7, s7_cdr(args)))
        return(s7_wrong_type_arg_error(s7, "ast-node-ref", 1, self_s7, "missing ref arg"));
        /* return(s7_make_integer(s7, 32)); */

    s7_pointer params = s7_cdr(args);

    log_debug("get_target");
    /* may return c-objects (node, nodelist) or primitives (s7_integer) */
    /* s7_pointer get_target = sunlark_resolve_path(s7, self_s7, params); */
    /* if (s7_is_c_object(get_target)) { */
    /*     log_debug("get_target tid: %d", sunlark_node_tid(s7, get_target)); */
    /* } */
    /* return get_target; */
}

/** sunlark_node_object_applicator

    (more accurate: object_applicator, not essentially tied to ref)

    registered by s7_c_type_set_ref

    first arg is "self"

    function called when objects of type ast_node are evaluated as
    functions, i.e. when they occur in function position (car of a
    list).

    not to be confused with generic ref of SRFI 123, e.g. (ref vec i),
    which s7 does not support.(?)

    by convention, same as ref_specialized (i.e ast-node-ref) but this
    is not a requirement. could be used for anything, not just
    reference. example: (o :fullname) concats (o :fname) and (o :lname)

    iow, it's a generic generic function, whereas generic ref is just
    a generic ref function. "meta generic?"

    in practice, its a dispatcher. sorta. its job is to inspect the args and
    decide what to do with them.
 */

/* arg1 is the "self" node, arg 2 is first path op */
s7_pointer sunlark_node_object_applicator(s7_scheme *s7, s7_pointer args)
{
#if defined(DEBUG_S7_API)
    log_debug(">>>>>>>>>>>>>>>> sunlark_node_object_applicator <<<<<<<<<<<<<<<<");
    log_debug("\tSELF tid: %d %s",
              sunlark_node_tid(s7, s7_car(args)),
              token_name[sunlark_node_tid(s7, s7_car(args))][0]);
    /* debug_print_s7(s7, "\tAPPLICATOR ARGS: ", s7_cdr(args)); */
    log_debug("\targs: %s",
              s7_object_to_c_string(s7, s7_cdr(args)));
    /* sealark_debug_print_ast_outline(s7_c_object_value(s7_car(args)), 0); */
#endif

    s7_pointer rest = s7_cdr(args);
    /* if (s7_is_null(s7, rest)) */
    /*     //FIXME: err msg depends on data type of car */
    /*     if (sunlark_node_tid(s7, s7_car(args)) == TK_Binding) { */
    /*         return(s7_wrong_type_arg_error(s7, "node applicator", */
    /*                                        1, rest, ":key or :value")); */
    /*     } else { */
    /*         return(s7_wrong_type_arg_error(s7, "node applicator", */
    /*                                        1, rest, "non-nil")); */
    /*     } */

    s7_pointer self_s7 = s7_car(args);
    s7_pointer params = s7_cdr(args);
    s7_pointer resolved_path;

    /* may return c-object node, s7 lists or primitives (s7_integer) */
    /* resolved_path = sunlark_resolve_path(s7, */
    /*                                              self_s7, */
    /*                                              params); */

    resolved_path = sunlark_dispatch(s7, self_s7, params);

    /* if (s7_is_c_object(resolved_path)) { */
    /*     log_debug("resolved path to tid: %d", */
    /*               sunlark_node_tid(s7, resolved_path)); */
    /* } */

#ifdef DEBUG_S7_API
    if (s7_is_c_object(resolved_path)) {
        struct node_s *node = s7_c_object_value(resolved_path);
        log_debug("<<<< sunlark_node_object_applicator, returning %d %s",
                  node->tid, TIDNAME(node));
    } else {
        log_debug("<<<< sunlark_node_object_applicator, return type: %s",
              s7_is_c_object(resolved_path) ? "c-object"
              : s7_is_list(s7, resolved_path) ? "s7 list"
              : "other");
    }
#endif
    return resolved_path;

    /* s7_pointer op = s7_car(rest); */
    /* if (s7_is_keyword(op)) { */
    /*     s7_pointer sym = s7_keyword_to_symbol(s7, op); */
    /*     char *kw = (char*)s7_symbol_name(sym); */
    /*     if (strrchr(kw, '?')) { */
    /*         log_debug("KW PREDICATE %s", kw); */
    /*         if (strncmp(kw, "printable?", 10) == 0) { */
    /*             s7_pointer node = s7_car(args); */
    /*             struct node_s *ast_node = s7_c_object_value(node); */
    /*             if (sealark_node_is_printable(ast_node)) */
    /*                 return s7_t(s7); */
    /*             else return s7_f(s7); */
    /*         } else { */
    /*             return sunlark_predication(s7, kw, args); */
    /*         } */
    /*     } else { */
    /*         s7_pointer s7_tmp = sunlark_node_ref_specialized(s7, args); */
    /*         return s7_tmp; */
    /*     } */
    /* } else { */
    /*     if (s7_is_integer(op)) { */
    /*         log_debug("0 xxxxxxxxxxxxxxxx"); */
    /*         s7_pointer node = s7_car(args); */
    /*         struct node_s *ast_node = s7_c_object_value(node); */
    /*         s7_pointer tmp = sunlark_nodelist_lookup(s7, */
    /*                                                  ast_node->subnodes, */
    /*                                                  op); */
    /*         return tmp; */
    /*         /\* if (s7_is_c_object(tmp)) { *\/ */
    /*         /\*     self = tmp; *\/ */
    /*         /\*     self_tid = sunlark_node_tid(s7, tmp); *\/ */
    /*         /\* } else { *\/ */
    /*         /\*     return tmp; *\/ */
    /*         /\* } *\/ */
    /*    } else { */
    /*         //FIXME: do we have any symbol ops? */
    /*         return(s7_wrong_type_arg_error(s7, "ast-node-ref", */
    /*                                        2, op, "a keyword or symbol")); */
    /*     } */
    /* } */
}

/* **************** */
/** sunlark_node_set_specialized

    registered twice: as a c-type generalize set! (s7_c_type_set_set()) and
    as procedure "ast-node-set!" (s7_define_typed_function())

    generalized set: (set! (c-obj :k) v)

    in this case set! will call the set method registered with the
    c-obj's c-type, passing the c-obj, key :k, and value v.

    note that outside of this set! context, (c-obj :k) will lookup the
    value bound to :k in c-obj (using g_struct_get).
 */
#define SUNLARK_NODE_SET_SPECIALIZED_HELP "(ast-node-set! b i x) sets the ast_node value at index i to x."

/* sig: returns node */
#define SUNLARK_NODE_SET_SPECIALIZED_SIG s7_make_signature(s7, 4, s7_make_symbol(s7, "node?"), s7_make_symbol(s7, "node?"), s7_make_symbol(s7, "integer?"), s7_t(s7))

static s7_pointer _update_ast_node_property(s7_scheme *s7,
                                  struct node_s *ast_node,
                                  s7_pointer key, s7_pointer val)
{
#ifdef DEBUG_TRACE
    log_debug("_update_ast_node_property tid: %d",
              ast_node->tid);
#endif

    if (key == s7_make_keyword(s7, "tid")) {
        if (!s7_is_integer(val))
            return(s7_wrong_type_arg_error(s7, "ast-node-set!",
                                           3, val, "an integer"));
        ast_node->tid = s7_integer(val);
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

/**
   update a starlark binding, i.e. named arg, like deps
   node arg type == :named_arg
   key: tells us what to update, 'name or 'value

   attrib structure:
       :binding
           :id
           :eq
           :list-expr || :string || dict-expr || ...etc
 */
static s7_pointer _update_starlark(s7_scheme *s7,
                                   /* struct node_s *node, */
                                   s7_pointer node_s7,
                                   const char *key,
                                   s7_pointer val)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("_update_starlark %s", key);
#endif

    /* struct node_s *node = s7_c_object_value(s7, node_s7); */
    struct node_s *target;

    /* switch(node->tid) { */
    switch( sunlark_node_tid(s7, node_s7) ) {
    /* case TK_Call_Expr: /\* build rule *\/ */
    /*     break; */
    case TK_Binding: /* rule binding */
        if ( strncmp(key, "name", 4) == 0 ) {
            return sunlark_update_binding_name(s7, node_s7, key, val);
        } else {
            if ( strncmp(key, "value", 5) == 0) {
                return sunlark_update_binding_value(s7, node_s7, key, val);
            } else {
                return(s7_wrong_type_arg_error(s7,
                                               "ast-node-set! attr update",
                                               2, val,
                                               "'name or 'value"));
            }
        }
        break;
    default:
        return(s7_error(s7, s7_make_symbol(s7, "not_yet_supported"),
                  s7_list(s7, 2,
                          s7_make_string(s7, "node type: ~D not yet supported"),
                          sunlark_node_tid(s7, node_s7))));
    }
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

    s7_c_type_set_ref(s7, ast_node_t, sunlark_node_object_applicator);
    s7_c_type_set_set(s7, ast_node_t, sunlark_node_set_generic);
}

/* /section: getters and setters */


/* **************************************************************** */
/* section: c-object construction */

/** sunlark_node_copy

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
static s7_pointer sunlark_node_copy(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_node_copy");
    debug_print_s7(s7, "COPY ARGS: ", args);
    /* debug_print_s7(s7, "COPY ARGS len: ", s7_list_length(s7, args)); */
#endif

    s7_pointer arg1, arg2;
    struct node_s *n1, *n2;

    arg1 = s7_car(args);
    if (s7_c_object_type(arg1) == ast_node_t) {
        /* log_debug("copy ast_node"); */
        n1 = (struct node_s*)s7_c_object_value(arg1);
        if (s7_is_pair(s7_cdr(args))) {
            /* log_debug("copy a b"); */
            arg2 = s7_cadr(args);
            if (s7_is_immutable(arg2))
                return(s7_wrong_type_arg_error(s7, "ast-node-copy!",
                                               0, arg2, "a mutable ast_node"));
            if (s7_c_object_type(arg2) == ast_node_t) {
                /* log_debug("copy ast_node to ast_node"); */
                n2 = (struct node_s*)s7_c_object_value(arg2);
                sealark_node_copy(n1, n2);
                return arg2;
            } else {
                return(s7_wrong_type_arg_error(s7, "ast-node-copy!",
                                               0, arg2, "a mutable ast_node"));
            }
        } else {
            /* log_debug("copy one"); */
            /* only one arg, copy it to new ast_node */
            /* struct node_s *n2 = (struct node_s *) */
            /*     calloc(1, sizeof(struct node_s)); */
            /* n2 = ast_node_init_default(n2); */
            n2 = sealark_node_new();
            sealark_node_copy(n2, n1);
            return s7_make_c_object(s7, ast_node_t,
                                    (void *)n2);
        }
    } else {
        ;
        //FIXME: implement
        /* log_debug("copy non-node"); */
        /* arg1 type != ast_node_t */
    }
}

/*
  sunlark_node_init_from_s7
  initialize a C struct from s7-scheme arg-list (compare
  'ast_node_init', initialize a ast_node from C args)
 */
static s7_pointer sunlark_node_init_from_s7(s7_scheme *s7, struct node_s *cs, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_node_init_from_s7");
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
            cs->tid = s7_integer(arg);
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

/** sunlark_make_ast_node
 */
/* docstring passed to the s7_define_.. used to register the fn in Scheme */
#define SUNLARK_MAKE_AST_NODE_HELP "(make-ast-node) returns a new ast_node with randome data"

#define SUNLARK_MAKE_AST_NODE_FORMAL_PARAMS "(type 0) (line 0) (col 0) (trailing_newline #f) (qtype 0) (s NULL) (comments NULL) (subnodes NULL)"

static s7_pointer sunlark_make_ast_node(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_make_ast_node");
#endif

    /* struct node_s *new_ast_node = (struct node_s *) */
    /*     calloc(1, sizeof(struct node_s)); */
    /* new_ast_node = ast_node_init_default(new_ast_node); */

    struct node_s *n = sealark_node_new();

    if (sunlark_node_init_from_s7(s7, n, args) != NULL) {
        log_debug("OOPS");
    }

    s7_pointer new_ast_node_s7 = s7_make_c_object(s7, ast_node_t,
                                                  (void *)n);

    _register_c_object_methods(s7, new_ast_node_s7);

    return(new_ast_node_s7);
}

EXPORT s7_pointer sunlark_node_new(s7_scheme *s7, struct node_s *node)
{
#if defined(DEBUG_MEM)
    log_debug("sunlark_node_new");
#endif

    //FIXME: maybe use s7_make_c_object_without_gc ???
    // for the original AST, we want without gc
    // but for transient nodes created by expressors,
    // we want gc?

    // then the user would have to explicitly destroy the ast?
    // or: only the root node is GCed?

    // BUT, we do not want copies of AST nodes; all c-object nodes
    // should just point to the AST. So that updates always update
    // the AST.  User must explicitly copy to get a copy.
    s7_pointer new_ast_node_s7 = s7_make_c_object(s7, ast_node_t,
                                                  (void *)node);

    _register_c_object_methods(s7, new_ast_node_s7);

    return new_ast_node_s7;
}
/* /section: c-object construction */

/* **************************************************************** */
/* section: c-object destruction */
static s7_pointer sunlark_destroy_ast_node(s7_scheme *s7, s7_pointer obj)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_destroy_ast_node");
#endif
    struct node_s *cs = (struct node_s*)s7_c_object_value(obj);
    sealark_node_free(cs);
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
static void _register_c_object_methods(s7_scheme *s7, s7_pointer ast_node)
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

static void _register_c_type_methods(s7_scheme *s7, s7_int ast_node_t)
{
#ifdef DEBUG_TRACE
    log_debug("_register_c_type_methods");
#endif
    s7_c_type_set_gc_free(s7, ast_node_t, sunlark_destroy_ast_node);
    s7_c_type_set_gc_mark(s7, ast_node_t, sunlark_node_gc_mark);

    /* s7_c_type_set_equal(s7, ast_node_t, _ast_nodes_are_c_eql); */
    s7_c_type_set_is_equal(s7, ast_node_t, sunlark_nodes_are_equal);
    s7_c_type_set_is_equivalent(s7, ast_node_t, sunlark_nodes_are_equivalent);

    s7_c_type_set_copy(s7, ast_node_t, sunlark_node_copy);

    /* nodes are not sequences - nodelists are */
    s7_c_type_set_length(s7, ast_node_t, sunlark_node_subnode_count);
    /* s7_c_type_set_reverse(s7, ast_node_t, sunlark_node_reverse); */
    /* s7_c_type_set_fill(s7, ast_node_t, sunlark_node_fill); */

    s7_c_type_set_to_string(s7, ast_node_t, sunlark_node_to_string);

    s7_c_type_set_to_list(s7, ast_node_t, sunlark_node_to_list);
}

/* /section: extension methods */


/* **************************************************************** */
/* section: c-type configuration */

static void _register_ast_node_fns(s7_scheme *s7)
{
#ifdef DEBUG_TRACE
    log_debug("_register_ast_node_fns");
#endif
    /* s7_define_safe_function(s7, "ast-node", g_to_ast_node, 0, 0, true, sunlark_node_help); */
    s7_define_safe_function_star(s7, "make-ast-node",
                                 sunlark_make_ast_node,
                                 SUNLARK_MAKE_AST_NODE_FORMAL_PARAMS,
                                 SUNLARK_MAKE_AST_NODE_HELP);

    s7_define_typed_function(s7, "sunlark-node?", sunlark_is_node,
                             1, 0, false,
                             SUNLARK_IS_NODE_HELP,
                             SUNLARK_IS_NODE_SIG);

    /* specialized get/set! */
    s7_define_typed_function(s7, "ast-node-ref",
                             sunlark_node_ref_specialized,
                             2, 1, true,
                             SUNLARK_NODE_REF_SPECIALIZED_HELP,
                             SUNLARK_NODE_REF_SPECIALIZED_SIG);

    s7_define_typed_function(s7, "ast-node-set!",
                             sunlark_node_set_specialized,
                             3, 0, true,
                             SUNLARK_NODE_SET_SPECIALIZED_HELP,
                             SUNLARK_NODE_SET_SPECIALIZED_SIG);

    s7_define_safe_function(s7, "sunlark->starlark",
                            sunlark_to_starlark,
                            1, 1, false,
                            SUNLARK_TO_STARLARK_HELP);
    
    // ast_node-let => s7_c_object_let, a let for the instance not the type
    /* s7_define_safe_function(s7, "ast-node-let", */
    /*                         sunlark_node_let, */
    /*                         1, 0, false, */
    /*                         sunlark_node_let_help); */

    /* parsing */
    s7_define_safe_function(s7,
                            "sunlark-parse-build-file",
                            sunlark_parse_build_file,
                            1, 0, false,
                            SUNLARK_PARSE_BUILD_FILE_HELP);

    s7_define_safe_function(s7,
                            "sunlark-parse-bzl-file",
                            sunlark_parse_bzl_file,
                            1, 0, false,
                            SUNLARK_PARSE_BZL_FILE_HELP);

    s7_define_safe_function(s7,
                            "sunlark-parse-string",
                            sunlark_parse_string,
                            1, 0, false,
                            SUNLARK_PARSE_STRING_HELP);


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
static s7_pointer sunlark_node_gc_mark(s7_scheme *s7, s7_pointer p)
{
    /* nothing to mark because we protect sunlark_node_methods_let, and all ast_node objects get the same let */
    return(p);
}

/* static s7_pointer sunlark_node_gc_free(s7_scheme *s7, s7_pointer obj) */
/* { */
/* #ifdef DEBUG_TRACE */
/*     log_debug("sunlark_node_gc_free"); */
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
    log_debug("%s", label);
    log_debug("\t%s", s7_object_to_c_string(s7, obj));
    /* s7_display(s7, obj, p); */
    /* s7_newline(s7, p); */
}

/* /section: debugging */
