#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"

#include "s7.h"

#include "ast_node_s7.h"

/* exported s7 c-types */
s7_int ast_node_t = 0;

static s7_pointer sunlark_node_methods_let;

/* forward decls */
/* section: identity */
s7_pointer sunlark_is_ast_node(s7_scheme *s7, s7_pointer args);

/* section: equality */
static bool _ast_nodes_are_value_equal(struct node_s *val1,
                                      struct node_s *val2);
static s7_pointer sunlark_nodes_are_equal(s7_scheme *s7, s7_pointer args);
static s7_pointer sunlark_nodes_are_equivalent(s7_scheme *s7, s7_pointer args);

/* section: getters and setters */
static s7_pointer sunlark_node_ref_specialized(s7_scheme *s7, s7_pointer args);
static s7_pointer sunlark_node_set_specialized(s7_scheme *s7, s7_pointer args);

static s7_pointer sunlark_node_object_applicator(s7_scheme *s7, s7_pointer args);

static void _register_get_and_set(s7_scheme *s7);

/* section: serialization */
/* we use a global buffer for serialization to avoid local allocs */
#if INTERFACE
#define SZDISPLAY_BUF (4096 * 4)
#endif
//FIXME: use UT_string
char *display_buf;
int   display_bufsz;
char *display_ptr;

char *sunlark_node_display(s7_scheme *s7, void *value);
char *sunlark_node_display_readably(s7_scheme *s7, void *value);
static s7_pointer sunlark_node_to_string(s7_scheme *s7, s7_pointer args);
static s7_pointer sunlark_node_to_starlark(s7_scheme *s7, s7_pointer args);

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

/* **************************************************************** */
/* section: identity */
/* public predicate returns Scheme boolean: sunlark_is_node */
/* internal returns C bool: is_sunlark_node */
#define SUNLARK_IS_AST_NODE_HELP "(ast-node? obj) returns #t if obj is an ast-node."
#define SUNLARK_IS_AST_NODE_SIG s7_make_signature(s7, 2, s7_make_symbol(s7, "boolean?"), s7_t(s7))

/* called by Scheme 'node?'; internally, use c_is_sunlark_node */
s7_pointer sunlark_is_ast_node(s7_scheme *s7, s7_pointer node_s7)
{
/* #ifdef DEBUG_TRACE */
/*     log_debug("sunlark_is_ast_node"); */
/* #endif */

    bool result = c_is_sunlark_node(s7, node_s7);
    return s7_make_boolean(s7, result);

}

/* for internal C use; Scheme 'node?' calls sunlark_is_ast_node */
bool c_is_sunlark_node(s7_scheme *s7, s7_pointer node_s7)
{
/* #ifdef DEBUG_TRACE */
/*     log_debug("c_is_sunlark_node"); */
/* #endif */

    if (s7_is_c_object(node_s7)) {
        bool eq = s7_c_object_type(node_s7) == ast_node_t;
        return eq;
    } else {
        if (s7_is_list(s7, node_s7)) {
            return s7_c_object_type(s7_car(node_s7)) == ast_node_t;
        } else {
            return false;
        }
    }
}

#define NODE_TID (NODE_S7) ((s7_c_object_value(s7, NODE_S7)->tid;))

int sunlark_node_tid(s7_scheme *s7, s7_pointer node_s7)
{
#ifdef DEBUG_TRACE
    /* log_debug("sunlark_node_tid %s", s7_object_to_c_string(s7, node_s7)); */
#endif

    if (s7_is_c_object(node_s7)) {
        if (c_is_sunlark_node(s7, node_s7)) {
            struct node_s *n = s7_c_object_value(node_s7);
            return n->tid;
        } else {
            if (c_is_sunlark_nodelist(s7, node_s7)) {
                return TK_Node_List;
            } else {
                return TK_Unspecified;
            }
        }
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

/* **************************************************************** */
/* section: getters and setters */
/* get and set are special, since there are two ways to do each:
   * 1. generic get and set! (c-type methods)
   * 2. specialized ast-node-get and ast-node-set! (Scheme procedures)
 */

/* **************** */
/* helper fn */
s7_pointer sunlark_predication(s7_scheme *s7, char *kw, struct node_s *self)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_predication: %s", kw);
#endif

    char buf[128];
    strncpy(buf, kw, strlen(kw) + 1);

    buf[strlen(buf) - 1] = '\0'; /* remove final ?  */

    int tokid = sealark_kw_to_tid(buf);
    if (tokid < 0) {
        /* log_error("predicate not found: :%s?", buf); */
        /* if (strchr(buf, '_')) { */
        /*     int len = strlen(buf); */
        /*     for (int i = 0; i < len; i++) if (buf[i] == '_') buf[i] = '-'; */
        /*     log_error("did you mean: :%s?", buf); */
        /* } */
        return s7_f(s7);
    }
    /* s7_pointer obj = s7_car(args); */
    /* struct node_s *n = s7_c_object_value(obj); */
    log_debug("self tid: %d, tokid %d", self->tid, tokid);
    return s7_make_boolean(s7, self->tid == tokid);
}
/* **************** */
/** sunlark_node_ref_specialized

    (ast-node-ref obj key)
    takes two args, a ast_node object and a keyword to look up in the object.
 */
#define SUNLARK_NODE_REF_SPECIALIZED_HELP "(ast-node-ref nd k) returns the value for property k (a keyword) of ast-node nd."
#define SUNLARK_NODE_REF_SPECIALIZED_SIG s7_make_signature(s7, 3, s7_t(s7), s7_make_symbol(s7, "ast-node?"), s7_make_symbol(s7, "integer?"))

/** sunlark_node_ref_specialized

    Looks up node properties, whose names are keywords. Each field in
    the node_s struct has a property whose name is formed by prefixing
    a colon: :tid, :line, :col, :trailing_newline, :qtype, :s,
    :comments, :subnodes.

    In addition the following pseudo-properties are supported:
        :print - returns string for printable nodes, with correct quoting.

        :@<attr> - only for nodes of type :call_expr. returns attribute
        (i.e. :arg_named node) whose :id is <attr>. E.g. (rulenode :deps)
        would return the 'deps' attribute of the rulenode.

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
    s7_pointer get_target = sunlark_resolve_path(s7, self_s7, params);
    if (s7_is_c_object(get_target)) {
        log_debug("get_target tid: %d", sunlark_node_tid(s7, get_target));
    }
    return get_target;
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
static s7_pointer sunlark_node_object_applicator(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_node_object_applicator");
    log_debug("NODE APPLICATOR SELF: %d %s",
              sunlark_node_tid(s7, s7_car(args)),
              token_name[sunlark_node_tid(s7, s7_car(args))][0]);
    debug_print_s7(s7, "APPLICATOR ARGS: ", s7_cdr(args));
#endif

    /* no need to check arg1, it's the "self" ast_node obj */
    /* s7_pointer g  = (struct node_s *)s7_c_object_value(obj); */

    s7_pointer rest = s7_cdr(args);
    if (s7_is_null(s7, rest))
        return(s7_wrong_type_arg_error(s7, "ast-node-fn",
                                       1, rest, "missing keyword arg"));

    s7_pointer self_s7 = s7_car(args);
    s7_pointer params = s7_cdr(args);

    /* log_debug("get_target, params: %s", s7_object_to_c_string(s7, params)); */
    /* log_debug("nl tid: %d", sunlark_node_tid(s7, self_s7)); */

    /* may return c-objects (node, nodelist) or primitives (s7_integer) */
    s7_pointer get_target = sunlark_resolve_path(s7,
                                                 self_s7,
                                                 params);
    /* if (s7_is_c_object(get_target)) { */
    /*     log_debug("got target tid: %d", sunlark_node_tid(s7, get_target)); */
    /* } */
    return get_target;

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

s7_pointer sunlark_resolve_path(s7_scheme *s7,
                                s7_pointer self_s7,
                                s7_pointer path_args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_resolve_path: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    /* struct node_s *self =  (struct node_s *)s7_c_object_value(self_s7); */
    s7_pointer self = self_s7;
    s7_pointer tmp;

    /* problem: three kinds of value: node, nodelist, scalar */
    int self_tid = sunlark_node_tid(s7, self);

    s7_pointer prev_path_arg = s7_car(path_args);

    s7_pointer path_arg;
    const char *prop;
    int loop_idx = 0;
    while ( !s7_is_null(s7, path_args) ) {
        /* log_debug("LOOP (resolve) %d: %s", loop_idx++, */
        /*           s7_object_to_c_string(s7, path_args)); */
        path_arg = s7_car(path_args);
        /* log_debug("path_arg: %s", s7_object_to_c_string(s7, path_arg)); */

        /* log_debug("self_tid: %d %s", */
        /*               self_tid, */
        /*               token_name[self_tid][0]); */

        if (s7_is_keyword(path_arg) || s7_is_integer(path_arg)) {
            if (s7_is_keyword(path_arg)) {
                s7_pointer sym = s7_keyword_to_symbol(s7, path_arg);
                prop = s7_symbol_name(sym);
                /* log_debug("prop: %s", prop); */
            }

            /* scalar-valued props (e.g. :tid) must come last */
            switch( self_tid ) {

            case TK_Build_File: /* build_target */
                /* e.g.(set! (bfnode :targets ::tgt :deps :name) "foo") */
                /* :targets, :loads, :package */
                tmp = sunlark_build_file_property_lookup(s7,
                                                         s7_c_object_value(self),
                                                         path_arg);
                if (s7_is_c_object(tmp)) {
                    self = tmp;
                    self_tid = sunlark_node_tid(s7, tmp);
                } else {
                    return tmp;
                }
                break;

            case TK_Node_List:
                /* int indexing */
                if (s7_is_integer(path_arg)) {
                    tmp = sunlark_nodelist_lookup(s7,
                                                  s7_c_object_value(self),
                                                  path_arg);
                    if (s7_is_c_object(tmp)) {
                        self = tmp;
                        self_tid = sunlark_node_tid(s7, tmp);
                    } else {
                        return tmp;
                    }
                }
                break;
            /* case TK_Load_Stmt: */
            /*     /\* :build-file > :stmt-list > smallstmt-list > :load-stmt *\/ */
            /*     break; */
            /* case Package - same as :call-expr */
            case TK_Call_Expr: /* build_target */
                /* e.g. (set! (target :attrs :deps :name) "foo") */
                /* :rule, :attrs */
                tmp = sunlark_target_property_lookup(s7,
                                                     s7_c_object_value(self),
                                                     path_arg);
                if (s7_is_c_object(tmp)) {
                    self = tmp;
                    self_tid = sunlark_node_tid(s7, tmp);
                } else {
                    return tmp;
                }
                break;
            case TK_Arg_List: /* rename :attr-list? */
                /* e.g. (set! (args :deps :name) "foo") */
                /* :<attrname>, :n (nth arg) */
                tmp = sunlark_attr_list_kw_lookup(s7,
                                                  s7_c_object_value(self),
                                                  path_arg);
                if (s7_is_c_object(tmp)) {
                    self = tmp;
                    self_tid = sunlark_node_tid(s7, tmp);
                } else {
                    return tmp;
                }
                break;
            case TK_Arg_Named: /* rule attribute */
                /* e.g. (set! (attr :name) "foo") */
                /* :name, :value */
                tmp = sunlark_attribute_property_lookup(s7,
                                                        s7_c_object_value(self),
                                                        prop);
                if (s7_is_c_object(tmp)) {
                    self = tmp;
                    self_tid = sunlark_node_tid(s7, tmp);
                } else {
                    return tmp;
                }
                break;
            default:
                /* usually :subnodes */
                /* log_warn("catch-all"); */
                if (s7_is_integer(path_arg)) {
                    struct node_s *n = s7_c_object_value(self);
                    if (n->subnodes) {
                        self = sunlark_nodelist_lookup(s7, n->subnodes,
                                                       path_arg);
                        self_tid = sunlark_node_tid(s7, self);
                        /* log_debug("0 xxxxxxxxxxxxxxxx %d %s", */
                        /*           self_tid, token_name[self_tid][0]); */
                    } else {
                        // error? unspecified?
                        return s7_unspecified(s7);
                    }
                } else {
                    tmp = sunlark_common_property_lookup(s7,
                                         s7_c_object_value(self),
                                                         path_arg);
                    if (s7_is_c_object(tmp)) {
                        self = tmp;
                        self_tid = sunlark_node_tid(s7, tmp);
                    } else {
                        return tmp;
                    }
                }
            }
        } else {
            if (s7_is_symbol(path_arg)) {
                /* log_debug("SYMBOL path step: %s", */
                /*           s7_object_to_c_string(s7, path_arg)); */
                /* log_debug("prev path step: %s", */
                /*           s7_object_to_c_string(s7, prev_path_arg)); */

                /* ok: symbol after :target, :attrs */
                /* not ok: symbol after :load */
                if (prev_path_arg == s7_make_keyword(s7, "load")) {
                    /* log_error("ERROR: path step %s may not be followed by symbol; found: '%s", */
                    /*           s7_object_to_c_string(s7, prev_path_arg), */
                    /*           s7_object_to_c_string(s7, path_arg)); */

                    return s7_error(s7, s7_make_symbol(s7,
                                                       "invalid_argument"),
                            s7_list(s7, 3, s7_make_string(s7,
                "ERROR: path step ~A may not be followed by symbol; found: '~A"),
                                    prev_path_arg, path_arg));

                }
                if (prev_path_arg == kw_target) {
                    /* symbols match starlark Id productions */
                    /* e.g. rule and attr names */
                    self = sunlark_get_target_by_rule_name(s7,
                                                           path_arg,
                                                           self);
                    self_tid = sunlark_node_tid(s7, self);
                }
                if (prev_path_arg == kw_attrs) {
                    /* symbols match starlark Id productions */
                    /* e.g. rule and attr names */
                    self = sunlark_get_attr_by_name(s7,
                                                    path_arg,
                                                    self);
                    self_tid = sunlark_node_tid(s7, self);
                }

            } else {
                if (s7_is_string(path_arg)) {
                    /* log_debug("STRING path step: %s", */
                    /*           s7_object_to_c_string(s7, path_arg)); */
                    /* :loads "foo" - get load("foo"...) node */
                    /* :targets "bar" - selects target :bar */
                    /* :attrs "baz" - selects "baz" attr */
                    /* better: special syntax? ::foo, ::bar? */
                    self = sunlark_handle_string_query_arg(s7,
                                                           s7_string(path_arg),
                                                           self);
                    self_tid = sunlark_node_tid(s7, self);
                } else {
                    if (s7_is_procedure(path_arg)) {
                        /* log_debug("running path function..."); */
                        s7_pointer args = s7_cons(s7,
                                                  self,
                                                  //s7_make_integer(s7, 2),
                                                  s7_nil(s7));
                        s7_call(s7, path_arg, args);
                    } else {
                        return(s7_wrong_type_arg_error(s7, "AST-node-ref",
                                                       2,
                                                       path_arg,
                                                       "a keyword"));
                    }
                }
            }
        }
        path_args  = s7_cdr(path_args);
        prev_path_arg = path_arg;
    }
    /* log_debug("RESOLVED! xxxxxxxxxxxxxxxx"); */
    return self;
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

#define SUNLARK_NODE_SET_SPECIALIZED_SIG s7_make_signature(s7, 4, s7_make_symbol(s7, "float?"), s7_make_symbol(s7, "ast-node?"), s7_make_symbol(s7, "integer?"), s7_make_symbol(s7, "float?"))

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
   update a starlark attribute, i.e. named arg, like deps
   node arg type == :named_arg
   key: tells us what to update, 'name or 'value

   attrib structure:
       :arg-named
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
    case TK_Arg_Named: /* rule attribute */
        if ( strncmp(key, "name", 4) == 0 ) {
            return sunlark_update_attribute_name(s7, node_s7, key, val);
        } else {
            if ( strncmp(key, "value", 5) == 0) {
                return sunlark_update_attribute_value(s7, node_s7, key, val);
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

/* sunlark_node_set_specialized

   implements (ast_node_set! ...)

   what about (set! (node :subnodes) nodelist)? we need ctype for nodelist
 */
static s7_pointer sunlark_node_set_specialized(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_node_set_specialized");
#endif
    struct node_s *node;
    s7_int typ;
    s7_pointer self, key;

    /* log_debug("set_specialized args (excluding self): %s", */
    /*           s7_object_to_c_string(s7, s7_cdr(args))); */

    /* last arg is new value to set? */

    /* set! methods/procedures need to check that they have
       been passed the correct number of arguments: 3 */

    /* if (s7_list_length(s7, args) != 3) */
    /*     return(s7_wrong_number_of_args_error(s7, "ast-node-set! takes 3 arguments: ~~S", args)); */

    self = s7_car(args);
    /* qua procedure, check type of first arg */
    typ = s7_c_object_type(self);
    if (typ != ast_node_t)
        return(s7_wrong_type_arg_error(s7, "ast-node-set!", 1, self, "a ast_node"));

    if (s7_is_immutable(self))
        return(s7_wrong_type_arg_error(s7, "ast-node-set!", 1, self, "a mutable ast_node"));

    s7_pointer params = s7_reverse(s7, s7_cdr(args));
    s7_pointer update_val = s7_car(params);
    params = s7_reverse(s7, s7_cdr(params));

    s7_pointer set_target = sunlark_resolve_path(s7, self, params);

    /* log_debug("set_target: %s", s7_object_to_c_string(s7, set_target)); */
    /* log_debug("update_val: %s", s7_object_to_c_string(s7, update_val)); */

    // now update set_target

    /* return sunlark_update_attribute_name(s7, node_s7, key, val); */
    /* return sunlark_update_attribute_value(s7, node_s7, key, val); */

    /* _update_ast_node_property(s7, node, key, s7_caddr(args)); */

    /* _update_starlark(s7, self, s7_symbol_name(key), s7_caddr(args)); */

    //FIXME: r7rs says result of set! is unspecified. does that mean
    //implementation-specified?
    return s7_unspecified(s7);

    /* return sunlark_node_new(set_target); */
}

static s7_pointer sunlark_node_set_generic(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_node_set_generic");
    /* debug_print_s7(s7, "set_generic spec: ", args); */
#endif
    return sunlark_node_set_specialized(s7, args);
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
/* section: serialization */
//FIXME: handle large files. use dynamic alloc
char *sunlark_node_display(s7_scheme *s7, void *value)
{
#ifdef DEBUG_TRACE
    /* log_debug("sunlark_node_display"); */
#endif

    struct node_s *nd = (struct node_s *)value;

    // check display_buf size, expand if needed

    char buf[128];
    int len;

    sprintf(buf, "#ast_node<\n");
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " tid  = %d,\n", nd->tid);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " tnm  = %s,\n", token_name[nd->tid][0]);
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

    if (nd->tid == TK_STRING) {
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
        sunlark_nodelist_display(s7, (UT_array*)nd->comments);

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
        sunlark_nodelist_display(s7, (UT_array*)nd->subnodes);

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

/** sunlark_node_display_readably

    produces a "roundtrippable" string, one that when read by the reader
    results in an object equal to the original.
 */
char *sunlark_node_display_readably(s7_scheme *s7, void *value)
{
/* #ifdef DEBUG_TRACE */
/*     log_debug("sunlark_node_display_readably"); */
/* #endif */

    struct node_s *nd = (struct node_s *)value;

    // check display_buf size, expand if needed

    char buf[128];
    int len;

    sprintf(display_ptr++, "(");

    sprintf(buf, "(tid %d) ;; %s\n", nd->tid, token_name[nd->tid][0]);
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

    if (nd->tid == TK_STRING) {
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
        sunlark_nodelist_display_readably(s7, (UT_array*)nd->comments);

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
        sunlark_nodelist_display_readably(s7, (UT_array*)nd->subnodes);

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

static s7_pointer sunlark_node_to_string(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    /* log_debug("sunlark_node_to_string"); */
    /* debug_print_s7(s7, "to_string cdr: ", s7_cdr(args)); */
#endif
    display_buf = calloc(1, SZDISPLAY_BUF);
    if (display_buf == NULL) {
        log_error("ERROR on calloc");
        //FIXME cleanup
        exit(EXIT_FAILURE);
    } else {
        display_bufsz = SZDISPLAY_BUF;
    }
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
        descr = sunlark_node_display_readably(s7, s7_c_object_value(obj));
    }
    else descr = sunlark_node_display(s7, s7_c_object_value(obj));

    /* log_debug("TO_STRING LEN: %d", strlen(descr)); */
    obj = s7_make_string(s7, descr);

    free(descr); // frees display_buf?
    display_bufsz = 0;
    return(obj);
}

#define SUNLARK_NODE_TO_STARLARK_HELP "(ast-node->starlark ast_node)"

static s7_pointer sunlark_node_to_starlark(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_node_to_starlark");
#endif

    s7_pointer node;
    UT_string *buf;
    utstring_new(buf);

    node = s7_car(args);
    struct node_s *ast_node = s7_c_object_value(node);
    starlark_node2string(ast_node, buf);
    return s7_make_string(s7, utstring_body(buf));
}

/* /section: serialization */

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
#ifdef DEBUG_TRACE
    log_debug("sunlark_node_new");
#endif

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
    "'signature (lambda (p) (list '#t 'ast-node? 'integer?)) " \
    "'type ast-node? " \
    "'foo (lambda (self) \"hello from foo method!\") " \
    "'memq (lambda (self arg) \"hello from perverse memq method!\") " \
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
    /* s7_c_type_set_length(s7, ast_node_t, sunlark_node_length); */
    /* s7_c_type_set_reverse(s7, ast_node_t, sunlark_node_reverse); */
    /* s7_c_type_set_fill(s7, ast_node_t, sunlark_node_fill); */

    s7_c_type_set_to_string(s7, ast_node_t, sunlark_node_to_string);

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

    s7_define_typed_function(s7, "ast-node?", sunlark_is_ast_node,
                             1, 0, false,
                             SUNLARK_IS_AST_NODE_HELP,
                             SUNLARK_IS_AST_NODE_SIG);

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

    s7_define_safe_function(s7, "ast-node->starlark",
                            sunlark_node_to_starlark,
                            1, 0, false,
                            SUNLARK_NODE_TO_STARLARK_HELP);

    // ast_node-let => s7_c_object_let, a let for the instance not the type
    /* s7_define_safe_function(s7, "ast-node-let", */
    /*                         sunlark_node_let, */
    /*                         1, 0, false, */
    /*                         sunlark_node_let_help); */

    /* parsing */
    s7_define_safe_function(s7,
                            "parse-build-file",
                            sunlark_parse_build_file,
                            1, 0, false,
                            SUNLARK_PARSE_BUILD_FILE_HELP);

    s7_define_safe_function(s7,
                            "parse-bzl-file",
                            sunlark_parse_bzl_file,
                            1, 0, false,
                            SUNLARK_PARSE_BZL_FILE_HELP);

    s7_define_safe_function(s7,
                            "parse-string",
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
    /* log_debug("debug_print_s7: "); */
    /* s7_pointer p = s7_current_output_port(s7); */
    /* s7_display(s7, s7_make_string(s7, label), p); */
    log_debug("%s", label);
    log_debug("\t%s", s7_object_to_c_string(s7, obj));
    /* s7_display(s7, obj, p); */
    /* s7_newline(s7, p); */
}

/* /section: debugging */
