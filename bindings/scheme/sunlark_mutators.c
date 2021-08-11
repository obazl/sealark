#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_mutators.h"

/* called for (set! (sunlark-node ...) val) */
s7_pointer sunlark_node_set_generic(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug(">>>>>>>>>>>>>>>> sunlark_node_set_generic <<<<<<<<<<<<<<<<");
    struct node_s *self = s7_c_object_value(s7_car(args));
    log_debug("self: %d %s", self->tid, TIDNAME(self));
    /* log_debug("args: %s", s7_object_to_c_string(s7, s7_cdr(args))); */
    /* debug_print_s7(s7, "args", args); */
#endif

    s7_pointer msg = s7_cdr(args);
    if (s7_list_length(s7, msg) == 2) {
        s7_pointer op  = s7_cdr(msg);
        if (op == KW(>>)) {
        } else {

        }
    }
    return sunlark_set_bang(s7, args);
}

/* sunlark_node_set_specialized

   implements (ast_node_set! sunlark-node key val)

 */
/* **************************************************************** */
/** sunlark_node_set_specialized

    registered twice: as a c-type generalize set! (s7_c_type_set_set()) and
    as procedure "ast-node-set!" (s7_define_typed_function())

    generalized set: (set! (c-obj :k) v)

    in this case set! will call the set method registered with the
    c-obj's c-type, passing the c-obj, key :k, and value v.

    note that outside of this set! context, (c-obj :k) will lookup the
    value bound to :k in c-obj (using g_struct_get).
 */
#if INTERFACE
#define SUNLARK_NODE_SET_SPECIALIZED_HELP "(ast-node-set! b i x) sets the ast_node value at index i to x."

/* sig: returns node */
#define SUNLARK_NODE_SET_SPECIALIZED_SIG s7_make_signature(s7, 4, s7_make_symbol(s7, "node?"), s7_make_symbol(s7, "node?"), s7_make_symbol(s7, "integer?"), s7_t(s7))
#endif

s7_pointer sunlark_node_set_specialized(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug(">>>>>>>>>>>>>>>> sunlark_node_set_specialized <<<<<<<<<<<<<<<<");
#endif

    return sunlark_set_bang(s7, args);
}

/* **************************************************************** */
s7_pointer sunlark_set_bang(s7_scheme *s7, s7_pointer args)
{
#if defined(DEBUG_TRACE)
    log_debug(">> sunlark_set_bang");
    /* log_debug("\t(car args): %s", s7_object_to_c_string(s7, s7_car(args))); */
    /* log_debug("\tcdr args: %s", s7_object_to_c_string(s7, s7_cdr(args))); */
#endif

    s7_pointer self = s7_car(args);
    struct node_s *self_node = s7_c_object_value(self);

    /* s7_pointer last_arg = s7_car(s7_reverse(s7,s7_cdr(args))); */
    /* s7_pointer ev = s7_eval(s7, last_arg, s7_nil(s7)); */
    /* log_debug("EV: %s", s7_object_to_c_string(s7, ev)); */

#ifdef DEBUG_AST
    /* sealark_debug_log_ast_outline(self_node, 0); */
    /* sealark_debug_print_node_starlark(self_node, true); */
    /* sunlark_debug_print_node(s7, self); */
#endif

    /*
      set! path expression (the get part): the subpath up to but not
      including the penultimate step resolves to the _context_. The
      penultimate element identifies the lvalue in the context. The
      ultimate element is the newval. examples:

      1. (set! (ast :> 1 :@ 'srcs) newval) ;; replaces entire srcs attrib
      2. (set! (ast :> 1 :@ 'srcs :key) "foo") ;; replaces key node
      3. (set! (ast :> 1 :@ 'srcs :value 1 "bar") ;; replaces second item in srcs string list

      The newval must have the correct type. In example 2 it must be a
      TK_ID node to replace the :key node; in example 1 it must be a
      complete TK_Binding node. However, in some cases Sunlark can
      infer the node. In example 2, newval could be a string, since
      the :key node is always a TK_STRING, so Sunlark can just replace
      the string field.

      obsolete (we do not need :$ for set?):

      if last elt of ref list is $, e.g. (set! (b :value 1 :$) "foo"),
      then we cannot use the ref applicator to turn it into an lval,
      since it may return a string, int, etc. IOW, :$ behaves
      differently in the ref and set! contexts. So in the set!
      context, we remove :$ from the path, which we then resolve as
      for ref, since we know that :$ denotes a property of the
      resolved expression, usually (always?) the 's' string prop. So
      the path expression without the final :$ will produce the
      desired lval (a node, not a field in a node struct).

      same goes for all struct node_s field properties? e.g. :line, :col,
      :qtype, etc. e.g. for (set! (n foo bar baz :line) 27), :line
      will be a field in the lval expressed by the path (n foo bar baz).
     */

    /* assumption: path has at least 2 elements? */
    s7_pointer get_path = s7_reverse(s7, s7_cdr(args));
    s7_pointer update_val = s7_car(get_path);
    s7_pointer selector;
    if (s7_is_null(s7, s7_cdr(get_path))) {
        selector = s7_nil(s7);
        get_path = s7_nil(s7);
    } else {
        selector = s7_cadr(get_path);
        get_path = s7_reverse(s7, s7_cddr(get_path));
    }

#if defined (DEBUG_SET)
    /* log_debug("get_path: %s", s7_object_to_c_string(s7, get_path)); */

    /* bool dollar = false; // FIXME: name */
    /* if (s7_make_keyword(s7,"$") == s7_car(get_path)) { */
    /*     dollar = true; */
    /*     get_path = s7_cdr(get_path); */
    /* } */

    log_debug("set! self: %d %s; get_path: %s; selector: %s; new val: %s",
              sunlark_node_tid(s7, self),
              token_name[sunlark_node_tid(s7, self)][0],
              s7_object_to_c_string(s7, get_path),
              s7_object_to_c_string(s7, selector),
              s7_object_to_c_string(s7, update_val));
#endif

    struct node_s *node;
    s7_int typ;
    s7_pointer key;

    /* qa procedure, check type of first arg */
    typ = s7_c_object_type(self);
    if (typ != ast_node_t)
        return(s7_wrong_type_arg_error(s7, "ast-node-set!", 1, self, "a ast_node"));

    if (s7_is_immutable(self))
        return(s7_wrong_type_arg_error(s7, "ast-node-set!", 1, self, "a mutable ast_node"));

    struct node_s *result;

    /* getter (pkg :> sel): get_path :>, sel: sel */
    if (s7_list_length(s7, get_path) < 2) {
        if (self_node->tid == TK_Package) {
            if (update_val == KW(null)) {
                /* case: (set! (pkg :>>) :null) - rm all targets */
                /* case: (set! (pkg :> :*) :null) - rm all targets */
                /* case: (set! (pkg :> "hello-lib") :null) - rm target */
                /* case: (set! (pkg :> :0) :null) - rm target */
                /* ditto for :load, :loads */
                if (s7_is_null(s7, selector)) {
                    log_error("Getter must include a selector");
                    return handle_errno(s7, EMISSING_GET_SELECTOR, s7_nil(s7));
                    return NULL;
                }
                return sunlark_remove(s7, self, get_path, selector);
            }
            /* case: (set! (pkg :> :0) #>(newtarg ...f)) - replace target */
            if (s7_car(get_path) == KW(>)) {
                result = sunlark_pkg_target_mutate(s7, self_node,
                                                    selector,
                                                    update_val);
                /* sealark_debug_log_ast_outline(result, 0); */
                if (result)
                    return sunlark_new_node(s7, result);
                else
                    return handle_errno(s7, ENOT_IMPLEMENTED, s7_cdr(args));
            }
        }
    }

    /* resolve context */


    /* getter (pkg :> tsel :@ asel ...): len(get_path) > 1 */

    /* if self == TK_Call_Expr (i.e. :target) */
    /* let (t (pkg :> "hello-lib")) */
    /* getter (t): get_path (), sel () */
    /* case: (set! (t) :null) - illegal */
    /* case: (set! (t :@) :null) */

    s7_pointer context = sunlark_node_object_applicator(s7,
                                                        s7_cons(s7,
                                                                self,
                                                                get_path));
    struct node_s *context_node = s7_c_object_value(context);
    /* log_debug("object_applicator returned context node: %d %s", */
    /*           context_node->tid, TIDNAME(context_node)); */

#if defined(DEBUG_SET)
    log_debug("getter self: %d %s", self_node->tid, TIDNAME(self_node));
    /* log_debug("RESOLVED context:"); */
    /* sunlark_debug_print_node(s7, context_node); */
    /* sealark_debug_print_node_starlark(context_node, true); // crush */
    /* sealark_debug_log_ast_outline(context_node, true); // crush */
#endif

    /* switch(self_node->tid) { */
    switch(context_node->tid) {
    case TK_STRING:
#if defined(DEBUG_SET)
        log_debug("case set! on context TK_STRING");
#endif
        /* NB: in this case selector should be '() - nothing to select */
        if ( s7_is_null(s7, selector) ) { //FIXME
            if (s7_is_c_object(update_val)) {
                struct node_s *newitem
                    = sealark_set_string_c_object(context_node,
                                  s7_c_object_value(update_val));
                return sunlark_new_node(s7,newitem);
            } else {
                struct node_s *newitem
                    = sealark_set_string(context_node, 0,
                                         s7_string(update_val));
                return self;
            }
        } else {
            /* should not happen, no futher selection possible */
            log_error("wtf????????????????");
        }
        break;
    case TK_Arg_List:           /* bindings */
        /* (set! (pkg :> "string-vectors" :@ 0) :null) */
        /* (set! (pkg :> "string-vectors" :@ 0) "foo") */
        /* (set! (pkg :> "string-vectors" :@ 'string_vecb) "foo") */
        /* context: bindings list; selector: index */
#if defined(DEBUG_SET)
        log_debug("set! context: TK_Arg_List (bindings)");
#endif
        {
        struct node_s *new
            = sunlark_arglist_mutate(s7, context_node, selector, update_val);
        if (new) {
            /* sealark_format_normalize(new); */
            /* sealark_format_rm_trailing_commas(s7_c_object_value(self)); */
            return context;
        } else
            return handle_errno(s7, errno, s7_cdr(args));
        }
        break;

    case TK_Binding:
#if defined(DEBUG_SET)
        log_debug("CASE SET! context: TK_Binding");
#endif
        if (selector == KW(key)) {
            /* log_debug("replacing selector: key"); */
            if (update_val == KW(null)) {
                log_error("Cannot delete key of binding");
                return handle_errno(s7, EINVALID_REMOVE, s7_cdr(args));
            }
            if (s7_is_symbol(update_val)) {
                const char *newstring = s7_symbol_name(update_val);
                int len = strlen(newstring);
                /* log_debug("updating -> %s", newstring); */
                struct node_s *key_node = utarray_eltptr(context_node->subnodes, 0);
                free(key_node->s);
                key_node->s = calloc(len, sizeof(char));
                strncpy(key_node->s, newstring, len);
                return s7_unspecified(s7); // r7rs: value of set! is unspecified; meaning???
            } else {
                log_error("Invalid arg: replacement for :key must be symbol; got: %s",
                          s7_object_to_c_string(s7, s7_cdr(args)));
                exit(EXIT_FAILURE); //FIXME
            }
        }

        if (selector == s7_make_keyword(s7, "$") || selector == KW(value)) {
            /* log_debug("replacing selector: :value"); */
            errno = 0;
            struct node_s *newb
                =  sunlark_mutate_binding_value(s7, context_node, update_val);
            if (newb) {
                /* log_debug("after replacement:"); */
                /* sealark_debug_log_ast_outline(newb, true); // crush */
                return sunlark_new_node(s7, newb);
                /* return s7_values(s7, s7_nil(s7)); */
            } else {
                return handle_errno(s7, errno, s7_cdr(args));
            }
        }

        if (update_val == KW(null)) {
#if defined(DEBUG_SET)
            log_debug("REMOVING BINDING from context %d %s",
                      context_node->tid, TIDNAME(context_node));
#endif
            return handle_errno(s7, EINVALID_REMOVE, args);
        }

#if defined(DEBUG_SET)
            log_debug("REPLACING BINDING");
#endif
        result = s7_c_object_value(context);

        struct node_s *res = sunlark_mutate_binding(s7, result, update_val);
        if (res)
            return s7_unspecified(s7);
        else
            return s7_unspecified(s7);
        break;

    case TK_Call_Expr:
#if defined(DEBUG_SET)
        log_debug("set! context: TK_Call_Expr, selector: %s; update: %s",
                  s7_object_to_c_string(s7, selector),
                  s7_object_to_c_string(s7, update_val));
#endif
        result = sunlark_target_mutate(s7, context_node,
                                       selector,
                                                      update_val);
        if (result)
            return s7_unspecified(s7);
        else
            return handle_errno(s7, errno, s7_nil(s7));
        break;

    case TK_Dict_Entry:
#if defined(DEBUG_SET)
        log_debug("set! context: TK_Dict_Entry");
        log_debug("selector: %s; update: %s", s7_object_to_c_string(s7, selector),
                  s7_object_to_c_string(s7, update_val));
#endif
        break;

    case TK_Dict_Expr:
#if defined(DEBUG_SET)
        log_debug("set! context: TK_Dict_Expr");
#endif
        {
        struct node_s *mu
            = sunlark_mutate_dict_expr(s7, context_node, selector, update_val);
        if (mu)
            return context;
        else
            return handle_errno(s7, errno, s7_cdr(args));
        }
        break;

    case TK_ID:                 /* Booleans, True or False */
#if defined(DEBUG_SET)
        log_debug("CASE: SET! on context TK_ID");
#endif
        if (update_val == KW(null)) {
            log_error("Cannot remove node in this context");
            return handle_errno(s7, EINVALID_REMOVE, args);
        }
        result
            = sunlark_set_id(s7, context_node, selector, update_val);
        if (result)
            return sunlark_new_node(s7, result);
        else {
            log_error("set! TK_ID error");
            return NULL;
        }
        break;
    case TK_List_Expr:          /* vector */

        /* return _set_list_expr(s7, context_node, selector, update_val); */
        /* updated =sunlark_mutate_vector(s7, result, update_val); */
#if defined(DEBUG_SET)
        log_debug("set! context: list-expr, selector: %s; newval: %s",
                  s7_object_to_c_string(s7, selector),
                  s7_object_to_c_string(s7, update_val));
#endif

        if (s7_is_keyword(selector)) {
            if (selector == KW(*)) {
                /* log_debug("Removing all items from :value list"); */
                struct node_s *exl = utarray_eltptr(context_node->subnodes, 1);
                utarray_clear(exl->subnodes);
                return context;
            }

            int idx = sunlark_kwindex_to_int(s7, selector);
            if (errno == 0) {// we got an int
                return sunlark_vector_mutate_item(s7, context,
                                                   idx, //selector,
                                                   update_val);
            } else {
                return(s7_error(s7,
                                s7_make_symbol(s7, "invalid_argument"),
                                s7_list(s7, 2, s7_make_string(s7,
            "Bad arg ~A - indexing keyword must be ':' followed by digit(s)."),
                                        selector)));
            }
        }
        if (s7_is_integer(selector)) {
            /* indexing int list by int value */
            log_error("Use int keywords (e.g. :%d) to index lists",
                      s7_integer(selector));
            return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 3, s7_make_string(s7,
             "Use :~A instead of ~A to index a list"),
                                    selector, selector)));
        }
        if (s7_is_string(selector)) {
            /* indexing string list by string value */
            UT_array *items
                = sealark_vector_items_for_string(s7_c_object_value(context),
                                                  s7_string(selector));
            sealark_update_vector_items(items,
                                        s7_string(update_val));
            /* sealark_debug_log_ast_outline(items, true); // crush */
            return nodelist_to_s7_list(s7, items);
        }

        if (s7_is_symbol(selector)) { /* find symbol value in list */
            log_debug("NOT YET IMPLEMENTED: indexing list by sym value");
            errno = ENOT_IMPLEMENTED;
            return NULL;
        }

        /* selector is compound expr */
        s7_pointer v = sunlark_vector_dispatcher(s7, context_node, selector);
        struct node_s *updated;
        if (v) {
            struct node_s *r = s7_c_object_value(v);
            switch(r->tid) {
            case TK_List_Expr:  /* (myvec) */
                updated =sunlark_mutate_vector(s7, r, update_val);
                /* sealark_debug_log_ast_outline(r, 4); */
                break;
            /* path indexed into vector */
            case TK_STRING:
                updated = sunlark_set_string(s7, r, update_val);
                break;
            case TK_ID:
                updated = sunlark_set_id(s7, r, selector, update_val);
                break;
            case TK_INT:
                updated = sealark_set_int(r, s7_integer(update_val));
                break;
            default:
                ;
            }
            if (updated)
                return sunlark_new_node(s7, updated);
            else
                //FIXME: error
                return s7_unspecified(s7);
        } else {
            // error
            return s7_unspecified(s7);
        }
        break;
    default:
        log_error("NOT IMPLEMENTED for context %d %s",
                  context_node->tid, TIDNAME(context_node));
        exit(EXIT_FAILURE);
        ;
    }

    //FIXME: r7rs says result of set! is unspecified. does that mean
    //implementation-specified?
    return s7_unspecified(s7);

    /* return sunlark_new_node(set_target); */
}

/* **************************************************************** */
#if INTERFACE
#define SUNLARK_REMOVE_BANG_HELP "(remove! b i) deletes the node at index i."

/* sig: returns node */
#define SUNLARK_REMOVE_BANG_SIG s7_make_signature(s7, 1, s7_make_symbol(s7, "node?"), s7_make_symbol(s7, "node?"))
#endif

s7_pointer sunlark_remove_bang(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug(">>>>>>>>>>>>>>>> sunlark_remove_bang <<<<<<<<<<<<<<<<");
#endif

    /* log_debug("args: %s", s7_object_to_c_string(s7, args)); */
    /* log_debug("car %s", s7_object_to_c_string(s7, s7_car(args))); */
    return NULL;
}

/* **************** */
LOCAL struct node_s *sunlark_set_string(s7_scheme *s7,
                                        struct node_s *old_vec,
                                        s7_pointer new_vec)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sunlark_set_string");
#endif

    log_error("NOT YET");

}

/* **************** */
LOCAL struct node_s *sunlark_set_id(s7_scheme *s7,
                                    struct node_s *context_node,
                                    s7_pointer selector,
                                    s7_pointer newval)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sunlark_set_id: %s", s7_object_to_c_string(s7, newval));
    log_debug("selector: %s", s7_object_to_c_string(s7, selector));
    log_debug("newval: %s", s7_object_to_c_string(s7, newval));
#endif

    assert(context_node->tid == TK_ID);


    /* NB: in this case selector should be '() - nothing to select */
    if ( s7_is_null(s7, selector) ) { //FIXME
        struct node_s *newitem;
        if (s7_is_c_object(newval)) {
            newitem = sealark_set_string_c_object(context_node,
                                                  s7_c_object_value(newval));
            return context_node;
        }
        if (s7_is_list(s7, newval)) {
            return sunlark_convert_node_to_list_expr(s7, context_node,
                                                     newval);
        }
        log_error("FIXME: fallthru on set_id");
    } else {
        /* should not happen, no futher selection possible */
        log_error("set! on TK_ID wtf????????????????");
        return NULL;
    }
}

struct node_s *sunlark_convert_node_to_list_expr(s7_scheme *s7,
                                                 struct node_s *node,
                                                 s7_pointer list)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_SET)
    log_debug("sunlark_convert_node_to_list_expr");
#endif

    if (node->s) {
        free(node->s);
        node->s = NULL;
    }

    if (node->subnodes) {
        utarray_free(node->subnodes);
        node->subnodes = NULL;
    }

    node->tid = TK_List_Expr;
    utarray_new(node->subnodes, &node_icd);

    struct node_s *lbrack = sealark_new_node(TK_LBRACK, without_subnodes);
    utarray_push_back(node->subnodes, lbrack);

    struct node_s *ls;
    if (s7_is_list(s7, list))
        ls = _new_vec_from_list(s7, list);
    else
        if (s7_is_vector(list))
            ls = _new_vec_from_vector(s7, list);

    utarray_push_back(node->subnodes, ls);

    struct node_s *rbrack = (struct node_s*)calloc(1, sizeof(struct node_s));
    rbrack->tid = TK_RBRACK;
    utarray_push_back(node->subnodes, rbrack);
    return node;
}

struct node_s *_new_vec_from_list(s7_scheme *s7, s7_pointer list)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_SET)
    log_debug("_new_vec_from_list");
#endif
    struct node_s *vec = sealark_new_node(TK_Expr_List, with_subnodes);
    struct node_s *new, *comma;
    int len = s7_list_length(s7, list);
    int i = 0;
    char ibuf[256]; // for int to string conversion
    const char *sbuf;
    int  slen;
    while( !s7_is_null(s7, list) ) {
        ibuf[0] = '\0';
        s7_pointer arg = s7_car(list);
        new = sealark_new_node(0, without_subnodes);

        if (s7_is_integer(arg)) {
            int i = s7_integer(arg);
            snprintf(ibuf, 256, "%d", i);
            slen = strlen(ibuf) + 1;
            new->s = calloc(slen, sizeof(char));
            strncpy(new->s, ibuf, slen);
            new->tid = TK_INT;
            goto loop;
        }
        /* new->s = (char*)calloc(8, sizeof(char)); */
        /* snprintf(new->s, 2, "%d", i); */
        /* log_debug("new int: %s", new->s); */

        if (s7_is_string(arg)) {
            new->tid = TK_STRING;
            sbuf = s7_string(arg);
            slen = strlen(sbuf) + 1;
            new->s = calloc(slen, sizeof(char));
            strncpy(new->s, sbuf, slen);
            goto loop;
        }

        if (s7_is_c_object(arg)) {
            /* FIXME: handle sunlark-make-string */
        }

    loop:
        utarray_push_back(vec->subnodes, new);
        if (len-i > 1) {
            comma = (struct node_s*)calloc(1, sizeof(struct node_s));
            comma->tid = TK_COMMA;
            utarray_push_back(vec->subnodes, comma);
        }
        i++;
        list = s7_cdr(list);
    }
    return vec;
}

struct node_s *_new_vec_from_vector(s7_scheme *s7, s7_pointer vector)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_SET)
    log_debug("_new_vec_from_vector");
#endif
    struct node_s *vec = sealark_new_node(TK_Expr_List, with_subnodes);
    struct node_s *new, *comma;
    int len = s7_vector_length(vector);
    char ibuf[256]; // for int to string conversion
    const char *sbuf;
    int  slen;
    int i = 0;
    for(int i = 0; i < len; i++) {
        ibuf[0] = '\0';
        s7_pointer arg = s7_vector_ref(s7, vector, i);
        new = sealark_new_node(0, without_subnodes);

        if (s7_is_integer(arg)) {
            int i = s7_integer(arg);
            snprintf(ibuf, 256, "%d", i);
            slen = strlen(ibuf) + 1;
            new->s = calloc(slen, sizeof(char));
            strncpy(new->s, ibuf, slen);
            new->tid = TK_INT;
            goto loop;
        }
        /* new->s = (char*)calloc(8, sizeof(char)); */
        /* snprintf(new->s, 2, "%d", i); */
        /* log_debug("new int: %s", new->s); */

        if (s7_is_string(arg)) {
            new->tid = TK_STRING;
            sbuf = s7_string(arg);
            slen = strlen(sbuf) + 1;
            new->s = calloc(slen, sizeof(char));
            strncpy(new->s, sbuf, slen);
            goto loop;
        }

        if (s7_is_c_object(arg)) {
            /* FIXME: handle sunlark-make-string */
        }

    loop:
        utarray_push_back(vec->subnodes, new);
        if (len-i > 1) {
            comma = (struct node_s*)calloc(1, sizeof(struct node_s));
            comma->tid = TK_COMMA;
            utarray_push_back(vec->subnodes, comma);
        }
    }
    return vec;
}
