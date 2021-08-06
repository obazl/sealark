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
    /* log_debug("args: %s", s7_object_to_c_string(s7, args)); */
    /* debug_print_s7(s7, "args", args); */
#endif
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
    log_debug("sunlark_set_bang");
    /* log_debug("\t(car args): %s", s7_object_to_c_string(s7, s7_car(args))); */
    log_debug("\tcdr args: %s", s7_object_to_c_string(s7, s7_cdr(args)));
#endif

    s7_pointer self = s7_car(args);
    struct node_s *self_node = s7_c_object_value(self);

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
    s7_pointer lval;
    if (s7_is_null(s7, s7_cdr(get_path))) {
        lval = s7_nil(s7);
        get_path = s7_nil(s7);
    } else {
        lval = s7_cadr(get_path);
        get_path = s7_reverse(s7, s7_cddr(get_path));
    }

#if defined (DEBUG_SET)
    log_debug("get_path: %s", s7_object_to_c_string(s7, get_path));

    /* bool dollar = false; // FIXME: name */
    /* if (s7_make_keyword(s7,"$") == s7_car(get_path)) { */
    /*     dollar = true; */
    /*     get_path = s7_cdr(get_path); */
    /* } */

    log_debug("set! self: %d %s; get_path: %s; lval: %s; new val: %s",
              sunlark_node_tid(s7, self),
              token_name[sunlark_node_tid(s7, self)][0],
              s7_object_to_c_string(s7, get_path),
              s7_object_to_c_string(s7, lval),
              s7_object_to_c_string(s7, update_val));
#endif

    if (s7_car(get_path) == s7_make_keyword(s7, "$")) {
        return self;
    }

    struct node_s *node;
    s7_int typ;
    s7_pointer key;

    /* qua procedure, check type of first arg */
    typ = s7_c_object_type(self);
    if (typ != ast_node_t)
        return(s7_wrong_type_arg_error(s7, "ast-node-set!", 1, self, "a ast_node"));

    if (s7_is_immutable(self))
        return(s7_wrong_type_arg_error(s7, "ast-node-set!", 1, self, "a mutable ast_node"));

    s7_pointer context = sunlark_node_object_applicator(s7, s7_cons(s7, self, get_path));
    struct node_s *context_node = s7_c_object_value(context);

#if defined(DEBUG_SET)
    log_debug("self_node %d %s", self_node->tid, TIDNAME(self_node));
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
        /* NB: in this case lval should be '() - nothing to select */
        if ( s7_is_null(s7, lval) ) { //FIXME
            if (s7_is_c_object(update_val)) {
                struct node_s *newitem
                    = sealark_set_string_c_object(context_node,
                                  s7_c_object_value(update_val));
                return sunlark_node_new(s7,newitem);
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
        /* context: bindings list; lval: index */
#if defined(DEBUG_SET)
        log_debug("set! context: TK_Arg_List (bindings)");
#endif
        struct node_s *new
            = _mutate_arglist(s7, context_node, lval, update_val);
        if (new)
            return context;
        else
            return handle_errno(s7, errno, s7_cdr(args));
        break;

    case TK_Binding:
#if defined(DEBUG_SET)
        log_debug("CASE SET! context: TK_Binding");
#endif
        if (lval == KW(key)) {
            log_debug("replacing lval: key");
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

        if (lval == s7_make_keyword(s7, "$") || lval == KW(value)) {
            log_debug("replacing lval: :value");
            errno = 0;
            struct node_s *newb
                =  sunlark_mutate_binding_value(s7, context_node, update_val);
            if (newb) {
                log_debug("after replacement:");
                sealark_debug_log_ast_outline(newb, true); // crush
                return sunlark_node_new(s7, newb);
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
        struct node_s *result = s7_c_object_value(context);

        struct node_s *res = _mutate_binding(s7, result, update_val);
        if (res)
            return s7_unspecified(s7);
        else
            return s7_unspecified(s7);
        break;

    case TK_Dict_Expr:
#if defined(DEBUG_SET)
        log_debug("set! context: TK_Dict_Expr");
#endif
        struct node_s *mu
            = sunlark_mutate_dict_expr(s7, context_node, lval, update_val);
        if (mu)
            return context;
        else
            return handle_errno(s7, errno, s7_cdr(args));
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
            = sunlark_set_id(s7, context_node, lval, update_val);
        if (result)
            return sunlark_node_new(s7, result);
        else {
            log_error("set! TK_ID error");
            return NULL;
        }
        break;
    case TK_List_Expr:          /* vector */

        /* return _set_list_expr(s7, context_node, lval, update_val); */
        /* updated =sunlark_mutate_vector(s7, result, update_val); */
#if defined(DEBUG_SET)
        log_debug("set! context: list-expr");
        log_debug("lval: %s", s7_object_to_c_string(s7, lval));
#endif

        if (s7_is_keyword(lval)) {
            if (lval == KW(*)) {
                log_debug("Removing all items from :value list");
                struct node_s *exl = utarray_eltptr(context_node->subnodes, 1);
                utarray_clear(exl->subnodes);
                return context;
            }
            s7_pointer sym = s7_keyword_to_symbol(s7, lval);
            const char *kw = s7_symbol_name(sym);
            int len = strlen(kw); // very unlikely to be more than 2
            int i = (kw[0] == '-')? 1 : 0;
            for ( ; i < len; i++) {
                if ( !isdigit( (int)kw[i] ) ) {
                    log_debug("Indexing keyword must be ':' followed by digit(s); got: %s", kw);
                    return(s7_error(s7,
                                    s7_make_symbol(s7, "invalid_argument"),
                                    s7_list(s7, 2, s7_make_string(s7,
                  "Bad arg ~A - indexing keyword must be ':' followed by digit(s)."),
                                            lval)));
                }
            }
            int idx = atoi(kw);
            return sunlark_vector_replace_item(s7, context,
                                               idx, //lval,
                                               update_val);
        }
        if (s7_is_integer(lval)) {
            /* indexing int list by int value */
            log_error("Use int keywords (e.g. :%d) to index lists",
                      s7_integer(lval));
            return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 3, s7_make_string(s7,
             "Use :~A instead of ~A to index a list"),
                                    lval, lval)));

            /* return sunlark_vector_replace_item(s7, context, lval, update_val); */
        }
        if (s7_is_string(lval)) {
            /* indexing string list by string value */
            UT_array *items
                = sealark_vector_items_for_string(s7_c_object_value(context),
                                                  s7_string(lval));
            sealark_update_vector_items(items,
                                        s7_string(update_val));
            /* sealark_debug_log_ast_outline(items, true); // crush */
            return nodelist_to_s7_list(s7, items);
        }

        if (s7_is_symbol(lval)) { /* find symbol value in list */
            log_debug("NOT YET indexing list by sym value");
            return NULL;
        }

        /* lval is compound expr */
        struct node_s *r = sunlark_vector_dispatcher(s7, context_node, lval);
        struct node_s *updated;
        if (r) {
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
                updated = sunlark_set_id(s7, r, lval, update_val);
                break;
            case TK_INT:
                updated = sealark_set_int(r, s7_integer(update_val));
                break;
            default:
                ;
            }
            if (updated)
                return sunlark_node_new(s7, updated);
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

    /* return sunlark_node_new(set_target); */
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

    log_debug("args: %s", s7_object_to_c_string(s7, args));
    log_debug("car %s", s7_object_to_c_string(s7, s7_car(args)));
    return NULL;
}

/* **************************************************************** */
LOCAL struct node_s *_mutate_arglist(s7_scheme *s7,
                                     struct node_s *context_node,
                                     s7_pointer lval,
                                     s7_pointer update_val)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("_mutate_arglist: %s => %s",
              s7_object_to_c_string(s7, lval),
              s7_object_to_c_string(s7, update_val));
#endif
    /* sealark_debug_log_ast_outline(context_node, 0); */

    assert(context_node->tid == TK_Arg_List);

    if (s7_is_string(lval)) {
        log_error("context: string, not yet");
        return NULL;
    }

    if (s7_is_integer(lval)) {
        if (update_val == KW(null)) {
            log_debug("Removing binding at %s",
                      s7_object_to_c_string(s7, lval));
            errno = 0;
            struct node_s *updated_node;
            updated_node
                = sealark_arglist_rm_binding_for_int(context_node,
                                                     s7_integer(lval));
            if (updated_node) {
                return updated_node;
            } else {
                return NULL; // errno set by subroutine
            }
        }

        if (s7_is_list(s7, update_val)) {
            log_debug("update val is list");
            errno = ENOT_IMPLEMENTED;
            return NULL;
        }
        log_error("Invalid update for binding: %s",
                  s7_object_to_c_string(s7, update_val));
        errno = EINVALID_UPDATE;
        return NULL;
    }

    if (s7_is_symbol(lval)) {
        if (update_val == KW(null)) {
            log_info("removing sym %s from bindings",
                      s7_object_to_c_string(s7, lval));
            int idx = sealark_binding_index_for_key(context_node,
                                                    s7_symbol_name(lval));
            if (idx < 0) {
                log_error("key not found: %s", s7_symbol_name(lval));
                errno = ENOT_FOUND_BINDING;
                return NULL;
            } else {
                log_debug("found %s at index %d (subnode %d)",
                          s7_symbol_name(lval),
                          idx/2, idx);
                sealark_remove_binding_at_index(context_node, idx);
                return context_node;
            }
        } else {
            log_error("update context: symbol, not yet");
            return NULL;
        }
    }

    log_error("Invalid lval for arglist: %s",
              s7_object_to_c_string(s7, lval));
    errno = EINVALID_ARG;
    return NULL;
}

/* **************************************************************** */
LOCAL struct node_s *_mutate_binding(s7_scheme *s7, struct node_s *binding, s7_pointer new_binding)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("_mutate_binding, new_binding: %s",
              s7_object_to_c_string(s7, new_binding));
#endif
    if (s7_is_list(s7, new_binding)) {
        log_debug("replacing binding");
        if ( !sunlark_new_binding_is_valid(s7, new_binding) )
            exit(EXIT_FAILURE); /* FIXME: s7_error */
        if (binding->tid != TK_Binding) {
            log_error("Expected TK_Binding node, got %d  %s",
                      binding->tid, TIDNAME(binding));
            exit(EXIT_FAILURE); /* FIXME: s7_error */
        }
        /* new_binding is valid */

        /* mutate key */
        struct node_s *bid = utarray_eltptr(binding->subnodes, 0);
        const char *new_key;
        if (s7_is_string(s7_car(new_binding))) {
            new_key = s7_string(s7_car(new_binding));
        } else {
            if (s7_is_symbol(s7_car(new_binding))) {
                new_key = s7_symbol_name(s7_car(new_binding));
            } else {
                // should not happen, has been validated above
            }
        }
        int len = strlen(new_key);
        free(bid->s);
        bid->s = calloc(len, sizeof(char));
        strncpy(bid->s, new_key, len);

        /* mutate value */
        struct node_s *old_value = utarray_eltptr(binding->subnodes, 2);
        s7_pointer new_value = s7_cadr(new_binding);

        if (s7_is_list(s7, new_value)) {
            if (old_value->tid == TK_List_Expr) {
                /* update existing vector */
                sunlark_mutate_vector(s7, old_value, new_value);
            } else {
                /* replace */
                sealark_node_free(old_value);
                old_value = sealark_new_node(TK_List_Expr, without_subnodes);
            }
        }
        return binding;
    } else {
        if (s7_is_string(new_binding)) {
            log_error("Cannot replace binding with string");
            exit(EXIT_FAILURE); /* FIXME: s7_error */
        }
    }
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
                                    s7_pointer lval,
                                    s7_pointer newval)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sunlark_set_id: %s", s7_object_to_c_string(s7, newval));
#endif

    assert(context_node->tid == TK_ID);

    log_debug("lval: %s", s7_object_to_c_string(s7, lval));
    log_debug("newval: %s", s7_object_to_c_string(s7, newval));

    /* NB: in this case lval should be '() - nothing to select */
    if ( s7_is_null(s7, lval) ) { //FIXME
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
        log_debug("1 xxxxxxxxxxxxxxxx");
        utarray_push_back(vec->subnodes, new);
        if (len-i > 1) {
            comma = (struct node_s*)calloc(1, sizeof(struct node_s));
            comma->tid = TK_COMMA;
            utarray_push_back(vec->subnodes, comma);
        }
        i++;
        list = s7_cdr(list);
    }
    utarray_push_back(node->subnodes, vec);

    struct node_s *rbrack = (struct node_s*)calloc(1, sizeof(struct node_s));
    rbrack->tid = TK_RBRACK;
    utarray_push_back(node->subnodes, rbrack);
    return node;
}
