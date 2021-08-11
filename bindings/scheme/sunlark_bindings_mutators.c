#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_bindings_mutators.h"

/* docs at bottom */

s7_pointer sunlark_update_binding_name(s7_scheme *s7,
                                         s7_pointer node_s7,
                                         const char *key,
                                         s7_pointer val)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug(">> sunlark_update_binding_name, tid: %d",
              sunlark_node_tid(s7, node_s7));
#endif

    /* log_debug("updating attr-name"); */
    const char *tmp_name;
    if (s7_is_string(val)) {
        tmp_name = s7_string(val);
    } else {
        if (s7_is_number(val)) {
            tmp_name = s7_number_to_string(s7, val, 10);
        } else {
            return(s7_wrong_type_arg_error(s7,
                                           "attr name edit value:",
                                           2, val,
                                           "string or number"));
        }
    }
    /* strings from s7 must not be freed? so... */
    int len = strlen(tmp_name) + 1; /* add one for newline */
    /* log_debug("TMP NAME: %d, %s", len, tmp_name); */
    char *new_name = calloc(len, sizeof(char));
    snprintf(new_name, len, "%s", tmp_name);

    /* first subnode is ID */
    struct node_s *node = s7_c_object_value(node_s7);
    struct node_s *target = utarray_eltptr(node->subnodes, 0);
    free(target->s);
    target->s = new_name;
    return s7_make_string(s7, new_name);
}

s7_pointer sunlark_update_binding_value(s7_scheme *s7,
                                          s7_pointer node_s7,
                                          const char *key,
                                          s7_pointer val)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug(">> sunlark_update_binding_value, tid: %d",
              sunlark_node_tid(s7, node_s7));
#endif

    struct node_s *node = s7_c_object_value(node_s7);

    /* 3rd subnode of attrib is attr-value */
    struct node_s *target = utarray_eltptr(node->subnodes, 2);
    if (target->tid == TK_STRING) {
        /* log_debug("updating string value"); */
        const char *tmp_value;
        if (s7_is_string(val)) {
            tmp_value = s7_string(val);
        } else {
            if (s7_is_number(val)) {
                tmp_value = s7_number_to_string(s7, val, 10);
            } else {
                if (s7_is_list(s7, val)) {
                    log_error("LIST edits for string value");
                    return(s7_error(s7, s7_make_symbol(s7,
                                                       "not implemented"),
                                    s7_list(s7, 2, s7_make_string(s7,
                       "list edits for string values not yet supported"),
                                            key)));

                } else {
                    log_error("ERROR: unexpected edit value");
                    exit(EXIT_FAILURE);
                }
            }
        }
        /* log_debug("updating string value"); */
        /* strings from s7 must not be freed? so... */
        int len = strlen(tmp_value) + 1;
        /* log_debug("TMP VALUE: %d, %s", len, tmp_value); */
        char *new_value = calloc(len, sizeof(char));
        snprintf(new_value, len, "%s", tmp_value);

        free(target->s);
        target->s = new_value;
        return s7_make_string(s7, new_value);
    }

    if (target->tid == TK_List_Expr) {
        /* log_debug("updating list valued attrib"); */
        // for now, replacement
        if (s7_is_list(s7, val)) {
            /* log_debug("set! val is list: %s", */
            /*           s7_object_to_c_string(s7, val)); */
            errno = 0;
            struct node_s *result = sunlark_update_list_value(s7, target, key, val);
            if (result == NULL) {
                log_error("errno: %d", errno);
                switch(errno) {
                case ESUNLARK_LOCN_ARG_ERR:
                    return(s7_error(s7, s7_make_symbol(s7,
                                                       "invalid_locator"),
                                    s7_list(s7, 2, s7_make_string(s7,
                     ":remove selectors must be keywordized integers, e.g. :1; got ~A"),
                                            (s7_cdr(val)))));
                    break;
                case ESUNLARK_ARG_TYPE_ERR:
                    return(s7_error(s7, s7_make_symbol(s7,
                                                       "arg type error: "),
                                    s7_list(s7, 2, s7_make_string(s7,
                                    "FIXME error msg"), key)));
                    break;
                case ESUNLARK_INVALID_ARG:
                    return(s7_error(s7, s7_make_symbol(s7,
                                                       "key error: "),
                                    s7_list(s7, 2, s7_make_string(s7,
                                    "FIXME error msg"), key)));
                    break;
                default:
                    return(s7_error(s7, s7_make_symbol(s7,
                                                       "key error: "),
                                    s7_list(s7, 2, s7_make_string(s7,
                                    "FIXME2 error msg"), key)));

                }
            }
            return sunlark_new_node(s7, result);
        } else {
            if (s7_is_vector(val)) {
            } else {
                return(s7_wrong_type_arg_error(s7,
                                               "ast-node-set! attr val",
                                               2, val,
                                               "list or vector"));
            }
        }
    }

    if (target->tid == TK_Dict_Expr) {
        log_error("updating dict value (not yet)");
        return(s7_error(s7, s7_make_symbol(s7, "not yet implemented"),
                    s7_list(s7, 2, s7_make_string(s7, "node type dict not yet supported for value update"), key)));

    }
    log_debug("update not implemented for tid %d", target->tid);
    return(s7_error(s7, s7_make_symbol(s7, "not implemented"),
                    s7_list(s7, 2, s7_make_string(s7, "node type not yet supported for value update"), key)));
}

/*

  edit dsl:

  (set! (node ref) (:<op> :<location> <edits>)

  :<op> == one of :add, :remove, :replace

  :<location> ==
      :n for index, e.g. :0 for first, :1 second, :-0 last
      "foo" for value match, e.g. (:replace "foo" "bar")
      :* for everything, e.g. (:remove :*) results in []
      <location> is optional for :add; default is :-0 (append)

  <edits> == a string or number

  examples:

      '(:add :0 "foo") ;; insert "foo" in first position
      '(:add "foo") ;; appends, same as (:add :-0 "foo")
      '(:replace :2 "foo") ;; replaces 3rd element with "foo"
      '(:replace "foo" "bar") ;; "bar" replaces "foo"
      '(:replace :* "foo") ;; result is ["foo"]
      '(:remove :1) ;; removes second element
      '(:remove "foo") ;; removes "foo"
      '(:remove :*) ;; result is []

      we need ' to prevent eval of keywords in fn position.
      alternative: reader macros, e.g. #+, #-, #!
 */

/*
          ;; to update a string-list attrib value: pass a list?
          ;; we need both an update! and a replace! function
          ;; in the set! context, (deps 'value) returns
          ;; the updatable value, and we can do anything
          ;; with it; we're not required to replace it in toto.
          ;; (set! (deps 'value) 99) ;; fail, wrong type
          ;; or

          (set! (deps 'value) '(:append "dep_x")) ;; ok, adds to 99 to list
          ;; here :append is metadata, a command. but what if
          ;; we want to add :append to the value?
          ;; use a hashtable instead of a list?
          ;; or a custom read macro?

          (set! (deps 'value) #+("dep_x")) ;; adds arg to val list
          (set! (deps 'value) #-("dep_x")) ;; removes arg from val list
          (set! (deps 'value) #!("dep_x")) ;; replaces val list

          ;; #+("depx") expands to (:::private_append_sym "depx")
          ;; or:  (#+(append) "depx"), (#+(remove) "depx") etc.
          ;; and #+(append) => '___append, or some such
          ;; maybe #@(...), since this is for attibutes

          ;; @+() works for strings too

          ;; what about regex replace expressions?
          ;; e.g. for each item in list, s/foo/bar/

          ;; (set! (deps 'value) "dep_x" :insert-after "dep_a") ;; ok, adds to 99 to list
          ;; (set! (deps 'value) 99 :replace) ;; replace entire value
          ;; ;;
          ;; (ast-node-replace! deps 'value 99) ;; ok
          ;; (set! (deps 'value) 99) ;; #(foo bar bazl))
*/

/* **************************************************************** */
//FIXME: return s7_pointer ?
struct node_s *sunlark_mutate_binding_value(s7_scheme *s7,
                                            struct node_s *binding,
                                            s7_pointer newval)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_SET)
    log_debug("sunlark_mutate_binding_value: %s",
              s7_object_to_c_string(s7, newval));
    log_debug("\tself: %d %s", binding->tid, TIDNAME(binding));
#endif

    if (newval == KW(null)) {
        log_error("Cannot nullify (remove) value of binding");
        errno = ENULL_BINDING_VAL;
        return NULL;
    }

    struct node_s *oldval = utarray_eltptr(binding->subnodes, 2);
    /* sealark_debug_log_ast_outline(binding, true); // crush */

#if defined(DEBUG_SET)
    log_debug("mutating %d %s with value of type %s",
              oldval->tid, TIDNAME(oldval),
              s7_object_to_c_string(s7, (s7_type_of(s7, newval))));
#endif
    /* utarray_free(val->subnodes); */
    /* utarray_clear(val->subnodes); */

    if (oldval->tid == TK_List_Expr) {
        if (s7_is_list(s7, newval)) {
            /* log_debug("replacing list with list"); */
            return sunlark_mutate_vector(s7, oldval, newval);
        }
    }

    /* if (oldval->tid == TK_Dict_Expr) { */
    /*     if (s7_is_list(s7, newval)) { */
    /*         log_debug("replacing dict with list"); */
    /*         return sunlark_mutate_dict_expr(s7, oldval, */
    /*                                         s7_nil(s7), */
    /*                                         newval); */
    /*     } */
    /*     if (s7_is_vector(newval)) { */
    /*         log_debug("replacing dict with vector"); */
    /*         return sunlark_mutate_dict_expr(s7, oldval, */
    /*                                         s7_nil(s7), */
    /*                                         newval); */
    /*     } */
    /* } */

    /* newval has no subnodes  */
    if (oldval->subnodes) {
        utarray_free(oldval->subnodes);
        /* utarray_clear(val->subnodes); */
        oldval->subnodes = NULL;
    }

    if (s7_is_c_object(newval)) {
        log_error("replacing with c-object");
        /* newval is e.g. sunlark-make-string */

        /* const char *s = s7_string(newval); */
        /* log_debug("new val: %s", s); */
        /* int len = strlen(s); */
        /* val->tid = TK_STRING; */
        /* val->s = calloc(len, sizeof(char)); */
        /* strncpy(val->s, s, len); */
        /* /\* val->qtype = DQUOTE; *\/ */
        return binding;
    }

    if (s7_is_list(s7, newval)) {
        return sunlark_convert_node_to_list_expr(s7, oldval, newval);
    }
    /* if (s7_is_list(s7, newval)) { */
    /*     s7_pointer action = s7_car(newval); */
    /*     if (action == KW(append)) { */
    /*         log_debug("ACTION: append"); */
    /*         /\* return _vector_append(s7, _list_expr, index, s7_cdr(newval)); *\/ */
    /*         return NULL; */
    /*     } */
    /*     errno = EINVALID_ARG; */
    /*     return NULL; */
    /* } */

    if (s7_is_vector(newval)) {
        return sunlark_convert_node_to_list_expr(s7, oldval, newval);
    }

    if (s7_is_pair(newval)) {
        log_debug("NOT YET: replacing with pair");
        return binding;
    }

    if (s7_is_string(newval)) {
        const char *s = s7_string(newval);
        /* log_debug("new val: %s", s); */
        int len = strlen(s);
        oldval->tid = TK_STRING;
        oldval->s = calloc(len, sizeof(char));
        strncpy(oldval->s, s, len);
        return binding;
    }

    if (s7_is_symbol(newval)) {
        const char *s = s7_symbol_name(newval);
        /* log_debug("new val: %s", s); */
        int len = strlen(s);
        oldval->tid = TK_ID;
        oldval->s = calloc(len, sizeof(char));
        strncpy(oldval->s, s, len);
        return binding;
    }

    if (s7_is_integer(newval)) {
        int d = s7_integer(newval);
        char buf[128];
        snprintf(buf, 128, "%d", d);
        int len = strlen(buf);
        /* log_debug("new val: %d", d); */
        oldval->tid = TK_INT;
        oldval->s = calloc(len, sizeof(char));
        strncpy(oldval->s, buf, len);
        return binding;
    }

    if (s7_is_boolean(newval)) {
        /* log_debug("new val: %s", s7_object_to_c_string(s7, newval)); */
        oldval->tid = TK_ID;
        if (newval == s7_t(s7)) {
            oldval->s = calloc(4, sizeof(char));
            strncpy(oldval->s, "True", 4);
        } else {
            oldval->s = calloc(5, sizeof(char));
            strncpy(oldval->s, "False", 5);
        }
        return binding;
    }

    return binding;
}

/* **************************************************************** */
//FIXME: return s7_pointer ?
struct node_s *Xsunlark_mutate_binding_value(s7_scheme *s7,
                                            struct node_s *binding,
                                            s7_pointer newval)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_SET)
    log_debug("sunlark_mutate_binding_value: %s",
              s7_object_to_c_string(s7, newval));
    log_debug("\tself: %d %s", binding->tid, TIDNAME(binding));
#endif

    if (newval == KW(null)) {
        log_error("Cannot nullify (remove) value of binding");
        errno = ENULL_BINDING_VAL;
        return NULL;
    }

    struct node_s *oldval = utarray_eltptr(binding->subnodes, 2);
    /* sealark_debug_log_ast_outline(binding, true); // crush */

#if defined(DEBUG_SET)
    log_debug("mutating %d %s with value of type %s",
              oldval->tid, TIDNAME(oldval),
              s7_object_to_c_string(s7, (s7_type_of(s7, newval))));
#endif
    /* utarray_free(val->subnodes); */
    /* utarray_clear(val->subnodes); */

    if (s7_is_list(s7, newval)) {
        s7_pointer action = s7_car(newval);
        if (action == KW(append)) {
            /* log_debug("ACTION: append"); */
            /* return _vector_append(s7, _list_expr, index, s7_cdr(newval)); */
            return NULL;
        }
        errno = EINVALID_ARG;
        return NULL;
    }

    if (oldval->tid == TK_List_Expr) {
        if (s7_is_list(s7, newval)) {
            /* log_debug("replacing list with list"); */
            return sunlark_mutate_vector(s7, oldval, newval);
        }
    }

    /* if (oldval->tid == TK_Dict_Expr) { */
    /*     if (s7_is_list(s7, newval)) { */
    /*         log_debug("replacing dict with list"); */
    /*         return sunlark_mutate_dict_expr(s7, oldval, */
    /*                                         s7_nil(s7), */
    /*                                         newval); */
    /*     } */
    /*     if (s7_is_vector(newval)) { */
    /*         log_debug("replacing dict with vector"); */
    /*         return sunlark_mutate_dict_expr(s7, oldval, */
    /*                                         s7_nil(s7), */
    /*                                         newval); */
    /*     } */
    /* } */

    /* newval has no subnodes  */
    if (oldval->subnodes) {
        utarray_free(oldval->subnodes);
        /* utarray_clear(val->subnodes); */
        oldval->subnodes = NULL;
    }

    if (s7_is_c_object(newval)) {
        log_error("replacing with c-object");
        /* newval is e.g. sunlark-make-string */

        /* const char *s = s7_string(newval); */
        /* log_debug("new val: %s", s); */
        /* int len = strlen(s); */
        /* val->tid = TK_STRING; */
        /* val->s = calloc(len, sizeof(char)); */
        /* strncpy(val->s, s, len); */
        /* /\* val->qtype = DQUOTE; *\/ */
        return binding;
    }

    if (s7_is_list(s7, newval)) {
        return sunlark_convert_node_to_list_expr(s7, oldval, newval);
    }

    if (s7_is_vector(newval)) {
        return sunlark_convert_node_to_list_expr(s7, oldval, newval);
    }

    if (s7_is_pair(newval)) {
        log_debug("NOT YET: replacing with pair");
        return binding;
    }

    if (s7_is_string(newval)) {
        const char *s = s7_string(newval);
        /* log_debug("new val: %s", s); */
        int len = strlen(s);
        oldval->tid = TK_STRING;
        oldval->s = calloc(len, sizeof(char));
        strncpy(oldval->s, s, len);
        return binding;
    }

    if (s7_is_symbol(newval)) {
        const char *s = s7_symbol_name(newval);
        /* log_debug("new val: %s", s); */
        int len = strlen(s);
        oldval->tid = TK_ID;
        oldval->s = calloc(len, sizeof(char));
        strncpy(oldval->s, s, len);
        return binding;
    }

    if (s7_is_integer(newval)) {
        int d = s7_integer(newval);
        char buf[128];
        snprintf(buf, 128, "%d", d);
        int len = strlen(buf);
        /* log_debug("new val: %d", d); */
        oldval->tid = TK_INT;
        oldval->s = calloc(len, sizeof(char));
        strncpy(oldval->s, buf, len);
        return binding;
    }

    if (s7_is_boolean(newval)) {
        /* log_debug("new val: %s", s7_object_to_c_string(s7, newval)); */
        oldval->tid = TK_ID;
        if (newval == s7_t(s7)) {
            oldval->s = calloc(4, sizeof(char));
            strncpy(oldval->s, "True", 4);
        } else {
            oldval->s = calloc(5, sizeof(char));
            strncpy(oldval->s, "False", 5);
        }
        return binding;
    }

    return binding;
}

/* **************************************************************** */
LOCAL struct node_s *sunlark_mutate_arglist_at_int(s7_scheme *s7,
                                     struct node_s *arg_list,
                                                   int index,
                                     s7_pointer update_val)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sunlark_mutate_arglist_at_int: %d => %s",
              index, s7_object_to_c_string(s7, update_val));
#endif
    /* sealark_debug_log_ast_outline(arg_list, 0); */

    assert(arg_list->tid == TK_Arg_List);

    index = sealark_normalize_index(arg_list, index);

    if (update_val == KW(null)) {
        /* log_debug("Removing binding at %d", index); */
        errno = 0;
        struct node_s *updated_node;
        updated_node
            = sealark_arglist_rm_binding_for_int(arg_list, index);
        if (updated_node) {
            return updated_node;
        } else {
            return NULL; // errno set by subroutine
        }
    }

    if (s7_is_list(s7, update_val)) {
        /* list means "splice before" */
            return _binding_splice_list(s7, arg_list, index, update_val);
        /* s7_pointer action = s7_car(update_val); */
        /* if (action == KW(splice)) { */
        /*     log_debug("ACTION: splice bindings"); */
        /*     return _binding_splice(s7, arg_list, index, s7_cdr(update_val)); */
        /*     return NULL; */
        /* } */
        /* if (action == KW(append)) { */
        /*     log_debug("ACTION: append bindings"); */
        /*     return NULL; */
        /* } */
    }

    if (s7_is_vector(update_val)) {
        return _binding_splice_vector(s7, arg_list, index, update_val);
    }

    if (s7_is_c_object(update_val)) {
        if (s7_c_object_type(update_val) == ast_node_t) {
            struct node_s *nd = s7_c_object_value(update_val);
            /* log_debug("update val is node: %d %s", */
            /*           nd->tid, TIDNAME(nd)); */
            if (nd->tid == TK_Binding) {
                return sealark_arglist_replace_binding_at_int(arg_list,
                                                              index, nd);
            } else {
            log_error("update val is unknown c_object");
            errno = ENOT_IMPLEMENTED;
            return NULL;
            }
        } else {
            log_error("update val is unknown c_object");
            errno = ENOT_IMPLEMENTED;
            return NULL;
        }
    }

    log_error("Invalid update for binding: %s",
              s7_object_to_c_string(s7, update_val));
    errno = EINVALID_UPDATE;
    return NULL;
}

struct node_s *sunlark_arglist_mutate(s7_scheme *s7,
                                     struct node_s *arglist,
                                     s7_pointer lval,
                                     s7_pointer update_val)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sunlark_arglist_mutate: %s => %s",
              s7_object_to_c_string(s7, lval),
              s7_object_to_c_string(s7, update_val));
#endif
    /* sealark_debug_log_ast_outline(arglist, 0); */

    assert(arglist->tid == TK_Arg_List);
    arglist->line = -1; //  mark as unformatted
    arglist->col  = -1; //  mark as unformatted

    if (s7_is_string(lval)) {
        log_error("context: string, not yet");
        return NULL;
    }

    if (s7_is_keyword(lval)) {
        int idx = sunlark_kwindex_to_int(s7, lval);
        if (errno == 0) {       // we got an int kw
            return sunlark_mutate_arglist_at_int(s7, arglist,
                                                 idx, update_val);
        } else {
            if (errno == ENOT_A_KW) {
                ; // log_debug("not a KW, continuing");
            } else {
                if (lval == KW(*)) {
                    /* log_debug("removing all bindings"); */
                    struct node_s *r
                        = sealark_target_bindings_rm_all(arglist);
                    if (r)
                        return r;
                    else
                        return NULL;
                } else {
                    log_error("FIXME: non-nbr kw path op: %s",
                              s7_object_to_c_string(s7, lval));
                    exit(EXIT_FAILURE);
                }
            }
        }
    }

    if (s7_is_integer(lval)) {
        if (update_val == KW(null)) {
            /* log_debug("Removing binding at %s", */
            /*           s7_object_to_c_string(s7, lval)); */
            errno = 0;
            struct node_s *updated_node;
            updated_node
                = sealark_arglist_rm_binding_for_int(arglist,
                                                     s7_integer(lval));
            if (updated_node) {
                return updated_node;
            } else {
                return NULL; // errno set by subroutine
            }
        }

        if (s7_is_list(s7, update_val)) {
            log_error("update val is list");
            errno = ENOT_IMPLEMENTED;
            return NULL;
        }
        log_error("Invalid update for binding: %s",
                  s7_object_to_c_string(s7, update_val));
        errno = EINVALID_UPDATE;
        return NULL;
    }

    if (s7_is_symbol(lval)) {
            int idx = sealark_binding_index_for_key(arglist,
                                                    s7_symbol_name(lval));
            if (idx < 0) {
                log_error("key not found: %s", s7_symbol_name(lval));
                errno = ENOT_FOUND_BINDING;
                return NULL;
            } else {
                /* log_debug("found %s at index %d (subnode %d)", */
                /*           s7_symbol_name(lval), */
                /*           idx/2, idx); */
            }

            if (update_val == KW(null)) {
                /* log_info("removing sym %s from bindings", */
                /*          s7_object_to_c_string(s7, lval)); */
                sealark_remove_binding_at_index(arglist, idx);
                return arglist;
            }

            if (s7_is_c_object(update_val)) {
                /* log_debug("context: c-object"); */
                if (s7_c_object_type(update_val) != ast_node_t) {
                    log_error("update val is unknown c_object");
                    errno = ENOT_IMPLEMENTED;
                    return NULL;
                } else {
                    struct node_s *b = s7_c_object_value(update_val);
                    if (b->tid != TK_Binding) {
                        log_error("Invalid type %s %s for mutating binding",
                                  b->tid, TIDNAME(b));
                        errno = ESUNLARK_ARG_TYPE_ERR;
                        return NULL;
                    } else {
                        /* log_error("updating with new binding at idx %d", idx); */
                        /* struct node_s *binding */
                        /*     = sealark_bindings_binding_for_index(arglist, */
                        /*                                          idx); */
                        struct node_s *r
                            = sealark_arglist_replace_binding_at_int(arglist,
                                                                     idx,
                                                                     b);
                        return r;
                    }
                }
            }
        log_error("update context: symbol, not yet");
        return NULL;
    }

    log_error("Invalid lval for arglist: %s",
              s7_object_to_c_string(s7, lval));
    errno = EINVALID_ARG;
    return NULL;
}

/* **************************************************************** */
struct node_s *sunlark_mutate_binding(s7_scheme *s7,
                                      struct node_s *binding,
                                      s7_pointer new_binding)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> _mutate_binding, new_binding: %s",
              s7_object_to_c_string(s7, new_binding));
#endif
    assert(binding->tid == TK_Binding);

    if (s7_is_c_object(new_binding)) {
        /* log_debug("new binding...."); */
        if (s7_c_object_type(new_binding) == ast_node_t) {
            /* log_debug("new ast node"); */
            struct node_s *b = s7_c_object_value(new_binding);
            if (b->tid != TK_Binding) {
                log_error("Invalid type %s %s for mutating binding",
                          b->tid, TIDNAME(b));
                errno = ESUNLARK_ARG_TYPE_ERR;
                return NULL;
            } else {
                /* overwrite old node */
                if (binding->subnodes)
                    utarray_free(binding->subnodes);
                if (binding->s)
                    free(binding->s);
                b->line = binding->line;
                b->col  = binding->col;
                //FIXME: mem leak?
                memcpy(binding, b, sizeof(struct node_s));
                return binding;
            }
        } else {
            log_error("Unknown c object type");
            errno = EUNKNOWN_C_OBJECT_TYPE;
            return NULL;
        }
    }

    if (s7_is_list(s7, new_binding)) {
        /* log_debug("replacing binding"); */
        if ( !sunlark_new_binding_is_valid(s7, new_binding) ) {
            log_error("failed: sunlark_new_binding_is_valid");
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
            errno = EINVALID_UPDATE;
            return NULL;
        } else {
            log_error("Unexpected state");
            errno = EUNEXPECTED_STATE;
            return NULL;
        }
    }
}

/* **************************************************************** */
LOCAL struct node_s *_binding_splice_list(s7_scheme *s7,
                                          struct node_s *bindings,
                                          int index,
                                          s7_pointer splice)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("_binding_splice at %d", index);
#endif
    assert(bindings->tid == TK_Arg_List);

    if ( !s7_is_list(s7, splice)) {
        log_error("Expected list, got %s",
                  s7_object_to_c_string(s7, splice));
        return NULL;
    }

    index = sealark_normalize_index(bindings, index);
    /* log_debug("normalized index: %d", index); */
    /* log_debug("splicing %s at locn: %d", s7_object_to_c_string(s7, splice), */
    /*           index); */

    int len = s7_list_length(s7, splice);
    struct node_s *new_binding_nd;

    int i = 0;
    s7_pointer iter = s7_make_iterator(s7, splice);
    s7_pointer new_binding = s7_iterate(s7, iter);
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        if ( !s7_is_c_object(new_binding)) {
            log_error("Binding :splice only accepts :binding nodes; got: %s of type %s",
                      s7_object_to_c_string(s7, new_binding),
                      s7_object_to_c_string(s7, s7_type_of(s7, new_binding)));
            errno = ESPLICE_BINDING_ARG;
            return NULL;
        }
        /* log_debug("splicing binding %d: %s", */
        /*           i, s7_object_to_c_string(s7, new_binding)); */
        new_binding_nd = s7_c_object_value(new_binding);
        /* new_binding_nd->line = -1; // mark as unformatted */
        /* log_debug("new_binding_nd %d %s", */
        /*           new_binding_nd->tid, TIDNAME(new_binding_nd)); */
        /* item must be binding */
        if (new_binding_nd->tid == TK_Binding) {
            errno = 0;
            sealark_arglist_insert_binding_at_int(bindings,
                                                  index + i,
                                                  new_binding_nd);
            if (errno != 0) {
                log_error("Error splicing...%d", errno);
                errno = ESPLICE_BINDING_ARG;
                return NULL;
            }
        } else {
            log_error("Trying to splice non-binding into bindings: %d %s",
                      new_binding_nd->tid, TIDNAME(new_binding_nd));
            errno = ESPLICE_BINDING_ARG;
            return NULL;
        }
        i++;
        new_binding = s7_iterate(s7, iter);
    }
    /* sealark_bindings_incr_lines_from_int(bindings, index+len); */
}

/* **************************************************************** */
LOCAL struct node_s *_binding_splice_vector(s7_scheme *s7,
                                          struct node_s *bindings,
                                          int index,
                                          s7_pointer splice)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("_binding_splice_vector at %d", index);
#endif
    assert(bindings->tid == TK_Arg_List);

    if ( !s7_is_vector(splice)) {
        log_error("Expected vector, got %s",
                  s7_object_to_c_string(s7, splice));
        errno = EINVALID_ARG;
        return NULL;
    }

    index = sealark_normalize_index(bindings, index);
    /* log_debug("normalized index: %d", index); */
    /* log_debug("splicing %s at locn: %d", s7_object_to_c_string(s7, splice), */
    /*           index); */

    int len = s7_vector_length(splice);
    s7_pointer new_binding;
    struct node_s *new_binding_nd;

    for (int i=0; i < len; i++) {
        /* log_debug("v xxxxxxxxxxxxxxxx"); */
        new_binding = s7_vector_ref(s7, splice, i);
        if ( !s7_is_c_object(new_binding)) {
            log_error("Binding :splice only accepts :binding nodes; got: %s of type %s",
                      s7_object_to_c_string(s7, new_binding),
                      s7_object_to_c_string(s7, s7_type_of(s7, new_binding)));
            errno = ESPLICE_BINDING_ARG;
            return NULL;
        }
        /* log_debug("splicing binding %d: %s", */
        /*           i, s7_object_to_c_string(s7, new_binding)); */
        new_binding_nd = s7_c_object_value(new_binding);
        /* log_debug("new_binding_nd %d %s", */
        /*           new_binding_nd->tid, TIDNAME(new_binding_nd)); */
        /* item must be binding */
        if (new_binding_nd->tid == TK_Binding) {
            errno = 0;
            sealark_arglist_insert_binding_at_int(bindings,
                                                  index + 1 + i,
                                                  new_binding_nd);
            if (errno != 0) {
                log_error("Error splicing...%d", errno);
                errno = ESPLICE_ERR;
                return NULL;
            }

        } else {
            log_error("Trying to splice non-binding into bindings: %d %s",
                      new_binding_nd->tid, TIDNAME(new_binding_nd));
            errno = ESPLICE_BINDING_ARG;
            return NULL;
        }
    }
}

