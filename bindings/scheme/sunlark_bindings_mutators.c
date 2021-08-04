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

    log_debug("updating attr-name");
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
    log_debug("TMP NAME: %d, %s", len, tmp_name);
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
        log_debug("updating string value");
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
        log_debug("updating string value");
        /* strings from s7 must not be freed? so... */
        int len = strlen(tmp_value) + 1;
        log_debug("TMP VALUE: %d, %s", len, tmp_value);
        char *new_value = calloc(len, sizeof(char));
        snprintf(new_value, len, "%s", tmp_value);

        free(target->s);
        target->s = new_value;
        return s7_make_string(s7, new_value);
    }

    if (target->tid == TK_List_Expr) {
        log_debug("updating list valued attrib");
        // for now, replacement
        if (s7_is_list(s7, val)) {
            log_debug("set! val is list: %s",
                      s7_object_to_c_string(s7, val));
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
            return sunlark_node_new(s7, result);
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
        log_debug("updating dict value (not yet)");
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
    sealark_debug_print_ast_outline(binding, true); // crush

#if defined(DEBUG_SET)
    log_debug("replacing %d %s with value of type %s",
              oldval->tid, TIDNAME(oldval),
              s7_object_to_c_string(s7, (s7_type_of(s7, newval))));
#endif
    /* utarray_free(val->subnodes); */
    /* utarray_clear(val->subnodes); */

    if (oldval->tid == TK_List_Expr) {
        if (s7_is_list(s7, newval)) {
            log_debug("replacing list with list");
            return sunlark_mutate_vector(s7, oldval, newval);
        }
    }

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

    if (s7_is_pair(newval)) {
        log_debug("NOT YET: replacing with pair");
        return binding;
    }

    if (s7_is_string(newval)) {
        const char *s = s7_string(newval);
        log_debug("new val: %s", s);
        int len = strlen(s);
        oldval->tid = TK_STRING;
        oldval->s = calloc(len, sizeof(char));
        strncpy(oldval->s, s, len);
        return binding;
    }

    if (s7_is_symbol(newval)) {
        const char *s = s7_symbol_name(newval);
        log_debug("new val: %s", s);
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
        log_debug("new val: %d", d);
        oldval->tid = TK_INT;
        oldval->s = calloc(len, sizeof(char));
        strncpy(oldval->s, buf, len);
        return binding;
    }

    if (s7_is_boolean(newval)) {
        log_debug("new val: %s", s7_object_to_c_string(s7, newval));
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
