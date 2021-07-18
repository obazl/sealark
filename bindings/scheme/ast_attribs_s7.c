#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "ast_attribs_s7.h"

/* docs at bottom */

static struct node_s *_add_attr_list_item(s7_scheme *s7,
                                          struct node_s *expr_list,
                                          s7_pointer edits)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("_add_attr_list_item, tid: %d", expr_list->tid);
#endif

    int subnode_ct = utarray_len(expr_list->subnodes);
    int attr_ct;
    struct node_s *last_node;
    s7_pointer additions;
    s7_pointer locn_s7 = NULL;
    char *locn_c;
    int attr_locn = 0; /* attr posn ignoring commas */
    int idx = 0;       /* subnode index corresponding to attr_locn */
    bool push_back = false;

    /* trailing comma? */
    // trailing comma not part of expr_list
    /* bool trailing_comma = false; */
    /* last_node = utarray_eltptr(expr_list->subnodes, subnode_ct - 1); */
    /* if (last_node->tid == TK_COMMA) { */
    /*     trailing_comma = true; */
    /* } */
    /* if (trailing_comma) { */
    /*     attr_ct = (subnode_ct - 1) / 2 + 1; */
    /* } else { */
    /*     attr_ct = subnode_ct / 2 + 1; /\* +1 for last node *\/ */
    /* } */
    /* log_debug("subnode_ct: %d, attr_ct: %d", subnode_ct, attr_ct); */

    attr_ct = (subnode_ct - 1) / 2 + 1;

    if ( s7_is_keyword(s7_cadr(edits)) ) {
        locn_s7 = s7_cadr(edits); /* :n */

        locn_c =  s7_object_to_c_string(s7, locn_s7);
        log_debug("locn_c: %s, %c", locn_c, locn_c[1]);
        s7_pointer sym = s7_keyword_to_symbol(s7, locn_s7);
        log_debug("SYM: %s", s7_object_to_c_string(s7, sym));
        attr_locn = atoi((char*)s7_symbol_name(sym));
        log_debug("attr_locn: %d", attr_locn);

        if (abs(attr_locn) > attr_ct) {
            /* subnode_ct = (subnode_ct + 1)/2; */
            log_error("ERROR: abs(edit location) %d > length %d of attr list value", abs(attr_locn), attr_ct);
            exit(EXIT_FAILURE); //FIXME
            /* return(s7_error(s7, */
            /*                 s7_make_symbol(s7, "invalid_arg"), */
            /*                 s7_list(s7, 2, */
            /*                         s7_make_string(s7, */
            /*                                        "edit locn ~D greater than length of values list ~A"), */
            /*                         locn, subnode_ct))); */
        }

        /* account for commas */
        if (attr_locn == 0) {
            if (locn_c[1] == '-') {
                push_back = true;
                additions = s7_cddr(edits);
            } else {
                /* push front */
                additions = s7_reverse(s7, s7_cddr(edits));
            }
        } else {
            if (attr_locn < 0) {
                attr_locn = attr_ct + attr_locn;
                if (attr_locn > 0)
                    idx = (attr_locn  * 2) - 1; /* no trailing , in list */
                additions = s7_reverse(s7, s7_cddr(edits));
            } else {
                if (attr_locn > 0) {
                    idx = (attr_locn  * 2) - 1; /* no trailing , in list */
                    additions = s7_reverse(s7, s7_cddr(edits));
                }
            }
        }
        log_debug("subnode_ct: %d, attr_ct %d, attr_locn: %d, idx: %d",
                  subnode_ct, attr_ct, attr_locn, idx);

    } else {
        /* appending */
        additions = s7_cdr(edits);
        push_back = true;
    }
    log_debug("subnode_ct: %d, attr_locn: %d, idx: %d", subnode_ct, attr_locn, idx);
    /* if (s7_list_length(s7, edits) == 2) { */
    /* if (locn_s7 == NULL) { */
        /* append */

        while(!s7_is_null(s7, additions)) {
            s7_pointer addition_s7 = s7_car(additions);
            char *addition =  s7_object_to_c_string(s7, addition_s7);
            int addition_len = strlen(addition);
            log_debug("addition: %s", addition);
            struct node_s *comma = calloc(1, sizeof(struct node_s));
            comma->tid = TK_COMMA;

            struct node_s *newnode = calloc(1, sizeof(struct node_s));
            newnode->tid = TK_STRING;
            newnode->s = calloc(addition_len + 1, sizeof(char));
            strncpy(newnode->s, addition, addition_len);
            log_debug("newnode->s: %s", newnode->s);
            newnode->qtype = DQUOTE;

            log_debug("attr_locn: %d, idx: %d", attr_locn, idx);
            if (idx == 0) {
                if (push_back) {
                    /* if (trailing_comma) { */
                    /*     log_debug("push back x"); */
                    /*     utarray_push_back(expr_list->subnodes, newnode); */
                    /*     trailing_comma = false; */
                    /* } else { */
                    log_debug("push back y");
                    utarray_push_back(expr_list->subnodes, comma);
                    utarray_push_back(expr_list->subnodes, newnode);
                    /* } */
                } else {
                    log_debug("push front");
                    utarray_insert(expr_list->subnodes, comma, idx);
                    utarray_insert(expr_list->subnodes, newnode, idx);
                }
            } else {
                log_debug("insert at %d", idx);
                utarray_insert(expr_list->subnodes, newnode, idx);
                utarray_insert(expr_list->subnodes, comma, idx);
                /* locn += 1; */
            }
            /* utarray_insert(expr_list->subnodes, comma, subnode_ct); */
            /* utarray_insert(expr_list->subnodes, newnode, subnode_ct+1); */

            additions = s7_cdr(additions);
        }
        /* we're creating a new s7 node for an existing c node
           - what about the existing s7 node? */
        return expr_list;
    }

static s7_pointer _update_list_value(s7_scheme *s7,
                              struct node_s *node,
                              const char *key,
                              s7_pointer edits)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("_update_list_value, tid: %d, key: %s", node->tid, key);
#endif

    if (node->tid != TK_List_Expr) {
        /* should not happen */
        log_error("ERROR: got node that is not a list-expr");
        exit(EXIT_FAILURE);
    }

    /* :named_arg structure: */
    /*   child[0] = '[', child[1] = expr_list, child[2] = ']'*/
    struct node_s *expr_list = utarray_eltptr(node->subnodes, 1);

    log_debug("edits: %s", s7_object_to_c_string(s7, edits));
    s7_pointer action = s7_car(edits);
    log_debug("action: %s", s7_object_to_c_string(s7, action));
    if (!s7_is_keyword(action)) {
        return(s7_wrong_type_arg_error(s7,
                                       "edits list",
                                       1, action,
                                       "a keyword"));
    }
    if (action == s7_make_keyword(s7, "add")) {
        log_debug("ACTION ADD");
        int editlen = s7_list_length(s7, edits);
        if (editlen < 2) {
            return(s7_wrong_number_of_args_error(s7,
               "edit list for :add must at least two elements: ~S", edits));
        }
        struct node_s *new_expr_list
            = _add_attr_list_item(s7, expr_list, edits);
        return ast_node_s7_new(s7, new_expr_list);
    }
    if (action == s7_make_keyword(s7, "replace")) {
        log_debug("ACTION REPLACE");
        int editlen = s7_list_length(s7, edits);
        if (editlen != 2) {
            return(s7_wrong_number_of_args_error(s7,
               "edit list for :replace must have length 2: ~S", edits));
        }
        struct node_s *new_expr_list
            = _add_attr_list_item(s7, expr_list, edits);
        return ast_node_s7_new(s7, new_expr_list);
    }
    if (action == s7_make_keyword(s7, "remove")) {
        log_debug("ACTION REMOVE");
        return s7_nil(s7);
    }
    return(s7_error(s7,
                    s7_make_symbol(s7, "invalid_arg"),
                    s7_list(s7, 2,
                            s7_make_string(s7,
                                           "action must be one of :add :replace :remove; got ~A"),
                                            action)));
}

s7_pointer ast_attrs_update_attribute_value(s7_scheme *s7,
                                            struct node_s *node,
                                            const char *key,
                                            s7_pointer val)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("ast_attrs_update_attribute_value, tid: %d",
              node->tid);
#endif

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
            return _update_list_value(s7, target, key, val);
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
