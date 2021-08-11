#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_vector_mutators.h"

/* **************** */
//FIXME: also update rbrack position?
/*  replace _content_ of list_expr, not the whole expr */
struct node_s *sunlark_mutate_vector(s7_scheme *s7,
                                        struct node_s *old_vec,
                                        s7_pointer new_vec)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATORS)
    log_debug("sunlark_mutate_vector => %s",
              s7_object_to_c_string(s7, new_vec));
#endif
#if defined(DEBUG_AST)
    sealark_debug_log_ast_outline(old_vec, 4);
#endif

    assert(old_vec->tid == TK_List_Expr);

    int new_ct = s7_list_length(s7, new_vec);

    /* :list_expr > :lbrack, :expr_list, :rbrack */
    struct node_s *old_items = utarray_eltptr(old_vec->subnodes, 1);
    int old_ct = (utarray_len(old_items->subnodes) + 1) / 2;
    log_debug("old_ct: %d", old_ct);

    struct node_s *old_item;

    s7_pointer new_item;
    /* need both, since an int/string list can contain syms (vars) */
    int new_vec_type;
    int new_item_type;

    if (s7_is_null(s7, new_vec)) {
        log_debug("emptying vector");
        utarray_clear(old_items->subnodes);
        struct node_s *lbrack = utarray_eltptr(old_vec->subnodes, 0);
        struct node_s *rbrack = utarray_eltptr(old_vec->subnodes, 2);
        rbrack->line = lbrack->line;
        rbrack->col = lbrack->col + 1;
        utarray_erase(old_vec->subnodes, 1,1);
        return old_vec;
    }

    if (s7_car(new_vec) == KW(remove!)) {
        sunlark_remove_attr_list_items(s7, old_items, s7_cdr(new_vec));
        return old_vec;
    }

    /* first element sets element type */
    /* BUT: what if first elt is sym? e.g. (myvar 8 9) */
    new_vec_type = sunlark_infer_vector_type_from_list(s7, new_vec);
    if (new_vec_type < 0) {
        //FIXME: throw s7 error
        log_error("bad vec type");
        return NULL;
    }
    log_debug("new_vec_type: %d %s",
              new_vec_type, token_name[new_vec_type][0]);

    const char *new_str;
    int new_str_len;

    /* FIXME: handle case where new item ct > old item ct */

    /* default: replace */
    int i = 0;
    int new_idx = 0;
    while( ! s7_is_null(s7, new_vec) ) {
        log_debug("new_idx: %d", new_idx);
        new_item = s7_car(new_vec);
        if (s7_is_string(new_item)) {
            new_item_type = TK_STRING;
            new_str = s7_string(new_item);
        }
        if (s7_is_symbol(new_item)) {
            new_item_type = TK_ID;
            new_str = s7_symbol_name(new_item);
        }
        if (s7_is_integer(new_item)) {
            new_item_type = TK_INT;
            int new_int = s7_integer(new_item);
            char ibuf[64];
            snprintf(ibuf, 64, "%d", new_int);
            new_str = (char*)ibuf;
        }
        new_str_len = strlen(new_str);
        log_debug("new_str: %s (%d %s)",
                  new_str, new_item_type,
                  token_name[new_item_type][0]);

        /* if (new_item_idx > old_items_ct) { */
        /*     malloc, init, add new item */
        /* } else { */

        if (new_idx > old_ct-1) {
            log_debug("extending vec");
            struct node_s *newnode = sealark_new_node(TK_COMMA, without_subnodes);
            /* newnode->tid = TK_COMMA; */
            //FIXME: set line, col
            utarray_push_back(old_items->subnodes, newnode);

            newnode = sealark_new_node(new_item_type, with_subnodes);
            newnode->tid = new_item_type;
            if (new_item_type == TK_STRING) {
                ; //newnode->qtype = DQUOTE;
            }
            /* newnode->s = calloc(new_str_len, sizeof(char)); */
            strncpy(newnode->s, new_str, new_str_len);
            //FIXME: set line, col
            utarray_push_back(old_items->subnodes, newnode);
        } else {

            old_item = utarray_eltptr(old_items->subnodes, i);
            log_debug("old item tid: %d %s",
                      old_item->tid, TIDNAME(old_item));

            /* skip commas but include symbols */
            if (old_item->tid == TK_COMMA) {
                i++;
                old_item = utarray_eltptr(old_items->subnodes, i);
            }

            /* now the update */
            log_debug("replacing old: %s", old_item->s);
            free(old_item->s);
            old_item->s = calloc(new_str_len, sizeof(char));
            strncpy(old_item->s, new_str, new_str_len);
            old_item->tid = new_item_type; // in case new item is sym
        }
        /* } else { */
        /*     if (old_item->tid == TK_ID) { */
        /*         log_warn("FIXME: update sym/ID"); */
        /*     } */
        /* } */

        /* FIXME: this logic is for updating items, not replacing vec */
        /* skip commas but include symbols */
        /* while( (old_item->tid != new_vec_type) */
        /*        && (old_item->tid != TK_ID)) { */
        /*     log_debug("\told item tid: %d %s", */
        /*               old_item->tid, TIDNAME(old_item)); */
        /*     i++; */
        /*     old_item = utarray_eltptr(old_items->subnodes, i); */
        /* } */
        /* i++; */

        /* if (old_item->tid == new_vec_type) { */
        /*     log_debug("updating old: %s", old_item->s); */
        /*     free(old_item->s); */
        /*     old_item->s = calloc(new_str_len, sizeof(char)); */
        /*     strncpy(old_item->s, new_str, new_str_len); */
        /*     old_item->tid = new_item_type; // in case new item is sym */
        /* } else { */
        /*     if (old_item->tid == TK_ID) { */
        /*         log_warn("FIXME: update sym/ID"); */
        /*     } */
        /* } */

        i++;
        new_idx++;
        new_vec = s7_cdr(new_vec);
    }

    if (new_ct < old_ct) {
        log_debug("removing extras");
        utarray_resize(old_items->subnodes, new_ct * 2 - 1);
    }
    /* old_vec was updated-in-place */
    return old_vec;
}

LOCAL struct node_s *_add_attr_list_item(s7_scheme *s7,
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
            errno = ESUNLARK_INVALID_ARG;
            return NULL;
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
        /* newnode->qtype = DQUOTE; */

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

struct node_s *_replace_attr_list_all(s7_scheme *s7,
                                      struct node_s *expr_list,
                                      char *replacement)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("_replace_attr_list_all, tid: %d", expr_list->tid);
#endif

    utarray_clear(expr_list->subnodes);
    struct node_s *node = sealark_new_node(TK_STRING, with_subnodes);
    /* node->tid = TK_STRING; */
    int len = strlen(replacement);
    /* node->s = calloc(1, len + 1); */
    strncpy(node->s, replacement, len);
    log_debug("node->s: %s", node->s);
    /* node->qtype = DQUOTE; */

    utarray_push_back(expr_list->subnodes, node);
    return expr_list;
}

struct node_s *_remove_attr_list_all(s7_scheme *s7,
                                     struct node_s *expr_list)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("_remove_attr_list_all, tid: %d", expr_list->tid);
#endif

    utarray_clear(expr_list->subnodes);
    /* struct node_s *node = sealark_new_node(); */
    /* node->tid = TK_STRING; */
    /* int len = strlen(replacement); */
    /* node->s = calloc(1, len + 1); */
    /* strncpy(node->s, replacement, len); */
    /* log_debug("node->s: %s", node->s); */
    /* node->qtype = DQUOTE; */

    /* utarray_push_back(expr_list->subnodes, node); */
    return expr_list;
}

struct node_s *_replace_attr_list_items(s7_scheme *s7,
                                    struct node_s *expr_list,
                                    s7_pointer edits)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("_replace_attr_list_items, tid: %d", expr_list->tid);
#endif

    log_debug("edits: %s", s7_object_to_c_string(s7, edits));

    int subnode_ct = utarray_len(expr_list->subnodes);
    int attr_ct;
    struct node_s *last_node;
    s7_pointer replacements;
    s7_pointer locn_s7 = NULL;
    char *locn_c;
    int attr_locn = 0; /* attr posn ignoring commas */
    int idx = 0;       /* subnode index corresponding to attr_locn */
    bool push_back = false;

    attr_ct = (subnode_ct - 1) / 2 + 1;
    log_debug("subnode_ct: %d, attr_ct: %d", subnode_ct, attr_ct);

    while(!s7_is_null(s7, edits)) {
        s7_pointer replacement_s7 = s7_cadr(edits);
        char *replacement =  s7_object_to_c_string(s7, replacement_s7);
        int replacement_len = strlen(replacement);
        if (s7_is_string(replacement_s7)) {
            /* omit quote marks */
            replacement_len -= 2;
            replacement++;
        } else {
        }
        log_debug("replacement: %s", replacement);

        if ( s7_is_keyword(s7_car(edits)) ) {
            locn_s7 = s7_car(edits); /* :n */
            locn_c =  s7_object_to_c_string(s7, locn_s7);
            log_debug("locn_c: %s, %c", locn_c, locn_c[1]);
            s7_pointer sym = s7_keyword_to_symbol(s7, locn_s7);
            log_debug("SYM: %s", s7_object_to_c_string(s7, sym));
            if (strncmp("*", s7_object_to_c_string(s7, sym), 1) == 0) {
                return _replace_attr_list_all(s7, expr_list, replacement);
            } else {
                attr_locn = atoi((char*)s7_symbol_name(sym));
            }
            log_debug("attr_locn: %d", attr_locn);

            if (abs(attr_locn) > attr_ct) {
                /* subnode_ct = (subnode_ct + 1)/2; */
                log_error("ERROR: abs(edit location) %d > length %d of attr list value", abs(attr_locn), attr_ct);
                errno = ESUNLARK_INVALID_ARG;
                return NULL;
            }

            if (attr_locn < 0) {
                    attr_locn = attr_ct + attr_locn;
                    if (attr_locn > 0)
                        idx = (attr_locn  * 2) - 1; /* no trailing , in list */
            } else {
                idx = (attr_locn  * 2);
            }

            log_debug("subnode_ct: %d, attr_ct %d, attr_locn: %d, idx: %d",
                      subnode_ct, attr_ct, attr_locn, idx);
        } else {
            log_error("ERROR: invalid location node, should be :n, got %s",
                      s7_object_to_c_string(s7, locn_s7));
            errno = ESUNLARK_ARG_TYPE_ERR;
            return NULL;
        }

        /* struct node_s *newnode = calloc(1, sizeof(struct node_s)); */
        /* newnode->tid = TK_STRING; */
        /* newnode->s = calloc(replacement_len + 1, sizeof(char)); */
        /* strncpy(newnode->s, replacement, replacement_len); */
        /* log_debug("newnode->s: %s", newnode->s); */
        /* newnode->qtype = DQUOTE; */

        struct node_s *target = utarray_eltptr(expr_list->subnodes, idx);
        log_debug("target[%d] tid: %d, s: %s",
                  idx, target->tid, target->s);
        if (target->tid != TK_STRING) {
            log_error("ERROR: replacement target wrong type: %d (should be %d, TK_STRING)", target->tid, TK_STRING);
            errno = ESUNLARK_ARG_TYPE_ERR;
        }

        target->tid = TK_STRING;
        target->s = calloc(replacement_len + 1, sizeof(char));
        strncpy(target->s, replacement, replacement_len);
        log_debug("target->s: %s", target->s);
        /* target->qtype = DQUOTE; */

        edits = s7_cddr(edits);
    }
    /* we're creating a new s7 node for an existing c node
       - what about the existing s7 node? */
    return expr_list;
}

/*FIXME: remove from right to left, so indexing will work after removals */
struct node_s *sunlark_remove_attr_list_items(s7_scheme *s7,
                                              struct node_s *expr_list,
                                              s7_pointer edits)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("sunlark_remove_attr_list_items, tid: %d", expr_list->tid);
#endif

    log_debug("edits: %s", s7_object_to_c_string(s7, edits));

    int subnode_ct = utarray_len(expr_list->subnodes);
    int attr_ct;
    struct node_s *last_node;
    s7_pointer removoids;
    s7_pointer locn_s7 = NULL;
    char *locn_c;
    int attr_locn = 0; /* attr posn ignoring commas */
    int idx = 0;       /* subnode index corresponding to attr_locn */
    bool push_back = false;

    attr_ct = (subnode_ct - 1) / 2 + 1;
    log_debug("subnode_ct: %d, attr_ct: %d", subnode_ct, attr_ct);

    s7_pointer tmp = edits, editem;
    while( ! s7_is_null(s7, tmp) ) {
        editem = s7_car(tmp);
        if (abs((int)s7_integer(editem)) >= attr_ct) {
            log_error("ERROR: abs(%d) (edit locn) >= length %d of attr list value", s7_integer(editem), attr_ct);
            /* return(s7_error(s7, */
            /*                 s7_make_symbol(s7, "invalid_argument"), */
            /*                 s7_list(s7, 2, s7_make_string(s7,                           "ERROR: abs(%~A) (edit locn) >= length ~A of attr list value"), */
            /*                         editem, s7_make_integer(s7,attr_ct)))); */
            errno = ESUNLARK_INVALID_ARG;
            return NULL;
        }
        tmp = s7_cdr(tmp);
    }

    /* for multiple removals, e.g. (:remove :0 :1), we need to account
       for elements already removed as we iterate over the list. */
    int remove_ct = 0;

    while(!s7_is_null(s7, edits)) {
        if ( s7_is_integer(s7_car(edits)) ) {
            locn_s7 = s7_car(edits); /* :n */
            attr_locn = s7_integer(locn_s7);
            /* locn_c =  s7_object_to_c_string(s7, locn_s7); */
            /* log_debug("locn_c: %s, %c", locn_c, locn_c[1]); */
            /* s7_pointer sym = s7_keyword_to_symbol(s7, locn_s7); */
            /* log_debug("SYM: %s", s7_object_to_c_string(s7, sym)); */
            /* if (strncmp("*", s7_object_to_c_string(s7, sym), 1) == 0) { */
            /*     utarray_clear(expr_list->subnodes); */
            /*     return expr_list; */
            /* } else { */
            /*     attr_locn = atoi((char*)s7_symbol_name(sym)); */
            /* } */
            log_debug("attr_locn: %d", attr_locn);

            if (abs(attr_locn) >= attr_ct) {
                /* subnode_ct = (subnode_ct + 1)/2; */
                log_error("ERROR: abs(edit location) %d >= length %d of attr list value", abs(attr_locn), attr_ct);
                errno = ESUNLARK_INVALID_ARG;
                return NULL;
            }

            if (attr_locn < 0) {
                    attr_locn = attr_ct + attr_locn;
                    if (attr_locn > 0)
                        idx = (attr_locn  * 2) - 1; /* no trailing , in list */
            } else {
                idx = (attr_locn  * 2);
                if (attr_locn == attr_ct - 1) {
                    /* last node: also remove preceding comma */
                    idx--;
                /* } else { */
                /*     idx = (attr_locn  * 2); */
                }
            }
            idx -= remove_ct;
            log_debug("subnode_ct: %d, attr_ct %d, attr_locn: %d, idx: %d, remove_ct: %d",
                      subnode_ct, attr_ct, attr_locn, idx, remove_ct);
        } else {
            log_error("ERROR: invalid location node, should be :n, got %s",
                      s7_object_to_c_string(s7, s7_car(edits)));
            errno = ESUNLARK_LOCN_ARG_ERR;
            return NULL;
        }

        struct node_s *target = utarray_eltptr(expr_list->subnodes, idx);
        log_debug("target[%d] tid: %d %s, s: %s",
                  idx, target->tid, TIDNAME(target), target->s);

        /* last element: remove preceding comma too */
        /* if (target->tid != TK_STRING) { */
        /*     log_error("ERROR: removoid target wrong type: %d (should be %d, TK_STRING)", target->tid, TK_STRING); */
        /*     errno = ESUNLARK_ARG_TYPE_ERR; */
        /* } */

        log_debug("erasing %d for len %d", idx, 2);
                  /* (attr_locn == attr_ct)? 1 :2); */
        // FIXME: handle end of list, never trailing comma
        utarray_erase(expr_list->subnodes, idx, 2);
                      /* (attr_locn == attr_ct - 1)? 1 :2); */
        remove_ct += 2;

        edits = s7_cdr(edits);
    }
    /* we're creating a new s7 node for an existing c node
       - what about the existing s7 node? */
    return expr_list;
}

/* **************** */
/* action: replace or splice */
/* selector: key int, or val (string or symbol) */
s7_pointer sunlark_vector_mutate_item(s7_scheme *s7,
                                       s7_pointer _list_expr,
                                       int index,
                                       /* s7_pointer selector, */
                                       s7_pointer newval)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATORS)
    log_debug("sunlark_vector_mutate_item @ %d => %s",
              index,
              /* s7_object_to_c_string(s7, selector), */
              s7_object_to_c_string(s7, newval));
#endif

    struct node_s *list_expr = s7_c_object_value(_list_expr);
    assert(list_expr->tid == TK_List_Expr);

#if defined(DEBUG_AST)
    sealark_debug_log_ast_outline(list_expr, 0);
#endif

    struct node_s *vector = utarray_eltptr(list_expr->subnodes, 1);
    assert(vector->tid == TK_Expr_List);
    int vec_type = sunlark_infer_expr_list_type(vector);
    log_debug("vec_type %d %s", vec_type, token_name[vec_type][0]);

    if (s7_is_procedure(newval)) {
        log_debug("ACTION: procedure");
        return NULL;
    }

    if (s7_is_list(s7, newval)) {
        s7_pointer action = s7_car(newval);
        if (action == KW(splice)) {
            return _vector_splice(s7, _list_expr, index, s7_cdr(newval));
        }
        if (action == KW(append)) {
            log_debug("0 xxxxxxxxxxxxxxxx");
            /* return _vector_append(s7, _list_expr, index, s7_cdr(newval)); */
        }
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                       "Invalid mutate action: ~A. Only :splice supported"),
                                newval)));
    } else {
        return _vector_mutate_item_at_index(s7,_list_expr, index, newval);
    }
}

/* **************** */
LOCAL s7_pointer _vector_splice(s7_scheme *s7,
                                       s7_pointer _list_expr,
                                       int index,
                                        s7_pointer _splice)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATORS)
    log_debug(">> _vector_splice @ %d => %s",
              index, s7_object_to_c_string(s7, _splice));
#endif

    struct node_s *list_expr = s7_c_object_value(_list_expr);
    assert(list_expr->tid == TK_List_Expr);

    if ( !s7_is_list(s7, _splice)) {
        log_error("Expected list, got %s", s7_object_to_c_string(s7, _splice));
        return NULL;
    }

    s7_pointer splice = s7_car(_splice);
    if ( !s7_is_vector(splice) ) {
        log_error("Expected vector, got %s",
                  s7_object_to_c_string(s7, splice));
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                        ":splice must be followed by vector, got: ~A"),
                                splice)));
    }

    index = sealark_normalize_index(list_expr, index);
    log_debug("normalized index: %d", index);

    log_debug("splicing %s at locn: %d", s7_object_to_c_string(s7, splice),
              index);

    int len = s7_vector_length(splice);
    for (int i=0; i < len; i++) {
        s7_pointer item = s7_vector_ref(s7, splice, i);
        log_debug("splicing item %d: %s", i, s7_object_to_c_string(s7, item));
        /* item could be any supported type */
        if (s7_is_integer(item)) {
            errno = 0;
            sealark_vector_insert_int_at_index(list_expr,
                                               s7_integer(item),
                                               index + i);
            if (errno != 0) {
                log_error("Error splicing...");
            }
        }
    }
}

/* **************** */
LOCAL s7_pointer _vector_mutate_item_at_index(s7_scheme *s7,
                                       s7_pointer _list_expr,
                                       int index,
                                       /* s7_pointer selector, */
                                       s7_pointer newval)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATORS)
    log_debug(">> _vector_mutate_item_at_index @ %d => %s",
              index, s7_object_to_c_string(s7, newval));
#endif

    struct node_s *list_expr = s7_c_object_value(_list_expr);
    assert(list_expr->tid == TK_List_Expr);

#if defined(DEBUG_AST)
    sealark_debug_log_ast_outline(list_expr, 0);
#endif

    struct node_s *vector = utarray_eltptr(list_expr->subnodes, 1);
    assert(vector->tid == TK_Expr_List);
    int vec_type = sunlark_infer_expr_list_type(vector);
    log_debug("vec_type %d %s", vec_type, token_name[vec_type][0]);

    if (s7_is_list(s7, newval)) {
        s7_pointer action = s7_car(newval);
        if (action == KW(splice)) {
            log_debug("ACTION: splice");
        } else {
            return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                    "Invalid mutate action: ~A. Only :splice supported"),
                                    newval)));
        }
    } else {
        log_debug("ACTION: replace");
    }

    /* get the item to update */
    struct node_s *item;
    item = sealark_vector_item_for_int(list_expr, index);
    if (!item)
        return handle_errno(s7, errno,
                            s7_list(s7, 2, _list_expr,
                                    s7_make_integer(s7,index)));

    /* we have the item, now update it */

    /* if (s7_is_integer(selector)) { /\* index by int *\/ */
        if (s7_is_string(newval)) {
            if (vec_type != TK_STRING) {
                log_error("Trying to insert string item into list of %d %s",
                          vec_type, token_name[vec_type][0]);
                return(s7_error(s7,
                                s7_make_symbol(s7, "invalid_argument"),
                                s7_list(s7, 3, s7_make_string(s7,
                   "Cannot put string ~S in list of type ~A"),
                                        newval,
                          s7_make_string(s7, token_name[vec_type][0]))));
            }
            log_debug("replacing item in string list");
            struct node_s *newitem
                = sealark_set_string(item, 0, s7_string(newval));
            /* free(item->s); */
            /* const char *newstring = s7_string(newval); */
            /* int len = strlen(newstring); */
            /* item->s = calloc(len, sizeof(char)); */
            /* strncpy(item->s, newstring, len); */
            return _list_expr;
        }
        if (s7_is_integer(newval)) {
            if (vec_type != TK_INT) {
                log_error("Trying to insert int item into list of type %d %s",
                          TK_STRING, token_name[TK_STRING][0]);
                return(s7_error(s7,
                                s7_make_symbol(s7, "invalid_argument"),
                            s7_list(s7, 3, s7_make_string(s7,
                        "Cannot put val ~A into list of type ~A"),
                                newval, s7_make_string(s7, token_name[vec_type][0]))));
            }
            struct node_s *newitem
                = sealark_set_int(item, s7_integer(newval));
            return _list_expr;
        }
        if (s7_is_keyword(newval)) {
            if (newval == KW(null)) {
                log_debug("nullifying (removing) item from list");
                errno = 0;
                struct node_s *updated_vector
                    = sealark_vector_remove_item(vector, index);
                if (updated_vector)
                    return _list_expr;
                else
                    return handle_errno(s7, errno,
                                        s7_list(s7, 2, _list_expr,
                                                s7_make_integer(s7,index)));
            } else {
                log_error("Cannot set item to keyword: %s",
                          s7_object_to_c_string(s7, newval));
                return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                                s7_list(s7, 2, s7_make_string(s7,
                              "Cannot set item to keyword ~A"),
                                    newval)));

            }
        }
        if (s7_is_symbol(newval)) {
            log_debug("replacing item in list with symbol");
            struct node_s *newitem
                = sealark_set_symbol(item, s7_symbol_name(newval));
            return _list_expr;
        }
        if (s7_is_c_object(newval)) {
            log_debug("replacing item in list with c-object");
            struct node_s *newval_node = s7_c_object_value(newval);
            if (newval_node->tid == TK_STRING) {
                if (vec_type == TK_STRING) {
                    struct node_s *newitem
                        = sealark_set_string_c_object(item, newval_node);
                    return _list_expr;
                } else {
                    log_error("only sunlark strings can be used to update string list items");
                    exit(EXIT_FAILURE);
                }
            }
        }

        if (s7_is_list(s7, newval)) {
            if (KW(splice) == s7_car(newval)) {
                s7_pointer lst = s7_cadr(newval);
                if ( ! s7_is_vector(lst) ) {
                    return(s7_error(s7,
                                    s7_make_symbol(s7, "invalid_argument"),
                                    s7_list(s7, 3, s7_make_string(s7,
         ":splice must be followed by a vector; found: ~A of type ~A"),
                                            lst,
                    s7_type_of(s7, lst))));
                }
                log_debug("splicing: %s", s7_object_to_c_string(s7, lst));
                log_debug("item: %d %s", item->tid, TIDNAME(item));
            }
            if (KW(remove!) == s7_car(newval)) {
                struct node_s *newitem
                    = sunlark_remove_attr_list_items(s7, item, s7_cdr(newval));
                    /* = sunlark_update_list_value(s7, item, "foo", newval); */
                if (newitem)
                    return _list_expr;
                else
                    return(s7_error(s7,
                                    s7_make_symbol(s7, "invalid_argument"),
                                    s7_list(s7, 2, s7_make_string(s7,
                                            "Arg out of bounds ~A"),
                                            newval)));
            }
        }
        log_error("Type mismatch: newval %s, list type %d %s",
                  s7_object_to_c_string(s7, newval),
                  vec_type, token_name[vec_type][0]);
        return(s7_error(s7,
                        s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 3, s7_make_string(s7,
                        "Cannot put val ~A into list of type ~A"),
                                newval, s7_make_string(s7, token_name[vec_type][0]))));
    /* } */

}

/* ******************************** */
struct node_s *sunlark_update_list_value(s7_scheme *s7,
                              struct node_s *list_expr_node,
                              const char *key,
                              s7_pointer edits)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_ATTR)
    log_debug("_update_list_value, tid: %d, key: %s", list_expr_node->tid, key);
#endif

    if (list_expr_node->tid != TK_List_Expr) {
        /* should not happen */
        log_error("ERROR: got node that is not a list-expr");
        exit(EXIT_FAILURE);
    }

    /* :binding structure: */
    /*   child[0] = '[', child[1] = expr_list, child[2] = ']'*/
    struct node_s *expr_list = utarray_eltptr(list_expr_node->subnodes, 1);

    log_debug("edits: %s", s7_object_to_c_string(s7, edits));
    s7_pointer action = s7_car(edits);
    log_debug("action: %s", s7_object_to_c_string(s7, action));
    if (!s7_is_keyword(action)) {
        log_error("ERROR: action arg should be :add! or :remove! (got %s)", s7_object_to_c_string(s7, action));
        errno = ESUNLARK_INVALID_ARG;
        return NULL;
    }
    if (action == s7_make_keyword(s7, "add!")) {
        log_debug("ACTION ADD");
        int editlen = s7_list_length(s7, edits);
        if (editlen < 2) {
            log_error("edit list for :add must at least two elements: %s",
                      s7_object_to_c_string(s7, action));
            errno = ESUNLARK_INVALID_ARG;
            return NULL;
        }
        struct node_s *new_expr_list
            = _add_attr_list_item(s7, expr_list, edits);
        return new_expr_list;
        /* return sunlark_new_node(s7, new_expr_list); */
    }
    if (action == s7_make_keyword(s7, "replace")) {
        log_debug("ACTION REPLACE");
        int editlen = s7_list_length(s7, s7_cdr(edits));
        if ( (editlen % 2) != 0 ) {
            log_error("ERROR: edit list for :add must have an even number of elements: %s", s7_object_to_c_string(s7, action));
            errno = ESUNLARK_INVALID_ARG;
            return NULL;
        }
        errno = 0;
        struct node_s *new_expr_list
            = _replace_attr_list_items(s7, expr_list, s7_cdr(edits));
        if (new_expr_list == NULL) {
            /* errno already set, msg printed */
            return NULL;
        }
        /* return sunlark_new_node(s7, new_expr_list); */
        return new_expr_list;
   }
    if (action == s7_make_keyword(s7, "remove!")) {
        log_debug("ACTION REMOVE");
        int editlen = s7_list_length(s7, s7_cdr(edits));
        if ( editlen == 0 ) {
            log_error("ERROR: edit list for :remove must have at least one arg: %s", s7_object_to_c_string(s7, action));
            errno = ESUNLARK_INVALID_ARG;
            return NULL;
        }
        errno = 0;
        struct node_s *new_expr_list
            = sunlark_remove_attr_list_items(s7, expr_list, s7_cdr(edits));
        if (new_expr_list == NULL) {
            /* errno already set, msg printed */
            return NULL;
        }
        /* return sunlark_new_node(s7, new_expr_list); */
        return new_expr_list;
    }
    log_error("ERROR: action must be one of :add :replace :remove; got %s",
              s7_object_to_c_string(s7, action));
    errno = ESUNLARK_INVALID_ARG;
    return NULL;
}
