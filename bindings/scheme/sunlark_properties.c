#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_properties.h"

/* common properties/ops/predicates:

   c struct fields:  :tid, :line, :col,
                     :subnodes, :comments,
                     :s, :qtype, :trailing_newline

   pseudo props: :tid->kw, :tik->string,
                 :printable?
                 :count (only in path expr)

   ops:  :print, :pprint

 */


/* target == Call_Expr */
s7_pointer sunlark_target_property_lookup(s7_scheme *s7,
                                          struct node_s *self,
                                          s7_pointer prop_kw)
                                          /* s7_pointer args) */
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_target_property_lookup: %s",
              s7_object_to_c_string(s7, prop_kw));
#endif

    /* if (prop_kw == KW(rule)) { /\* e.g. cc_library *\/ */
    /*     struct node_s *id = utarray_eltptr(self->subnodes, 0); */
    /*     if (id->tid != TK_ID) { */
    /*         log_error("ERROR: expected tid %d for target, got: %d", */
    /*                   TK_ID, id->tid); */
    /*         return NULL; */
    /*     } */
    /*     s7_pointer str =  s7_make_string(s7, id->s); */
    /*     return str; */
    /* } */

    if (prop_kw == KW(attrs)) {
        /* log_debug("matched :attrs"); */
        struct node_s *attrs = sunlark_get_attrs_list(s7, self);
        log_debug("got attrs: %d %s", attrs->tid, token_name[attrs->tid][0]);
        return sunlark_new_node(s7, self);

        /* } else { */
        /*     /\* the only propert :attr-list understands is :<attrname> *\/ */
        /*     return sunlark_attr_list_kw_lookup(s7, attrs, args); */
        /* } */
    }

    if (prop_kw == KW(rule)) {
        /* struct node_s *node = s7_c_object_value(self); */
        struct node_s *rulename = sealark_ruleid_for_target(self);
        return sunlark_new_node(s7, rulename); /* tid :id */
    }

    /* common */
    s7_pointer result = sunlark_common_property_lookup(s7, self, prop_kw);
    if (result == NULL) {
    /*     /\* by elimination, prop_kw must be an binding name *\/ */
    /*     result = sunlark_attr_list_kw_lookup(s7, self, prop_kw); */
    /* } else { */
        return(s7_error(s7, s7_make_symbol(s7,
                                           "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                                                      "ast-node-ref target arg must be one of :rule, :attrs, :tid, :line, :col, :trailine_newline, :s, :qtype, :subnodes, :comment; got ~A"),
                                prop_kw)));
    } else {
        return result;
    }
}

/* bindings have two predefined props: :name, :value */
s7_pointer sunlark_binding_property_lookup(s7_scheme *s7,
                                             struct node_s *attr_node,
                                             const char *key)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_binding_property_lookup tid %d, prop: %s",
              attr_node->tid, key);
#endif

    struct node_s *c_result;

    if ( strncmp(key, "name", 4) == 0 ) {
        c_result = utarray_eltptr(attr_node->subnodes, 0);
        return sunlark_new_node(s7, c_result);
        /* return sunlark_update_binding_name(s7, node_s7, key, val); */
    }

    if ( strncmp(key, "value", 5) == 0) {
        c_result = utarray_eltptr(attr_node->subnodes, 2);
        return sunlark_new_node(s7, c_result);
        /* return sunlark_update_binding_value(s7, node_s7, key, val); */
    }

    s7_pointer result = sunlark_common_property_lookup(s7, attr_node,
                                                       s7_make_keyword(s7, key));
    if (result == NULL) {
        return(s7_wrong_type_arg_error(s7,
                                       "attr prop lookup",
                                       2,
                                       s7_make_keyword(s7, key),
                                       "'name or 'value"));
    } else {
        return result;
    }
}

/* **************************************************************** */
/* nodelist properties */
s7_pointer sunlark_nodelist_property_lookup(s7_scheme *s7,
                                            s7_pointer nodelist,
                                            s7_pointer key)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_nodelist_property_lookup tid %d, prop: %s",
              sunlark_node_tid(s7, nodelist),
              s7_object_to_c_string(s7, key));
#endif

    if (key == KW(tid)) {
        return s7_make_integer(s7, TK_Node_List);
    }
    if (key == KW(tid->kw)) {
        return KW(node_list);
    }
    if (key == KW(tid->string)) {
        return s7_make_string(s7, "node_list");
    }
    if (key == KW(node?)) {
        return s7_f(s7);
    }
    if (key == KW(nodelist?)) {
        return s7_t(s7);
    }
    return s7_unspecified(s7);
}

/*
  all node types support these
 */
s7_pointer sunlark_common_property_lookup(s7_scheme *s7,
                                            struct node_s *ast_node,
                                            s7_pointer kw)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_common_property_lookup: %s for %d %s",
              s7_object_to_c_string(s7, kw), ast_node->tid, TIDNAME(ast_node));
#endif

    /* pseudo-bindings */
    if (kw == KW(ast)) {
        UT_string *buf = sealark_display_ast_outline(ast_node, 0);
        struct node_s *s = sealark_new_node(TK_STRING, without_subnodes);
        strncpy(s->s, utstring_body(buf), utstring_len(buf));
        s7_pointer out = s7_make_string(s7, utstring_body(buf));
        utstring_free(buf);
        return out;
    }

    if (kw == KW(starlark)) {
        return sunlark_to_starlark(s7,
                            s7_list(s7, 2,
                                    sunlark_new_node(s7,ast_node),
                                    KW(crush)));
        /* UT_string *buf; */
        /* utstring_new(buf); */
        /* sealark_node_to_starlark(ast_node, buf); */
        /* utstring_free(buf); */
        /* return NULL; */
    }

    if (kw == s7_make_keyword(s7, "$")) {
        /* if (sealark_is_printable(ast_node)) { */
            s7_pointer val;
            switch(ast_node->tid) {
            case TK_STRING: {
                UT_string *buf;
                utstring_new(buf);
                char *br = SEALARK_STRTYPE(ast_node->qtype);
                char *q = sealark_quote_type(ast_node);
                utstring_printf(buf,
                                "%s%s%s%s",
                                br,
                                q,
                                ast_node->s,
                                q);
                val =  s7_make_string(s7, utstring_body(buf));
                utstring_free(buf);
            }
                break;
            case TK_ID:
                if (strncmp("True", ast_node->s, 4) == 0) {
                    val = s7_t(s7);
                } else {
                    if (strncmp("False", ast_node->s, 4) == 0) {
                        val = s7_make_symbol(s7, "#f");
                    } else {
                        val =  s7_make_symbol(s7, ast_node->s);
                    }
                }
                break;
            case TK_INT:
                val =  s7_make_integer(s7, atoi(ast_node->s));
                break;
            case TK_List_Expr:
                /* what is the :value of a :list-expr? */
            default:
                ; //???
            }
            return val;
        /* } */
    }

    /* "real" bindings, corresponding to fields in the struct */
    if (kw == KW(tid)) {
        log_debug("tid: %d", ast_node->tid);
        return s7_make_integer(s7, ast_node->tid);
    }

    if (kw == KW(tid->kw)) {
        log_debug("tid: %d %s", ast_node->tid, TIDNAME(ast_node));
        char *s = sealark_tid_to_string(ast_node->tid);
        return s7_make_keyword(s7, s);
    }

    if (kw == KW(tid->string)) {
        log_debug("tid: %d", ast_node->tid);
        char *s = sealark_tid_to_string(ast_node->tid);
        return s7_make_string(s7, s);
    }

    if (kw == KW(node-type)) {
        log_debug("node-type: %d", ast_node->tid);
        char *s = sealark_tid_to_string(ast_node->tid);
        return s7_make_keyword(s7, s);
    }

    if (kw == KW(line)) {
        return s7_make_integer(s7, ast_node->line);
    }

    if (kw == KW(col))
        return s7_make_integer(s7, ast_node->col);

    if (kw == KW(trailing_newine))
        return s7_make_boolean(s7, ast_node->trailing_newline);

    if (kw == KW(qtype))
        return s7_make_integer(s7, ast_node->qtype);

    if (kw == KW(s)) {
        return s7_make_string(s7, ast_node->s);
    }

    if (kw == KW(subnode-count)) {
        int ct = sealark_subnode_count(ast_node, false, false, false);
        ct--; // do not count self;
        return s7_make_integer(s7, ct);
    }

    if (kw == KW(subnode-count-recursive)) {
        int ct = sealark_subnode_count(ast_node, false, false, true);
        ct--; // exclude self
        return s7_make_integer(s7, ct);
    }

    if (kw == KW(printable-subnode-count-recursive)) {
        int ct = sealark_subnode_count(ast_node, false, true, true);
        return s7_make_integer(s7, ct);
    }

    if (kw == KW(length)) {
        int ct;
        switch(ast_node->tid) {
        case TK_Arg_List:
            ct = sealark_subnode_count(ast_node, true, false, false);
            break;
        case TK_Binding:
            ct = sealark_subnode_count(ast_node, true, false, false);
            /* return s7_make_integer(s7, 2); */
            break;
        case TK_Call_Expr:
            ct = sealark_subnode_count(ast_node, true, false, false);
            break;
        case TK_List_Expr:
            ct = sealark_subnode_count(ast_node, true, false, false);
            break;
        case TK_Load_Stmt:
            ct = sealark_subnode_count(ast_node, true, false, false);
            break;
        case TK_Package:
            ct = sealark_subnode_count(ast_node, true, false, false);
            break;
        default:
            log_warn("Uncaught node type for :length: %d %s",
                     ast_node->tid, TIDNAME(ast_node));
            ct = sealark_subnode_count(ast_node, true, false, false);
            /* return s7_undefined(s7); */
            break;
        }
        ct--; // exclude self
        return s7_make_integer(s7, ct);
    }

    if (kw == KW(subnodes)) {
        if (ast_node->subnodes) {
            return nodelist_to_s7_list(s7, ast_node->subnodes);
        } else {
            return s7_nil(s7);
        }
    }

    /* pseudo-props */
    //FIXME: nodelist?
    if (kw == KW(node?)) { // || kw == KW(sunlark-node?)) {
        return s7_t(s7);
    }

    /* type predicate */
    if (s7_is_keyword(kw)) {
        if (sunlark_op_is_predicate(s7, kw)) {
            s7_pointer is = sunlark_node_satisfies_kw_pred(s7, kw, ast_node);
            log_debug("is? %s", s7_object_to_c_string(s7, is));
            return is;
            /* if (is_p == s7_f(s7)) { */
            /*     return is_p; */
            /* } else { */
            /*     return s7_unspecified(s7); */
            /* } */
        }
    }
    log_error("common prop not found: %s", s7_object_to_c_string(s7,kw));
    return handle_errno(s7, ENOT_FOUND, kw);
}
