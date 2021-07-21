#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_properties.h"

s7_pointer sunlark_build_file_property_lookup(s7_scheme *s7,
                                              struct node_s *bf_node,
                                              s7_pointer kw)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_build_file_property_lookup: %s",
              s7_object_to_c_string(s7, kw));
#endif

    if (s7_is_keyword(kw)) {

        if (kw == s7_make_keyword(s7, "targets")) {
            return sunlark_fetch_targets(s7, bf_node);
        }

        if (kw == s7_make_keyword(s7, "target")) {
            /* NB: same as :targets */
            return sunlark_fetch_targets(s7, bf_node);
        }

        if (kw == s7_make_keyword(s7, "load")) {
            // :build-file > :stmt-list :smallstmt-list > load-expr,...
            return sunlark_fetch_load_stmts(s7, bf_node);
        }

        if (kw == s7_make_keyword(s7, "package")) {
            /* struct node_s *attrs = sunlark_get_attrs_list(s7, bf_node); */
            /* s7_pointer attrs_s7 = sunlark_node_new(s7, attrs); */
            /* return attrs_s7; */
            return NULL;
        }

        /* predicates */
        s7_pointer sym = s7_keyword_to_symbol(s7, kw);
        char *key = (char*)s7_symbol_name(sym);
        if (strrchr(key, '?') - key == strlen(key)-1 ) {
            return sunlark_predication(s7, key, bf_node);
        }

        /* common */
        s7_pointer result = sunlark_common_property_lookup(s7, bf_node, kw);
        if (result == NULL) {
            /* return s7_unspecified(s7); */
            return(s7_error(s7, s7_make_symbol(s7,
                                               "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                                                          "ast-node-ref arg must be one of :package, :loads, :targets; got ~A"),
                                    kw)));
        } else {
            /* matched common property */
            return result;
        }
    } else {
        if (s7_is_integer(kw)) {
            /* FIXME: check against length */
            struct node_s *n = utarray_eltptr(bf_node->subnodes, s7_integer(kw));
            log_debug("build file lookup tid: %d %s",
                      n->tid, token_name[n->tid][0]);
            return sunlark_node_new(s7, n);
        }
    }
}

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

    /* if (prop_kw == s7_make_keyword(s7, "rule")) { /\* e.g. cc_library *\/ */
    /*     struct node_s *id = utarray_eltptr(self->subnodes, 0); */
    /*     if (id->tid != TK_ID) { */
    /*         log_error("ERROR: expected tid %d for target, got: %d", */
    /*                   TK_ID, id->tid); */
    /*         return NULL; */
    /*     } */
    /*     s7_pointer str =  s7_make_string(s7, id->s); */
    /*     return str; */
    /* } */

    if (prop_kw == s7_make_keyword(s7, "attrs")) {
        /* log_debug("matched :attrs"); */
        struct node_s *attrs = sunlark_get_attrs_list(s7, self);
        /* log_debug("got attrs: %d %s", attrs->tid, token_name[attrs->tid][0]); */
        return sunlark_node_new(s7, attrs);

        /* } else { */
        /*     /\* the only propert :attr-list understands is :<attrname> *\/ */
        /*     return sunlark_attr_list_kw_lookup(s7, attrs, args); */
        /* } */
    }

    /* common */
    s7_pointer result = sunlark_common_property_lookup(s7, self, prop_kw);
    if (result == NULL) {
    /*     /\* by elimination, prop_kw must be an attribute name *\/ */
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

/* attributes have two predefined props: :name, :value */
s7_pointer sunlark_attribute_property_lookup(s7_scheme *s7,
                                             struct node_s *attr_node,
                                             const char *key)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_attribute_property_lookup tid %d, prop: %s",
              attr_node->tid, key);
#endif

    struct node_s *c_result;

    if ( strncmp(key, "name", 4) == 0 ) {
        c_result = utarray_eltptr(attr_node->subnodes, 0);
        return sunlark_node_new(s7, c_result);
        /* return sunlark_update_attribute_name(s7, node_s7, key, val); */
    }

    if ( strncmp(key, "value", 5) == 0) {
        c_result = utarray_eltptr(attr_node->subnodes, 2);
        return sunlark_node_new(s7, c_result);
        /* return sunlark_update_attribute_value(s7, node_s7, key, val); */
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

    if (key == s7_make_keyword(s7, "tid")) {
        return s7_make_integer(s7, TK_Node_List);
    }
    if (key == s7_make_keyword(s7, "tid->kw")) {
        return s7_make_keyword(s7, "node_list");
    }
    if (key == s7_make_keyword(s7, "tid->string")) {
        return s7_make_string(s7, "node_list");
    }
    if (key == s7_make_keyword(s7, "node?")) {
        return s7_f(s7);
    }
    if (key == s7_make_keyword(s7, "nodelist?")) {
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
    log_debug("sunlark_common_property_lookup: %s",
              s7_object_to_c_string(s7, kw));
#endif

    /* pseudo-attributes */
    if (kw == s7_make_keyword(s7, "pprint")) {
        char *s = sealark_node_printable_string(ast_node);
        s7_pointer str =  s7_make_string(s7, s);
        return str;
    }

    /* "real" attributes, corresponding to fields in the struct */
    if (kw == kw_tid) { //s7_make_keyword(s7, "tid")) {
        log_debug("tid: %d", ast_node->tid);
        return s7_make_integer(s7, ast_node->tid);
    }

    if (kw == s7_make_keyword(s7, "tid->kw")) {
        log_debug("tid: %d", ast_node->tid);
        char *s = sealark_tid_to_string(ast_node->tid);
        return s7_make_keyword(s7, s);
    }

    if (kw == s7_make_keyword(s7, "tid->string")) {
        log_debug("tid: %d", ast_node->tid);
        char *s = sealark_tid_to_string(ast_node->tid);
        return s7_make_string(s7, s);
    }

    /* if (kw == s7_make_keyword(s7, "line")) */
    if (kw == kw_line)
        return s7_make_integer(s7, ast_node->line);

    if (kw == s7_make_keyword(s7, "col"))
        return s7_make_integer(s7, ast_node->col);

    if (kw == s7_make_keyword(s7, "trailing_newine"))
        return s7_make_boolean(s7, ast_node->trailing_newline);

    if (kw == s7_make_keyword(s7, "qtype"))
        return s7_make_integer(s7, ast_node->qtype);

    if (kw == s7_make_keyword(s7, "s")) {
        return s7_make_string(s7, ast_node->s);
    }

    if (kw == s7_make_keyword(s7, "subnodes")) {
        if (ast_node->subnodes) {
            s7_pointer new_nodelist
                = sunlark_nodelist_new(s7, ast_node->subnodes);
            return new_nodelist;
        } else {
            return s7_nil(s7);
        }
    }

    /* pseudo-props */
    //FIXME: nodelist?
    if (kw == s7_make_keyword(s7, "node?")) {
        return s7_t(s7);
    }

    /* type predicate */
    s7_pointer sym = s7_keyword_to_symbol(s7, kw);
    char *key = (char*)s7_symbol_name(sym);
    if (strrchr(key, '?') - key == strlen(key)-1 ) {
        s7_pointer is_p = sunlark_predication(s7, key, ast_node);
        if (is_p == s7_f(s7)) {
            return is_p;
        } else {
            return s7_unspecified(s7);
        }
    }

    return s7_unspecified(s7);
    /* return(s7_error(s7, s7_make_symbol(s7, */
    /*                                    "invalid_argument"), */
    /*                 s7_list(s7, 2, s7_make_string(s7, */
    /*                  "ast-node-ref common-property arg must be one of :tid, :line, :col, :qtype, :s, :subnodes, :comments, :attrs, :print, :trailing_newline; got ~A"), */
    /*                         kw))); */
}
