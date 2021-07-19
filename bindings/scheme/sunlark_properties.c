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
    log_debug("sunlark_build_file_property_lookup");
#endif

    /* pseudo-attributes */
    if (kw == s7_make_keyword(s7, "targets")) {
        char *s = sealark_node_printable_string(bf_node);
        s7_pointer str =  s7_make_string(s7, s);
        return str;
    }

    if (kw == s7_make_keyword(s7, "loads")) {
        /* char *s = sealark_node_printable_string(bf_node); */
        /* s7_pointer str =  s7_make_string(s7, s); */
        /* return str; */
        return NULL;
    }

    if (kw == s7_make_keyword(s7, "package")) {
        /* struct node_s *attrs = sunlark_get_attrs_list(s7, bf_node); */
        /* s7_pointer attrs_s7 = sunlark_node_new(s7, attrs); */
        /* return attrs_s7; */
        return NULL;
    }

    /* common */
    s7_pointer result = sunlark_common_property_lookup(s7, bf_node, kw);
    if (result == NULL) {
        /* anything else treated as an attr name? */
        return(s7_error(s7, s7_make_symbol(s7,
                                           "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                                                      "ast-node-ref arg must be one of :package, :loads, :targets; got ~A"),
                                kw)));

    } else {
        return result;
    }
}

/* target == Call_Expr */
s7_pointer sunlark_target_property_lookup(s7_scheme *s7,
                                          struct node_s *self,
                                          s7_pointer prop_kw,
                                          s7_pointer args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_target_property_lookup: %s",
              s7_object_to_c_string(s7, prop_kw));
#endif

    if (prop_kw == s7_make_keyword(s7, "rule")) { /* e.g. cc_library */
        struct node_s *id = utarray_eltptr(self->subnodes, 0);
        if (id->tid != TK_ID) {
            log_error("ERROR: expected tid %d for target, got: %d",
                      TK_ID, id->tid);
            return NULL;
        }
        s7_pointer str =  s7_make_string(s7, id->s);
        return str;
    }

    if (prop_kw == s7_make_keyword(s7, "attrs")) {
        log_debug("matched :attrs");
        struct node_s *attrs = sunlark_get_attrs_list(s7, self);
        if (s7_is_null(s7, args)) {
            s7_pointer attrs_s7 = sunlark_node_new(s7, attrs);
            return attrs_s7;
        } else {
            /* the only propert :attr-list understands is :<attrname> */
            return sunlark_attr_list_kw_lookup(s7, attrs, args);
        }
    }

    /* common */
    s7_pointer result = sunlark_common_property_lookup(s7, self, prop_kw);
    if (result == NULL) {
        return(s7_error(s7, s7_make_symbol(s7,
                                           "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                                                      "ast-node-ref arg must be one of :rule, :attrs, :tid, :line, :col, :trailine_newline, :s, :qtype, :subnodes, :comment; got ~A"),
                                prop_kw)));

    } else {
        return result;
    }
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
    if (kw == s7_make_keyword(s7, "tid"))
        return s7_make_integer(s7, ast_node->tid);

    if (kw == s7_make_keyword(s7, "line"))
        return s7_make_integer(s7, ast_node->line);

    if (kw == s7_make_keyword(s7, "col"))
        return s7_make_integer(s7, ast_node->col);

    if (kw == s7_make_keyword(s7, "trailing_newine"))
        return s7_make_boolean(s7, ast_node->trailing_newline);

    if (kw == s7_make_keyword(s7, "qtype"))
        return s7_make_integer(s7, ast_node->qtype);

    if (kw == s7_make_keyword(s7, "s"))
        return s7_make_string(s7, ast_node->s);

    if (kw == s7_make_keyword(s7, "subnodes")) {
        if (ast_node->subnodes) {
            s7_pointer new_nodelist
                = ast_nodelist_s7_new(s7, ast_node->subnodes);
            return new_nodelist;
        } else {
            return s7_nil(s7);
        }
    }

    //FIXME: implement
    /* if (kw == s7_make_keyword(s7, "comments")) */
    /*     construct and return nodelist */

    return(s7_error(s7, s7_make_symbol(s7,
                                       "invalid_argument"),
                    s7_list(s7, 2, s7_make_string(s7,
                     "ast-node-ref arg must be one of :tid, :line, :col, :qtype, :s, :subnodes, :comments, :attrs, :print, :trailing_newline; got ~A"),
                            kw)));
}
