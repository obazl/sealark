#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_expressors.h"

/* ******************************** */
s7_pointer sunlark_targets_for_buildfile(s7_scheme *s7,
                                         struct node_s *buildfile_node)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sunlark_targets_for_buildfile");
#endif

    if (buildfile_node->tid != TK_Package) {
        /* log_warn("property :targets only valid for :package nodes"); */
        return s7_unspecified(s7);
    }

    UT_array *target_list = sealark_targets_for_pkg(buildfile_node);

    log_debug("found %d targets", utarray_len(target_list));

    return nodelist_to_s7_list(s7, target_list);
}

/* **************************************************************** */
//FIXME: where does this go? sunlark_utils?
s7_pointer nodelist_to_s7_list(s7_scheme *s7, UT_array *target_list)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    /* log_debug("nodelist_to_s7_list"); */
#endif

    s7_pointer node_list = s7_make_list(s7,
                                        utarray_len(target_list),
                                        s7_nil(s7));

    s7_pointer item;

    struct node_s *nd=NULL;
    int i = 0;
    while( (nd=(struct node_s*)utarray_next(target_list, nd)) ) {
        /* log_debug("wrapping TID: %d %s", nd->tid, TIDNAME(nd)); */
        item = sunlark_new_node(s7, nd);
        s7_list_set(s7, node_list, i, item);
        /* node_list = s7_cons(s7, item, node_list); */
        i++;
    }
    /* log_debug("new list len: %d", s7_list_length(s7, node_list)); */

    return node_list;
}

/* **************************************************************** */
//FIXME: where does this go? sunlark_utils?
s7_pointer vec_entries_to_s7_list(s7_scheme *s7, UT_array *target_list)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("vec_entries_to_s7_list");
#endif

    s7_pointer node_list = s7_make_list(s7,
                                        utarray_len(target_list),
                                        s7_nil(s7));

    s7_pointer item;

    struct node_s *nd=NULL;
    int i = 0;
    while( (nd=(struct node_s*)utarray_next(target_list, nd)) ) {
        /* log_debug("wrapping TID: %d %s", nd->tid, TIDNAME(nd)); */
        item = sunlark_new_node(s7, nd);
        s7_list_set(s7, node_list, i, item);
        /* node_list = s7_cons(s7, item, node_list); */
        i++;
    }
    /* log_debug("new list len: %d", s7_list_length(s7, node_list)); */

    return node_list;
}

/* **************************************************************** */
LOCAL s7_pointer _get_attr_for_name_unique(s7_scheme *s7,
                                          s7_pointer attr_sym,
                                          s7_pointer self)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("_get_attr_for_name_unique");
#endif

    // FIXME: call sealark_get_call_attr_for_name

    char *attr_name = s7_object_to_c_string(s7, attr_sym);
    log_debug("attr_name: %s", attr_name);

    struct node_s *arg_list = s7_c_object_value(self);
    log_debug("nodelist len: %d", utarray_len(arg_list->subnodes));

    struct node_s *tmp=NULL;
    struct node_s *node, *attr;
    /* int slen = strlen(str); */

    struct node_s *id;
    /* struct node_s *val; */
    int name_len = strlen(attr_name);
    /* int attr_val_len = strlen(attr_val); */

    log_debug("SEARCHING arg_list %d %s, child ct: %d",
              arg_list->tid,
              token_name[arg_list->tid][0],
              utarray_len(arg_list->subnodes));

    struct node_s *arg_node = NULL;
    int i = 0;
    while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  arg_node))) {
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++, arg_node->tid,
                  token_name[arg_node->tid][0]);

        if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes
            id = utarray_eltptr(arg_node->subnodes, 0);
            /* log_debug("testing id[%d]: %d %s", i, id->tid, id->s); */

            if ((strncmp(id->s, attr_name, name_len) == 0)
                && strlen(id->s) == name_len ){

                /* log_debug("MATCH"); */
                return sunlark_new_node(s7, arg_node);

                /* name matches, now test value */
                /* val = utarray_eltptr(arg_node->subnodes, 2); */
                /* log_debug("\tattr: %s = %s", id->s, val->s); */

                /* if ( (strncmp(val->s, attr_val, attr_val_len) == 0) */
                /*      && strlen(val->s) == attr_val_len ) { */
                /*     /\* log_debug("3 xxxxxxxxxxxxxxxx MATCH %d %s == %s", *\/ */
                /*     /\*           attr_val_len, val->s, attr_val); *\/ */
                /*     return arg_node; */
                /* } */
            }
        }
    }
    return NULL;
}

/* **************************************************************** */
s7_pointer sunlark_get_attr_for_name(s7_scheme *s7,
                                    s7_pointer attrname,
                                    s7_pointer attr_list)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sunlark_get_attr_for_name: %s",
              s7_object_to_c_string(s7, attrname));
#endif

        /* if (c_is_sunlark_node(s7, attr_list)) { */
        if (sunlark_is_node(s7, attr_list)) {
            // node is a list node but not a list, from one target
            // attrs are unique
            s7_pointer n = _get_attr_for_name_unique(s7, attrname, attr_list);
            return n;
        } else {
            //FIXME: handle
            log_error("ERROR: unexpected node type");
        }
        return s7_unspecified(s7);
    /* } */
}
