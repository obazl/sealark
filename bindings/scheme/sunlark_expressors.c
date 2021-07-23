#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
/* #include "utarray.h" */

#include "s7.h"

#include "sunlark_expressors.h"

/* **************************************************************** */
/* **************** */
s7_pointer sunlark_path_for_buildfile(s7_scheme *s7,
                                      struct node_s *bf_node,
                                      s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PROPERTIES)
    log_debug("sunlark_path_for_buildfile: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    s7_pointer kw = s7_car(path_args);

    s7_pointer result_list;

    if (s7_is_keyword(kw)) {

        if (kw == KW(targets)) {
            result_list = sunlark_targets_for_buildfile(s7, bf_node);
        }

        if (kw == KW(target)) {
            /* NB: same as :targets */
            result_list = sunlark_targets_for_buildfile(s7, bf_node);
        }

        if (kw == s7_make_keyword(s7, "load")) {
            // :build-file > :stmt-list :smallstmt-list > load-expr,...
            result_list = sunlark_fetch_load_stmts(s7, bf_node);
        }

        if (kw == s7_make_keyword(s7, "package")) {
            /* struct node_s *attrs = sunlark_get_attrs_list(s7, bf_node); */
            /* s7_pointer attrs_s7 = sunlark_node_new(s7, attrs); */
            /* result_list = attrs_s7; */
            result_list = NULL;
        }

        /* predicates */
        s7_pointer sym = s7_keyword_to_symbol(s7, kw);
        char *key = (char*)s7_symbol_name(sym);
        if (strrchr(key, '?') - key == strlen(key)-1 ) {
            result_list = sunlark_is_kw(s7, key, bf_node);
        }

        /* common */
        result_list = sunlark_common_property_lookup(s7, bf_node, kw);
        if (result_list == NULL) {
            /* result_list = s7_unspecified(s7); */
            result_list =(s7_error(s7, s7_make_symbol(s7,
                                               "invalid_argument"),
                            s7_list(s7, 2, s7_make_string(s7,
                                                          "ast-node-ref arg must be one of :package, :loads, :targets; got ~A"),
                                    kw)));
        }
    } else {
        if (s7_is_integer(kw)) {
            /* FIXME: check against length */
            struct node_s *n = utarray_eltptr(bf_node->subnodes, s7_integer(kw));
            log_debug("build file lookup tid: %d %s",
                      n->tid, token_name[n->tid][0]);
            result_list = sunlark_node_new(s7, n);
        }
    }

    s7_pointer next_step = s7_cadr(path_args);
    if (s7_is_null(s7, next_step)) {
        return result_list;
    } else {
        return sunlark_dispatch(s7, result_list, s7_cdr(path_args));
    }
}

/*
  only called with :build-file node

 */
s7_pointer sunlark_fetch_load_stmts(s7_scheme *s7,
                                    struct node_s *buildfile_node)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sunlark_fetch_load_stmts");
#endif

    if (buildfile_node->tid != TK_Build_File) {
        log_warn("property :loads only valid for :build-file nodes");
        return s7_unspecified(s7);
    }

    // :build-file > :stmt-list :smallstmt-list > load-stmt,...

    struct node_s *stmt_list = utarray_eltptr(buildfile_node->subnodes, 0);
    struct node_s *smalllist = utarray_eltptr(stmt_list->subnodes, 0);
    /* log_debug("smalllist child ct: %d", utarray_len(smalllist->subnodes)); */

    struct node_s *n=NULL;

    /* load_stmts will be freed when gc calls g_destroy_ast_nodelist? */
    UT_array *load_stmts;
    utarray_new(load_stmts, &node_icd);

    while( (n=(struct node_s*)utarray_next(smalllist->subnodes, n)) ) {
        /* log_debug("child %d %s", n->tid, token_name[n->tid][0]); */
        if (n->tid == TK_Load_Stmt) {
            utarray_push_back(load_stmts, n);
        }
    }
    /* log_debug("load-stmt ct: %d", utarray_len(load_stmts)); */

    return sunlark_nodelist_new(s7, load_stmts);
}

/* ******************************** */
s7_pointer sunlark_targets_for_buildfile(s7_scheme *s7,
                                         struct node_s *buildfile_node)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sunlark_targets_for_buildfile");
#endif
/* s7_pointer sunlark_fetch_targets(s7_scheme *s7, */
/*                                  struct node_s *buildfile_node) */
/* { */
/* #if defined (DEBUG_TRACE) || defined(DEBUG_QUERY) */
/*     log_debug("sunlark_fetch_targets"); */
/* #endif */

    if (buildfile_node->tid != TK_Build_File) {
        /* log_warn("property :targets only valid for :build-file nodes"); */
        return s7_unspecified(s7);
    }

    UT_array *target_list = sealark_targets_for_buildfile(buildfile_node);

    log_debug("found %d targets", utarray_len(target_list));

    return nodelist_to_s7_list(s7, target_list);
}

/* **************************************************************** */
/* **************************************************************** */

s7_pointer nodelist_to_s7_list(s7_scheme *s7, UT_array *target_list)
{
    s7_pointer node_list = s7_make_list(s7,
                                        utarray_len(target_list),
                                        s7_nil(s7));

    s7_pointer item;

    struct node_s *nd=NULL;
    int i = 0;
    while( (nd=(struct node_s*)utarray_next(target_list, nd)) ) {
        log_debug("wrapping TID: %d", nd->tid);
        item = sunlark_node_new(s7, nd);
        s7_list_set(s7, node_list, i, item);
        /* node_list = s7_cons(s7, item, node_list); */
        i++;
    }
    log_debug("new list len: %d", s7_list_length(s7, node_list));

    return node_list;
}

/* **************************************************************** */
LOCAL s7_pointer _handle_nodelist_string_query(s7_scheme *s7,
                                                const char *str,
                                                s7_pointer self)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("_handle_nodelist_string_query");
#endif

    UT_array *nodes = s7_c_object_value(self);
    /* log_debug("nodelist len: %d", utarray_len(nodes)); */
    struct node_s *tmp=NULL;
    struct node_s *node, *attr;
    int slen = strlen(str);

    /* PROBLEM: unlikely but possible:
       load("foo"...)
       cc_library(name="foo"...)

       we need to know which kind we're querying
     */

    int i = 0;
    while((tmp=(struct node_s*)utarray_next(nodes, tmp))) {
        /* log_debug("child[%d] %d %s", i++, tmp->tid, token_name[tmp->tid][0]); */
        switch(tmp->tid) {
        case TK_Call_Expr:      /* target */
            attr = sealark_get_target_by_binding(tmp, "name", str);
            if (attr) {
                /* log_debug("GOT ATTR node %d", attr->tid); */
                /* return sunlark_node_new(s7, attr); */
                return sunlark_node_new(s7, tmp);
            }
            break;
        case TK_Load_Stmt:
            node = utarray_eltptr(tmp->subnodes, 2);
            /* log_debug("node key: %s", node->s); */
            if (strncmp(str, node->s, slen) == 0)
                return sunlark_node_new(s7, tmp);
            break;
        default:
            ;
        }
    }
}

LOCAL s7_pointer _handle_nodelist_symbol_query(s7_scheme *s7,
                                               s7_pointer sym,
                                                /* const char *str, */
                                                s7_pointer self)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("_handle_nodelist_symbol_query");
#endif

    char *str = s7_object_to_c_string(s7, sym);
    /* log_debug("sym: %s", str); */
    UT_array *nodes = s7_c_object_value(self);
    /* log_debug("nodelist len: %d", utarray_len(nodes)); */
    struct node_s *tmp=NULL;
    struct node_s *node, *attr;
    int slen = strlen(str);

    /* PROBLEM: unlikely but possible:
       load("foo"...)
       cc_library(name="foo"...)

       we need to know which kind we're querying
     */

    int i = 0;
    while((tmp=(struct node_s*)utarray_next(nodes, tmp))) {
        /* log_debug("child[%d] %d %s", i++, tmp->tid, token_name[tmp->tid][0]); */
        switch(tmp->tid) {
        case TK_Call_Expr:      /* target */
            attr = sealark_get_target_by_binding(tmp, "name", str);
            if (attr) {
                /* log_debug("GOT ATTR node %d", attr->tid); */
                /* return sunlark_node_new(s7, attr); */
                return sunlark_node_new(s7, tmp);
            }
            break;
        case TK_Load_Stmt:
            node = utarray_eltptr(tmp->subnodes, 2);
            /* log_debug("node key: %s", node->s); */
            if (strncmp(str, node->s, slen) == 0)
                return sunlark_node_new(s7, tmp);
            break;
        default:
            ;
        }
    }
}

/* **************************************************************** */
s7_pointer sunlark_handle_string_query_arg(s7_scheme *s7,
                                           const char *str,
                                           s7_pointer self)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sunlark_handle_string_query_arg");
#endif
    /* log_debug("query self: %d %s", */
    /*           sunlark_node_tid(s7, self), */
    /*           token_name[sunlark_node_tid(s7, self)][0]); */

    if (c_is_sunlark_node(s7, self)) {
        /* log_debug("0 xxxxxxxxxxxxxxxx %d %s", */
        /*           sunlark_node_tid(s7, self), */
        /*           token_name[sunlark_node_tid(s7, self)][0]); */
        /* s7_pointer n = _handle_node_string_query(s7, str, self); */
   } else {
        if (c_is_sunlark_nodelist(s7, self)) {
             s7_pointer n = _handle_nodelist_string_query(s7, str, self);
             return n;
        }
    }
    return s7_unspecified(s7);
}

/* **************************************************************** */
LOCAL s7_pointer _get_targets_by_rule_sym(s7_scheme *s7,
                                          s7_pointer sym,
                                          s7_pointer self)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("_get_targets_by_rule_sym");
#endif

    UT_array *target_list;
    utarray_new(target_list, &node_icd);

    char *str = s7_object_to_c_string(s7, sym);
    /* log_debug("sym: %s", str); */
    int symlen = strlen(str);
    UT_array *nodes = s7_c_object_value(self);
    /* log_debug("nodelist len: %d", utarray_len(nodes)); */
    struct node_s *tmp=NULL;
    struct node_s *node, *attr;
    int slen = strlen(str);

    struct node_s *id;
    int i = 0;
    /* log_debug("SEARCHING TARGETS"); */
    while((tmp=(struct node_s*)utarray_next(nodes, tmp))) {
        /* log_debug("  LOOP child[%d] %d %s", i++, tmp->tid, TIDNAME(tmp)); */

        switch(tmp->tid) {
        case TK_Call_Expr:      /* target */
            id = utarray_eltptr(tmp->subnodes, 0);
            /* log_debug("testing ID: %d %s: %s", */
            /*           id->tid, TIDNAME(id), id->s); */
            if ( (strncmp(id->s, str, symlen) == 0)
                 && strlen(id->s) == symlen ) {
                /* log_debug("GOT ID node %d", id->tid); */
                utarray_push_back(target_list, tmp);
            }
            break;
        default:
            ;
        }
    }
    log_debug("found %d targets for rule %s",
              utarray_len(target_list), str);
    return sunlark_nodelist_new(s7, target_list);
}

/* **************************************************************** */
LOCAL s7_pointer _get_attr_by_name_unique(s7_scheme *s7,
                                          s7_pointer attr_sym,
                                          s7_pointer self)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("_get_attr_by_name_unique");
#endif

    // FIXME: call sealark_get_call_attr_by_name

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
                return sunlark_node_new(s7, arg_node);

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
s7_pointer sunlark_get_targets_by_rule_name(s7_scheme *s7,
                                           s7_pointer rulename,
                                           s7_pointer target_list)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sunlark_get_targets_by_rule_name: %s",
              s7_object_to_c_string(s7, rulename));
#endif

    /* log_debug("\ttarget_list tid %d %s", */
    /*           sunlark_node_tid(s7, target_list), */
    /*           token_name[sunlark_node_tid(s7, target_list)][0]); */

    if (c_is_sunlark_nodelist(s7, target_list)) {
        s7_pointer n = _get_targets_by_rule_sym(s7, rulename, target_list);
        return n;
    } else {
        if (c_is_sunlark_node(s7, target_list)) {
            log_warn("single target node %d %s",
                      sunlark_node_tid(s7, target_list),
                      token_name[sunlark_node_tid(s7, target_list)][0]);
            /* s7_pointer n = _handle_node_string_query(s7, str, target_list); */
        } else {
            //FIXME: handle
            log_error("ERROR: unexpected node type");
        }
        return s7_unspecified(s7);
    }
}

/* **************************************************************** */
s7_pointer sunlark_get_attr_by_name(s7_scheme *s7,
                                    s7_pointer attrname,
                                    s7_pointer attr_list)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sunlark_get_attr_by_name: %s",
              s7_object_to_c_string(s7, attrname));
#endif

    /* log_debug("\tattr_list tid %d %s", */
    /*           sunlark_node_tid(s7, attr_list), */
    /*           token_name[sunlark_node_tid(s7, attr_list)][0]); */

    if (c_is_sunlark_nodelist(s7, attr_list)) {
        // node is a list (utarray), may span multiple targets
        // may contain duplicate attrs
        /* s7_pointer n = _get_attr_by_name(s7, attrname, attr_list); */
        /* return n; */
    } else {
        /* if (c_is_sunlark_node(s7, attr_list)) { */
        if (sunlark_is_node(s7, attr_list)) {
            // node is a list node but not a list, from one target
            // attrs are unique
            s7_pointer n = _get_attr_by_name_unique(s7, attrname, attr_list);
            return n;
        } else {
            //FIXME: handle
            log_error("ERROR: unexpected node type");
        }
        return s7_unspecified(s7);
    }
}
