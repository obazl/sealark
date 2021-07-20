#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_query.h"


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
    log_debug("smalllist child ct: %d", utarray_len(smalllist->subnodes));

    struct node_s *n=NULL;

    /* load_stmts will be freed when gc calls g_destroy_ast_nodelist? */
    UT_array *load_stmts;
    utarray_new(load_stmts, &node_icd);

    while( (n=(struct node_s*)utarray_next(smalllist->subnodes, n)) ) {
        log_debug("child %d %s", n->tid, token_name[n->tid][0]);
        if (n->tid == TK_Load_Stmt) {
            utarray_push_back(load_stmts, n);
        }
    }
    log_debug("load-stmt ct: %d", utarray_len(load_stmts));

    return sunlark_nodelist_new(s7, load_stmts);
}

/* ******************************** */
s7_pointer sunlark_fetch_targets(s7_scheme *s7,
                                 struct node_s *buildfile_node)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sunlark_fetch_targets");
#endif

    if (buildfile_node->tid != TK_Build_File) {
        log_warn("property :targets only valid for :build-file nodes");
        return s7_unspecified(s7);
    }

    // :build-file > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *stmt_list = utarray_eltptr(buildfile_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);
    /* log_debug("small_list child ct: %d", */
    /*           utarray_len(small_list->subnodes)); */

    // each call_expr is wrapped in expr_list

    /* target_exprs will be freed when gc calls g_destroy_ast_nodelist? */
    UT_array *target_list;
    utarray_new(target_list, &node_icd);

    struct node_s *exprs=NULL;
    struct node_s *target;
    log_debug("SEARCHING %s (%d), child ct: %d for %s (%d)",
              TIDNAME(small_list),
              small_list->tid,
              utarray_len(small_list->subnodes),
              token_name[TK_Expr_List][0],
              TK_Expr_List);
    int i = 0;
    while((exprs
           =(struct node_s*)utarray_next(small_list->subnodes, exprs))) {
        log_debug("  LOOP (fetch) %d: %s (%d)",
                  i++, TIDNAME(exprs), exprs->tid);
        if (exprs->tid == TK_Expr_List) {
            target = utarray_eltptr(exprs->subnodes, 0);
            utarray_push_back(target_list, target);
        } else {
            /* ignore non-targets */
        }
    }
    /* struct node_s *expr_list = utarray_eltptr(small_list->subnodes, 0); */
    /* log_debug("expr_list child ct: %d", utarray_len(expr_list->subnodes)); */

    log_debug("found %d nodes", utarray_len(target_list));

    return sunlark_nodelist_new(s7, target_list);
}

LOCAL struct node_s *_get_target_by_attribute(s7_scheme *s7,
                                              struct node_s *call_expr,
                                              const char *attr_name,
                                              const char *attr_val)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("_get_target_attribute %s = %s", attr_name, attr_val);
#endif

    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    /* log_debug("call_expr %d %s", call_expr->tid, */
    /*           token_name[call_expr->tid][0]); */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    /* log_debug("call_sfx %d %s", call_sfx->tid, */
    /*           token_name[call_sfx->tid][0]); */

    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    struct node_s *id, *val;
    int name_len = strlen(attr_name);
    int attr_val_len = strlen(attr_val);

    //FIXME: call _get_attr_by_name_unique

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

        if (arg_node->tid == TK_Arg_Named) { // skip TK_COMMA nodes
            id = utarray_eltptr(arg_node->subnodes, 0);
            log_debug("testing id[%d]: %d %s", i, id->tid, id->s);

            if ((strncmp(id->s, attr_name, name_len) == 0)
                && strlen(id->s) == name_len ){

                /* name matches, now test value */
                val = utarray_eltptr(arg_node->subnodes, 2);
                /* log_debug("\tattr: %s = %s", id->s, val->s); */

                if ( (strncmp(val->s, attr_val, attr_val_len) == 0)
                     && strlen(val->s) == attr_val_len ) {
                    /* log_debug("3 xxxxxxxxxxxxxxxx MATCH %d %s == %s", */
                    /*           attr_val_len, val->s, attr_val); */
                    return arg_node;
                }
            }
        }
    }
    log_debug("NOT FOUND");
    return NULL;
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
    log_debug("nodelist len: %d", utarray_len(nodes));
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
        log_debug("child[%d] %d %s", i++, tmp->tid, token_name[tmp->tid][0]);
        switch(tmp->tid) {
        case TK_Call_Expr:      /* target */
            attr = _get_target_by_attribute(s7, tmp, "name", str);
            if (attr) {
                log_debug("GOT ATTR node %d", attr->tid);
                /* return sunlark_node_new(s7, attr); */
                return sunlark_node_new(s7, tmp);
            }
            break;
        case TK_Load_Stmt:
            node = utarray_eltptr(tmp->subnodes, 2);
            log_debug("node key: %s", node->s);
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
    log_debug("sym: %s", str);
    UT_array *nodes = s7_c_object_value(self);
    log_debug("nodelist len: %d", utarray_len(nodes));
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
        log_debug("child[%d] %d %s", i++, tmp->tid, token_name[tmp->tid][0]);
        switch(tmp->tid) {
        case TK_Call_Expr:      /* target */
            attr = _get_target_by_attribute(s7, tmp, "name", str);
            if (attr) {
                log_debug("GOT ATTR node %d", attr->tid);
                /* return sunlark_node_new(s7, attr); */
                return sunlark_node_new(s7, tmp);
            }
            break;
        case TK_Load_Stmt:
            node = utarray_eltptr(tmp->subnodes, 2);
            log_debug("node key: %s", node->s);
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

    log_debug("query self: %d %s",
              sunlark_node_tid(s7, self),
              token_name[sunlark_node_tid(s7, self)][0]);

    if (c_is_sunlark_node(s7, self)) {
        log_debug("0 xxxxxxxxxxxxxxxx %d %s",
                  sunlark_node_tid(s7, self),
                  token_name[sunlark_node_tid(s7, self)][0]);
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
LOCAL s7_pointer _get_target_by_rule_sym(s7_scheme *s7,
                                          s7_pointer sym,
                                          s7_pointer self)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("_get_target_by_rule_sym");
#endif

    UT_array *target_list;
    utarray_new(target_list, &node_icd);

    char *str = s7_object_to_c_string(s7, sym);
    log_debug("sym: %s", str);
    int symlen = strlen(str);
    UT_array *nodes = s7_c_object_value(self);
    log_debug("nodelist len: %d", utarray_len(nodes));
    struct node_s *tmp=NULL;
    struct node_s *node, *attr;
    int slen = strlen(str);

    struct node_s *id;
    int i = 0;
    log_debug("SEARCHING TARGETS");
    while((tmp=(struct node_s*)utarray_next(nodes, tmp))) {
        log_debug("  LOOP child[%d] %d %s", i++, tmp->tid, TIDNAME(tmp));

        switch(tmp->tid) {
        case TK_Call_Expr:      /* target */
            id = utarray_eltptr(tmp->subnodes, 0);
            log_debug("testing ID: %d %s: %s",
                      id->tid, TIDNAME(id), id->s);
            if ( (strncmp(id->s, str, symlen) == 0)
                 && strlen(id->s) == symlen ) {
                log_debug("GOT ID node %d", id->tid);
                utarray_push_back(target_list, tmp);
            }
            break;
        default:
log_debug("0 xxxxxxxxxxxxxxxx");
            ;
        }
    }
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

        if (arg_node->tid == TK_Arg_Named) { // skip TK_COMMA nodes
            id = utarray_eltptr(arg_node->subnodes, 0);
            log_debug("testing id[%d]: %d %s", i, id->tid, id->s);

            if ((strncmp(id->s, attr_name, name_len) == 0)
                && strlen(id->s) == name_len ){

                log_debug("MATCH");
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

/*     while((tmp=(struct node_s*)utarray_next(arg_list->subnodes, tmp))) { */
/*         log_debug("  LOOP child[%d] %d %s", i++, tmp->tid, TIDNAME(tmp)); */

/*         switch(tmp->tid) { */
/*         case TK_Arg_Named:      /\* attr *\/ */
/*             id = utarray_eltptr(tmp->subnodes, 0); */
/*             log_debug("testing ID: %d %s: %s", */
/*                       id->tid, TIDNAME(id), id->s); */
/*             if ( (strncmp(id->s, str, symlen) == 0) */
/*                  && strlen(id->s) == symlen ) { */
/*                 log_debug("GOT ID node %d", id->tid); */
/*                 return sunlark_node_new(s7, tmp); */
/*             } */
/*             break; */
/*         default: */
/* log_debug("0 xxxxxxxxxxxxxxxx"); */
/*             ; */
/*         } */
/*     } */
    log_debug("NOT FOUND");

    return NULL;
}

/* **************************************************************** */
s7_pointer sunlark_get_target_by_rule_name(s7_scheme *s7,
                                           s7_pointer rulename,
                                           s7_pointer target_list)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sunlark_get_target_by_rule_name: %s",
              s7_object_to_c_string(s7, rulename));
#endif

    log_debug("\ttarget_list tid %d %s",
              sunlark_node_tid(s7, target_list),
              token_name[sunlark_node_tid(s7, target_list)][0]);

    if (c_is_sunlark_nodelist(s7, target_list)) {
        s7_pointer n = _get_target_by_rule_sym(s7, rulename, target_list);
        return n;
    } else {
        if (c_is_sunlark_node(s7, target_list)) {
            log_debug("node xxxxxxxxxxxxxxxx %d %s",
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

    log_debug("\tattr_list tid %d %s",
              sunlark_node_tid(s7, attr_list),
              token_name[sunlark_node_tid(s7, attr_list)][0]);

    if (c_is_sunlark_nodelist(s7, attr_list)) {
        // node is a list (utarray), may span multiple targets
        // may contain duplicate attrs
        /* s7_pointer n = _get_attr_by_name(s7, attrname, attr_list); */
        /* return n; */
    } else {

        if (c_is_sunlark_node(s7, attr_list)) {
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
