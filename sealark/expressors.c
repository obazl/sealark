#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "expressors.h"

/* returns tid TK-ID */
EXPORT struct node_s *sealark_ruleid_for_target(struct node_s *call_expr)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_ruleid_for_target %d %s",
              call_expr->tid, TIDNAME(call_expr));
#endif

    /* :call-expr[0] > :id */

    struct node_s *tmp = utarray_eltptr(call_expr->subnodes, 0);
    /* log_debug("ruleid: %d %s", tmp->tid, TIDNAME(tmp)); */
    return tmp;
}

/* ******************************** */
/* EXPORT struct node_s */
/* /\* *sealark_target_for_binding(struct node_s *call_expr, *\/ */
/* *sealark_get_target_for_binding(struct node_s *call_expr, */
/*                                  const char *binding_name, */
/*                                  const char *binding_val) */
/* { */
/* #if defined (DEBUG_TRACE) || defined(DEBUG_QUERY) */
/*     log_debug("sealark_get_target_for_binding %s = %s", */
/*               binding_name, binding_val); */
/* #endif */

/*     /\* :call-expr[1] > :call-sfx[1] > :arg-list[0] *\/ */
/*     /\* then search arg-list children for arg-named/name=str *\/ */
/*     /\* :arg-named[0] = :id *\/ */

/*     /\* log_debug("call_expr %d %s", call_expr->tid, *\/ */
/*     /\*           token_name[call_expr->tid][0]); *\/ */

/*     struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1); */
/*     struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1); */

/*     struct node_s *id, *val; */
/*     int name_len = strlen(binding_name); */
/*     int binding_val_len = strlen(binding_val); */

/*     //FIXME: call _get_binding_for_name_unique */

/* #if defined(DEBUG_QUERY) */
/*     log_debug("SEARCHING arg_list %d %s, child ct: %d", */
/*               arg_list->tid, */
/*               token_name[arg_list->tid][0], */
/*               utarray_len(arg_list->subnodes)); */
/* #endif */
/*     struct node_s *arg_node = NULL; */
/*     int i = 0; */
/*     while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes, */
/*                                                   arg_node))) { */
/* #if defined(DEBUG_QUERY) */
/*         log_debug(" LOOP arg_list[%d] tid: %d %s", i++, arg_node->tid, */
/*                   token_name[arg_node->tid][0]); */
/* #endif */
/*         if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes */
/*             id = utarray_eltptr(arg_node->subnodes, 0); */
/*             /\* log_debug("testing id[%d]: %d %s", i, id->tid, id->s); *\/ */

/*             if ((strncmp(id->s, binding_name, name_len) == 0) */
/*                 && strlen(id->s) == name_len ){ */

/*                 /\* name matches, now test value *\/ */
/*                 val = utarray_eltptr(arg_node->subnodes, 2); */
/*                 /\* log_debug("\tbinding: %s = %s", id->s, val->s); *\/ */

/*                 if ( (strncmp(val->s, binding_val, binding_val_len) == 0) */
/*                      && strlen(val->s) == binding_val_len ) { */
/*                     /\* log_debug("3 xxxxxxxxxxxxxxxx MATCH %d %s == %s", *\/ */
/*                     /\*           binding_val_len, val->s, binding_val); *\/ */
/*                     return arg_node; */
/*                 } */
/*             } */
/*         } */
/*     } */
/*     /\* log_debug("NOT FOUND"); *\/ */
/*     return NULL; */
/* } */

/* ******************************** */
/* includes all args, named or not, including commas */
EXPORT struct node_s *sealark_arglist_for_target_for_name
(struct node_s *package, const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_arglist_for_target_for_name %s", name);
#endif

    struct node_s *target = sealark_target_for_name(package, name);

    struct node_s *args = sealark_arglist_for_target(target);
    return args;
}

/* ******************************** */
/* includes all args, named or not, including commas */
EXPORT struct node_s *sealark_arglist_for_target_for_index
(struct node_s *package, int i)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_arglist_for_target_for_index %d", i);
#endif

    struct node_s *target = sealark_target_for_index(package, i);

    struct node_s *args = sealark_arglist_for_target(target);
    return args;
}

/* **************************************************************** */
EXPORT
struct node_s *sealark_ruleid_for_target_for_name(struct node_s *bf_node,
                                                 const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_ruleid_for_target_for_name: %s", name);
#endif
    struct node_s *tgt = sealark_target_for_name(bf_node, name);
    return sealark_ruleid_for_target(tgt);
}

/* **************************************************************** */
EXPORT
struct node_s *sealark_ruleid_for_target_for_index(struct node_s *bf_node,
                                                  int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_ruleid_for_target_for_index: %d", index);
#endif
    struct node_s *tgt = sealark_target_for_index(bf_node, index);
    return sealark_ruleid_for_target(tgt);
}

/* ******************************** */
/* returns TK_Arg_List node of all args */
EXPORT struct node_s *sealark_arglist_for_target(struct node_s *call_expr)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_arglist_for_target");
#endif

    log_debug("target %p", call_expr); // tid: %d", call_expr->tid);

    /* :call-expr[1] => :call-sfx[1] => :arg-list */
    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    return arg_list;
}

/* ******************************** */
EXPORT struct node_s *sealark_bindings_binding_for_key(UT_array *bindings,
                                                       const char *key)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_bindings_binding_for_key: %s", key);
#endif
    /* struct node_s *node, *binding; */

    struct node_s *key_id;
    int key_len = strlen(key);
    struct node_s *binding = NULL;
    int i = 0;

    while((binding=(struct node_s*)utarray_next(bindings, binding))) {
        /* log_debug(" LOOP bindings[%d] tid: %d %s", i++, binding->tid, */
        /*           token_name[binding->tid][0]); */

        if (binding->tid == TK_Binding) { // skip TK_COMMA nodes
            key_id = utarray_eltptr(binding->subnodes, 0);
            /* log_debug("testing key_id[%d]: %d %s", i, key_id->tid, key_id->s); */

            if ((strncmp(key_id->s, key, key_len) == 0)
                && strlen(key_id->s) == key_len ){

                /* log_debug("MATCHED key: %s", key); */
                return binding;

            }
        } else {
        }
    }
    return NULL;
}

/* ******************************** */
EXPORT struct node_s
*sealark_target_binding_for_key_from_bindings(UT_array *bindings,
                                              const char *key)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_target_binding_for_key_from_bindings");
#endif
    /* struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1); */
    /* struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1); */

    /* /\* struct node_s *node, *binding; *\/ */

    /* log_debug("SEARCHING arg_list %d %s, child ct: %d", */
    /*           arg_list->tid, */
    /*           token_name[arg_list->tid][0], */
    /*           utarray_len(arg_list->subnodes)); */

    /* struct node_s *id; */
    /* int key_len = strlen(key); */
    /* struct node_s *arg_node = NULL; */
    /* int i = 0; */

    /* while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes, */
    /*                                               arg_node))) { */
    /*     log_debug(" LOOP arg_list[%d] tid: %d %s", i++, arg_node->tid, */
    /*               token_name[arg_node->tid][0]); */

    /*     if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes */
    /*         id = utarray_eltptr(arg_node->subnodes, 0); */
    /*         log_debug("testing id[%d]: %d %s", i, id->tid, id->s); */

    /*         if ((strncmp(id->s, key, key_len) == 0) */
    /*             && strlen(id->s) == key_len ){ */

    /*             log_debug("MATCHED key: %s", key); */
    /*             return arg_node; */

    /*         } */
    /*     } else { */
    /*     } */
    /* } */
    /* return NULL; */
}

/* ******************************** */
EXPORT struct node_s *sealark_binding_for_key_from_target_for_name(struct node_s *bf_node, const char *target_name, const char *binding_key)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_binding_for_key_from_bindings_of_target_for_name");
#endif

    /* :target "name" :bindings 'key */

    /* no need to fetch entire set of bindings */
    struct node_s *tgt = sealark_target_for_name(bf_node, target_name);

    return sealark_target_binding_for_key(tgt, binding_key);
}

/* ******************************** */
EXPORT struct node_s *sealark_binding_for_key_from_target_for_index(struct node_s *bf_node, int target_index, const char *binding_key)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_binding_for_key_from_target_for_index");
#endif

    /* :target 0 :bindings 'srcs */

    /* no need to fetch entire set of bindings */
    struct node_s *tgt = sealark_target_for_index(bf_node, target_index);

    return sealark_target_binding_for_key(tgt, binding_key);
}

/* ******************************** */
EXPORT struct node_s *sealark_binding_for_index_from_target_for_index(struct node_s *bf_node, int target_index, int binding_index)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_binding_for_index_from_target_for_index");
#endif

    /* :target 0 :bindings 0 */

    /* no need to fetch entire set of bindings */
    struct node_s *tgt = sealark_target_for_index(bf_node, target_index);

    return sealark_target_binding_for_index(tgt, binding_index);
}

/* ******************************** */
EXPORT struct node_s *sealark_binding_for_index_from_target_for_name(struct node_s *bf_node, const char *target_name, int index)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_binding_for_index_from_bindings_of_target_for_name");
#endif

    /* :target "name" :bindings 'key */

    /* no need to fetch entire set of bindings */
    struct node_s *tgt = sealark_target_for_name(bf_node, target_name);

    return sealark_target_binding_for_index(tgt, index);
}

/* **************************************************************** */
/*         top-levels          */
/* ******************************** */
/* ******************************** */
EXPORT UT_array *sealark_definitions(struct node_s *buildfile_node)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_definitions");
#endif

    struct node_s *stmt_list = utarray_eltptr(buildfile_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    UT_array *definitions;
    utarray_new(definitions, &node_icd);

    struct node_s *small_subn = NULL;
    while((small_subn=(struct node_s*)utarray_next(small_list->subnodes,
                                                   small_subn))) {
        if (small_subn->tid == TK_Assign_Stmt) {
            utarray_push_back(definitions, small_subn);
        }
    }
    return definitions;
}

/* ******************************** */
EXPORT UT_array *sealark_vardefs(struct node_s *buildfile_node)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_vardefs");
#endif

    // FIXME: same as definitions but exclude func defns
    return sealark_definitions(buildfile_node);
}

/* ******************************** */
EXPORT struct node_s *sealark_package(struct node_s *buildfile_node)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_package");
#endif

    // :build-file > :assign-stmt

    struct node_s *expr_list = utarray_eltptr(buildfile_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(expr_list->subnodes, 0);

    struct node_s *small_subn = NULL;
    while((small_subn=(struct node_s*)utarray_next(small_list->subnodes,
                                                   small_subn))) {
        log_debug("small subn %d %s", small_subn->tid, TIDNAME(small_subn));
        struct node_s *subn = NULL;
        while((subn=(struct node_s*)utarray_next(small_subn->subnodes,
                                                       subn))) {
            log_debug("  subn %d %s", subn->tid, TIDNAME(subn));
        }
    }
    return NULL;
}

/* ******************************** */
/* FIXME won't work for proc_id "target" since that's a pseudo id
 */
EXPORT UT_array *sealark_procs_for_id(struct node_s *buildfile_node,
                                      char *proc_id)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_procs_for_id: %s", proc_id);
#endif

    //FIXME: verify that build-file nodes always start with
    //  :build-file > :stmt_list : :small-stmt-list
    struct node_s *stmt_list = utarray_eltptr(buildfile_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    //FIXME: verify assumption:
    //    each expr_list subnode of a build-file node has just one
    //    subnode, a call_expr.

    // NB: load statments are proc applications, although they parse
    // as TK_Load_Stmt instead of TK_Call_Expr

    UT_array *procs;
    utarray_new(procs, &node_icd);

    int proc_id_len = strlen(proc_id);

    bool get_loads;
    if ((strncmp(proc_id, "load", 4) == 0) && strlen(proc_id) == 4)
        get_loads = true;

    struct node_s *call_expr, *call_id;

    /* log_debug(" bf %d %s", */
    /*               buildfile_node->tid, TIDNAME(buildfile_node)); */

    struct node_s *subnode = NULL;
    while((subnode=(struct node_s*)utarray_next(small_list->subnodes,
                                                  subnode))) {
        /* log_debug("  subnode %d %s", */
        /*           subnode->tid, TIDNAME(subnode)); */

        if (get_loads)
            if (subnode->tid == TK_Load_Stmt) {
                utarray_push_back(procs, subnode);
            }

        if (subnode->tid == TK_Expr_List) {
            call_expr = utarray_eltptr(subnode->subnodes, 0);
            call_id = utarray_eltptr(call_expr->subnodes, 0);

            //FIXME: if proc_id == "target", then we need to examine
            //further to see if this call expr is a target directive

            if ((strncmp(call_id->s, proc_id, proc_id_len) == 0)
                && strlen(call_id->s) == proc_id_len ){

                utarray_push_back(procs, call_expr);
            }
        }
    }
    /* log_debug("procs len: %d", utarray_len(procs)); */
    return procs;
}

/* ******************************** */
/* returns all "procs": loads, package, targets, fn applications */
EXPORT UT_array *sealark_procs(struct node_s *buildfile_node)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_procs");
#endif

    // :build-file > :assign-stmt

    struct node_s *stmt_list = utarray_eltptr(buildfile_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    struct node_s *call_expr;

    UT_array *procs;
    utarray_new(procs, &node_icd);

    struct node_s *node = NULL;
    while((node=(struct node_s*)utarray_next(small_list->subnodes,
                                                   node))) {
        log_debug("node %d %s", node->tid, TIDNAME(node));
        if (node->tid == TK_Expr_List) {
            call_expr = utarray_eltptr(node->subnodes, 0);
            if (call_expr->tid == TK_Call_Expr) {
                utarray_push_back(procs, node);
            }
        }
        if (node->tid == TK_Load_Stmt)
            utarray_push_back(procs, node);
    }
    return procs;
}

/* ******************************** */
/* returns all procs and definitions */
EXPORT UT_array *sealark_directives(struct node_s *buildfile_node)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_directives");
#endif

    struct node_s *stmt_list = utarray_eltptr(buildfile_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    struct node_s *call_expr;

    UT_array *procs;
    utarray_new(procs, &node_icd);

    struct node_s *node = NULL;
    while((node=(struct node_s*)utarray_next(small_list->subnodes,
                                                   node))) {
        log_debug("node %d %s", node->tid, TIDNAME(node));
        if (node->tid == TK_Expr_List) {
            call_expr = utarray_eltptr(node->subnodes, 0);
            if (call_expr->tid == TK_Call_Expr) {
                utarray_push_back(procs, node);
            }
        }
        if (node->tid == TK_Load_Stmt)
            utarray_push_back(procs, node);
        if (node->tid == TK_Assign_Stmt)
            utarray_push_back(procs, node);
    }
    return procs;
}
