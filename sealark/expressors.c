#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "expressors.h"

EXPORT UT_array *sealark_targets_for_buildfile(struct node_s *buildfile_node)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_targets_for_buildfile");
#endif
/* LOCAL UT_array *_get_all_targets(struct node_s *buildfile_node) */
/* { */
/* #if defined (DEBUG_TRACE) || defined(DEBUG_QUERY) */
/*     log_debug("_get_all_targets"); */
/* #endif */

    // :build-file > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *stmt_list = utarray_eltptr(buildfile_node->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);
    log_debug("small_list child ct: %d",
              utarray_len(small_list->subnodes));

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
            // target is TK_Call_Expr
            node_s *binding = sealark_target_binding_for_key(target, "name");
            if (binding) {
                utarray_push_back(target_list, target);
            }
        } else {
            /* ignore non-targets */
        }
    }
    return target_list;
}

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
EXPORT struct node_s
/* *sealark_target_for_binding(struct node_s *call_expr, */
*sealark_get_target_by_binding(struct node_s *call_expr,
                                 const char *binding_name,
                                 const char *binding_val)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_get_target_by_binding %s = %s",
              binding_name, binding_val);
#endif

    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    /* log_debug("call_expr %d %s", call_expr->tid, */
    /*           token_name[call_expr->tid][0]); */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    struct node_s *id, *val;
    int name_len = strlen(binding_name);
    int binding_val_len = strlen(binding_val);

    //FIXME: call _get_binding_by_name_unique

#if defined(DEBUG_QUERY)
    log_debug("SEARCHING arg_list %d %s, child ct: %d",
              arg_list->tid,
              token_name[arg_list->tid][0],
              utarray_len(arg_list->subnodes));
#endif
    struct node_s *arg_node = NULL;
    int i = 0;
    while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  arg_node))) {
#if defined(DEBUG_QUERY)
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++, arg_node->tid,
                  token_name[arg_node->tid][0]);
#endif
        if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes
            id = utarray_eltptr(arg_node->subnodes, 0);
            /* log_debug("testing id[%d]: %d %s", i, id->tid, id->s); */

            if ((strncmp(id->s, binding_name, name_len) == 0)
                && strlen(id->s) == name_len ){

                /* name matches, now test value */
                val = utarray_eltptr(arg_node->subnodes, 2);
                /* log_debug("\tbinding: %s = %s", id->s, val->s); */

                if ( (strncmp(val->s, binding_val, binding_val_len) == 0)
                     && strlen(val->s) == binding_val_len ) {
                    /* log_debug("3 xxxxxxxxxxxxxxxx MATCH %d %s == %s", */
                    /*           binding_val_len, val->s, binding_val); */
                    return arg_node;
                }
            }
        }
    }
    /* log_debug("NOT FOUND"); */
    return NULL;
}

/* **************************************************************** */
EXPORT UT_array *sealark_bindings_for_target_by_name
(struct node_s *build_file, const char *tgt_name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_bindings_for_target_by_name %s", tgt_name);
#endif

    struct node_s *target = sealark_target_by_name(build_file, tgt_name);

    UT_array *bindings = sealark_bindings_for_target(target);
    return bindings;
}

/* **************************************************************** */
EXPORT UT_array *sealark_bindings_for_target_by_index
(struct node_s *build_file, int i)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_bindings_for_target_by_index %d", i);
#endif

    struct node_s *target = sealark_target_by_index(build_file, i);

    UT_array *bindings = sealark_bindings_for_target(target);
    return bindings;
}

/* ******************************** */
/* includes all args, named or not, including commas */
EXPORT struct node_s *sealark_arglist_for_target_by_name
(struct node_s *build_file, const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_arglist_for_target_by_name %s", name);
#endif

    struct node_s *target = sealark_target_by_name(build_file, name);

    struct node_s *args = sealark_arglist_for_target(target);
    return args;
}

/* ******************************** */
/* includes all args, named or not, including commas */
EXPORT struct node_s *sealark_arglist_for_target_by_index
(struct node_s *build_file, int i)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_arglist_for_target_by_index %d", i);
#endif

    struct node_s *target = sealark_target_by_index(build_file, i);

    struct node_s *args = sealark_arglist_for_target(target);
    return args;
}

/* **************************************************************** */
EXPORT
struct node_s *sealark_ruleid_for_target_by_name(struct node_s *bf_node,
                                                 const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_ruleid_for_target_by_name: %s", name);
#endif
    struct node_s *tgt = sealark_target_by_name(bf_node, name);
    return sealark_ruleid_for_target(tgt);
}

/* **************************************************************** */
EXPORT
struct node_s *sealark_ruleid_for_target_by_index(struct node_s *bf_node,
                                                  int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_ruleid_for_target_by_index: %d", index);
#endif
    struct node_s *tgt = sealark_target_by_index(bf_node, index);
    return sealark_ruleid_for_target(tgt);
}

/* **************************************************************** */
EXPORT
struct node_s *sealark_target_by_name(struct node_s *build_file,
                                      const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_by_name %s", name);
#endif

    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    struct node_s *stmt_list = utarray_eltptr(build_file->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    // :build-file > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *expr_nd=NULL;
    struct node_s *target_nd=NULL;

    int name_len = strlen(name);

    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {

        while( (target_nd=(struct node_s*)utarray_next(expr_nd->subnodes,
                                                       target_nd)) ) {
            if (target_nd->tid == TK_Call_Expr) {
                bool ok = sealark_target_has_binding(target_nd,
                                                    "name", name);
                /* struct node_s *binding */
                /*     = sealark_target_binding_for_key(target_nd, "name"); */
                if (ok) {
                    return target_nd;
                }
            }
        }
    }
    return NULL;
}

/* **************************************************************** */
EXPORT
struct node_s *sealark_target_by_index(struct node_s *build_file, int i)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_by_index %d", i);
#endif

    /* :call-expr[1] > :call-sfx[1] > :arg-list[0] */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    struct node_s *stmt_list = utarray_eltptr(build_file->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    // each call_expr is wrapped in expr_list
    /* we cannot just take the ith child, we have to take the ith
       target. toplevel children include load stmts, etc. */
    /* struct node_s *expr_list = utarray_eltptr(small_list->subnodes, i); */

    int target_ct = 0;

    // :build-file > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *expr_nd=NULL;
    struct node_s *target_nd=NULL;

    /*
      if index i < 0, we have no way of knowing target ct without
      iterating. then we have to iterate again to get the item.
     */

    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {

        while( (target_nd=(struct node_s*)utarray_next(expr_nd->subnodes,
                                                       target_nd)) ) {
            if (target_nd->tid == TK_Call_Expr) {
                if (sealark_target_has_binding_key(target_nd, "name")) {
                    log_debug("target_ct: %d, i: %d", target_ct, i);
                    if (target_ct == i) {
                        log_debug("MATCH");
                        return target_nd;
                    }
                    target_ct++;
                }
            }
        }
    }
    log_debug("target_ct: %d", target_ct);

    if (i < 0) {
        if (abs(i) > target_ct) {
            log_error("abs(%d) > target count", i);
            errno = 3;
            return NULL;
        } else {
            i = target_ct + i;
            return sealark_target_by_index(build_file, i);
        }
    }

    if (i > target_ct-1) {
        log_error("index > target count");
        errno = 2;              /* FIXME */
   }

    return NULL;
    /* if (nd) { */
    /* } else { */
    /*     return NULL; */
    /* } */
}

/* **************************************************************** */
EXPORT struct node_s *sealark_target_for_name(struct node_s *build_file,
                                              const char *name)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_target_for_name %s", name);
#endif

    // :build-file > :stmt-list :smallstmt-list > expr-list > call-expr

    struct node_s *stmt_list = utarray_eltptr(build_file->subnodes, 0);
    struct node_s *small_list = utarray_eltptr(stmt_list->subnodes, 0);

    struct node_s *expr_nd=NULL;
    struct node_s *target_nd=NULL;

    while( (expr_nd=(struct node_s*)utarray_next(small_list->subnodes,
                                                 expr_nd)) ) {

        while( (target_nd=(struct node_s*)utarray_next(expr_nd->subnodes,
                                                       target_nd)) ) {
            if (target_nd->tid == TK_Call_Expr) {
                if (sealark_get_target_by_binding(target_nd, "name",
                                                    name)) {
                    return target_nd;
                }
            }
        }
    }

    return NULL;
}

/* ******************************** */
/* returns only bindings (TK_Named_Arg) in a new UT_array */
EXPORT UT_array *sealark_bindings_for_target(struct node_s *call_expr)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_QUERY)
    log_debug("sealark_bindings_for_target");
#endif

    log_debug("target tid: %d %s", call_expr->tid, TIDNAME(call_expr));

    /* :call-expr[1] => :call-sfx[1] => :arg-list */
    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    UT_array *attribs;
    utarray_new(attribs, &node_icd);

    struct node_s *nd=NULL;
    while( (nd=(struct node_s*)utarray_next(arg_list->subnodes, nd)) ) {
        if (nd->tid == TK_Binding)
            utarray_push_back(attribs, nd);
    }
    log_debug("found %d bindings (named args)", utarray_len(attribs));
    return attribs;
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
EXPORT struct node_s *sealark_target_binding_for_key(struct node_s *call_expr, const char *key)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_target_binding_for_key");
#endif
    /* :call-expr[1] > :call-sfx[1] > :arg-list */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    /* struct node_s *node, *binding; */

    log_debug("SEARCHING arg_list %d %s, child ct: %d",
              arg_list->tid,
              token_name[arg_list->tid][0],
              utarray_len(arg_list->subnodes));

    struct node_s *id;
    int key_len = strlen(key);
    struct node_s *arg_node = NULL;
    int i = 0;

    while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  arg_node))) {
        log_debug(" LOOP arg_list[%d] tid: %d %s", i++, arg_node->tid,
                  token_name[arg_node->tid][0]);

        if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes
            id = utarray_eltptr(arg_node->subnodes, 0);
            log_debug("testing id[%d]: %d %s", i, id->tid, id->s);

            if ((strncmp(id->s, key, key_len) == 0)
                && strlen(id->s) == key_len ){

                log_debug("MATCHED key: %s", key);
                return arg_node;

            }
        } else {
        }
    }
    return NULL;
}

/* ******************************** */
EXPORT struct node_s *sealark_target_binding_by_index(struct node_s *call_expr, int index)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_target_binding_by_index");
#endif
    /* :call-expr[1] > :call-sfx[1] > :arg-list */
    /* then search arg-list children for arg-named/name=str */
    /* :arg-named[0] = :id */

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    /* struct node_s *node, *binding; */

    log_debug("SEARCHING arg_list %d %s, child ct: %d",
              arg_list->tid,
              token_name[arg_list->tid][0],
              utarray_len(arg_list->subnodes));

    /* struct node_s *id; */
    struct node_s *arg_node = NULL;

    /* for target nodes, only bindings (attrs) are allowed in arglist,
       so no need to account for non-attribute args
     */
    int binding_ct = 0;
    while((arg_node=(struct node_s*)utarray_next(arg_list->subnodes,
                                                  arg_node))) {
        log_debug(" LOOP arg_list[%d] tid: %d %s",
                  binding_ct, arg_node->tid,
                  token_name[arg_node->tid][0]);

        if (arg_node->tid == TK_Binding) { // skip TK_COMMA nodes
            if (binding_ct == index) {
                log_debug("MATCHED index: %d", binding_ct);
                return arg_node;
            }
            binding_ct++;
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
EXPORT struct node_s *sealark_binding_for_key_from_target_by_index(struct node_s *bf_node, int target_index, const char *binding_key)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_binding_for_key_from_target_by_index");
#endif

    /* :target 0 :bindings 'srcs */

    /* no need to fetch entire set of bindings */
    struct node_s *tgt = sealark_target_by_index(bf_node, target_index);

    return sealark_target_binding_for_key(tgt, binding_key);
}

/* ******************************** */
EXPORT struct node_s *sealark_binding_by_index_from_target_by_index(struct node_s *bf_node, int target_index, int binding_index)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_binding_by_index_from_target_by_index");
#endif

    /* :target 0 :bindings 0 */

    /* no need to fetch entire set of bindings */
    struct node_s *tgt = sealark_target_by_index(bf_node, target_index);

    return sealark_target_binding_by_index(tgt, binding_index);
}

/* ******************************** */
EXPORT struct node_s *sealark_binding_by_index_from_target_for_name(struct node_s *bf_node, const char *target_name, int index)
{
#ifdef DEBUG_QUERY
    log_debug("sealark_binding_by_index_from_bindings_of_target_for_name");
#endif

    /* :target "name" :bindings 'key */

    /* no need to fetch entire set of bindings */
    struct node_s *tgt = sealark_target_for_name(bf_node, target_name);

    return sealark_target_binding_by_index(tgt, index);
}
