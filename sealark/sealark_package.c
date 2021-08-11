#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_package.h"

/* **************************************************************** */
EXPORT struct node_s *sealark_pkg_remove_all_targets(struct node_s *pkg)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sealark_pkg_remove_all_targets");
#endif

    assert(pkg->tid == TK_Package);

    /* sealark_debug_log_ast_outline(pkg, 0); */

    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    struct node_s *small_stmt_list = utarray_eltptr(stmt_list->subnodes, 0);

    int toplevel_ct = utarray_len(small_stmt_list->subnodes);

    struct node_s *node;
    struct node_s *maybe_call;

    for (int i = 0; i < toplevel_ct; i++) {
        node = utarray_eltptr(small_stmt_list->subnodes, i);
        if (node->tid == TK_Expr_List) {
            maybe_call = utarray_eltptr(node->subnodes, 0);
            if (maybe_call->tid == TK_Call_Expr) {
                if (sealark_call_expr_is_target(maybe_call)) {
                    log_warn("Deleting target at toplevel %d", i);
                    utarray_erase(small_stmt_list->subnodes, i, 1);
                    // now shrink the counters too
                    i--; toplevel_ct--;
                }
            }
        }
    }

    return pkg;
}

/* **************************************************************** */
EXPORT struct node_s *sealark_pkg_rm_target_at_int(struct node_s *pkg,
                                                       int index)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sealark_pkg_remove_target_at_int: %d", index);
#endif

    assert(pkg->tid == TK_Package);

    /* sealark_debug_log_ast_outline(pkg, 0); */

    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    struct node_s *small_stmt_list = utarray_eltptr(stmt_list->subnodes, 0);

    int toplevel_ct = utarray_len(small_stmt_list->subnodes);

    struct node_s *node;
    struct node_s *maybe_call;

    int target_ct = (index < 0)? -1 : 0;
    int delta = (index < 0)? 0 : toplevel_ct;
    /* int stop  = (index < 0)? 0 : toplevel_ct; */
    int j = 0;
    log_debug("target_ct: %d, delta: %d; j: %d",
              target_ct, delta, j);

    for (int i = 0; i < toplevel_ct; i++) {
        /* log_debug("i: %d", i); */
        j = (index < 0)? toplevel_ct - i - 1 : i;
        /* log_debug("j: %d, tgt ct: %d", j, target_ct); */
        node = utarray_eltptr(small_stmt_list->subnodes, j);
        if (node->tid == TK_Expr_List) {
            maybe_call = utarray_eltptr(node->subnodes, 0);
            if (maybe_call->tid == TK_Call_Expr) {
                if (sealark_call_expr_is_target(maybe_call)) {
                    if (target_ct == index) {
                        /* log_debug("HIT"); */
                        utarray_erase(small_stmt_list->subnodes, j, 1);
                        return pkg;
                    }
                    if (index < 0)
                        target_ct--;
                    else
                        target_ct++;
                }
            }
        }
    }
    errno = EINDEX_OUT_OF_BOUNDS;
    return NULL;
}

/* **************************************************************** */
EXPORT struct node_s *sealark_pkg_replace_target_at_int(struct node_s *pkg,
                                                        int index,
                                                     struct node_s *newtgt)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sealark_pkg_replace_target_at_int: %d", index);
#endif

    assert(pkg->tid == TK_Package);
    assert(newtgt->tid == TK_Call_Expr);

    /* sealark_debug_log_ast_outline(pkg, 0); */

    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    struct node_s *small_stmt_list = utarray_eltptr(stmt_list->subnodes, 0);

    int toplevel_ct = utarray_len(small_stmt_list->subnodes);

    struct node_s *node;
    struct node_s *maybe_call;

    int target_ct = (index < 0)? -1 : 0;
    int delta = (index < 0)? 0 : toplevel_ct;
    /* int stop  = (index < 0)? 0 : toplevel_ct; */
    int j = 0;
    log_debug("target_ct: %d, delta: %d; j: %d",
              target_ct, delta, j);

    for (int i = 0; i < toplevel_ct; i++) {
        /* log_debug("i: %d", i); */
        j = (index < 0)? toplevel_ct - i - 1 : i;
        /* log_debug("j: %d, tgt ct: %d", j, target_ct); */
        node = utarray_eltptr(small_stmt_list->subnodes, j);
        if (node->tid == TK_Expr_List) {
            maybe_call = utarray_eltptr(node->subnodes, 0);
            if (maybe_call->tid == TK_Call_Expr) {
                if (sealark_call_expr_is_target(maybe_call)) {
                    if (target_ct == index) {
                        log_debug("HIT - replacing at %d", j);
                        /* now replace */
                        newtgt->line = maybe_call->line;
                        newtgt->col = maybe_call->col;
                        if (maybe_call->s)
                            free(maybe_call->s);
                        if (maybe_call->subnodes)
                            utarray_free(maybe_call->subnodes);
                        memcpy(maybe_call, newtgt, sizeof(struct node_s));
                        free(newtgt);
                        return maybe_call;
                    }
                    if (index < 0)
                        target_ct--;
                    else
                        target_ct++;
                }
            }
        }
    }
    errno = EINDEX_OUT_OF_BOUNDS;
    return NULL;
}

/* **************************************************************** */
EXPORT struct node_s *sealark_pkg_splice_target_at_int(struct node_s *pkg,
                                                       int target_ct,
                                                       int index,
                                                       struct node_s *newtgt)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sealark_pkg_splice_target_at_int: %d, tgt ct: %d",
              index, target_ct);
#endif

    assert(pkg->tid == TK_Package);
    assert(newtgt->tid == TK_Call_Expr);

    /* sealark_debug_log_ast_outline(pkg, 0); */

    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    struct node_s *small_stmt_list = utarray_eltptr(stmt_list->subnodes, 0);

    int toplevel_ct = utarray_len(small_stmt_list->subnodes);

    struct node_s *node;
    struct node_s *maybe_call;

    /* index is already normalized */
    int target_idx = 0;

    /* int target_idx = (index < 0)? -1 : 0; */
    /* int delta = (index < 0)? 0 : toplevel_ct; */
    /* int stop  = (index < 0)? 0 : toplevel_ct; */
    /* int j = 0; */
    /* log_debug("target_idx: %d, delta: %d; j: %d", */
    /*           target_idx, delta, j); */

    for (int i = 0; i < toplevel_ct; i++) {
        /* log_debug("i: %d", i); */
        /* j = (index < 0)? toplevel_ct - i - 1 : i; */
        /* log_debug("j: %d, tgt ct: %d", j, target_idx); */
        node = utarray_eltptr(small_stmt_list->subnodes, i);
        if (node->tid == TK_Expr_List) {
            maybe_call = utarray_eltptr(node->subnodes, 0);
            if (maybe_call->tid == TK_Call_Expr) {
                if (sealark_call_expr_is_target(maybe_call)) {
                    log_debug("target %d", target_idx);
                    if (target_idx == index) {
                        log_debug("HIT - splicing at %d", i);
                        struct node_s *nd = sealark_new_node(TK_Expr_List,
                                                             with_subnodes);
                        utarray_push_back(nd->subnodes, newtgt);
                        nd->line = -1; // mark as unformatted
                        utarray_insert(small_stmt_list->subnodes, nd, i);
                        return pkg;
                    } else {
                        if (target_idx == target_ct - 1) {
                            log_debug("appending");
                            struct node_s *nd = sealark_new_node(TK_Expr_List,
                                                                 with_subnodes);
                            utarray_push_back(nd->subnodes, newtgt);
                            nd->line = -1; // mark as unformatted
                            utarray_push_back(small_stmt_list->subnodes, nd);
                            return pkg;
                        }
                    }
                    /* if (index < 0) */
                    /*     target_idx--; */
                    /* else */
                    target_idx++;
                }
            }
        }
    }
    errno = EINDEX_OUT_OF_BOUNDS;
    return NULL;
}

/* **************************************************************** */
EXPORT struct node_s *sealark_pkg_rm_target_at_key(struct node_s *pkg,
                                                   const char *key)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sealark_pkg_remove_target_at_key: %s", key);
#endif

    assert(pkg->tid == TK_Package);

    /* sealark_debug_log_ast_outline(pkg, 0); */

    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    struct node_s *small_stmt_list = utarray_eltptr(stmt_list->subnodes, 0);

    int toplevel_ct = utarray_len(small_stmt_list->subnodes);

    errno = ENOT_IMPLEMENTED;
    return NULL;

    /* struct node_s *node; */
    /* struct node_s *maybe_call; */

    /* int target_ct = (index < 0)? -1 : 0; */
    /* int delta = (index < 0)? 0 : toplevel_ct; */
    /* /\* int stop  = (index < 0)? 0 : toplevel_ct; *\/ */
    /* int j = 0; */
    /* log_debug("target_ct: %d, delta: %d; j: %d", */
    /*           target_ct, delta, j); */

    /* for (int i = 0; i < toplevel_ct; i++) { */
    /*     /\* log_debug("i: %d", i); *\/ */
    /*     j = (index < 0)? toplevel_ct - i - 1 : i; */
    /*     /\* log_debug("j: %d, tgt ct: %d", j, target_ct); *\/ */
    /*     node = utarray_eltptr(small_stmt_list->subnodes, j); */
    /*     if (node->tid == TK_Expr_List) { */
    /*         maybe_call = utarray_eltptr(node->subnodes, 0); */
    /*         if (maybe_call->tid == TK_Call_Expr) { */
    /*             if (sealark_call_expr_is_target(maybe_call)) { */
    /*                 if (target_ct == index) { */
    /*                     /\* log_debug("HIT"); *\/ */
    /*                     utarray_erase(small_stmt_list->subnodes, j, 1); */
    /*                     return pkg; */
    /*                 } */
    /*                 if (index < 0) */
    /*                     target_ct--; */
    /*                 else */
    /*                     target_ct++; */
    /*             } */
    /*         } */
    /*     } */
    /* } */
    /* errno = EINDEX_OUT_OF_BOUNDS; */
    /* return NULL; */
}

/* **************************************************************** */
/* **************************************************************** */
EXPORT struct node_s *sealark_pkg_remove_loadstmt_at_int(struct node_s *pkg,
                                                         int index)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sealark_pkg_remove_loadstmt_at_int: %d", index);
#endif

    assert(pkg->tid == TK_Package);

    /* sealark_debug_log_ast_outline(pkg, 0); */

    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    struct node_s *small_stmt_list = utarray_eltptr(stmt_list->subnodes, 0);

    int toplevel_ct = utarray_len(small_stmt_list->subnodes);

    struct node_s *node;
    struct node_s *maybe_call;

    int loadstmt_ct = (index < 0)? -1 : 0;
    int delta = (index < 0)? 0 : toplevel_ct;
    /* int stop  = (index < 0)? 0 : toplevel_ct; */
    int j = 0;
    log_debug("loadstmt_ct: %d, delta: %d; j: %d",
              loadstmt_ct, delta, j);

    for (int i = 0; i < toplevel_ct; i++) {
        log_debug("i: %d", i);
        j = (index < 0)? toplevel_ct - i - 1 : i;
        log_debug("j: %d, tgt ct: %d", j, loadstmt_ct);
        node = utarray_eltptr(small_stmt_list->subnodes, j);
        if (node->tid == TK_Load_Stmt) {
            if (loadstmt_ct == index) {
                log_debug("HIT");
                utarray_erase(small_stmt_list->subnodes, j, 1);
                return pkg;
            }
            if (index < 0)
                loadstmt_ct--;
            else
                loadstmt_ct++;
        }
    }
    errno = EINDEX_OUT_OF_BOUNDS;
    return NULL;
}

/* **************************************************************** */
EXPORT struct node_s *sealark_pkg_remove_loadstmt_at_key(struct node_s *pkg,
                                                         const char *key)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sealark_pkg_remove_loadstmt_at_key: %s", key);
#endif

    assert(pkg->tid == TK_Package);

    /* sealark_debug_log_ast_outline(pkg, 0); */

    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    struct node_s *small_stmt_list = utarray_eltptr(stmt_list->subnodes, 0);

    int toplevel_ct = utarray_len(small_stmt_list->subnodes);
    int loadstmt_ct = 0;

    int keylen = strlen(key);

    struct node_s *node;
    struct node_s *key_node;

    for (int i = 0; i < toplevel_ct; i++) {
        node = utarray_eltptr(small_stmt_list->subnodes, i);
        if (node->tid == TK_Load_Stmt) {
            key_node = utarray_eltptr(node->subnodes, 2);
            if ( (strncmp(key_node->s, key, keylen) == 0)
                 && strlen(key_node->s) == keylen ) {
                log_debug("HIT");
                utarray_erase(small_stmt_list->subnodes, i, 1);
                return pkg;
            }
            loadstmt_ct++;
        }
    }
    errno = ENOT_FOUND;
    return NULL;
}

/* **************************************************************** */
EXPORT struct node_s *sealark_pkg_remove_all_loadstmts(struct node_s *pkg)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sealark_pkg_remove_all_loadstmts");
#endif

    assert(pkg->tid == TK_Package);

    /* sealark_debug_log_ast_outline(pkg, 0); */

    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    struct node_s *small_stmt_list = utarray_eltptr(stmt_list->subnodes, 0);

    int toplevel_ct = utarray_len(small_stmt_list->subnodes);

    struct node_s *node;
    struct node_s *maybe_call;

    for (int i = 0; i < toplevel_ct; i++) {
        node = utarray_eltptr(small_stmt_list->subnodes, i);
        if (node->tid == TK_Load_Stmt) {
            utarray_erase(small_stmt_list->subnodes, i, 1);
            // now shrink the counters too
            i--; toplevel_ct--;
        }
    }
    return pkg;
}

/* **************************************************************** */
EXPORT int sealark_pkg_targets_count(struct node_s *pkg)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug(">> sealark_pkg_targets_count");
#endif
    assert(pkg->tid == TK_Package);

    /* sealark_debug_log_ast_outline(pkg, 0); */

    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    struct node_s *small_stmt_list = utarray_eltptr(stmt_list->subnodes, 0);

    int toplevel_ct = utarray_len(small_stmt_list->subnodes);

    int target_ct = 0;

    struct node_s *node;
    struct node_s *maybe_call;

    for (int i = 0; i < toplevel_ct; i++) {
        node = utarray_eltptr(small_stmt_list->subnodes, i);
        if (node->tid == TK_Expr_List) {
            maybe_call = utarray_eltptr(node->subnodes, 0);
            if (maybe_call->tid == TK_Call_Expr) {
                if (sealark_call_expr_is_target(maybe_call)) {
                    target_ct++;
                }
            }
        }
    }
    return target_ct;
}

/* **************************************************************** */
EXPORT void sealark_pkg_format(struct node_s *pkg)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    log_debug("sealark_pkg_format");
#endif

    assert(pkg->tid == TK_Package);

    int indent = 4;
    int mrl = 0;
    int mrc = 0;

    /* FIXME: so far we always get one stmt_list with one small_stmt_list.
       will that always be the case?
     */
    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    assert(stmt_list->tid == TK_Stmt_List);
    struct node_s *small_stmts = utarray_eltptr(stmt_list->subnodes, 0);
    assert(small_stmts->tid == TK_Small_Stmt_List);

    int toplevel_ct = utarray_len(small_stmts->subnodes);
    bool dirty = false;

    struct node_s *sub = NULL;
    for (int i = 0; i < toplevel_ct; i++) {
        sub = utarray_eltptr(small_stmts->subnodes, i);
        log_debug("toplevel: %d %s, mrl: %d", sub->tid, TIDNAME(sub), mrl);
        if (dirty)
            _pkg_format_toplevel(sub, &mrl, &mrc);
        else {
            if (sub->line < 0) {
                log_debug("1 xxxxxxxxxxxxxxxx");
                dirty = true;
                mrl = _most_recent_line(small_stmts, i);
                log_debug("mrl: %d", mrl);
                _pkg_format_toplevel(sub, &mrl, &mrc);
            }
        }
    }
}

/* **************************************************************** */
EXPORT void sealark_pkg_format_force(struct node_s *pkg)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    log_debug("sealark_pkg_format_force");
#endif

    assert(pkg->tid == TK_Package);

    int indent = 4;
    int mrl = 0;                /* most recent line */
    int mrc = 0;                /* most recent col */

    /* FIXME: so far we always get one stmt_list with one small_stmt_list.
       will that always be the case?
     */
    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    assert(stmt_list->tid == TK_Stmt_List);
    struct node_s *small_stmts = utarray_eltptr(stmt_list->subnodes, 0);
    assert(small_stmts->tid == TK_Small_Stmt_List);

    int toplevel_ct = utarray_len(small_stmts->subnodes);

    struct node_s *sub = NULL;
    for (int i = 0; i < toplevel_ct; i++) {
        sub = utarray_eltptr(small_stmts->subnodes, i);
        log_debug("toplevel: %d %s, mrl: %d", sub->tid, TIDNAME(sub), mrl);
        _pkg_format_toplevel(sub, &mrl, &mrc);
    }
}

/* **************************************************************** */
EXPORT int _most_recent_line(struct node_s *small_stmts, int index)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    log_debug("_most_recent_line");
#endif

    int ct = 0;
    struct node_s *last;

    if (index > 0) {
        struct node_s *nd = utarray_eltptr(small_stmts->subnodes, index-1);
        switch(nd->tid) {
        case TK_Load_Stmt:
            ct = utarray_len(nd->subnodes);
            last = utarray_eltptr(nd->subnodes, ct-1);
            assert(last->tid == TK_RPAREN);
            return last->line;
            break;
        case TK_Expr_List:
            /* may contain just about any kind of node... */
            ct = utarray_len(nd->subnodes);
            last = utarray_eltptr(nd->subnodes, ct-1);
            return _last_line(last);
            break;
        case TK_Assign_Stmt:
            last = utarray_eltptr(nd->subnodes, 2);
            return _last_line(last);
            break;
        default:
            ;
        }
    } else {
        return 0;
    }
}

EXPORT int _last_line(struct node_s *nd)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    /* log_debug("_last_line %d %s", nd->tid, TIDNAME(nd)); */
#endif

    if (sealark_is_printable(nd)) {
        /* log_debug("printable %d %s last line: %d", */
        /*           nd->tid, TIDNAME(nd), nd->line); */
        return nd->line;
    }

    struct node_s *sub = NULL;
    int last = 0;
    while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
        last = _last_line(sub);
    }

    return last;

    /* int ct; */
    /* struct node_s *last; */

    /* switch(nd->tid) { */
    /* case TK_Load_Stmt: */
    /*     ct = utarray_len(nd->subnodes); */
    /*     last = utarray_eltptr(nd->subnodes, ct-1); */
    /*     assert(last->tid == TK_RPAREN); */
    /*     return last->line; */
    /*     break; */
    /* case TK_Expr_List: */
    /*     /\* may contain just about any kind of node... *\/ */
    /*     ct = utarray_len(nd->subnodes); */
    /*     last = utarray_eltptr(nd->subnodes, ct-1); */
    /*     return _last_line(last); */
    /*     break; */
    /* case TK_List_Expr: */
    /*     ct = utarray_len(nd->subnodes); */
    /*     last = utarray_eltptr(nd->subnodes, ct-1); */
    /*     return _last_line(last); */
    /*     break; */
    /* case TK_Assign_Stmt: */
    /*     last = utarray_eltptr(nd->subnodes, 2); */
    /*     break; */
    /* default: */
    /*     log_error("last line %d %s", nd->tid, TIDNAME(nd)); */
    /*     exit(-1); */
    /* } */
}

/* **************************************************************** */
EXPORT void _pkg_format_toplevel(struct node_s *tl_nd, int *mrl, int *mrc)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    log_debug("_pkg_format_toplevel: %d %s, nd line %d; mrl: %d, mrc: %d",
              tl_nd->tid, TIDNAME(tl_nd),
              tl_nd->line, *mrl, *mrc);
#endif

    struct node_s *sub = NULL;
    if (tl_nd->line < 0) {
        /* unformatted */
        *mrl += 2;
        tl_nd->line = *mrl;
        tl_nd->col  = col;
        while( (sub=(struct node_s*)utarray_next(tl_nd->subnodes, sub)) ) {
            sealark_format_dirty_node(sub, mrl, &col);
        }
    } else {
        /* int delta = *mrl - tl_nd->line; */
        /* log_debug("DELTA %d", delta); */
        /* if (delta < 0) */
        /*     return; */


        if (tl_nd->line < *mrl) {
            tl_nd->line = *mrl + format.leading;
            *mrl = tl_nd->line;
        }

        while( (sub=(struct node_s*)utarray_next(tl_nd->subnodes, sub)) ) {
            sealark_format_clean_node(sub, mrl, mrc);
        }
    }
}

