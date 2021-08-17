#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_loadstmts_set.h"

/* FIXME: return deletion count? */
EXPORT
void sealark_loadstmt_rm_args(struct node_s *loadstmt)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sealark_loadstmt_rm_args");
#endif
    /* sealark_debug_log_ast_outline(loadstmt, 0); */
    int subnode_ct = utarray_len(loadstmt->subnodes);
    /* log_debug("subnode_ct: %d", subnode_ct); */
    int span = 2;
    struct node_s *arg_nd=NULL;

    /* work backwards, stopping at key (third subnode) */
    for (int i = subnode_ct-1; i > 2; i--) {
    /* while( (arg_nd=(struct node_s*)utarray_next(loadstmt->subnodes, */
    /*                                              arg_nd)) ) { */
        /* if (i < 3) { continue; } // omit load("key" */
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        /* log_debug("item %d %s", i, item->s); */

        if (item->tid == TK_STRING) {
            /* log_debug("STRING %d", i); */
            /* last subnode is ')' */
            if (i == subnode_ct - 2) span = 1;
            else span = 2;
            /* log_debug("erasing %d span %d: %s", i, span, */
            /*           item->s); */
            utarray_erase(loadstmt->subnodes, i, span);
        }
    }
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_replace_arg_at_int(struct node_s *loadstmt,
                                                int index,
                                                const char *newval)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_replace_arg_at_int %d", index);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    /* sealark_debug_log_ast_outline(loadstmt, 0); */

    int subnode_ct = utarray_len(loadstmt->subnodes);
    /* log_debug("subnode_ct: %d", subnode_ct); */

    /* normalize index - cannot use util routine since there is no
       args node type */
    int arg_ct = sealark_loadstmt_arg_count(loadstmt);
    /* log_debug("arg_ct: %d", arg_ct); */
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > arg_ct) {
            log_error("abs(%d) > arg_ct", index, arg_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            return;
        } else {
            index = arg_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > arg_ct-1) {
        log_error("index > target count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return;
    }
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("\tnormalized idx: %d", index);
#endif

    int arg_idx = 0;

    /* start at idx 4, skipping 'load("key",' */
    for (int i = 4; i < subnode_ct; i++) {
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        /* log_debug("item %d/%d %s", i, arg_idx, item->s); */
        if (item->tid == TK_STRING) {
            arg_idx = (i - 4) / 2;
            /* log_debug("STRING %d, arg %d", i, arg_idx); */
            if (arg_idx == index) {
                /* sealark_debug_log_ast_outline(item, 0); */
                free(item->s);
                int len = strlen(newval);
                item->s = calloc(len, sizeof(char*));
                strncpy(item->s, newval, len);
                return;
            }
            i++;
        }
    }
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_insert_arg_at_int(struct node_s *loadstmt,
                                               int index,
                                               struct node_s *newval)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_insert_arg_at_int %d", index);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    /* sealark_debug_log_ast_outline(loadstmt, 0); */

    int subnode_ct = utarray_len(loadstmt->subnodes);
    /* log_debug("subnode_ct: %d", subnode_ct); */

    /* normalize index - cannot use util routine since there is no
       args node type */
    int arg_ct = sealark_loadstmt_arg_count(loadstmt);
    /* log_debug("arg_ct: %d", arg_ct); */
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > arg_ct) {
            log_error("abs(%d) > arg_ct", index, arg_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            return;
        } else {
            index = arg_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > arg_ct) {
        log_error("index %d > arg count %d", index, arg_ct);
        errno = EINDEX_OUT_OF_BOUNDS;
        return;
    }
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("\tnormalized idx: %d", index);
#endif

    struct node_s *comma = sealark_new_node(TK_COMMA, without_subnodes);

    //FIXME! verify that splicing at end-of-args list is correct

    int arg_idx = 4 + index * 2;
    if (arg_idx == subnode_ct) {
        /* append */
        arg_idx--; // last subnode is ')' so insert before it
        utarray_insert(loadstmt->subnodes, comma, arg_idx);
        utarray_insert(loadstmt->subnodes, newval, arg_idx+1);
    } else {
        /* log_debug("inserting at %d", arg_idx); */
        utarray_insert(loadstmt->subnodes, newval, arg_idx);
        utarray_insert(loadstmt->subnodes, comma, arg_idx+1);
        /* sealark_debug_log_ast_outline(loadstmt, 0); */
    }
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_rm_arg_at_int(struct node_s *loadstmt,
                                          int index)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_rm_arg_at_int: %d", index);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    /* sealark_debug_log_ast_outline(loadstmt, 0); */

    int subnode_ct = utarray_len(loadstmt->subnodes);
    /* log_debug("subnode_ct: %d", subnode_ct); */

    /* normalize index - cannot use util routine since there is no
       args node type */
    int arg_ct = sealark_loadstmt_arg_count(loadstmt);
    /* log_debug("arg_ct: %d", arg_ct); */
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > arg_ct) {
            log_error("abs(%d) > arg_ct", index, arg_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            return;
        } else {
            index = arg_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > arg_ct-1) {
        log_error("index > target count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return;
    }
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("\tnormalized: %d", index);
#endif

    int span = 2;
    int arg_idx = 0;

    /* start at idx 4, skipping 'load("key",' */
    for (int i = 4; i < subnode_ct; i++) {
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        /* log_debug("item %d/%d %s", i, arg_idx, item->s); */
        if (item->tid == TK_STRING) {
            arg_idx = (i - 4) / 2;
            /* log_debug("STRING %d, arg %d", i, arg_idx); */
            if (arg_idx == index) {
                /* last subnode is ')' */
                if (i == subnode_ct - 2) span = 1;
                else span = 2;
                /* log_debug("erasing %d span %d: %s", i, span, */
                /*           item->s); */
                utarray_erase(loadstmt->subnodes, i, span);
                return;
            }
            i++;
        }
    }
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_rm_arg_at_str(struct node_s *loadstmt,
                                           const char *key)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_rm_arg_at_str: %s", key);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    /* sealark_debug_log_ast_outline(loadstmt, 0); */

    int klen = strlen(key);
    int subnode_ct = utarray_len(loadstmt->subnodes);
    /* log_debug("subnode_ct: %d", subnode_ct); */

    int span = 2;
    int arg_idx = 0;

    /* start at idx 4, skipping 'load("key",' */
    for (int i = 4; i < subnode_ct; i++) {
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        /* log_debug("item %d/%d %s", i, arg_idx, item->s); */
        if (item->tid == TK_STRING) {
            arg_idx = (i - 4) / 2;
            /* log_debug("STRING %d, arg %d", i, arg_idx); */
            if ( (strncmp(item->s, key, klen) == 0)
                 && (strlen(item->s) == klen) ) {
                if (i == subnode_ct - 2) span = 1;
                else span = 2;
                /* log_debug("erasing %d span %d: %s", i, span, */
                /*           item->s); */
                utarray_erase(loadstmt->subnodes, i, span);
                return;
            }
        }
    }
    errno = ENOT_FOUND;
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_replace_arg_at_str(struct node_s *loadstmt,
                                                const char *key,
                                                const char *newval)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_replace_arg_at_str: %s, %s",
              key, newval);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    /* sealark_debug_log_ast_outline(loadstmt, 0); */

    int klen = strlen(key);
    int subnode_ct = utarray_len(loadstmt->subnodes);
    /* log_debug("subnode_ct: %d", subnode_ct); */

    int span = 2;
    int arg_idx = 0;

    /* start at idx 4, skipping 'load("key",' */
    for (int i = 4; i < subnode_ct; i++) {
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        /* log_debug("item %d/%d %s", i, arg_idx, item->s); */
        if (item->tid == TK_STRING) {
            arg_idx = (i - 4) / 2;
            /* log_debug("STRING %d, arg %d", i, arg_idx); */
            if ( (strncmp(item->s, key, klen) == 0)
                 && (strlen(item->s) == klen) ) {
                free(item->s);
                int len = strlen(newval);
                item->s = calloc(len, sizeof(char*));
                strncpy(item->s, newval, len);
                return;
            }
        }
    }
    errno = ENOT_FOUND;
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_insert_arg_at_str(struct node_s *loadstmt,
                                               const char *key,
                                               const char *newval)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_insert_arg_at_str: %s", key);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    /* sealark_debug_log_ast_outline(loadstmt, 0); */

    int klen = strlen(key);
    int subnode_ct = utarray_len(loadstmt->subnodes);
    /* log_debug("subnode_ct: %d", subnode_ct); */

    int span = 2;
    int arg_idx = 0;

    /* start at idx 4, skipping 'load("key",' */
    for (int i = 4; i < subnode_ct; i++) {
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        /* log_debug("item %d/%d %s", i, arg_idx, item->s); */
        if (item->tid == TK_STRING) {
            arg_idx = (i - 4) / 2;
            /* log_debug("STRING %d, arg %d", i, arg_idx); */
            if ( (strncmp(item->s, key, klen) == 0)
                 && (strlen(item->s) == klen) ) {
                free(item->s);
                int len = strlen(newval);
                item->s = calloc(len, sizeof(char*));
                strncpy(item->s, newval, len);
                return;
            }
        }
    }
    errno = ENOT_FOUND;
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_insert_binding_at_int(struct node_s *loadstmt,
                                                   int index,
                                                   struct node_s *newattr)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_insert_binding_at_int %d", index);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    /* sealark_debug_log_ast_outline(loadstmt, 0); */

    /* subtract 1 for terminal ')' */
    int subnode_ct = utarray_len(loadstmt->subnodes) - 1;
    /* log_debug("subnode_ct: %d", subnode_ct); */

    /* normalize index - cannot use util routine since there is no */
    /*    args node type */
    int attr_ct = sealark_loadstmt_binding_count(loadstmt);
    /* log_debug("attr_ct: %d", attr_ct); */
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > attr_ct) {
            log_error("abs(%d) > attr_ct", index, attr_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            return;
        } else {
            index = attr_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > attr_ct) {
        log_error("index %d > attr count %d", attr_ct);
        errno = EINDEX_OUT_OF_BOUNDS;
        return;
    }
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("normalized idx: %d", index);
#endif

    /* int arg_idx = 0; */

    if (index == attr_ct) {
        /* appending */
    }

    errno = 0;
    int idx = sealark_loadstmt_binding_subidx_for_idx(loadstmt,
                                                      index);
        /* log_debug("new binding:"); */
        /* sealark_debug_log_ast_outline(newattr, 0); */

    struct node_s *comma = sealark_new_node(TK_COMMA, without_subnodes);

    if (errno == 0) {
        /* log_debug("inserting new binding at subidx %d for idx %d", idx, index); */
        /* if (idx == subnode_ct) { */
        if (index == attr_ct) {
            utarray_insert(loadstmt->subnodes, comma, idx);
            utarray_insert(loadstmt->subnodes, newattr, idx + 1);
        } else {
            utarray_insert(loadstmt->subnodes, newattr, idx);
            utarray_insert(loadstmt->subnodes, comma, idx + 1);
        }
    } else {
        log_error("Error finding subidx for idx %d", errno);
        return;
    }
    return;
}

/* **************************************************************** */
/*                         BINDINGS OPS                             */
/* **************************************************************** */

/* **************************************************************** */
EXPORT
void sealark_loadstmt_binding_for_int_replace_key(struct node_s *loadstmt,
                                                  int index,
                                                  const char *newkey)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_binding_for_int_replace_key");
#endif
    assert(loadstmt->tid == TK_Load_Stmt);

    struct node_s *b = sealark_loadstmt_binding_for_int(loadstmt, index);
    struct node_s *k = utarray_eltptr(b->subnodes, 0);
    free(k->s);
    int klen = strlen(newkey);
    k->s = calloc(klen, sizeof(char));
    strncpy(k->s, newkey, klen);
    return;
}

/* **************************************************************** */
EXPORT
void sealark_loadstmt_binding_for_int_replace_val(struct node_s *loadstmt,
                                                  int index,
                                                  const char *newval)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_binding_for_int_replace_val");
#endif
    assert(loadstmt->tid == TK_Load_Stmt);

    struct node_s *b = sealark_loadstmt_binding_for_int(loadstmt, index);
    struct node_s *k = utarray_eltptr(b->subnodes, 2);
    free(k->s);
    int klen = strlen(newval);
    k->s = calloc(klen, sizeof(char));
    strncpy(k->s, newval, klen);
    return;
}

/* **************************************************************** */
EXPORT
void sealark_loadstmt_binding_for_sym_replace_key(struct node_s *loadstmt,
                                                  const char *sym,
                                                  const char *newkey)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_binding_for_sym_replace_key");
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    struct node_s *b = sealark_loadstmt_binding_for_sym(loadstmt, sym);
    if (errno != 0) {
        return;
    }

    struct node_s *k = utarray_eltptr(b->subnodes, 0);
    free(k->s);
    int klen = strlen(newkey);
    k->s = calloc(klen, sizeof(char));
    strncpy(k->s, newkey, klen);
    return;
}

/* **************************************************************** */
EXPORT
void sealark_loadstmt_binding_for_sym_replace_val(struct node_s *loadstmt,
                                                  const char *sym,
                                                  const char *newval)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_binding_for_sym_replace_val");
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    struct node_s *b = sealark_loadstmt_binding_for_sym(loadstmt, sym);
    if (errno != 0) {
        return;
    }

    struct node_s *k = utarray_eltptr(b->subnodes, 2);
    free(k->s);
    int klen = strlen(newval);
    k->s = calloc(klen, sizeof(char));
    strncpy(k->s, newval, klen);
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_replace_attr_at_int(struct node_s *loadstmt,
                                                 int index,
                                                 struct node_s *newattr)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_replace_attr_at_int %d", index);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    /* sealark_debug_log_ast_outline(loadstmt, 0); */

    int subnode_ct = utarray_len(loadstmt->subnodes);
    /* log_debug("subnode_ct: %d", subnode_ct); */

    /* normalize index - cannot use util routine since there is no
       args node type */
/*     int attr_ct = sealark_loadstmt_binding_count(loadstmt); */
/*     log_debug("attr_ct: %d", attr_ct); */
/*     /\* reverse indexing *\/ */
/*     if (index < 0) { */
/*         if (abs(index) > attr_ct) { */
/*             log_error("abs(%d) > attr_ct", index, attr_ct); */
/*             errno = EINDEX_OUT_OF_BOUNDS; */
/*             return; */
/*         } else { */
/*             index = attr_ct + index; */
/*             /\* log_debug("recurring..."); *\/ */
/*             /\* return sealark_vector_item_for_int(node, index); *\/ */
/*         } */
/*     } */

/*     if (index > attr_ct-1) { */
/*         log_error("index > target count"); */
/*         errno = EINDEX_OUT_OF_BOUNDS; */
/*         return; */
/*     } */
/* #if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS) */
/*     log_debug("\tnormalized idx: %d", index); */
/* #endif */

/*     int arg_idx = 0; */

    errno = 0;
    struct node_s *item = sealark_loadstmt_binding_for_int(loadstmt, index);
    if (errno == 0) {
        /* log_debug("item at %d: %d %s", index, item->tid, TIDNAME(item)); */
        /* sealark_debug_log_ast_outline(item, 0); */
        /* log_debug("new binding:"); */
        /* sealark_debug_log_ast_outline(newattr, 0); */
        struct node_s *newid = utarray_eltptr(newattr->subnodes, 0);
        utarray_erase(item->subnodes, 0, 1);
        utarray_insert(item->subnodes, newid, 0);

        struct node_s *newval = utarray_eltptr(newattr->subnodes, 2);
        utarray_erase(item->subnodes, 2, 1);
        utarray_insert(item->subnodes, newval, 2);
    } else {
        return;
    }
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_rm_attr_at_int(struct node_s *loadstmt,
                                          int index)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_rm_attr_at_int: %d", index);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    /* sealark_debug_log_ast_outline(loadstmt, 0); */

    int subnode_ct = utarray_len(loadstmt->subnodes);
    /* log_debug("subnode_ct: %d", subnode_ct); */

    /* normalize index - cannot use util routine since there is no
       args node type */
    int attr_ct = sealark_loadstmt_binding_count(loadstmt);
    /* log_debug("attr_ct: %d", attr_ct); */
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > attr_ct) {
            log_error("abs(%d) > attr_ct", index, attr_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            return;
        } else {
            index = attr_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > attr_ct-1) {
        log_error("index > tattret count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return;
    }
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("\tidx normalized: %d", index);
#endif

    int span = 2;
    int attr_idx = 0;

    /* start at idx 4, skipping 'load("key",' */
    for (int i = 4; i < subnode_ct; i++) {
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        /* log_debug("item %d/%d %s", i, attr_idx, item->s); */
        if (item->tid == TK_Binding) {
            /* attr_idx = (i - 4) / 2; */
            if (attr_idx == index) {
                /* last subnode is ')' */
                if (i == subnode_ct - 2) span = 1;
                else span = 2;
                /* log_debug("erasing %d span %d: %s", i, span, */
                /*           item->s); */
                utarray_erase(loadstmt->subnodes, i, span);
                return;
            }
            attr_idx++;
        }
    }
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_rm_attr_at_sym(struct node_s *loadstmt,
                                            const char *key)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_rm_attr_at_sym: %s", key);
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    /* sealark_debug_log_ast_outline(loadstmt, 0); */

    int klen = strlen(key);
    int subnode_ct = utarray_len(loadstmt->subnodes);
    /* log_debug("subnode_ct: %d", subnode_ct); */

    int span = 2;
    int attr_idx = 0;
    struct node_s *attr_id;

    /* start at idx 4, skipping 'load("key",' */
    for (int i = 4; i < subnode_ct; i++) {
        struct node_s *item = utarray_eltptr(loadstmt->subnodes, i);
        /* log_debug("item %d/%d %s", i, attr_idx, item->s); */
        if (item->tid == TK_Binding) {
            attr_id = utarray_eltptr(item->subnodes, 0);
            /* log_debug("test attr key: %s", attr_id->s); */
            if ( (strncmp(attr_id->s, key, klen) == 0)
                 && (strlen(attr_id->s) == klen) ) {
                /* last subnode is ')' */
                if (i == subnode_ct - 2) span = 1;
                else span = 2;
                /* log_debug("erasing %d span %d: %s", i, span, */
                /*           item->s); */
                errno = 0;
                utarray_erase(loadstmt->subnodes, i, span);
                return;
            }
            attr_idx++;
        }
    }
    errno = ENOT_FOUND;
    return;
}

/* **************************************************************** */
EXPORT void sealark_loadstmt_replace(struct node_s *loadstmt,
                                            /* int index, */
                                            struct node_s *newval)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_loadstmt_replace");
#endif
    assert(loadstmt->tid == TK_Load_Stmt);
    assert(newval->tid == TK_Load_Stmt);

    utarray_free(loadstmt->subnodes);
    memcpy(loadstmt, newval, sizeof(struct node_s));
    loadstmt->line = -1;
    loadstmt->col  = -1;

    return;
}

/* **************************************************************** */
EXPORT void sealark_pkg_insert_loadstmt_at_int(struct node_s *pkg,
                                               int index,
                                               struct node_s *loadstmt)
{
#ifdef DEBUG_TRACE
    log_debug(">> sealark_pkg_insert_loadstmt_at_int %d", index);
#endif
    assert(pkg->tid == TK_Package);
    assert(loadstmt->tid == TK_Load_Stmt);

    int loadstmt_ct = sealark_pkg_loadstmt_count(pkg);
    /* log_debug("loadstmt_ct: %d", loadstmt_ct); */
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > loadstmt_ct) {
            log_error("abs(%d) > loadstmt_ct", index, loadstmt_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            /* return NULL; */
        } else {
            index = loadstmt_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > loadstmt_ct) {
        log_error("index > target count");
        errno = EINDEX_OUT_OF_BOUNDS;
        return;
    }
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("\tnormalized idx: %d", index);
#endif

    struct node_s *stmt_list = utarray_eltptr(pkg->subnodes, 0);
    struct node_s *small_stmt_list = utarray_eltptr(stmt_list->subnodes, 0);

    int sub_idx = 0;

    sub_idx = sealark_pkg_subidx_for_loadstmt_idx(pkg, index);
    if (sub_idx < 0) {
    }
    /* log_debug("loadstmt index %d == subnode %d", index, sub_idx); */
    struct node_s *comma;

    if (index == loadstmt_ct) {
        /* appending */
        /* log_debug("appending at sub_idx %d, index %d", sub_idx, index); */
        utarray_insert(small_stmt_list->subnodes, loadstmt, sub_idx);
    } else {
        /* log_debug("prepending at %d", sub_idx); */
        utarray_insert(small_stmt_list->subnodes, loadstmt, sub_idx);
    }
    return;
}
