#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_format.h"

#if INTERFACE
struct format_s {
    int leading;
    int toplevel_indent;
    int indent;
};
#endif

struct format_s format = {
                          .leading = 2,
                          .toplevel_indent = 0,
                          .indent = 4
};

/* **************************************************************** */
/* dirty node: added or edited, needs formatting */
EXPORT void sealark_format_dirty_node(struct node_s *nd, int *line, int *col)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    log_debug("sealark_format_DIRTY_node %d %s @ %d:%d",
              nd->tid, TIDNAME(nd), *line, *col);
#endif

    if (sealark_is_printable(nd)) {
        log_debug("formatting %d %s", nd->tid, TIDNAME(nd));
        nd->line = *line;
        nd->col  = *col;
        char *s = sealark_node_printable_string(nd);
        *col += strlen(s);
        if (nd->tid == TK_COMMA) *col++; //???
        return;
    }

    struct node_s *sub;

    switch(nd->tid) {
    case TK_Call_Expr:
        log_debug("formatting TK_Call_Expr");
        /* nd->line = *line; */
        /* nd->col  = *col; */
        sealark_format_call_expr(nd, line, col);
        /* sub = NULL; */
        /* while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) { */
        /*     sealark_format_dirty_node(sub, line, col); */
        /* } */
        break;
    /* case TK_Call_Sfx: */
    /*     log_debug("formatting TK_Call_Sfx"); */
    /*     nd->line = *line; */
    /*     nd->col  = *col; */
    /*     sub = NULL; */
    /*     /\* while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) { *\/ */
    /*     /\*     sealark_format_dirty_node(sub, line, col); *\/ */
    /*     /\* } *\/ */
    /*     break; */
    /* case TK_Arg_List: */
    /*     log_debug("formatting TK_Arg_List"); */
    /*     nd->line = *line; */
    /*     nd->col  = *col; */
    /*     sub = NULL; */
    /*     while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) { */
    /*         sealark_format_dirty_node(sub, line, col); */
    /*     } */
    /*     break; */
    /* case TK_Binding: */
    /*     log_debug("formatting TK_Binding"); */
    /*     nd->line = *line; */
    /*     nd->col  = *col; */
    /*     sub = NULL; */
    /*     _format_binding(nd, line, col); */
    /*     /\* while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) { *\/ */
    /*     /\*     sealark_format_dirty_node(sub, line, col); *\/ */
    /*     /\* } *\/ */
    /*     break; */
    case TK_Load_Stmt:
        log_debug("formatting TK_Load_Stmt");
        nd->line = *line;
        nd->col  = *col;
        *col = *col + 4;
        /* recur on subnodes */
        break;
    case TK_Expr_List:
        log_debug("formatting TK_Expr_List");
        break;
    case TK_List_Expr:
        log_debug("formatting TK_List_Expr");
        break;
    case TK_Assign_Stmt:
        log_debug("formatting TK_Assign_Stmt");
        break;
    default:
        log_error("formatting other %d %s", nd->tid, TIDNAME(nd));
        exit(-1);
    }
}

/* **************************************************************** */
/* clean node: may need vertical adjustment due to preceding splices */
EXPORT void sealark_format_clean_node(struct node_s *nd,
                                      int *mrl, int *mrc)
                                     /* int delta) */
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    /* log_debug("sealark_format_CLEAN_node %d %s, delta %d", */
    /*           nd->tid, TIDNAME(nd), rml); */
#endif

    if (nd->line < *mrl) {
        log_error("xxxxxxxxxxxxxxxx nd->line %d < *mrl %d",
                  nd->line, *mrl);
        /* line depends on node type */
        switch(nd->tid) {
        case TK_Arg_List:
            /* log_debug("FIXME: TK_Arg_List line %d, mrl %d", nd->line, *mrl); */
            /* nd->line = ++(*mrl); */
            (*mrl)++;
            _format_arg_list(nd, mrl, mrc);
            return;
            break;
        case TK_Call_Expr:
            /* log_debug("FIXME: TK_Call_Expr line %d, mrl %d", nd->line, *mrl); */
            sealark_format_call_expr(nd, mrl, mrc);
            break;
        case TK_LOAD:
            log_debug("FIXME: TK_LOAD line %d, mrl %d", nd->line, *mrl);
            break;
        case TK_COMMA:
        case TK_RPAREN:
            nd->line = ++(*mrl);
            /* nd->col  = *mrc = 0; */
            break;
        default:
            log_error("DIRTY NODE! %d %s", nd->tid, TIDNAME(nd));
            //exit(-1);
        }
    }

    *mrl = nd->line;
    if (nd->subnodes) {
        struct node_s *sub = NULL;
        while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
            sealark_format_clean_node(sub, mrl, mrc);
        }
    }
    /* return *mrl; */
}

/* **************************************************************** */
EXPORT void sealark_format_call_expr(struct node_s *call_expr,
                                  int *mrl, int *mrc)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    log_debug("sealark_format_call_expr @ %d:%d", *mrl, *mrc);
#endif

    assert(call_expr->tid == TK_Call_Expr);

    call_expr->line = *mrl;
    call_expr->col = *mrc = 0;

    struct node_s *tgt_id = utarray_eltptr(call_expr->subnodes, 0);
    tgt_id->line = *mrl;
    tgt_id->col  = *mrc;

    struct node_s *call_sfx = utarray_eltptr(call_expr->subnodes, 1);
    call_sfx->line = *mrl;
    call_sfx->col  = *mrc + strlen(tgt_id->s);
    *mrc = call_sfx->col;

    struct node_s *lparen = utarray_eltptr(call_sfx->subnodes, 0);
    lparen->line = *mrl;
    lparen->col  = ++(*mrc);

    struct node_s *arglist = utarray_eltptr(call_sfx->subnodes, 1);
    *mrl += 1;
    _format_arg_list(arglist, mrl, mrc);

    struct node_s *rparen = utarray_eltptr(call_sfx->subnodes, 2);
    rparen->line = ++(*mrl);
    rparen->col  = 0;
    *mrc = 0;
}

/* **************************************************************** */
LOCAL void _format_arg_list(struct node_s *arg_list,
                            int *mrl, int *mrc)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    log_debug("sealark_format_arg_list @ %d:%d (mr: %d:%d)",
              arg_list->line, arg_list->col,
              *mrl, *mrc);
#endif

    assert(arg_list->tid == TK_Arg_List);

    if (arg_list->line < 0) {
        arg_list->line = *mrl;
        /* arg_list->col  = format.indent; */
        *mrc = arg_list->col;

        int blen;
        struct node_s *sub = NULL;
        int i = 0;
        while( (sub=(struct node_s*)utarray_next(arg_list->subnodes, sub)) ) {
            if (sub->tid == TK_Binding) {
                blen = _format_binding(sub, mrl, mrc);
            } else {
                if (sub->tid == TK_COMMA) {
                    sub->line = (*mrl) ++;
                    /* sub->col  = blen + 1; */
                    *mrc = sub->col;
                } else {
                    log_error("UNEXPECTED NODE TIME IN ARGLIST");
                    errno = EUNEXPECTED_STATE;
                    //return -1;
                }
            }
            i++;
        }
    } else {
        if (arg_list->line < *mrl) {
            /* needs line-only reformatting because of previous edits */
            int delta = *mrl - arg_list->line;
            arg_list->line += delta;

            struct node_s *sub = NULL;
            int i = 0;
            while( (sub=(struct node_s*)utarray_next
                    (arg_list->subnodes, sub)) ) {
                _vformat(sub, delta);
            }
        }
    }
}

LOCAL int _format_binding(struct node_s *binding, int *mrl, int *mrc)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    log_debug("_format_binding @ %d:%d", *mrl, *mrc);
#endif
    int blen = 0;

    /* int delta = *mrl - binding->line; */
    /* if (delta <= 0) */
    /*     delta = 0; */
    /* log_debug("DELTA XXXX: %d", delta); */

    /* (*mrl)++; */
    *mrc = format.indent;

    binding->line = *mrl;
    binding->col  = *mrc;

    struct node_s *bkey = utarray_eltptr(binding->subnodes, 0);
    bkey->line = *mrl;
    bkey->col  = *mrc;

    struct node_s *beq = utarray_eltptr(binding->subnodes, 1);
    beq->line = *mrl;
    beq->col  = *mrc + strlen(bkey->s) + 1;

    struct node_s *bval = utarray_eltptr(binding->subnodes, 2);
    bval->line = *mrl;
    bval->col  = beq->col + 2;
    *mrc = bval->col;

    /* now advance mrc IF we were dirty */
    switch(bval->tid) {
    case TK_STRING:
        *mrc += strlen(bval->s);
        break;
    case TK_ID:
        *mrc += strlen(bval->s);
        break;
    case TK_INT:
        *mrc += strlen(bval->s);
        break;
    case TK_List_Expr:
        _format_list_expr(bval, mrl, mrc);
        break;
    default:
        log_error("binding val %d %s", bval->tid, TIDNAME(bval));
        exit(-1);
    }
}

/* **************************************************************** */
LOCAL void _format_list_expr(struct node_s *list_expr,
                            int *mrl, int *mrc)
{
    struct node_s *lbrack = utarray_eltptr(list_expr->subnodes, 0);
    lbrack->line = *mrl; //list_expr->line;
    /* lbrack->col  = *mrc; //list_expr->col; */

    struct node_s *expr_list = utarray_eltptr(list_expr->subnodes, 1);
    expr_list->line = *mrl; // lbrack->line;
    /* expr_list->col  = ++(*mrc); // lbrack->col + 1; */

    /* int col = expr_list->col; */
    struct node_s *sub = NULL;
    while( (sub=(struct node_s*)utarray_next(expr_list->subnodes, sub)) ) {
        if (sub->tid == TK_INT) {
            sub->line = *mrl;
            /* sub->col  = *mrc; */
            *mrc += strlen(sub->s);
            continue;
        }
        if (sub->tid == TK_STRING) {
            sub->line = *mrl;
            /* sub->col  = *mrc; */
            *mrc += strlen(sub->s);
            continue;
        }
        if (sub->tid == TK_ID) {
            sub->line = *mrl;
            /* sub->col  = *mrc; */
            *mrc += strlen(sub->s);
            continue;
        }
        if (sub->tid == TK_COMMA) {
            sub->line = *mrl;
            /* sub->col  = *mrc; */
            *mrc += 2;
            continue;
        }
        log_debug("UNEXPECTED node type in bval list: %d %s",
                  sub->tid, TIDNAME(sub));
        errno = EUNEXPECTED_STATE;
    }
    struct node_s *rbrack = utarray_eltptr(list_expr->subnodes, 2);
    rbrack->line = *mrl;
    rbrack->col  = *mrc;
    (*mrc)++;
}

/* vertical adjustment only */
LOCAL int _vformat(struct node_s *node, int delta)
{
/* #if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT) */
/*     log_debug("_vformat %d", delta); */
/* #endif */

    node->line += delta;
    if (node->subnodes) {
        struct node_s *sub = NULL;
        while( (sub=(struct node_s*)utarray_next(node->subnodes, sub)) ) {
            _vformat(sub, delta);
        }
    }
}

/* **************************************************************** */
EXPORT struct node_s *sealark_format_pkg_normalize(struct node_s *pkg)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    log_debug("sealark_format_normalize");
#endif
    assert(pkg->tid == TK_Package);

    int line = 0;
    int col  = 0;

    pkg->line = line;
    pkg->col  = col;

    struct node_s *sub = NULL;
    int i = 0;
    while( (sub=(struct node_s*)utarray_next(pkg->subnodes, sub)) ) {
        _format_normalize(sub, &line, &col);
    }
}

LOCAL struct node_s *_format_normalize(struct node_s *nd,
                                       int *line, int *col)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT)
    log_debug("_format_normalize");
#endif

    switch(nd->tid) {
    }

    struct node_s *sub = NULL;
    int i = 0;
    /* while( (sub=(struct node_s*)utarray_next(pkg->subnodes, sub)) ) { */
    /*     _format_normalize(sub, &line, &col); */
    /* } */
}

EXPORT struct node_s *sealark_format_rm_trailing_commas(struct node_s *node)
{
/* #if defined(DEBUG_TRACE) || defined(DEBUG_FORMAT) */
/*     log_debug("sealark_format_rm_trailing_commas %d %s", */
/*               node->tid, TIDNAME(node)); */
/* #endif */

    struct node_s *maybe;
    struct node_s *last;

    if (node->subnodes) {
        struct node_s *sub = NULL;
        while( (sub=(struct node_s*)utarray_next(node->subnodes, sub)) ) {
            sealark_format_rm_trailing_commas(sub);
        }
        int len = utarray_len(node->subnodes);
        if (len > 0) {
            sub = utarray_eltptr(node->subnodes, len-1);
            if ( (sub->tid == TK_RBRACK)
                 || (sub->tid == TK_RPAREN)
                 || (sub->tid == TK_RBRACE) ) {
                maybe = utarray_eltptr(node->subnodes, len-2);
                if (maybe->tid == TK_COMMA) {
                    /* log_debug("COMMA!!!!"); */
                    utarray_erase(node->subnodes, len-2, 1);
                }
            }
        }
    }
    return node;
}
