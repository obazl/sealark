#include "log.h"
#include "nodes.h"

#if INTERFACE
#include "utarray.h"

struct obazl_buildfile_s {
    char *fname;
    struct node_s *root;
    /* UT_array *nodelist; */
};

struct node_s {
    /* enum node_type_e type; */
    int type;
    int line, col;
    bool trailing_newline;
    char q;
    /* union { */
        char *s;
        UT_array *subnodes;
    /* }; */
    UT_array *comments;         /* list of struct node_s type comment */
};
#endif

void nodelist_copy(UT_array *_dst, UT_array *_src)
{
    log_debug("node_copy: %p <- %p", _dst, _src);
}

void node_copy(void *_dst, const void *_src)
{
    log_debug("node_copy"); // : %p <- %p", _dst, _src);
    struct node_s *dst = (struct node_s*)_dst;
    struct node_s *src = (struct node_s*)_src;
    dst->type = src->type;

    /* log_debug("node posn: %d:%d", src->line, src->col); */
    /* log_debug("\tnode type: %d", src->type); */
    log_debug("\t%s[%d] %c (%d:%d) %s",
              token_name[src->type][0],
              src->type,
              src->q? src->q : ' ',
              src->line, src->col,
              (src->s == NULL? "" : src->s));

    /* if (src->subnodes) */
    /*     log_trace("  src subnode ct: %d", */
    /*               utarray_len(src->subnodes)); */
    *dst = *src;
    /* if (dst->subnodes) */
    /*     log_trace("  dest subnode ct: %d", */
    /*               utarray_len(dst->subnodes)); */
}

void node_dtor(void *_elt) {
    /* log_debug("NODE_DTOR: %s (%d)", */
    /*           token_name[((struct node_s*)_elt)->type][0], */
    /*           ((struct node_s*)_elt)->type); */
    struct node_s *elt = (struct node_s*)_elt;
    if (elt->s) {
        /* log_debug("\tfreeing string %s", elt->s); */
        free(elt->s);
    }
    if (elt->comments) utarray_free(elt->comments);
    if (elt->subnodes) utarray_free(elt->subnodes);
}

/* nodelist: UT_array of node_s */
UT_icd node_icd = {sizeof(struct node_s), NULL, node_copy, node_dtor};

#if INTERFACE
struct comma_s {
    int line, col;
}
#endif

#if INTERFACE
struct comment_s {
    int line, col;
    char *s;
}
#endif

void comment_copy(void *_dst, const void *_src)
{
    /* log_debug("comment_copy"); */
    struct comment_s *src = (struct comment_s*)_src;
    struct comment_s *dst = (struct comment_s*)_dst;
    dst->line = src->line;
    dst->col  = src->col;
    dst->s = src->s? strdup(src->s) : NULL;
}

void comment_dtor(void *_comment) {
    log_debug("XXXXXXXXXXXXXXXX COMMENT_DTOR");
    struct comment_s *comment = (struct comment_s*)_comment;
    if (comment->s) free(comment->s);
}

UT_icd comments_icd = {sizeof(struct comment_s), NULL,
                       comment_copy, comment_dtor};

#if INTERFACE
struct symdecl_s {
    int line, col;
    char *sym;
}
#endif

void sym_copy(void *_dst, void *_src)
{
    /* log_debug("sym_copy"); */
    struct symdecl_s *src = (struct symdecl_s*)_src;
    struct symdecl_s *dst = (struct symdecl_s*)_dst;
    dst->line = src->line;
    dst->col  = src->col;
    dst->sym = src->sym? strdup(src->sym) : NULL;
}

void sym_dtor(void *_sym) {
    log_debug("XXXXXXXXXXXXXXXX SYM_DTOR");
    struct symdecl_s *sym = (struct symdecl_s*)_sym;
    if (sym->sym) free(sym->sym);
}

UT_icd symdecl_icd = {sizeof(struct symdecl_s), NULL, NULL, NULL};

#if INTERFACE
struct identifier_s {
    int line, col;
    char *s;
}
#endif

/* #if INTERFACE */
/* struct alias_s { */
/*     int line, col; */
/*     UT_array *subnodes;         /\* ID cmt* EQ cmt* SYM *\/ */
/*     /\* struct { *\/ */
/*     /\*     int line, col; *\/ */
/*     /\*     char *s; *\/ */
/*     /\* } alias; *\/ */
/*     /\* struct { *\/ */
/*     /\*     int line, col; *\/ */
/*     /\*     char *s; *\/ */
/*     /\* } sym; *\/ */
/* } */
/* #endif */

/* void alias_copy(void *_dst, void *_src) */
/* { */
/*     log_debug("alias_copy"); */
/*     _dst = _src; */
/*     /\* struct alias_s *src = (struct alias_s*)_src; *\/ */
/*     /\* struct alias_s *dst = (struct alias_s*)_dst; *\/ */
/*     /\* dst->alias.line = src->alias.line; *\/ */
/*     /\* dst->alias.col  = src->alias.col; *\/ */
/*     /\* dst->alias.s = src->alias.s? strdup(src->alias.s) : NULL; *\/ */
/*     /\* dst->sym.line = src->sym.line; *\/ */
/*     /\* dst->sym.col  = src->sym.col; *\/ */
/*     /\* dst->sym.s = src->sym.s? strdup(src->sym.s) : NULL; *\/ */
/* } */

/* void alias_dtor(void *_alias) { */
/*     log_debug("XXXXXXXXXXXXXXXX ALIAS_DTOR"); */
/*     struct alias_s *alias = (struct alias_s*)_alias; */
/*     /\* if (alias->alias) free(alias->alias); *\/ */
/* } */
