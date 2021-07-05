#include "log.h"
#include "nodes.h"

EXPORT const char *token_name[256][2] =
    {
     [TK_ALIAS] = { "TK_ALIAS", "" },
     [TK_AMP] = { "TK_AMP", "&" },
     [TK_AMP_EQ] = { "TK_AMP_EQ", "&=" },
     [TK_AND] = { "TK_AND", "and" },
     [TK_ARROW] = {"TK_ARROW", "->"},
     [TK_AS] = { "TK_AS", "as" },
     [TK_ASSERT] = { "TK_ASSERT", "assert" },
     [TK_BANG] = { "TK_BANG", "!" },
     [TK_BANG_EQ] = { "TK_BANG_EQ", "!=" },
     [TK_BREAK] = { "TK_BREAK", "break" },
     [TK_BLANK] = { "TK_BLANK", "" },
     [TK_CARET] = { "TK_CARET", "^" },
     [TK_CARET_EQ] = { "TK_CARET_EQ", "^=" },
     [TK_CLASS] = { "TK_CLASS", "class" },
     [TK_COLON] = { "TK_COLON", ":" },
     [TK_ICOLON] = { "TK_ICOLON", ":" },
     [TK_COMMA] = { "TK_COMMA", "," },
     [TK_COMMENT] = { "TK_COMMENT", "" },
     [TK_CONTINUE] = { "TK_CONTINUE", "continue" },
     [TK_DEF] = { "TK_DEF", "def" },
     [TK_DEL] = { "TK_DEL", "del" },
     [TK_SLASH2] = { "TK_SLASH2", "//" },
     [TK_SLASH2_EQ] = { "TK_SLASH2_EQ", "//=" },
     [TK_DIV_EQ] = { "TK_DIV_EQ", "/=" },
     [TK_DOT] = { "TK_DOT", "." },
     [TK_DQ] = { "TK_DQ", "\"" },
     [TK_ELIF] = { "TK_ELIF", "elif" },
     [TK_ELSE] = { "TK_ELSE", "else" },
     [TK_EQ] = { "TK_EQ", "=" },
     [TK_EQ2] = { "TK_EQ2", "==" },
     [TK_ESC_BACKSLASH] = { "TK_ESC_BACKSLASH", "\\" },
     [TK_EXCEPT] = { "TK_EXCEPT", "except" },
     [TK_FINALLY] = { "TK_FINALLY", "finally" },
     [TK_FLOAT] = { "TK_FLOAT", "float" },
     [TK_FLOAT_LIT] = { "TK_FLOAT_LIT", "" },
     [TK_FOR] = { "TK_FOR", "for" },
     [TK_FROM] = { "TK_FROM", "from" },
     [TK_GE] = {"TK_GE", ">="},
     [TK_GLOBAL] = { "TK_GLOBAL", "global" },
     [TK_ID] = { "TK_ID", "" },
     [TK_IF] = { "TK_IF", "if" },
     [TK_IMPORT] = { "TK_IMPORT", "import" },
     [TK_INT_DEC] = { "TK_INT_DEC", "" },
     [TK_INT_HEX] = { "TK_INT_HEX", "" },
     [TK_INT_OCT] = { "TK_INT_OCT", "" },
     [TK_INT] = { "TK_INT", "" },
     [TK_IN] = { "TK_IN", "in" },
     [TK_IS] = { "TK_IS", "is" },
     [TK_LAMBDA] = { "TK_LAMBDA", "lambda" },
     [TK_LANGLE] = { "TK_LANGLE", "<" },
     [TK_LBRACE] = { "TK_LBRACE", "{" },
     [TK_LBRACK] = { "TK_LBRACK", "[" },
     [TK_LE] = {"TK_LE", "<="},
     [TK_LLANGLE] = { "TK_LLANGLE", "<<" },
     [TK_LLANGLE_EQ] = { "TK_LLANGLE_EQ", "<<=" },
     [TK_LOAD] = { "TK_LOAD", "load" },
     [TK_LPAREN] = { "TK_LPAREN", "(" },
     [TK_MINUS] = { "TK_MINUS", "-" },
     [TK_MINUS_EQ] = { "TK_MINUS_EQ", "-=" },
     [TK_NONLOCAL] = { "TK_NONLOCAL", "nonlocal" },
     [TK_NOT] = { "TK_NOT", "not" },
     [TK_OR] = { "TK_OR", "or" },
     [TK_PASS] = { "TK_PASS", "pass" },
     [TK_PCT] = { "TK_PCT", "%" },
     [TK_PCT_EQ] = { "TK_PCT_EQ", "%=" },
     [TK_PLUS] = { "TK_PLUS", "+" },
     [TK_PLUS_EQ] = { "TK_PLUS_EQ", "+=" },
     [TK_RAISE] = { "TK_RAISE", "raise" },
     [TK_RANGLE] = { "TK_RANGLE", ">" },
     [TK_RBRACE] = { "TK_RBRACE", "}" },
     [TK_RBRACK] = { "TK_RBRACK", "]" },
     [TK_RETURN] = { "TK_RETURN", "return" },
     [TK_RPAREN] = { "TK_RPAREN", ")" },
     [TK_RRANGLE] = { "TK_RRANGLE", ">>" },
     [TK_RRANGLE_EQ] = { "TK_RRANGLE_EQ", ">>=" },
     [TK_SEMI] = { "TK_SEMI", ";" },
     [TK_SLASH] = { "TK_SLASH", "/" },
     [TK_SQ] = { "TK_SQ", "'" },
     [TK_STAR] = { "TK_STAR", "*" },
     [TK_STAR2] = { "TK_STAR2", "**" },
     [TK_STAR_EQ] = { "TK_STAR_EQ", "*=" },
     [TK_STRING] = { "TK_STRING", ""},
     [TK_BSTRING] = { "TK_BSTRING", ""},    /* byte string */
     [TK_BRSTRING] = { "TK_BRSTRING", ""},    /* raw byte string */
     [TK_RSTRING] = { "TK_RSTRING", ""},
     [TK_RBSTRING] = { "TK_RBSTRING", ""},    /* raw byte string */
     [TK_MLSTRING] = { "TK_MLSTRING", ""},    /* multi-line string */
     [TK_MLBSTRING] = { "TK_MLBSTRING", ""},
     [TK_MLBRSTRING] = { "TK_MLBRSTRING", ""},
     [TK_MLRSTRING] = { "TK_MLRSTRING", ""},
     [TK_MLRBSTRING] = { "TK_MLRBSTRING", ""},
     [TK_TILDE] = { "TK_TILDE", "~" },
     [TK_TRY] = { "TK_TRY", "try" },
     [TK_VBAR] = { "TK_VBAR", "|" },
     [TK_VBAR_EQ] = { "TK_VBAR_EQ", "|=" },
     [TK_WHILE] = { "TK_WHILE", "while" },
     [TK_WITH] = { "TK_WITH", "with" },
     [TK_YIELD] = { "TK_YIELD", "yield" },
     [TK_NEWLINE] = { "TK_NEWLINE", "" },

     /* non-terminals */
     [TK_Arg_List] = { "TK_Arg_List", "" },
     [TK_Arg_Named] = { "TK_Arg_Named", "" },
     [TK_Arg_Star] = { "TK_Arg_Star", "" },
     [TK_Arg_Star2] = { "TK_Arg_Star2", "" },
     [TK_Assign_Stmt] = { "TK_Assign_Stmt", "" },
     [TK_Bin_Expr] = { "TK_Bin_Expr", "" },
     [TK_Call_Expr] = { "TK_Call_Expr", "" },
     [TK_Call_Sfx] = { "TK_Call_Sfx", "" },
     [TK_Comp_Clause] = { "TK_Comp_Clause", "" },
     [TK_Def_Stmt] = { "TK_Def_Stmt", "" },
     [TK_Dict_Comp] = { "TK_Dict_Comp", "" },
     [TK_Dict_Entry] = { "TK_Dict_Entry", "" },
     [TK_Dict_Entry_List] = { "TK_Dict_Entry_List", "" },
     [TK_Dict_Expr] = { "TK_Dict_Expr", "" },
     [TK_Dot_Expr] = { "TK_Dot_Expr", "" },
     [TK_Dot_Sfx] = { "TK_Dot_Sfx", "" },
     [TK_Expr] = { "TK_Expr", "" },
     [TK_Expr_List] = { "TK_Expr_List", "" },
     [TK_Expr] = { "TK_Expr", "" },
     [TK_For_Stmt] = { "TK_For_Stmt", "" },
     [TK_If_Expr] = { "TK_If_Expr", "" },
     [TK_If_Stmt] = { "TK_If_Stmt", "" },
     [TK_Indent_Block] = { "TK_Indent_Block", "" },
     [TK_Lambda_Expr] = { "TK_Lambda_Expr", "" },
     [TK_List_Comp] = { "TK_List_Comp", "" },
     [TK_List_Expr] = { "TK_List_Expr", "" },
     [TK_Load_Stmt] = { "TK_Load_Stmt", "" },
     [TK_Loop_Vars] = { "TK_Loop_Vars", "" },
     [TK_Param_List] = { "TK_Param_List", "" },
     [TK_Param_Named] = { "TK_Param_Named", "" },
     [TK_Param_Star] = { "TK_Param_Star", "" },
     [TK_Param_Star2] = { "TK_Param_Star2", "" },
     [TK_Paren_Expr] = { "TK_Paren_Expr", "" },
     [TK_Primary_Expr] = { "TK_Primary_Expr", "" },
     [TK_Return_Expr] = { "TK_Return_Expr", "" },
     [TK_Slice_Sfx] = { "TK_Slice_Sfx", "" },
     [TK_Slice_Expr] = { "TK_Slice_Expr", "" },
     [TK_SmallStmt_List] = { "TK_SmallStmt_List", "" },
     [TK_Stmt] = { "TK_Stmt", "" },
     [TK_Stmt_List] = { "TK_Stmt_List", "" },
     [TK_Unary_Expr] = { "TK_Unary_Expr", "" },

     NULL
    };

struct obazl_buildfile_s {
    char *fname;
    struct node_s *root;
    /* UT_array *nodelist; */
};

#if EXPORT_INTERFACE
#include "utarray.h"
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

EXPORT void node_dtor(void *_elt) {
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
