#include "log.h"
#include "utarray.h"
#include "token_debug.h"

const char *token_name[256][2] =
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
/* /\* BSTRING (byte string) below *\/ */
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
/* [TK_RBDQ] = { "TK_RBDQ",  */
[TK_RBRACE] = { "TK_RBRACE", "}" },
[TK_RBRACK] = { "TK_RBRACK", "]" },
/* [TK_RBSQ] = { "TK_RBSQ", */
/* [TK_RDQ] = { "TK_RDQ", */
[TK_RETURN] = { "TK_RETURN", "return" },
[TK_RPAREN] = { "TK_RPAREN", ")" },
[TK_RRANGLE] = { "TK_RRANGLE", ">>" },
[TK_RRANGLE_EQ] = { "TK_RRANGLE_EQ", ">>=" },
/* [TK_RSQ] = { "TK_RSQ", */
/* RSTRING below */
[TK_SEMI] = { "TK_SEMI", ";" },
[TK_SLASH] = { "TK_SLASH", "/" },
[TK_SQ] = { "TK_SQ", "'" },
/* [TK_SQ3] = { "TK_SQ3", */
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
/* [TK_NUMBER] = { "TK_NUMBER", */
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
};

/* void dump_ast(struct obazl_buildfile_s *ast) */
/* { */
/*     log_debug("dump_ast"); */
/*     log_debug("fname: %s", ast->fname); */

/*     struct node_s *p; */
/*     for(p=(struct node_s*)utarray_front(ast->nodelist); */
/*         p!=NULL; */
/*         p=(struct node_s*)utarray_next(ast->nodelist, p)) { */
/*         log_debug("node type: %d", p->type); */
/*     } */
/* } */

void dump_node(struct node_s *node)
{
    log_debug("dump_node: %p", node);
    log_debug("%s[%d] %c (%d:%d)",
              token_name[node->type][0],
              node->type,
              node->type == TK_STRING? node->q: ' ',
              node->line, node->col);
    switch (node->type) {
    case TK_COMMENT: log_debug("\tstarttok: %s", node->s); break;
    case TK_ID: log_debug("\tstarttok: %s", node->s); break;
    case TK_INT: log_debug("\tstarttok: %s", node->s); break;
    case TK_STRING: log_debug("\tstarttok: %s", node->s); break;
    }
    if (node->comments) {
        if (utarray_len(node->comments) > 0) {
            log_debug("\tdumping comments");
            dump_nodes(node->comments);
            log_debug("\tend dumping comments");
        }
    }
    if (node->subnodes) dump_nodes(node->subnodes);
    log_debug("/dump_node");
}

void dump_nodes(UT_array *nodes)
{
    log_debug("dump_nodes: %p, ct: %d", nodes, utarray_len(nodes));

    struct node_s *node=NULL;
    while( (node=(struct node_s*)utarray_next(nodes, node))) {
        /* log_debug("type: %d %s", */
        /*           node->type, */
        /*           token_name[node->type][0]); */
        log_debug("%s[%d] %c (%d:%d)",
                  token_name[node->type][0],
                  node->type,
                  node->type == TK_STRING? node->q: ' ',
                  node->line, node->col);
        /* if (node->s) log_debug("\tstarttok: :]%s[:", node->s); */
        switch (node->type) {
        case TK_COMMENT: log_debug("\tstarttok: %s", node->s); break;
        case TK_ID: log_debug("\tstarttok: :]%s[:", node->s); break;
        case TK_INT: log_debug("\tstarttok: :]%s[:", node->s); break;
        case TK_STRING: log_debug("\tstarttok: %s", node->s); break;
        /* case TK_Call_Sfx: log_debug("\tstarttok: %s", node->s); break; */
        case TK_Dot_Sfx: log_debug("\tstarttok: %s", node->s); break;
        }
        if (node->comments) {
            if (utarray_len(node->comments) > 0) {
                log_debug("  dumping comments");
                dump_nodes(node->comments);
                log_debug("  /dumping comments");
            }
        }
        if (node->subnodes) {
            if (utarray_len(node->subnodes) > 0) {
                log_debug("  subnodes:");
                dump_nodes(node->subnodes);
                log_debug("  /subnodes");
            }
        }
    }
}
