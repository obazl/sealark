#include <ctype.h>

#include "log.h"
#include "sealark_nodes.h"

EXPORT const int printable_tokens[] =
    {
     TK_ID, TK_STRING,
     TK_AMP, TK_AMP_EQ, TK_AND, TK_ARROW, TK_AS,
     TK_ASSERT, TK_BANG, TK_BANG_EQ, TK_BREAK, TK_CARET,
     TK_CARET_EQ, TK_CLASS, TK_COLON, TK_COMMA, TK_COMMENT, TK_CONTINUE,
     TK_DEF, TK_DEL, TK_DOT, TK_ELIF, TK_ELSE,
     TK_EQ, TK_EQ2, TK_BACKSLASH2, TK_EXCEPT, TK_FINALLY,
     TK_FLOAT, TK_FOR, TK_FROM, TK_GE, TK_GLOBAL,
     TK_IF, TK_IMPORT, TK_IN, TK_INT, TK_IS, TK_LAMBDA,
     TK_LANGLE, TK_LBRACE, TK_LBRACK, TK_LE, TK_LLANGLE,
     TK_LLANGLE_EQ, TK_LOAD, TK_LPAREN, TK_MINUS, TK_MINUS_EQ,
     TK_NONLOCAL, TK_NOT, TK_OR, TK_PASS, TK_PCT,
     TK_PCT_EQ, TK_PLUS, TK_PLUS_EQ, TK_RAISE, TK_RANGLE,
     TK_RBRACE, TK_RBRACK, TK_RETURN, TK_RPAREN, TK_RRANGLE,
     TK_RRANGLE_EQ, TK_SEMI, TK_SLASH, TK_SLASH_EQ, TK_SLASH2,
     TK_SLASH2_EQ, TK_STAR, TK_STAR2, TK_STAR_EQ, TK_TILDE,
     TK_TRY, TK_VBAR, TK_WHILE, TK_WITH, TK_YIELD,
     0
    };

#if EXPORT_INTERFACE
#define TIDNAME(X) (token_name[ X->tid ][0])
#endif

const int token_ct = 136;
EXPORT const char *token_name[136][2] =
    {
     /* [TK_ALIAS] = { "TK_ALIAS", "" }, */
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
     [TK_COMMA] = { "TK_COMMA", "," },
     [TK_COMMENT] = { "TK_COMMENT", "" },
     [TK_CONTINUE] = { "TK_CONTINUE", "continue" },
     [TK_DEF] = { "TK_DEF", "def" },
     [TK_DEL] = { "TK_DEL", "del" },
     [TK_DOT] = { "TK_DOT", "." },
     [TK_ELIF] = { "TK_ELIF", "elif" },
     [TK_ELSE] = { "TK_ELSE", "else" },
     [TK_EQ] = { "TK_EQ", "=" },
     [TK_EQ2] = { "TK_EQ2", "==" },
     [TK_BACKSLASH2] = { "TK_BACKSLASH2", "\\" },
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
     [TK_SLASH_EQ] = { "TK_SLASH_EQ", "/=" },
     [TK_SLASH2] = { "TK_SLASH2", "//" },
     [TK_SLASH2_EQ] = { "TK_SLASH2_EQ", "//=" },
     [TK_STAR] = { "TK_STAR", "*" },
     [TK_STAR_EQ] = { "TK_STAR_EQ", "*=" },
     [TK_STAR2] = { "TK_STAR2", "**" },
     [TK_STRING] = { "TK_STRING", ""},
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
     [TK_Binding] = { "TK_Binding", "" },
     [TK_Arg_Star] = { "TK_Arg_Star", "" },
     [TK_Arg_Star2] = { "TK_Arg_Star2", "" },
     [TK_Assign_Stmt] = { "TK_Assign_Stmt", "" },
     [TK_Attr] = { "TK_Attr", "" },
     [TK_Attr_Name] = { "TK_Attr_Name", "" },
     [TK_Attr_Value] = { "TK_Attr_Value", "" },
     [TK_Bin_Expr] = { "TK_Bin_Expr", "" },
     [TK_Package] = { "TK_Package", "" },
     [TK_Call_Expr] = { "TK_Call_Expr", "" },
     [TK_Call_Sfx] = { "TK_Call_Sfx", "" },
     [TK_Comp_Clause] = { "TK_Comp_Clause", "" },
     [TK_Def_Compound] = { "TK_Def_Compound", "" },
     [TK_Def_Stmt] = { "TK_Def_Stmt", "" },
     [TK_Dict_Comp] = { "TK_Dict_Comp", "" },
     [TK_Dict_Entry] = { "TK_Dict_Entry", "" },
     [TK_Dict_Entry_List] = { "TK_Dict_Entry_List", "" },
     [TK_Dict_Expr] = { "TK_Dict_Expr", "" },
     [TK_Dot_Expr] = { "TK_Dot_Expr", "" },
     [TK_Dot_Sfx] = { "TK_Dot_Sfx", "" },
     [TK_Expr] = { "TK_Expr", "" },
     [TK_Expr_List] = { "TK_Expr_List", "" },
     [TK_For_Stmt] = { "TK_For_Stmt", "" },
     [TK_If_Expr] = { "TK_If_Expr", "" },
     [TK_If_Stmt] = { "TK_If_Stmt", "" },
     [TK_Indent_Block] = { "TK_Indent_Block", "" },
     [TK_Lambda_Expr] = { "TK_Lambda_Expr", "" },
     [TK_List_Comp] = { "TK_List_Comp", "" },
     [TK_List_Expr] = { "TK_List_Expr", "" },
     [TK_Load_Stmt] = { "TK_Load_Stmt", "" },
     [TK_Loop_Vars] = { "TK_Loop_Vars", "" },
     [TK_Node_List] = { "TK_Node_List", "" },
     [TK_Param_List] = { "TK_Param_List", "" },
     [TK_Param_Named] = { "TK_Param_Named", "" },
     [TK_Param_Star] = { "TK_Param_Star", "" },
     [TK_Param_Star2] = { "TK_Param_Star2", "" },
     [TK_Paren_Expr] = { "TK_Paren_Expr", "" },
     [TK_Primary_Expr] = { "TK_Primary_Expr", "" },
     [TK_Return_Expr] = { "TK_Return_Expr", "" },
     [TK_Slice_Sfx] = { "TK_Slice_Sfx", "" },
     [TK_Slice_Expr] = { "TK_Slice_Expr", "" },
     [TK_Small_Stmt_List] = { "TK_Small_Stmt_List", "" },
     [TK_Stmt] = { "TK_Stmt", "" },
     [TK_Stmt_List] = { "TK_Stmt_List", "" },
     [TK_Target] = { "TK_Target", "" },
     [TK_Target_List] = { "TK_Target_List", "" },
     [TK_Unary_Expr] = { "TK_Unary_Expr", "" },
     [TK_Unspecified] = { "TK_Unspecified", "" }

     /* NULL */
    };

#if EXPORT_INTERFACE
#include "utarray.h"

enum quote_type_e
    {
     SQUOTE     = 1,  /* single quote */
     /* DQUOTE     = 2,  /\* double quote *\/ */
     TRIPLE     = 2, //4,  /* triple quote */
     BINARY_STR = 4, //16, /* PLAIN if both of these unset */
     RAW_STR    = 16 //32
    };

struct node_s {
    /* enum node_type_e type; */
    int tid;
    int line, col;

    char *fname; // until we come up with a better solution

    bool trailing_newline; // FIXME: do we need to retain this?
    enum quote_type_e qtype;
    char *s;
    UT_array *comments;         /* list of struct node_s type comment */
    UT_array *subnodes;
};
#endif

#if EXPORT_INTERFACE
struct mapentry_s {
    int index;
    struct node_s *node;
};
#endif

/* mapentry node pointer points to object owned by other */
EXPORT UT_icd mapentry_icd = {sizeof(struct mapentry_s), NULL, NULL, NULL};

/* **************************************************************** */
UT_array *split_small_stmt_list(struct node_s *iblock,
                                struct node_s* list, int indent)
{
    // log_debug("split_small_stmt_list, indent: %d", indent);

    UT_array *blocks;
    utarray_new(blocks, &node_icd);

    UT_array *instmts;
    utarray_new(instmts, &node_icd);

    struct node_s *outblock = calloc(sizeof(struct node_s), 1);
    utarray_new(outblock->subnodes, &node_icd);
    outblock->tid = list->tid;
    /* outblock->line = list->line; */
    /* outblock->col  = list->col; */

    int block_indent;
    struct node_s *node=NULL;
    int len = utarray_len(list->subnodes);
    // log_debug("small stmt list len: %d", len);
    int i;
    for (i = 0; i < len; i++) {
        // log_debug("small stmt list item %d", i);
        node = utarray_eltptr(list->subnodes, i);
        /* log_debug("small stmt list node t: %s[%d] indent %d", */
        /*           token_name[node->tid][0], node->tid, node->col); */

        if (i == 0) {
            if (node->col > indent) {
                // log_debug("first: IN");
                block_indent = node->col;
                utarray_push_back(instmts, node);
            } else {
                // log_debug("EMPTY BLOCK");
            }
        } else {
            if (node->col == block_indent) {
                // log_debug("IN");
                utarray_push_back(instmts, node);
            } else {
                if (node->col > block_indent) {
                    log_warn("EXTRA INDENTATION at line %d", node->line);
                    utarray_push_back(instmts, node);
                } else {
                    if (node->col > indent) {
                        log_error("ERROR: bad indent on line %d", node->line);
                        exit(EXIT_FAILURE);
                    } else {
                        // log_debug("OUT");
                        utarray_push_back(outblock->subnodes, node);
                    }
                }
            }
        }
    }
    iblock->subnodes = instmts;
    utarray_push_back(blocks, iblock);
    utarray_push_back(blocks, outblock);
    /* dump_nodes(blocks); */
    /* return blocks; */

    /* testing */
    /* utarray_push_back(blocks, list); */
    return blocks;
}

UT_array *split_stmt_list(struct node_s* iblock, int indent)
{
    // log_debug("split_stmt_list, indent: %d", indent);
    log_error("NOT YET SUPPORTED");
    exit(EXIT_FAILURE);
}

UT_array *split_iblock(struct node_s* iblock, int indent)
{
    // log_debug("split_iblock, indent: %d", indent);
    UT_array *blocks;
    utarray_new(blocks, &node_icd);

    int block_indent;
    /* struct node_s *front = utarray_front(iblock->subnodes); */
    /* block_indent = front->col; */
    /* struct node_s * = utarray_next(iblock->subnodes, front); */

    struct node_s *node=NULL;
    int len = utarray_len(iblock->subnodes);
    UT_array *test_blocks;
    for (int i = 0; i < len; i++) {
        node = utarray_eltptr(iblock->subnodes, i);
        /* log_debug("iblock node t: %s[%d] indent %d", */
        /*           token_name[node->tid][0], node->tid, node->col); */

        if (node->tid == TK_Small_Stmt_List) {
            blocks = split_small_stmt_list(iblock, node, indent);
        } else {
            if (node->tid == TK_Stmt_List) {
                blocks = split_stmt_list(node, indent);
            } else {
            }
        }
        /* i++; */
    }
    /* first block is iblock */
    return blocks;
}

/* **************************************************************** */
EXPORT char *sealark_tid_to_string(int tid)
{
/* #ifdef DEBUG_TRACE */
/*     log_debug("token_id_to_kw %d", tid); */
/* #endif */

    char *tag = calloc(64, sizeof(char));
    int len = strlen(token_name[tid][0]);
    strncpy(tag, (char*)token_name[tid][0], len);
    tag[len+1] = '\0';
    /* log_debug("tag %s", tag); */


    /* to camel_case */
    for (int j = 0; j < len; j++) {
        if (tag[j] == '_') tag[j] = '-';
        tag[j] = tolower(tag[j]);
    }
    /* log_debug("tag %s %d", tag, strlen(tag)); */

    return tag + 3; // remove tk- prefix

    /* /\* the C names use _ but scheme uses - so we have to convert *\/ */
    /* /\* fix me: make a static table so we don't have to do all this computing *\/ */
    /* char workbuf[64]; */
    /* int j; */
    /* for(int i = 1; i < 256; i++) { */
    /*     /\* log_debug("tag: %s, toknm: %s", tag, token_name[i][0]); *\/ */
    /*     if (token_name[i][0] == NULL) return -1; */
    /*     len = strlen(token_name[i][0]) + 1; /\* add one for \0 *\/ */
    /*     strncpy(workbuf, token_name[i][0], len); */
    /*     for(j=0; j < len; j++) if (workbuf[j] == '_') workbuf[j] = '-'; */
    /*     /\* log_debug("CONVERTED TAGSTRING: %s", workbuf); *\/ */
    /*     if ( strncmp(tag, workbuf, len) == 0 ) { */
    /*         /\* log_debug("MATCH %d", i); *\/ */
    /*         return i; */
    /*     } */
    /* } */
}

EXPORT int sealark_kw_to_tid(char *kw)
{
#ifdef DEBUG_TRACE
    log_debug("sealark_kw_to_tid: %s", kw);
#endif
    char tag[128];
    sprintf(tag, "TK-%s", kw);

    /* tokens for strings and identifiers are special cased */
    if (strcmp(kw, "string") == 0 && strlen(kw) == 6) {
        sprintf(tag, "TK-STRING");
    } else {
        if (strcmp(kw, "id") == 0 && strlen(kw) == 2) {
            sprintf(tag, "TK-ID");
        } else {
            if (strcmp(kw, "int") == 0 && strlen(kw) == 3) {
                sprintf(tag, "TK-INT");
            } else {
                int len = strlen(tag);
                tag[3] = toupper(tag[3]);
                /* to camel_case */
                for (int j = 0; j < len; j++) {
                    if (tag[j-1] == '-')
                        tag[j] = toupper(tag[j]);
                }
            }
        }
    }
    log_debug("sealark_kw_to_tid: %s", tag);

    /* the C names use _ but scheme uses - so we have to convert */
    /* fix me: make a static table so we don't have to do all this computing */
    char workbuf[64];
    int j, len;
    for(int i = 1; i < token_ct; i++) {
        /* log_debug("tag: %s, toknm: %s", tag, token_name[i][0]); */
        if (token_name[i][0] == NULL) return -1;
        len = strlen(token_name[i][0]) + 1; /* add one for \0 */
        strncpy(workbuf, token_name[i][0], len);
        for(j=0; j < len; j++) if (workbuf[j] == '_') workbuf[j] = '-';
        /* log_debug("CONVERTED TAGSTRING: %s", workbuf); */
        if ( strncmp(tag, workbuf, len) == 0 ) {
            /* log_debug("MATCH %d", i); */
            return i;
        }
    }
    return -1;
}

/* **************************************************************** */
#if EXPORT_INTERFACE
#define without_subnodes false
#define with_subnodes true
#endif

//FIXME: no need for init_subnodes, we can decide based on node type
EXPORT struct node_s *sealark_new_node(int type, bool init_subnodes)
{
#if defined(DEBUG_MEM)
    log_debug("sealark_new_node");
#endif

    struct node_s *nd = (struct node_s *)calloc(1, sizeof(struct node_s));
    nd->line = nd->col = -1;
    nd->tid = type;
    /* if tid is printable, no subnodes, else init subnodes */
    if (init_subnodes)
        utarray_new(nd->subnodes, &node_icd);
    return nd;
}

EXPORT struct node_s *sealark_new_node_s(int type, const char *str)
{
#if defined(DEBUG_MEM)
    log_debug("sealark_new_node");
#endif
    struct node_s *nd = (struct node_s *)calloc(1, sizeof(struct node_s));
    nd->line = nd->col = -1;
    nd->tid = type;
    int len = strlen(str);
    nd->s = calloc(len, sizeof(char));
    strncpy(nd->s, str, len+1);
    return nd;
}

EXPORT struct node_s *sealark_new_int_node(int i)
{
#if defined(DEBUG_MEM)
    log_debug("sealark_new_int_node: %d", i);
#endif
    struct node_s *nd = (struct node_s *)calloc(1, sizeof(struct node_s));
    nd->line = nd->col = -1;
    nd->tid = TK_INT;
    char buf[64]; //FIXME how much is enough in practice?
    snprintf(buf, 64, "%d", i);
    int len = strlen(buf);
    nd->s = calloc(len, sizeof(char));
    strncpy(nd->s, buf, len+1);
    return nd;
}

/* **** */
EXPORT struct node_s *sealark_new_list_expr(void)
{
#if defined(DEBUG_MEM)
    log_debug("sealark_new_list_expr");
#endif
    struct node_s *newlist
        = (struct node_s *)calloc(1, sizeof(struct node_s));
    newlist->tid = TK_List_Expr;
    utarray_new(newlist->subnodes, &node_icd);

    struct node_s *brack = sealark_new_node(TK_LBRACK, without_subnodes);
    utarray_push_back(newlist->subnodes, brack);

    struct node_s *list = sealark_new_node(TK_Expr_List, with_subnodes);
    utarray_push_back(newlist->subnodes, list);

    brack = sealark_new_node(TK_RBRACK, without_subnodes);
    utarray_push_back(newlist->subnodes, brack);
    return newlist;
}

/* **************** */
EXPORT void sealark_node_free(void *_elt) {
#if defined(DEBUG_MEM)
    log_debug("sealark_node_free: %s (%d)",
              token_name[((struct node_s*)_elt)->tid][0],
              ((struct node_s*)_elt)->tid);
#endif
    struct node_s *elt = (struct node_s*)_elt;
    if (elt->s) {
#if defined(DEBUG_MEM)
        log_debug("\tfreeing string %s", elt->s);
#endif
        free(elt->s);
    }
#if defined(DEBUG_MEM)
    log_debug("freeing comments");
#endif
    if (elt->comments) utarray_free(elt->comments);
#if defined(DEBUG_MEM)
    log_debug("freeing subnodes");
#endif
    if (elt->subnodes) utarray_free(elt->subnodes);
    elt->subnodes = NULL;
#if defined(DEBUG_MEM)
    log_debug("done freeing subnodes");
#endif

    /* DO NOT FREE: utarray copies elements into a buffer, so its
       pointers are not malloced */
    /* free(_elt); */
}

/* count subnodes. a little overloaded */
EXPORT int sealark_subnode_count(struct node_s *node,
                                 bool exclude_meta,
                                 bool all_printables,
                                 bool recursive)
{
    int ct = 0;

#if defined(DEBUG_TRACE)
    /* log_debug("sealark_subnode_count"); */
#endif

    struct node_s *count_node = NULL;  /* we'll iterate over this */

    switch(node->tid) {
    case TK_Arg_List:
        if (all_printables)
            ct--; // self is non-printable
        count_node = node;
        break;
    case TK_Binding:
        if (all_printables)
            ct--; // self is non-printable
        count_node = node;
        break;
    case TK_Call_Expr:
        if (all_printables) ct--; // self is non-printable
        count_node = node;
        break;
    case TK_Call_Sfx:
        if (all_printables) ct--; // self is non-printable
        count_node = node;
        break;
    case TK_Expr_List:
        if (all_printables)
            ct--; // self is non-printable
        count_node = node;
        break;
    case TK_List_Expr:
        if (node->subnodes)
            if ( !all_printables ) {
                if (exclude_meta) {
                    count_node = utarray_eltptr(node->subnodes, 1);
                } else
                    count_node = node;
            } else {
                ct--; // self is non-printable
                count_node = node;
            }
        else
            return 1;
        break;
    case TK_Load_Stmt:
        if (all_printables)
            ct--; // self is non-printable
        count_node = node;
        break;
    default:
        if (node->subnodes) {
            log_error("length not yet implemented for %d %s",
                      node->tid, TIDNAME(node));
            exit(EXIT_FAILURE);
        }
        count_node=node;
    }

    if (count_node->subnodes) {
        ct++; // count count_node?
        struct node_s *subnode = NULL;
        while( (subnode=(struct node_s*)utarray_next(count_node->subnodes, subnode)) ) {
            if ( !all_printables ) {
                if (exclude_meta) {
                    if (subnode->tid == TK_COMMA) continue;
                    if (subnode->tid == TK_COLON) continue;
                    if (subnode->tid == TK_LBRACK) continue;
                    if (subnode->tid == TK_RBRACK) continue;
                    if (subnode->tid == TK_LBRACE) continue;
                    if (subnode->tid == TK_RBRACE) continue;
                    if (subnode->tid == TK_LPAREN) continue;
                    if (subnode->tid == TK_RPAREN) continue;
                    if (subnode->tid == TK_LOAD) continue;
                /* } else { */
                }
            }
            if (recursive)
                ct=ct+sealark_subnode_count(subnode, exclude_meta, all_printables, recursive);
            else
                if (all_printables) {
                    if (sealark_is_printable(subnode)) {
                        log_debug("is printable: %d, %s",
                                  subnode->tid, TIDNAME(subnode));
                        ct++;
                    }
                } else {
                    ct++;
                }
        }
        return ct;
    } else
        return 1;
}

/* FIXME: rename? to_string? */
/**
   assumption: already verified node is printable
*/
char *_print_string_node(struct node_s *node)
{
    static UT_string *workbuf;
    utstring_renew(workbuf);

    char * br =
        ((node->qtype & BINARY_STR) &&
         (node->qtype & RAW_STR))? "br"
        : (node->qtype & BINARY_STR)? "b"
        : (node->qtype & RAW_STR)? "r"
        : "";
    char *q;
    if (node->qtype & SQUOTE) {
        if (node->qtype & TRIPLE) {
            q = "'''";
        } else {
            q = "'";
        }
    } else {
        if ( !(node->qtype & SQUOTE) ) {
            if (node->qtype & TRIPLE) {
                q = "\"\"\"";
            } else {
                q = "\"";
            }
        } else {
            q = "";
        }
    }
    utstring_printf(workbuf,
                    "%s%s%s%s",
                    br,
                    q,
                    node->s,
                    q);

    return utstring_body(workbuf);
}

EXPORT char  *sealark_node_printable_string(struct node_s *node)
{
    if ( !sealark_is_printable(node) ) return NULL;

    switch(node->tid) {
    case TK_STRING:
        return _print_string_node(node);
        break;
    case TK_ID:
        return node->s;
        break;
    case TK_INT:
        return node->s;
        break;
    default:
        return (char*)token_name[node->tid][1];
    }
}

/**
   if current node n is call_expr, then path to attr is:

       :call_expr
         :call_sfx  (second child)
           :arg_list (second child)
             list of alternating :arg_named and :comma
               :id (first child of :arg_named)
 */
EXPORT struct node_s *sealark_get_binding_node(struct node_s *node, char *kw)
{
#ifdef DEBUG_TRACE
    log_debug("sealark_node_rule_attrib: %s", kw);
#endif

    if ( node->tid != TK_Call_Expr ) return NULL;
    UT_array *call_expr_subnodes = node->subnodes;
    /* :call-sfx is second child of :call-expr */
    struct node_s *call_sfx = utarray_eltptr(call_expr_subnodes, 1);
    /* log_debug("call_expr[1].tid %d", call_sfx->tid); // 95 = Call_Sfx */

    UT_array *call_sfx_subnodes = call_sfx->subnodes;
    struct node_s *arg_list = utarray_eltptr(call_sfx_subnodes, 1);
    /* log_debug("call_sfx[1].tid %d", arg_list->tid); // 88 TK_Arg_List */

    UT_array *arg_list_subnodes = arg_list->subnodes;
    /* log_debug("arg_list subnode ct: %d", utarray_len(arg_list_subnodes)); */

    /* now search for kw */
    int kwlen = strlen(kw);
    struct node_s *attr = NULL;
    while( (attr=(struct node_s*)utarray_next(arg_list_subnodes, attr)) ) {
        if (attr->tid == TK_Binding) {
            struct node_s *id = utarray_eltptr(attr->subnodes, 0);
            if (strncmp(id->s, kw, kwlen) == 0) {
                return attr;
            }
        }
    }
    return NULL;
}

EXPORT void sealark_alias_node(void *_dst, const void *_src)
{
    /* log_debug("node_copy"); // : %p <- %p", _dst, _src); */
    struct node_s *dst = (struct node_s*)_dst;
    struct node_s *src = (struct node_s*)_src;
    dst->tid = src->tid;

    /* log_debug("node posn: %d:%d", src->line, src->col); */
    /* log_debug("\tnode type: %d", src->tid); */
    /* log_debug("\t%s[%d] %c (%d:%d) %s", */
    /*           token_name[src->tid][0], */
    /*           src->tid, */
    /*           (src->qtype == SQUOTE)? '\'' */
    /*           : (src->qtype == DQUOTE)? '"' */
    /*           : ' ', */
    /*           src->line, src->col, */
    /*           (src->s == NULL? "" : src->s)); */
    /* if (src->subnodes) */
    /*     log_trace("  src subnode ct: %d", */
    /*               utarray_len(src->subnodes)); */
    *dst = *src;
    /* if (dst->subnodes) */
    /*     log_trace("  dest subnode ct: %d", */
    /*               utarray_len(dst->subnodes)); */
}

/* nodelist: UT_array of node_s */
EXPORT UT_icd node_icd = {sizeof(struct node_s), NULL, sealark_alias_node, sealark_node_free};

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
    /* log_debug("comment_dtor"); */
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
    /* log_debug("sym_dtor"); */
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
