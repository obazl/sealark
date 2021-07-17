%include {
#include <setjmp.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "utarray.h"
#include "log.h"

/* static int indent = 2; */
/* static int delta = 2; */
/* static char *sp = " "; */

#if INTERFACE
#ifndef YYMALLOCARGTYPE
#define YYMALLOCARGTYPE size_t
#endif
#define YYCOVERAGE
#endif

}

/* %extra_argument { struct node_s **root} */
%extra_argument { struct parse_state_s *parse_state}

/* WARNING WARNING: if you change these tokens, you must update the
#defines in const.c! */
%token_prefix TK_ /* TK_ prefix will be added by lemon */
%token AMP .
%token AMP_EQ .
%token AND .
%token ARROW .
%token AS .
%token ASSERT .
%token BANG .
%token BANG_EQ .
%token BLANK .                  /* blank line */
%token BREAK .
%token CARET .
%token CARET_EQ .
%token CLASS .
%token COLON .
/* %token ICOLON . */
%token COMMA .
%token COMMENT .
%token CONTINUE .
%token DEF .
%token DEL .
%token SLASH2 .                 /* floored division binop */
%token SLASH2_EQ .
%token SLASH_EQ .
%token DOT .
/* %token DQ . */
%token ELIF .
%token ELSE .
%token EQ .
%token EQ2 .
%token BACKSLASH2 .
%token EXCEPT .
%token FINALLY .
%token FLOAT_LIT .
%token FLOAT .
%token FOR .
%token FROM .
%token GE .
%token GLOBAL .
%token ID .
%token IF .
%token IMPORT .
%token INT .
%token INT_DEC .
%token INT_HEX .
%token INT_OCT .
%token IN .
%token IS .
%token LAMBDA .
%token LANGLE .
%token LBRACE .
%token LBRACK .
%token LE .
%token LLANGLE .
%token LLANGLE_EQ .
%token LOAD .
%token LPAREN .
%token MINUS .
%token MINUS_EQ .
%token NEWLINE .
%token NONLOCAL .
%token NOT .
%token OR .
%token PASS .
%token PCT .                    /* remainder binop */
%token PCT_EQ .
%token PLUS .
%token PLUS_EQ .
%token RAISE .
%token RANGLE .
%token RBRACE .
%token RBRACK .
%token RETURN .
%token RPAREN .
%token RRANGLE .
%token RRANGLE_EQ .
%token SEMI .
%token SLASH .
/* %token SQ . */
%token STAR .
%token STAR_EQ .
%token STAR2 .
%token STRING .
/* %token BSTRING . */
/* %token BRSTRING . */
/* %token RSTRING . */
/* %token RBSTRING . */
/* %token MLSTRING . */
/* %token MLBSTRING . */
/* %token MLBRSTRING . */
/* %token MLRSTRING . */
/* %token MLRBSTRING . */
%token TILDE .
%token TRY .
%token VBAR .
%token VBAR_EQ .
%token WHILE .
%token WITH .
%token YIELD .

/* token constants used to tag non-terminals */
%token ALIAS .
%token Arg_List .
%token Arg_Named .
%token Arg_Star .
%token Arg_Star2 .
%token Assign_Stmt .
%token Bin_Expr .
%token Call_Expr .
%token Call_Sfx .
%token Comp_Clause .
%token Def_Compound .
%token Def_Stmt .
%token Dict_Comp .
%token Dict_Entry .
%token Dict_Entry_List .
%token Dict_Expr .
%token Dot_Expr .
%token Dot_Sfx .
%token Expr .
%token Expr_List .
%token For_Stmt .
%token If_Expr .
%token If_Stmt .
%token Indent_Block .
%token Lambda_Expr .
%token List_Comp .
%token List_Expr .
%token Load_Stmt .
%token Loop_Vars .
%token Param_List .
%token Param_Named .
%token Param_Star .
%token Param_Star2 .
%token Primary_Expr .
%token Paren_Expr .
%token Return_Expr .
%token Slice_Expr .
%token Slice_Sfx .
%token SmallStmt_List .
%token Stmt .
%token Stmt_List .
%token Unary_Expr .
/* %token SYM . */

/* Python op precedence: */
/* https://docs.python.org/3/reference/expressions.html#operator-precedence */
 /* https://www.mathcs.emory.edu/~valerie/courses/fall10/155/resources/op_precedence.html */
%right LAMBDA .
%left OR .
%left AND .
%right NOT .
/* membership: in, not in */
/* identity: is, is not */
/* comparison: <, <=,  >,  >=, !=, == ('<>' not supported)*/
/* NB: 'not in', 'is not' are non-terminals */
%left EQ .
%nonassoc COMMENT .             /* make statement ::= COMMENT work */
%nonassoc PLUS_EQ MINUS_EQ .
%nonassoc IN IS LANGLE LE RANGLE GE BANG_EQ EQ2 .
%nonassoc VBAR .                /* bitwise OR binop */
%nonassoc CARET .               /* bitwise XOR binop */
%nonassoc AMP .                 /* bitwise AND binop */
%nonassoc LLANGLE RRANGLE .     /* bitwise shift binops */
%right LBRACK LBRACE LPAREN .   /* prevent ambiguities */
%left RBRACK RBRACE RPAREN .
%right DEF RETURN BREAK CONTINUE PASS LOAD.
%right PLUS MINUS .
%left STAR SLASH SLASH2 PCT .
/* %right PLUS .                   /\* positive, e.g. +3 *\/ */
/* %right MINUS .                  /\* negative, e.g. -3 *\/ */
%right TILDE .                  /* bitwise NOT (unary) */
%right STAR2 .                  /* exponentiation */
%nonassoc IF ELSE . /* may go both ways: foo IF bar ELSE baz v. IF foo ELSE bar */
/* the following prevents conflicts like:
 FLOAT shift-reduce 49     primx ::= FLOAT
 FLOAT reduce       32      ** Parsing conflict **
 {default} reduce       32     return_stmt ::= RETURN
*/
%nonassoc FLOAT ID INT STRING . // BSTRING .

%left COMMA .
%right FOR .
/* %right ELSE . */
/* binops: all nonassoc, mult ops higher precedence than add ops */
/* unary ops: +, -, not, ~ */

/* **************** */
%token_type { struct node_s* }  /* terminals */
%default_type { struct node_s* } /* non-terminals */
%default_destructor {
    log_trace("freeing non-terminal, type %d", $$->tid);
    free($$);
}

%type load_list { UT_array* }    /* list of struct node_s */
%destructor load_list {
    log_trace("freeing load_list");
    /* utarray_free($$->list); */
}

%syntax_error {
    log_trace("**************** Syntax error! ****************");
    exit(EXIT_FAILURE);
}

%parse_failure {
    log_trace("\n\n\t\t%%%%%%%%%%%%%%%% PARSE: FAIL %%%%%%%%%%%%%%%%\n");
}

%parse_accept {
    /* log_trace("\n\n\t\t%%%%%%%%%%%%%%%% PARSE: SUCCESS %%%%%%%%%%%%%%%%\n"); */
}

%start_symbol buildfile
/* File = {Statement | newline} eof . */
buildfile ::= stmt_list(SS) .
{
    /* log_trace(">>buildfile ::= stmt_list ."); */
    parse_state->root = SS;
}

%ifdef TEST
/* buildfile(File) ::= small_stmt_list(S) . { */
/*     log_trace(">>buildfile(File) ::= small_stmt ."); */
/*     parse_state->root = S; */
/* } */

/* buildfile(File) ::= expr(X) . { */
/*     log_trace(">>buildfile(File) ::= expr(X) ."); */
/*     parse_state->root = X; */
/* } */

/* buildfile(File) ::= primary_expr(X) . { */
/*     log_trace(">>buildfile(File) ::= primary_expr(X) ."); */
/*     *root = X; */
/* } */

/* buildfile(File) ::= list_expr(X) . [NOT] { */
/*     log_trace(">>buildfile(File) ::= list_expr(X) ."); */
/*     log_debug("START dump"); */
/*     dump_node(X); */
/*     log_debug("/START dump"); */
    /* parse_state->root = X; */
/* } */

/* buildfile(File) ::= unary_expr(X) . { */
/*     log_trace(">>buildfile(File) ::= unary_expr(X) ."); */
/*     log_debug("START dump"); */
/*     dump_node(X); */
/*     log_debug("/START dump"); */
    /* parse_state->root = X; */
/* } */

/* buildfile(File) ::= binary_expr(X) . { */
/*     log_trace(">>buildfile(File) ::= binary_expr(X) ."); */
/*     /\* log_debug("START dump"); *\/ */
/*     /\* dump_node(X); *\/ */
/*     /\* log_debug("/START dump"); *\/ */
    /* parse_state->root = X; */
/* } */
%endif

/* %ifdef STRINGS || ALL */
/* %type string_list { UT_array* } */
/* %destructor string_list { */
/*     log_trace("freeing string_list"); */
/*     /\* utarray_free($$->list); *\/ */
/* } */

/* %endif */

/* %if LOAD_STMT */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* str_assign is for load stmts */
str_assign(ALIAS) ::= ID(Id) EQ(Eq) STRING(S). {
    /* log_trace(">>str_assign ::= ID EQ STRING"); */
    /* log_trace("  rhs ID(Id)"); */
    /* log_trace("  rhs EQ(Eq)"); */
    /* log_trace("  rhs STRING(S)"); */
    ALIAS = calloc(sizeof(struct node_s), 1);
    ALIAS->tid = TK_ALIAS;
    ALIAS->line  = Id->line;
    ALIAS->col   = Id->col;
    utarray_new(ALIAS->subnodes, &node_icd);
    utarray_push_back(ALIAS->subnodes, Id);
    utarray_push_back(ALIAS->subnodes, Eq);
    utarray_push_back(ALIAS->subnodes, S);
}

/* param_opt is only for optional def params, i.e. name=expr */
/* param_opt(PARAM) ::= xID(IDENT) xEQ(XEQ) expr(S). { */
/*     log_trace(">>str_assign ::= xID xEQ xSTRING"); */
/*     log_trace("  rhs xID(IDENT)"); */
/*     log_trace("  rhs xEQ(XEQ)"); */
/*     log_trace("  rhs xSTRING(S)"); */
/*     ALIAS = calloc(sizeof(struct node_s), 1); */
/*     ALIAS->tid = TK_ALIAS; */
/*     ALIAS->line  = IDENT->line; */
/*     ALIAS->col   = IDENT->col; */
/*     utarray_new(ALIAS->subnodes, &node_icd); */
/*     utarray_push_back(ALIAS->subnodes, IDENT); */
/*     utarray_push_back(ALIAS->subnodes, XEQ); */
/*     utarray_push_back(ALIAS->subnodes, S); */
/* } */

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
load_list(LOAD_LIST) ::= str_assign(A) . {
    /* log_trace(">>load_list ::= load_list str_assign"); */
    utarray_new(LOAD_LIST, &node_icd);
    utarray_push_back(LOAD_LIST, A);
}

load_list(LOAD_LIST) ::= STRING(S) . {
    /* log_trace(">>load_list ::= STRING"); */
    utarray_new(LOAD_LIST, &node_icd);
    utarray_push_back(LOAD_LIST, S);
}

load_list(LOAD_LIST) ::= load_list(LL) COMMA(Comma) STRING(S) . {
    /* log_trace(">>load_list ::= load_list COMMA STRING"); */
    utarray_push_back(LL, Comma);
    utarray_push_back(LL, S);
    LOAD_LIST = LL;
}

load_list(LOAD_LIST) ::= load_list(LL) COMMA(Comma) str_assign(A) . {
    // log_trace(">>load_list ::= load_list COMMA str_assign");
    utarray_push_back(LL, Comma);
    utarray_push_back(LL, A);
    LOAD_LIST = LL;
}

/* optional trailing comma */
load_list(LOAD_LIST) ::= load_list(LL) COMMA(Comma) . {
    // log_trace(">>load_list ::= load_list COMMA");
    utarray_push_back(LL, Comma);
    LOAD_LIST = LL;
}

/* cmt_list(COMMENTS) ::= cmt_list(CL) COMMENT(C) . { */
/*     log_trace(">>cmt_list ::= cmt_list COMMENT"); */
/*     utarray_push_back(CL, C); */
/*     COMMENTS = CL; */
/* } */

/* %endif */

/* %if PARAMS || ALL */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* PARAMETERS */

/* Parameters = Parameter {',' Parameter}. */
/* NB: trailing comma allowed in 'def' params but not in lambda */
param_list(Params) ::= param(Param) . {
    // log_trace(">>param_list(Params) ::= param(Param)");
    Params = calloc(sizeof(struct node_s), 1);
    Params->tid = TK_Param_List;
    Params->line  = Param->line;
    Params->col   = Param->col;
    utarray_new(Params->subnodes, &node_icd);
    utarray_push_back(Params->subnodes, Param);
}

param_list(Params) ::= param_list(Params_rhs) COMMA(Comma) param(Param) . {
    // log_trace(">>param_list(Params) ::= param_list COMMA param");
    utarray_push_back(Params_rhs->subnodes, Comma);
    utarray_push_back(Params_rhs->subnodes, Param);
    Params = Params_rhs;
}

/* Parameter = identifier | identifier '=' Test | '*' identifier | '**' identifier . */

param(Param) ::= ID(Id) . {
    // log_trace(">>param(PARAM) ::= ID(Id)");
    Param = Id;
}

param(Param) ::= param_named(P) . {
    // log_trace(">>param(Param) ::= param_named(P)");
    Param = P;
}
param_named(Param) ::= ID(Id) EQ(Eq) expr(X). {
    // log_trace(">>param(Param) ::= ID(Id) EQ(Eq) expr(X)");
    Param = calloc(sizeof(struct node_s), 1);
    Param->tid = TK_Param_Named;
    Param->line = Id->line;
    Param->col  = Id->col;
    utarray_new(Param->subnodes, &node_icd);
    utarray_push_back(Param->subnodes, Id);
    utarray_push_back(Param->subnodes, Eq);
    utarray_push_back(Param->subnodes, X);
}

param(Param) ::= param_star(P) . {
    // log_trace(">>param(Param) ::= param_star(P)");
    Param = P;
}
param_star(Param) ::= STAR(Star) ID(Id) . {
    // log_trace(">>param(Param) ::= STAR ID(Id)");
    Param = calloc(sizeof(struct node_s), 1);
    Param->tid = TK_Param_Star;
    Param->line = Star->line;
    Param->col  = Star->col;
    utarray_new(Param->subnodes, &node_icd);
    utarray_push_back(Param->subnodes, Star);
    utarray_push_back(Param->subnodes, Id);
}

param(Param) ::= param_star2(P) . {
    // log_trace(">>param(Param) ::= param_star2(P)");
    Param = P;
}
param_star2(Param) ::= STAR2(Star2) ID(Id) . {
    // log_trace(">>param(Param) ::= STAR2 ID(Id)");
    Param = calloc(sizeof(struct node_s), 1);
    Param->tid = TK_Param_Star2;
    Param->line = Star2->line;
    Param->col  = Star2->col;
    utarray_new(Param->subnodes, &node_icd);
    utarray_push_back(Param->subnodes, Star2);
    utarray_push_back(Param->subnodes, Id);
}

/* %endif */

/* %if STATEMENTS || ALL */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
// STATEMENTS
/* Statement = DefStmt | IfStmt | ForStmt | SimpleStmt . */
/* DefStmt = 'def' identifier '(' [Parameters [',']] ')' ':' Suite . */
/*IfStmt = 'if' Test ':' Suite {'elif' Test ':' Suite} ['else' ':' Suite].*/
/* ForStmt = 'for' LoopVariables 'in' Expression ':' Suite . */
/* SimpleStmt = SmallStmt {';' SmallStmt} [';'] '\n' . */
/* # NOTE: '\n' optional at EOF */
/* SmallStmt = ReturnStmt */
/*           | BreakStmt | ContinueStmt | PassStmt */
/*           | AssignStmt */
/*           | ExprStmt */
/*           | LoadStmt */
/*           . */

/* AssignStmt   = Expression ('=' | '+=' | '-=' | '*=' | '/=' | '//=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=') Expression . */

// MB: we use 'indent_block' instead of 'suite'
/* suite :: approximately 'body' (of a fn defn, for example) */
/* Suite = [newline indent {Statement} outdent] | SimpleStmt . */
/* %%%%%%%%%%%%%%%% */
/* precedence: must be lower than binary_expr */
expr_list(Xs) ::= expr(X) . [PLUS]
{
    // log_trace(">>expr_list(XList) ::= expr(X");
    Xs = calloc(sizeof(struct node_s), 1);
    Xs->tid  = TK_Expr_List;
    Xs->line  = X->line;
    Xs->col   = X->col;
    Xs->trailing_newline = X->trailing_newline;
    utarray_new(Xs->subnodes, &node_icd);
    utarray_push_back(Xs->subnodes, X);
    /* log_debug("expr_list type: %s[%d]", */
    /*           token_name[Xs->tid][0], Xs->tid); */
}

/* expr_list_comma(XList) ::= expr_list(XList_rhs) COMMA(Comma) . [FOR] */
/* { */
/*     log_trace(">>expr_list_comma(XList) ::= expr_list(XList_rhs) COMMA(Comma)"); */
/*     if (XList_rhs->tid != TK_List_Expr) { */
/*         struct node_s *Xs = calloc(sizeof(struct node_s), 1); */
/*         Xs->tid = TK_Expr_List; */
/*         Xs->line  = XList_rhs->line; */
/*         Xs->col   = XList_rhs->col; */
/*         utarray_new(Xs->subnodes, &node_icd); */
/*         utarray_push_back(Xs->subnodes, XList_rhs); */
/*         utarray_push_back(Xs->subnodes, Comma); */
/*         XList = Xs; */
/*     } else { */
/*         utarray_push_back(XList_rhs->subnodes, Comma); */
/*         XList = XList_rhs; */
/*     } */
/* } */

expr_list(Xs) ::= expr_list(Xs_rhs) COMMA(Comma) expr(X) . [PLUS]
{
    // log_trace(">>expr_list(Xs) ::= expr_list(Xs_rhs) COMMA(Comma) expr(X)");
    /* XList = calloc(sizeof(struct node_s), 1); */
    /* XList->tid = TK_Expr_List; */
    /* XList->line  = XList_rhs->line; */
    /* XList->col   = XList_rhs->col; */
    Xs_rhs->trailing_newline = X->trailing_newline;
    /* utarray_new(XList->subnodes, &node_icd); */
    utarray_push_back(Xs_rhs->subnodes, Comma);
    utarray_push_back(Xs_rhs->subnodes, X);
    Xs = Xs_rhs;
}

stmt_list(Stmts) ::= statement(Stmt) . [FOR]
{
    // log_trace(">>stmt_list ::= statement");
    Stmts = calloc(sizeof(struct node_s), 1);
    Stmts->tid = TK_Stmt_List;
    Stmts->line = Stmt->line;
    Stmts->col  = Stmt->col;
    utarray_new(Stmts->subnodes, &node_icd);
    if (Stmt->tid == TK_Def_Compound) {
        /* log_debug("HIT DEF COMPOUND"); */
        utarray_concat(Stmts->subnodes, Stmt->subnodes);
    } else {
        utarray_push_back(Stmts->subnodes, Stmt);
    }
}

stmt_list(Stmts) ::= stmt_list(Stmts_rhs) statement(Stmt) . [PLUS]
{
    // log_trace(">>stmt_list ::= stmt_list statement");
    if (Stmt->tid == TK_Def_Compound) {
        log_debug("HIT DEF COMPOUND");
        utarray_concat(Stmts_rhs->subnodes, Stmt->subnodes);
    } else {
        utarray_push_back(Stmts_rhs->subnodes, Stmt);
    }
    Stmts = Stmts_rhs;
}

/* stmt_list(Stmts) ::= stmt_list(Stmts_rhs) statement(Stmt) . */
/* { */
/*     log_trace(">>stmt_list ::= stmt_list statement"); */
/*     /\* Stmts = calloc(sizeof(struct node_s), 1); *\/ */
/*     /\* Stmts->tid = TK_Def_Stmt; *\/ */
/*     /\* Stmts->line  = LBrack->line; *\/ */
/*     /\* Stmts->col   = LBrack->col; *\/ */
/*     /\* utarray_new(Stmts->subnodes, &node_icd); *\/ */
/*     utarray_push_back(Stmts_rhs->subnodes, Stmt); */
/*     Stmts = Stmts_rhs; */
/* } */

/* Suite = [newline indent {Statement} outdent] | SimpleStmt . */
/* SimpleStmt = SmallStmt {';' SmallStmt} [';'] '\n' . */
/* # NOTE: '\n' optional at EOF */
/* SmallStmt = ReturnStmt */
/*           | BreakStmt | ContinueStmt | PassStmt */
/*           | AssignStmt */
/*           | ExprStmt */
/*           | LoadStmt */

/* here: Statement => stmtm_list, SimpleStmt => small_stmt_list */
statement(Stmt) ::= COMMENT(C) . [FOR]
{
    // log_trace(">>statement ::= COMMENT");
    Stmt = calloc(sizeof(struct node_s), 1);
    Stmt->tid = TK_Stmt;
    Stmt->line  = C->line;
    Stmt->col   = C->col;
    utarray_new(Stmt->subnodes, &node_icd);
    utarray_push_back(Stmt->subnodes, C);
}

/* comment(Comment) ::= COMMENT(C) . [TILDE] */
/* { */
/*     log_trace(">>statement ::= COMMENT"); */
/*     Comment = C; */
/* } */

/* 'def' defines a function */
statement(Stmt) ::= def_stmt(DefStmt) . [TILDE]
{
    // log_trace(">>statement ::= def_stmt");
    Stmt = DefStmt;
}

statement(Stmt) ::= if_stmt(IfStmt) . [TILDE]
{
    // log_trace(">>statement ::= if_stmt");
    Stmt = IfStmt;
}

statement(Stmt) ::= for_stmt(ForStmt) . [TILDE]
{
    // log_trace("\n");
    // log_trace(">>statement ::= for_stmt");
    Stmt = ForStmt;
}

statement(Stmt) ::= small_stmt_list(SmallStmts) . [AMP]
{
    /* FIXME: validate required trailing newline */
    /* if present it will be embedded in the final token */
    // log_trace(">>statement ::= small_stmt_list");
    struct node_s *n = utarray_back(SmallStmts->subnodes);
    /* log_debug("LAST SMALL STMT nl? %d: %d %s: %s", */
    /*           n->trailing_newline, */
    /*           n->tid, token_name[n->tid][0], n->s); */
    if ( ! n->trailing_newline ) {
        // Starlark grammar: "# NOTE: '\n' optional at EOF
        // FIXME: detect if we are at EOF
        log_error("Missing newline after SimpleStmt %s", n->s);
        // FIXME: print entire expr
        /* exit(EXIT_FAILURE); */
    }
    /* utarray_push_back(SmallStmts->subnodes, Newline); */
    Stmt = SmallStmts;
}

/* optional trailing semi */
statement(Stmt) ::= small_stmt_list(SmallStmts) SEMI(Semi) . [AMP]
{
    /* FIXME: validate required trailing newline */
    /* if present it will be embedded in the final token (SEMI in this
       case) */
    // log_trace(">>statement ::= small_stmt_list");
    // log_trace("SEMI trailing newline? %d", Semi->trailing_newline);
    if ( ! Semi->trailing_newline ) {
        // Starlark grammar: "# NOTE: '\n' optional at EOF
        // FIXME: detect if we are at EOF
        log_error("Missing newline after SimpleStmt");
        /* exit(EXIT_FAILURE); */
    }
    /* log_trace("DUMPING SEMI ================"); */
    /* dump_node(Semi); */
    utarray_push_back(SmallStmts->subnodes, Semi);
    /* utarray_push_back(SmallStmts->subnodes, Newline); */
    Stmt = SmallStmts;
}

/* must be left-associative */
indent_block(Block) ::= small_stmt_list(SmallStmts) . [RBRACK]
{
    // log_trace(">>indent_block(Block) ::= simple_stmt(SmallStmts)");
    Block = calloc(sizeof(struct node_s), 1);
    Block->tid = TK_Indent_Block;
    Block->line  = SmallStmts->line;
    Block->col   = SmallStmts->col;
    utarray_new(Block->subnodes, &node_icd);
    utarray_push_back(Block->subnodes, SmallStmts);
}

indent_block(Block) ::= stmt_list(Stmts) . [DEF]
{
    // log_trace(">>indent_block(Block) ::= stmt_list(Stmts)");
    Block = calloc(sizeof(struct node_s), 1);
    Block->tid = TK_Indent_Block;
    Block->line  = Stmts->line;
    Block->col   = Stmts->col;
    utarray_new(Block->subnodes, &node_icd);
    utarray_push_back(Block->subnodes, Stmts);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* IfStmt = 'if' Test ':' Suite {'elif' Test ':' Suite} ['else' ':' Suite] . */
if_stmt(IfStmt) ::= IF(If) expr(X) COLON(Colon) indent_block(IBlock) .
{
    // log_trace(">>if_stmt ::= IF expr COLON indent_block ");
    IfStmt = calloc(sizeof(struct node_s), 1);
    IfStmt->tid  = TK_If_Stmt;
    IfStmt->line  = If->line;
    IfStmt->col   = If->col;
    IfStmt->trailing_newline = IBlock->trailing_newline;
    utarray_new(IfStmt->subnodes, &node_icd);
    utarray_push_back(IfStmt->subnodes, If);
    utarray_push_back(IfStmt->subnodes, X);
    utarray_push_back(IfStmt->subnodes, Colon);
    utarray_push_back(IfStmt->subnodes, IBlock);
}

/* ForStmt = 'for' LoopVariables 'in' Expression ':' Suite . */
for_stmt(ForStmt) ::= FOR(For) loop_vars(LoopVars) IN(In) expr(X) COLON(Colon) indent_block(IBlock) . [FOR]
{
    // log_trace(">>for_stmt ::= FOR loop_vars IN expr COLON stmt_list");
    ForStmt = calloc(sizeof(struct node_s), 1);
    ForStmt->tid  = TK_For_Stmt;
    ForStmt->line  = For->line;
    ForStmt->col   = For->col;
    utarray_new(ForStmt->subnodes, &node_icd);
    utarray_push_back(ForStmt->subnodes, For);
    utarray_push_back(ForStmt->subnodes, LoopVars);
    utarray_push_back(ForStmt->subnodes, In);
    utarray_push_back(ForStmt->subnodes, X);
    utarray_push_back(ForStmt->subnodes, Colon);
    utarray_push_back(ForStmt->subnodes, IBlock);
}

/* LoopVariables = PrimaryExpr {',' PrimaryExpr} . */
loop_vars(LoopVars) ::= primary_expr(PrimX) . {
    // log_trace(">>loop_vars(LoopVars) ::= primary_expr(Primx)");
    LoopVars = calloc(sizeof(struct node_s), 1);
    LoopVars->tid = TK_Loop_Vars;
    LoopVars->line = PrimX->line;
    LoopVars->col  = PrimX->col;
    utarray_new(LoopVars->subnodes, &node_icd);
    utarray_push_back(LoopVars->subnodes, PrimX);
}

loop_vars(LoopVars) ::= loop_vars(LoopVars_rhs) COMMA(Comma) primary_expr(X) . {
    // log_trace(">>loop_vars ::= loop_vars COMMA primary_expr");
    LoopVars = calloc(sizeof(struct node_s), 1);
    LoopVars->tid = TK_Loop_Vars;
    LoopVars->line  = LoopVars_rhs->line;
    LoopVars->col   = LoopVars_rhs->col;
    utarray_new(LoopVars->subnodes, &node_icd);
    utarray_push_back(LoopVars->subnodes, LoopVars_rhs);
    utarray_push_back(LoopVars->subnodes, Comma);
    utarray_push_back(LoopVars->subnodes, X);
}

/* DefStmt = 'def' identifier '(' [Parameters [',']] ')' ':' Suite . */
def_stmt(Stmt) ::= DEF(Def) ID(Id) LPAREN(Lparen) param_list(Params) RPAREN(Rparen) COLON(Colon) indent_block(IBlock). [LAMBDA]
{
    // log_trace(">>def_stmt ::= DEF ID LPAREN params RPAREN COLON indent_block");
    Stmt = calloc(sizeof(struct node_s), 1);
    Stmt->tid = TK_Def_Compound;
    Stmt->line = Def->line;
    Stmt->col  = Def->col;
    utarray_new(Stmt->subnodes, &node_icd);

    struct node_s *DefStmt = calloc(sizeof(struct node_s), 1);
    DefStmt->tid = TK_Def_Stmt;
    DefStmt->line = Def->line;
    DefStmt->col  = Def->col;
    utarray_new(DefStmt->subnodes, &node_icd);

    /* blocks must not be freed until the entire tree is processed */
    UT_array *blocks = split_iblock(IBlock, Def->col);
    /* log_debug("split ct: %d", utarray_len(blocks)); */

    struct node_s *defblock = utarray_front(blocks);

    utarray_push_back(DefStmt->subnodes, Def);
    utarray_push_back(DefStmt->subnodes, Id);
    utarray_push_back(DefStmt->subnodes, Lparen);
    utarray_push_back(DefStmt->subnodes, Params);
    utarray_push_back(DefStmt->subnodes, Rparen);
    utarray_push_back(DefStmt->subnodes, Colon);
    utarray_push_back(DefStmt->subnodes, defblock);
    utarray_push_back(Stmt->subnodes, DefStmt);

    int len = utarray_len(blocks);
    struct node_s *node=NULL;
    for (int i = 1; i < len; i++) {
        /* log_debug("trailing %d", i); */
        node = utarray_eltptr(blocks, i);
        /* log_debug("block t: %s[%d] indent %d", */
        /*           token_name[node->tid][0], node->tid, node->col); */
        utarray_push_back(Stmt->subnodes, node);
    }

}

def_stmt(Stmt) ::= DEF(Def) ID(Id) LPAREN(Lparen) RPAREN(Rparen) COLON(Colon) indent_block(IBlock). [FOR]
{
    // log_trace(">>def_stmt ::= DEF ID LPAREN RPAREN COLON stmt_list");
    Stmt = calloc(sizeof(struct node_s), 1);
    Stmt->tid = TK_Def_Stmt;
    Stmt->line = Def->line;
    Stmt->col  = Def->col;
    utarray_new(Stmt->subnodes, &node_icd);

    utarray_push_back(Stmt->subnodes, Def);
    utarray_push_back(Stmt->subnodes, Id);
    utarray_push_back(Stmt->subnodes, Lparen);
    utarray_push_back(Stmt->subnodes, Rparen);
    utarray_push_back(Stmt->subnodes, Colon);
    utarray_push_back(Stmt->subnodes, IBlock);
}

/* optional trailing comma in parm list */
def_stmt(Stmt) ::= DEF(Def) ID(Id) LPAREN(Lparen) param_list(Params) COMMA(Comma) RPAREN(Rparen) COLON(Colon) indent_block(IBlock). [LAMBDA]
{
    // log_trace(">>def_stmt ::= DEF ID LPAREN params RPAREN COLON stmt_list");
    Stmt = calloc(sizeof(struct node_s), 1);
    Stmt->tid = TK_Def_Stmt;
    Stmt->line = Def->line;
    Stmt->col  = Def->col;
    utarray_new(Stmt->subnodes, &node_icd);

    utarray_push_back(Stmt->subnodes, Def);
    utarray_push_back(Stmt->subnodes, Id);
    utarray_push_back(Stmt->subnodes, Lparen);
    utarray_push_back(Stmt->subnodes, Params);
    utarray_push_back(Stmt->subnodes, Comma);
    utarray_push_back(Stmt->subnodes, Rparen);
    utarray_push_back(Stmt->subnodes, Colon);
    utarray_push_back(Stmt->subnodes, IBlock);
}

/* LoadStmt = 'load' '(' string {',' [identifier '='] string} [','] ')' . */
load_stmt(Stmt) ::= LOAD(Load) LPAREN(LParen) STRING(S) COMMA(Comma) load_list(LL) RPAREN(RParen) . {
    // log_trace(">>load_stmt ::= LOAD LPAREN STRING COMMA load_list RPAREN");
    Stmt = calloc(sizeof(struct node_s), 1);
    Stmt->tid = TK_Load_Stmt;
    Stmt->line  = Load->line;
    Stmt->col   = Load->col;
    Stmt->trailing_newline = RParen->trailing_newline;
    utarray_new(Stmt->subnodes, &node_icd);
    utarray_push_back(Stmt->subnodes, Load);
    utarray_push_back(Stmt->subnodes, LParen);
    utarray_push_back(Stmt->subnodes, S);
    utarray_push_back(Stmt->subnodes, Comma);
    utarray_concat(Stmt->subnodes, LL);
    utarray_push_back(Stmt->subnodes, RParen);
}

/* SimpleStmt = SmallStmt {';' SmallStmt} [';'] '\n' . */
/* SmallStmt = ReturnStmt */
/*           | BreakStmt | ContinueStmt | PassStmt */
/*           | AssignStmt */
/*           | ExprStmt */
/*           | LoadStmt */
/*           . */
/* # NOTE: '\n' optional at EOF */
/* FIXME: must end in newline except at EOF */
small_stmt_list(SmallList) ::= small_stmt(SmallStmt) . [FOR]
{
    // log_trace(">>small_stmt_list ::= small_stmt");
    SmallList = calloc(sizeof(struct node_s), 1);
    SmallList->tid = TK_SmallStmt_List;
    SmallList->line  = SmallStmt->line;
    SmallList->col   = SmallStmt->col;
    SmallList->trailing_newline = SmallStmt->trailing_newline;
    utarray_new(SmallList->subnodes, &node_icd);
    utarray_push_back(SmallList->subnodes, SmallStmt);
}

small_stmt_list(SmallStmts) ::= small_stmt_list(SmallStmts_rhs) small_stmt(SmallStmt) . [FOR]
{
    // log_trace(">>small_stmt_list ::= small_stmt_list small_stmt");
    utarray_push_back(SmallStmts_rhs->subnodes, SmallStmt);
    SmallStmts = SmallStmts_rhs;
}

small_stmt_list(SmallStmts) ::= small_stmt_list(SmallStmts_rhs) SEMI(Semi) small_stmt(SmallStmt) . [FOR]
{
    // log_trace(">>small_stmt_list ::= small_stmt_list small_stmt");
    utarray_push_back(SmallStmts_rhs->subnodes, Semi);
    utarray_push_back(SmallStmts_rhs->subnodes, SmallStmt);
    SmallStmts = SmallStmts_rhs;
}

small_stmt(Stmt) ::= return_stmt(Return) .
{
    // log_trace(">>small_stmt(Stmt) ::= return_stmt");
    Stmt = Return;
}

return_stmt(Stmt) ::= RETURN(Return) . [PLUS]
{
    // log_trace(">>return_stmt(Stmt) ::= RETURN");
    Stmt = Return;
}

return_stmt(Stmt) ::= RETURN(Return) expr_list(Xs) . [STAR]
{
    // log_trace(">>return_stmt(Stmt) ::= RETURN expr_list");
    Stmt = calloc(sizeof(struct node_s), 1);
    Stmt->tid = TK_Return_Expr;
    Stmt->line  = Return->line;
    Stmt->col   = Return->col;
    Stmt->trailing_newline = Xs->trailing_newline;
    utarray_new(Stmt->subnodes, &node_icd);
    utarray_push_back(Stmt->subnodes, Return);
    utarray_push_back(Stmt->subnodes, Xs);
}

small_stmt(Stmt) ::= BREAK(Break) . [PLUS]
{
    // log_trace(">>small_stmt(Stmt) ::= BREAK");
    Stmt = Break;
}

small_stmt(Stmt) ::= CONTINUE(Continue) . [FOR]
{
    // log_trace(">>small_stmt(Stmt) ::= CONTINUE");
    Stmt = Continue;
}

small_stmt(Stmt) ::= PASS(Pass) . [FOR]
{
    // log_trace(">>small_stmt(Stmt) ::= PASS");
    Stmt = Pass;
}

small_stmt(Stmt) ::= assign_stmt(AssignStmt) .
{
    // log_trace(">>small_stmt(Stmt) ::= assign_stmt(AssignStmt)");
    Stmt = AssignStmt;
}

/* ExprStmt     = Expression . */
/* here, Expression => expr_list */
small_stmt(Stmt) ::= expr_list(Xs) .
{
    // log_trace(">>small_stmt(Stmt) ::= expr_list");
    Stmt = Xs;
}

small_stmt(Stmt) ::= load_stmt(LoadStmt) .
{
    // log_trace(">>small_stmt(Stmt) ::= load_stmt(LoadStmt)");
    Stmt = LoadStmt;
}

/* AssignStmt   = Expression ('=' | '+=' | '-=' | '*=' | '/=' | '//=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=') Expression . */

assign_stmt(AssignStmt) ::= expr_list(Xs1) assign_op(AssignOp) expr_list(Xs2) . [LAMBDA]
{
    // log_trace(">>assign_stmt ::= expr_list assign_op expr_list");
    AssignStmt = calloc(sizeof(struct node_s), 1);
    AssignStmt->tid = TK_Assign_Stmt;
    AssignStmt->line  = Xs1->line;
    AssignStmt->col   = Xs1->col;
    AssignStmt->trailing_newline = Xs2->trailing_newline;
    utarray_new(AssignStmt->subnodes, &node_icd);
    utarray_push_back(AssignStmt->subnodes, Xs1);
    utarray_push_back(AssignStmt->subnodes, AssignOp);
    utarray_push_back(AssignStmt->subnodes, Xs2);
}

assign_op ::= EQ .
assign_op ::= PLUS_EQ .
assign_op ::= MINUS_EQ .
assign_op ::= STAR_EQ .
assign_op ::= SLASH_EQ .
assign_op ::= SLASH2_EQ .
assign_op ::= PCT_EQ .
assign_op ::= AMP_EQ .
assign_op ::= VBAR_EQ .
assign_op ::= CARET_EQ .
assign_op ::= LLANGLE_EQ .
assign_op ::= RRANGLE_EQ .

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* EXPRESSIONS */

/* WARNING: We rename these:  'Test' => expr, 'Expression' => expr_list */
/* Test = IfExpr | PrimaryExpr | UnaryExpr | BinaryExpr | LambdaExpr . */
/* Expression = Test {',' Test} . */
expr(X) ::= if_expr(X_rhs) . [LAMBDA]
{
    // log_trace(">>expr(Expr) ::= if_expr(IfExpr");
    X = calloc(sizeof(struct node_s), 1);
    X->tid  = TK_Expr;
    X->line  = X_rhs->line;
    X->col   = X_rhs->col;
    X->trailing_newline = X_rhs->trailing_newline;
    utarray_new(X->subnodes, &node_icd);
    utarray_push_back(X->subnodes, X_rhs);
}

expr(X) ::= primary_expr(X_rhs) . [LAMBDA]
{
    /* log_trace(">>expr(X) ::= primary_expr(X_rhs)"); */
    /* X = calloc(sizeof(struct node_s), 1); */
    /* X->tid  = TK_Expr; */
    /* X->line  = X_rhs->line; */
    /* X->col   = X_rhs->col; */
    /* X->trailing_newline = X_rhs->trailing_newline; */
    /* utarray_new(X->subnodes, &node_icd); */
    /* utarray_push_back(X->subnodes, X_rhs); */
    X = X_rhs;
}

expr(X) ::= binary_expr(X_rhs) . [FOR]
{
    // log_trace(">>expr(X) ::= binary_expr(X_rhs)");
    X = calloc(sizeof(struct node_s), 1);
    X->tid  = TK_Expr;
    X->line  = X_rhs->line;
    X->col   = X_rhs->col;
    X->trailing_newline = X_rhs->trailing_newline;
    utarray_new(X->subnodes, &node_icd);
    utarray_push_back(X->subnodes, X_rhs);
}

expr(X) ::= unary_expr(X_rhs) . [LAMBDA]
{
    // log_trace(">>expr(X) ::= unary_expr(X_rhs)");
    X = calloc(sizeof(struct node_s), 1);
    X->tid  = TK_Expr;
    X->line  = X_rhs->line;
    X->col   = X_rhs->col;
    utarray_new(X->subnodes, &node_icd);
    utarray_push_back(X->subnodes, X_rhs);
}

expr(X) ::= lambda_expr(X_rhs) .
{
    // log_trace(">>expr(X) ::= lambda_expr(X_rhs)");
    X = calloc(sizeof(struct node_s), 1);
    X->tid  = TK_Expr;
    X->line  = X_rhs->line;
    X->col   = X_rhs->col;
    X->trailing_newline = X_rhs->trailing_newline;
    utarray_new(X->subnodes, &node_icd);
    utarray_push_back(X->subnodes, X_rhs);
}

/* maybe_expr(Expr) ::= expr(Expr_rhs) . */
/* { log_trace(">>maybe_expr(Expr) ::= expr(Expr_rhs)"); } */
/* maybe_expr(Expr) ::= . */

/* { log_trace(">>maybe_expr(Expr) ::= . "); } */

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* IF EXPRESSIONS */
/* IfExpr = Test 'if' Test 'else' Test . */
/* not to be confused with if_stmt! */

if_expr(IfX) ::= expr(X1) IF(If) expr(X2) ELSE(Else) expr(X3) . [LAMBDA]
{
    // log_trace(">>if_expr(IfX) ::= expr IF expr ELSE expr");
    IfX = calloc(sizeof(struct node_s), 1);
    IfX->tid = TK_If_Expr;
    IfX->line  = X1->line;
    IfX->col   = X1->col;
    IfX->trailing_newline = X3->trailing_newline;
    utarray_new(IfX->subnodes, &node_icd);
    utarray_push_back(IfX->subnodes, X1);
    utarray_push_back(IfX->subnodes, If);
    utarray_push_back(IfX->subnodes, X2);
    utarray_push_back(IfX->subnodes, Else);
    utarray_push_back(IfX->subnodes, X3);
 }

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* PRIMARY EXPRESSIONS */
/* WARNING: according to this the following should be legal:
   1.a (or: 1 .a), 1(x), 1[:], etc.
   i.e. suffixes should combine with Operand (int, float, etc.)
 */
/* PrimaryExpr = Primx // => 'Operand' in the original */
/*             | PrimaryExpr DotSuffix */
/*             | PrimaryExpr CallSuffix */
/*             | PrimaryExpr SliceSuffix */

/* %%%% Primx (Operand) */
primary_expr(X) ::= primx(X_rhs) .
{
    // log_trace(">>primary_expr(X) ::= primx(X_rhs)");
    X = X_rhs;
}
/* Terminology: 'operand' => 'primx' (PRimaryEXPRession) */
/* Primx = identifier */
/*         | int | float | string | bytes */
/*         | ListExpr | ListComp */
/*         | DictExpr | DictComp */
/*         | '(' [Expression [',']] ')' */
/*         . */
primx(PrimX) ::= ID(Id)   . {
    // log_trace(">>primx ::= ID(Id) :]%s[:", Id->s);
    PrimX = Id;
}
primx(PrimX) ::= INT(Int) . {
    // log_trace(">>primx(X) ::= INT(Int) .");
    PrimX = Int;
}
primx(PrimX) ::= FLOAT(Float) . {
    // log_trace(">>primx ::= FLOAT(Float) .");
    PrimX = Float;
}
primx(PrimX) ::= STRING(S) . {       /* includes rawstrings */
    // log_trace(">>primx ::= STRING :]%s[:", S->s);
    PrimX = S;
}
/* primx(PrimX) ::= BSTRING(B) . [COMMA] {      /\* bytes *\/ */
/*     log_trace(">>primx ::= BSTRING(B) ."); */
/*     PrimX = B; */
/* } */
primx(PrimX) ::= list_expr(ListX) . [COMMA] {
    // log_trace(">>primx ::= list_expr(ListX) .");
    PrimX = ListX;
}
primx(PrimX) ::= list_comp(LComp) . [COMMA] {
    // log_trace(">>primx ::= list_comp(LComp) .");
    PrimX = LComp;
}
primx(PrimX) ::= dict_expr(DictX) . [COMMA] {
    // log_trace(">>primx ::= dict_expr(DictX) .");
    PrimX = DictX;
}
primx(PrimX) ::= dict_comp(DictX) . [COMMA] {
    // log_trace(">>primx ::= dict_comp(DictX) .");
    PrimX = DictX;
}
primx(PrimX) ::= paren_expr(ParenX) . [COMMA] {
    // log_trace(">>primx ::= paren_comp .");
    PrimX = ParenX;
}

/* %%%% DotSuffix primary expr - type TK_Dot_Expr */
primary_expr(PrimX) ::= primary_expr(PrimX_rhs) dot_suffix(DotSfx) .
{
    // log_trace(">>primary_expr(PrimX) ::= primary_expr(PrimX_rhx) dot_suffix(DotSfx");
    PrimX = calloc(sizeof(struct node_s), 1);
    PrimX->tid = TK_Dot_Expr; // TK_Primary_Expr;
    PrimX->line  = PrimX_rhs->line;
    PrimX->col   = PrimX_rhs->col;
    PrimX->trailing_newline = DotSfx->trailing_newline;
    utarray_new(PrimX->subnodes, &node_icd);
    utarray_push_back(PrimX->subnodes, PrimX_rhs);
    utarray_push_back(PrimX->subnodes, DotSfx);
}

dot_suffix(DotSfx) ::= DOT(Dot) ID(Id) .
{
    // log_trace(">>dot_suffix(DotSfx) ::= DOT ID(Id)");
    DotSfx = calloc(sizeof(struct node_s), 1);
    DotSfx->tid  = TK_Dot_Sfx;
    DotSfx->line  = Dot->line;
    DotSfx->col   = Dot->col;
    DotSfx->trailing_newline = Id->trailing_newline;
    utarray_new(DotSfx->subnodes, &node_icd);
    utarray_push_back(DotSfx->subnodes, Dot);
    utarray_push_back(DotSfx->subnodes, Id);
}

/* %%%% CallSuffix primary expr - type TK_Call_Expr */
primary_expr(PrimX) ::= primary_expr(PrimX_rhs) call_suffix(CallSfx) .
{
    // log_trace(">>primary_expr ::= primary_expr call_suffix");
    PrimX = calloc(sizeof(struct node_s), 1);
    PrimX->tid = TK_Call_Expr; // TK_Primary_Expr;
    PrimX->line  = PrimX_rhs->line;
    PrimX->col   = PrimX_rhs->col;
    PrimX->trailing_newline = CallSfx->trailing_newline;
    utarray_new(PrimX->subnodes, &node_icd);
    utarray_push_back(PrimX->subnodes, PrimX_rhs);
    utarray_push_back(PrimX->subnodes, CallSfx);
}

/* CallSuffix  = '(' [Arguments [',']] ')' . */
/* NB: supports optional trailing comma */
call_suffix(CallSfx) ::= LPAREN(LParen) RPAREN(RParen) . [FOR]
{
    // log_trace(">>call_suffix(CallSfx) ::= LPAREN RPAREN");
    CallSfx = calloc(sizeof(struct node_s), 1);
    CallSfx->tid  = TK_Call_Sfx;
    CallSfx->line  = LParen->line;
    CallSfx->col   = LParen->col;
    CallSfx->trailing_newline = RParen->trailing_newline;
    utarray_new(CallSfx->subnodes, &node_icd);
    utarray_push_back(CallSfx->subnodes, LParen);
    utarray_push_back(CallSfx->subnodes, RParen);
}

/* optional trailing comma */
call_suffix(CallSfx) ::= LPAREN(LParen) arg_list(Args) COMMA(Comma) RPAREN(RParen) . [FOR]
{
    // log_trace(">>call_suffix(CallSfx) ::= LPAREN arg_list(Args) RPAREN");
    CallSfx = calloc(sizeof(struct node_s), 1);
    CallSfx->tid = TK_Call_Sfx;
    CallSfx->line  = LParen->line;
    CallSfx->col   = LParen->col;
    CallSfx->trailing_newline = RParen->trailing_newline;
    utarray_new(CallSfx->subnodes, &node_icd);
    utarray_push_back(CallSfx->subnodes, LParen);
    utarray_push_back(CallSfx->subnodes, Args);
    utarray_push_back(CallSfx->subnodes, Comma);
    utarray_push_back(CallSfx->subnodes, RParen);
}

call_suffix(CallSfx) ::= LPAREN(LParen) arg_list(Args) RPAREN(RParen) . [FOR]
{
    // log_trace(">>call_suffix(CallSfx) ::= LPAREN arg_list(Args) RPAREN");
    CallSfx = calloc(sizeof(struct node_s), 1);
    CallSfx->tid = TK_Call_Sfx;
    CallSfx->line  = LParen->line;
    CallSfx->col   = LParen->col;
    CallSfx->trailing_newline = RParen->trailing_newline;
    utarray_new(CallSfx->subnodes, &node_icd);
    utarray_push_back(CallSfx->subnodes, LParen);
    utarray_push_back(CallSfx->subnodes, Args);
    utarray_push_back(CallSfx->subnodes, RParen);
}

/* Arguments = Argument {',' Argument} . (Optional) */
/* arg_list(Args) ::= . { */
/*     log_trace(">>arg_list(Args) ::= ."); */
/*     Args = calloc(sizeof(struct node_s), 1); */
/*     Args->tid = TK_Arg_List; */
/*     Args->subnodes = NULL;      /\* Or: empty list? *\/ */
/* } */

arg_list(Args) ::= arg(Arg) . {
    // log_trace(">>arg_list(Args) ::= arg(Arg)");
    Args = calloc(sizeof(struct node_s), 1);
    Args->tid = TK_Arg_List;
    Args->line  = Arg->line;
    Args->col   = Arg->col;
    utarray_new(Args->subnodes, &node_icd);
    utarray_push_back(Args->subnodes, Arg);
}

/* arg_list_comma(Args) ::= arg_list(Args_rhs) COMMA(Comma) . { */
/*     log_trace(">>arg_list(Args) ::= arg_list(Args_rhs COMMA arg(Arg)"); */
/*     /\* retain the comma, for comments and ws *\/ */
/*     utarray_push_back(Args_rhs->subnodes, Comma); */
/*     Args = Args_rhs; */
/* } */

arg_list(Args) ::= arg_list(Args_rhs) COMMA(Comma) arg(Arg) . {
    // log_trace(">>arg_list(Args) ::= arg_list(Args_rhs COMMA arg(Arg)");
    /* retain the comma, for comments and ws */
    utarray_push_back(Args_rhs->subnodes, Comma);
    utarray_push_back(Args_rhs->subnodes, Arg);
    Args = Args_rhs;
}

/* WARNING: we do not currently validate ordering of arg types */
/* Argument  = Test | identifier '=' Test | '*' Test | '**' Test . */
arg(Arg) ::= expr(X) . {
    // log_trace(">>arg(Arg) ::= expr(Expr)");
    /* log_trace("s: %s", X->s); */
    Arg = X;
}

/* arg(Arg) ::= arg_named(Arg_rhs) . { */
/*     log_trace(">>arg(Arg) ::= arg_named(Arg_rhs)"); */
/* } */
arg(Arg) ::= ID(Id) EQ(Eq) expr(X) . {
    // log_trace(">>arg ::= ID EQ expr");
    Arg = calloc(sizeof(struct node_s), 1);
    Arg->tid = TK_Arg_Named;
    Arg->line  = Id->line;
    Arg->col   = Id->col;
    utarray_new(Arg->subnodes, &node_icd);
    utarray_push_back(Arg->subnodes, Id);
    utarray_push_back(Arg->subnodes, Eq);
    utarray_push_back(Arg->subnodes, X);
}

/* Argument  = ... | '*' Test | ... */
arg(Arg) ::= STAR(Star) expr(X) . {
    // log_trace(">>arg(Arg) ::= STAR expr(X)");
    Arg = calloc(sizeof(struct node_s), 1);
    Arg->tid = TK_Arg_Star;
    Arg->line  = Star->line;
    Arg->col   = Star->col;
    utarray_new(Arg->subnodes, &node_icd);
    utarray_push_back(Arg->subnodes, Star);
    utarray_push_back(Arg->subnodes, X);
}

/* Argument  = ... | '**' Test . */
arg(Arg) ::= STAR2(Star2) expr(X) . {
    // log_trace(">>arg(Arg) ::= STAR expr(X)");
    Arg = calloc(sizeof(struct node_s), 1);
    Arg->tid = TK_Arg_Star2;
    Arg->line  = Star2->line;
    Arg->col   = Star2->col;
    utarray_new(Arg->subnodes, &node_icd);
    utarray_push_back(Arg->subnodes, Star2);
    utarray_push_back(Arg->subnodes, X);
}


/* %%%% SliceSuffix primary expr - type TK_Slice_Expr */
/* Expression = Test {',' Test} => expr {',' expr} */
/* i.e. Expression = expr_list */
/* foo[bar], foo[:], foo[a:], foo[a:b], foo[a:b:c] */
/* with Expression (=Test list):  foo[a,b,c:d] */
/* expr_list may be empty; ditto for expr? */

primary_expr(PrimX) ::= primary_expr(PrimX_rhs) slice_suffix(SliceSfx) .
{
    // log_trace(">>primary_expr(PrimX) ::= primary_expr slice_suffix");
    PrimX = calloc(sizeof(struct node_s), 1);
    PrimX->tid = TK_Slice_Expr;
    PrimX->line  = PrimX_rhs->line;
    PrimX->col   = PrimX_rhs->col;
    PrimX->trailing_newline = SliceSfx->trailing_newline;
    utarray_new(PrimX->subnodes, &node_icd);
    utarray_push_back(PrimX->subnodes, PrimX_rhs);
    utarray_push_back(PrimX->subnodes, SliceSfx);
}

/* SliceSuffix = '[' [Expression] ':' [Test] [':' [Test]] ']' */
/*             | '[' Expression ']' */
/* What does e.g. foo[a,b:] mean? */
/* two colons: extended slicing. see https://docs.python.org/release/2.3.5/whatsnew/section-slices.html */
/* last arg is stride/step, e.g. foo[start:end:stride] */
/* we'll do this the hard way, instead of trying to use empty exprs */
/* combinations:
 foo[], foo[:], foo[::],
 foo[a], foo[a:], foo[a::], foo[a:b], foo[a:b:], foo[a::c], foo[:a:b:c]
 foo[:b], foo[:b:], foo[:b:c]
 foo[::c]
 */

/* foo[] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBRACK RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[:] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[::] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon1) COLON(Colon2) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}

 /* foo[a], foo[a:], foo[a::], foo[a:b], foo[a:b:], foo[a::c], foo[a:b:c] */
/* foo[a] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBRACK expr_list RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a:] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a::] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon1) COLON(Colon2) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a:b] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon) expr(X) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a:b:] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon1) expr(X) COLON(Colon2) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON expr COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a::c] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon1) COLON(Colon2) expr(X) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a:b:c] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon1) expr(X1) COLON(Colon2) expr(X2) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON expr COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, X1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, X2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
 /* foo[:b], foo[:b:], foo[:b:c] */
 /* foo[::c] */

/* foo[:b] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon) expr(X) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[:b:] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon1) expr(X) COLON(Colon2) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON expr COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[:b:c] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon1) expr(X1) COLON(Colon2) expr(X2) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON expr COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, X1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, X2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[::c] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon1) COLON(Colon2) expr(X) RBRACK(RBrack) .
{
    // log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->tid = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    SliceSfx->trailing_newline = RBrack->trailing_newline;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* LIST EXPRESSIONS */
/* PrimX (Operand) = ... | ListExpr | ListComp | ... */
/* ListExpr = '[' [Expression [',']] ']' . */
/* ListComp = '[' Test {CompClause} ']'. */
list_expr(ListX) ::= LBRACK(LBrack) RBRACK(RBrack) .
{
    // log_trace(">>list_expr(List) ::= LBRACK RBRACK");
    ListX = calloc(sizeof(struct node_s), 1);
    ListX->tid = TK_List_Expr;
    ListX->line  = LBrack->line;
    ListX->col   = LBrack->col;
    ListX->trailing_newline = RBrack->trailing_newline;
    utarray_new(ListX->subnodes, &node_icd);
    utarray_push_back(ListX->subnodes, LBrack);
    utarray_push_back(ListX->subnodes, RBrack);
}

list_expr(ListX) ::= LBRACK(LBrack) expr_list(Xs) RBRACK(RBrack) .
{
    // log_trace(">>list_expr(List) ::= LBRACK expr_list RBRACK");
    /* if (Xs->tid != TK_List_Expr) { */
    /*     struct node_s *XList = calloc(sizeof(struct node_s), 1); */
    /*     XList->tid = TK_List_Expr; */
    /*     utarray_new(XList->subnodes, &node_icd); */
    /*     utarray_push_back(XList->subnodes, Xs); */
    /*     utarray_push_back(ListX->subnodes, XList); */
    /* } else { */
    /*     utarray_push_back(ListX->subnodes, Xs); */
    /* } */
    ListX = calloc(sizeof(struct node_s), 1);
    ListX->tid = TK_List_Expr;
    ListX->line  = LBrack->line;
    ListX->col   = LBrack->col;
    ListX->trailing_newline = RBrack->trailing_newline;
    utarray_new(ListX->subnodes, &node_icd);
    utarray_push_back(ListX->subnodes, LBrack);
    utarray_push_back(ListX->subnodes, Xs);
    utarray_push_back(ListX->subnodes, RBrack);
}

/* optional trailing comma */
list_expr(ListX) ::= LBRACK(LBrack) expr_list(Xs) COMMA(Comma) RBRACK(RBrack) .
{
    // log_trace(">>list_expr(List) ::= LBRACK expr_list COMMA RBRACK");
    ListX = calloc(sizeof(struct node_s), 1);
    ListX->tid = TK_List_Expr;
    ListX->line  = LBrack->line;
    ListX->col   = LBrack->col;
    ListX->trailing_newline = RBrack->trailing_newline;
    utarray_new(ListX->subnodes, &node_icd);
    utarray_push_back(ListX->subnodes, LBrack);
    /* if (Xs->tid != TK_List_Expr) { */
    /*     struct node_s *XList = calloc(sizeof(struct node_s), 1); */
    /*     XList->tid = TK_List_Expr; */
    /*     utarray_new(XList->subnodes, &node_icd); */
    /*     utarray_push_back(XList->subnodes, Xs); */
    /*     utarray_push_back(ListX->subnodes, XList); */
    /* } else { */
    /*     utarray_push_back(ListX->subnodes, Xs); */
    /* } */
    utarray_push_back(ListX->subnodes, Xs);
    utarray_push_back(ListX->subnodes, Comma);
    utarray_push_back(ListX->subnodes, RBrack);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* LIST COMPREHENSIONS */
/* PrimX (Operand) = ... | ListExpr | ListComp | ... */
/* ListComp = '[' Test {CompClause} ']'. */
/* CompClause = 'for' LoopVariables 'in' Test | 'if' Test . */
/* LoopVariables = PrimaryExpr {',' PrimaryExpr} . */

list_comp(LComp) ::= LBRACK(LBrack) expr(X) comp_clause(Comp) RBRACK(RBrack) .
{
    // log_trace(">>list_comp(LComp) ::= LBRACK expr_list comp_clause RBRACK");
    LComp = calloc(sizeof(struct node_s), 1);
    LComp->tid = TK_List_Comp;
    LComp->line  = LBrack->line;
    LComp->col   = LBrack->col;
    LComp->trailing_newline = RBrack->trailing_newline;
    utarray_new(LComp->subnodes, &node_icd);
    utarray_push_back(LComp->subnodes, LBrack);
    utarray_push_back(LComp->subnodes, X);
    utarray_push_back(LComp->subnodes, Comp);
    utarray_push_back(LComp->subnodes, RBrack);
}

comp_clause(Comp) ::= comp_for_clause(CompFor) .
{
    // log_trace(">>comp_clause(Comp) ::= comp_for_clause");
    Comp = CompFor;
}

comp_clause(Comp) ::= comp_clause(Comp_rhs) comp_if_clause(CompIf) .
{
    // log_trace(">>comp_clause(Comp) ::= comp_clause comp_if_clause");
    Comp = calloc(sizeof(struct node_s), 1);
    Comp->tid = TK_Comp_Clause;
    Comp->line  = Comp_rhs->line;
    Comp->col   = Comp_rhs->col;
    utarray_new(Comp->subnodes, &node_icd);
    utarray_push_back(Comp->subnodes, Comp_rhs);
    utarray_push_back(Comp->subnodes, CompIf);
}

comp_clause(Comp) ::= comp_clause(Comp_rhs) comp_for_clause(CompFor) . [IF]
{
    // log_trace(">>comp_clause(Comp) ::= comp_clause comp_for_clause");
    Comp = calloc(sizeof(struct node_s), 1);
    Comp->tid = TK_Comp_Clause;
    Comp->line  = Comp_rhs->line;
    Comp->col   = Comp_rhs->col;
    utarray_new(Comp->subnodes, &node_icd);
    utarray_push_back(Comp->subnodes, Comp_rhs);
    utarray_push_back(Comp->subnodes, CompFor);
}

/* high precedence to disambiguate e.g.
[a for b in c if x] since 'c if x' matches if_expr
 */
comp_for_clause(Comp) ::= FOR(For) loop_vars(LoopVars) IN(In) expr(X) .
{
    // log_trace(">>comp_clause(Comp) ::= For loop_vars IN expr");
    Comp = calloc(sizeof(struct node_s), 1);
    Comp->tid = TK_Comp_Clause;
    Comp->line  = For->line;
    Comp->col   = For->col;
    utarray_new(Comp->subnodes, &node_icd);
    utarray_push_back(Comp->subnodes, For);
    /* abstract singleton loop var */
    if (LoopVars->tid != TK_Loop_Vars) {
        struct node_s *LVs = calloc(sizeof(struct node_s), 1);
        LVs->tid = TK_Loop_Vars;
        utarray_new(LVs->subnodes, &node_icd);
        utarray_push_back(LVs->subnodes, LoopVars);
        utarray_push_back(Comp->subnodes, LVs);
    } else {
        utarray_push_back(Comp->subnodes, LoopVars);
    }
    utarray_push_back(Comp->subnodes, In);
    utarray_push_back(Comp->subnodes, X);
}

comp_if_clause(Comp) ::= IF(If) expr(X) .
{
    // log_trace(">>comp_if_clause(Comp) ::= IF expr");
    Comp = calloc(sizeof(struct node_s), 1);
    Comp->tid = TK_Comp_Clause;
    Comp->line  = If->line;
    Comp->col   = If->col;
    utarray_new(Comp->subnodes, &node_icd);
    utarray_push_back(Comp->subnodes, If);
    utarray_push_back(Comp->subnodes, X);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* DICTIONARIES */
/* Operand (PrimX) = ... | DictExpr | DictComp | ... */
/* DictExpr = '{' [Entries [',']] '}' . */
/* DictComp = '{' Entry {CompClause} '}' . */
/* Entries  = Entry {',' Entry} . */
/* Entry    = Test ':' Test . */

entry_list(EList) ::= entry(Entry) . [FOR]
{
    // log_trace(">>entry_list(EList) ::= entry(Entry)");
    EList = calloc(sizeof(struct node_s), 1);
    EList->tid = TK_Dict_Entry_List;
    EList->line  = Entry->line;
    EList->col   = Entry->col;
    utarray_new(EList->subnodes, &node_icd);
    utarray_push_back(EList->subnodes, Entry);
}

entry_list(EList) ::= entry_list(EList_rhs) COMMA(Comma) entry(Entry) .
{
    // log_trace(">>entry_list(EList) ::= entry_list(EList_rhs) entry(Entry)");
    utarray_push_back(EList_rhs->subnodes, Comma);
    utarray_push_back(EList_rhs->subnodes, Entry);
    EList = EList_rhs;
}

entry(Entry) ::= expr(X1) COLON(Colon) expr(X2) . [FOR] //highest prec
{
    // log_trace(">>entry(Entry) ::= expr(X1) COLON expr(X2)");
    Entry = calloc(sizeof(struct node_s), 1);
    Entry->tid = TK_Dict_Entry;
    Entry->line  = X1->line;
    Entry->col   = X1->col;
    utarray_new(Entry->subnodes, &node_icd);
    utarray_push_back(Entry->subnodes, X1);
    utarray_push_back(Entry->subnodes, Colon);
    utarray_push_back(Entry->subnodes, X2);
}

dict_expr(DictX) ::= LBRACE(LBrace) RBRACE(RBrace) .
{
    // log_trace(">>dict_expr(DictX) ::= LBRACE RBRACE");
    DictX = calloc(sizeof(struct node_s), 1);
    DictX->tid = TK_Dict_Expr;
    DictX->line  = LBrace->line;
    DictX->col   = LBrace->col;
    DictX->trailing_newline = RBrace->trailing_newline;
    utarray_new(DictX->subnodes, &node_icd);
    utarray_push_back(DictX->subnodes, LBrace);
    utarray_push_back(DictX->subnodes, RBrace);
}

dict_expr(DictX) ::= LBRACE(LBrace) entry_list(EList) RBRACE(RBrace) .
{
    // log_trace(">>dict_expr(DictX) ::= LBRACE entry_list RBRACE");
    DictX = calloc(sizeof(struct node_s), 1);
    DictX->tid = TK_Dict_Expr;
    DictX->line  = LBrace->line;
    DictX->col   = LBrace->col;
    DictX->trailing_newline = RBrace->trailing_newline;
    utarray_new(DictX->subnodes, &node_icd);
    utarray_push_back(DictX->subnodes, LBrace);
    /* abstract singleton entry */
    if (EList->tid != TK_Dict_Entry_List) {
        log_debug("2 xxxxxxxxxxxxxxxx");
        struct node_s *Es = calloc(sizeof(struct node_s), 1);
        Es->tid = TK_Dict_Entry_List;
        utarray_new(Es->subnodes, &node_icd);
        utarray_push_back(Es->subnodes, EList);
        utarray_push_back(DictX->subnodes, Es);
    } else {
        utarray_push_back(DictX->subnodes, EList);
    }
    utarray_push_back(DictX->subnodes, RBrace);
}

dict_comp(DictX) ::= LBRACE(LBrace) entry(Entry) comp_clause(DClause) RBRACE(RBrace) .
{
    // log_trace(">>dict_comp(DictX) ::= LBRACE entry comp_clause RBRACE");
    DictX = calloc(sizeof(struct node_s), 1);
    DictX->tid = TK_Dict_Comp;
    DictX->line  = LBrace->line;
    DictX->col   = LBrace->col;
    DictX->trailing_newline = RBrace->trailing_newline;
    utarray_new(DictX->subnodes, &node_icd);
    utarray_push_back(DictX->subnodes, LBrace);
    utarray_push_back(DictX->subnodes, Entry);
    utarray_push_back(DictX->subnodes, DClause);
    utarray_push_back(DictX->subnodes, RBrace);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* PAREN EXPRESSIONS */
/* Operand (PrimX) = ... | '(' [Expression [',']] ')' */

paren_expr(ParenX) ::= LPAREN(LParen) RPAREN(RParen) .
{
    // log_trace(">>paren_expr(Paren) ::= LPAREN RPAREN");
    ParenX = calloc(sizeof(struct node_s), 1);
    ParenX->tid  = TK_Paren_Expr;
    ParenX->line  = LParen->line;
    ParenX->col   = LParen->col;
    ParenX->trailing_newline = RParen->trailing_newline;
    utarray_new(ParenX->subnodes, &node_icd);
    utarray_push_back(ParenX->subnodes, LParen);
    utarray_push_back(ParenX->subnodes, RParen);
}

paren_expr(ParenX) ::= LPAREN(LParen) expr_list(Xs) RPAREN(RParen) .
{
    // log_trace(">>paren_expr(Paren) ::= LPAREN expr_list RPAREN");
    ParenX = calloc(sizeof(struct node_s), 1);
    ParenX->tid  = TK_Paren_Expr;
    ParenX->line  = LParen->line;
    ParenX->col   = LParen->col;
    ParenX->trailing_newline = RParen->trailing_newline;
    utarray_new(ParenX->subnodes, &node_icd);
    utarray_push_back(ParenX->subnodes, LParen);
    utarray_push_back(ParenX->subnodes, Xs);
    utarray_push_back(ParenX->subnodes, RParen);
}

/* optional trailing comma */
paren_expr(ParenX) ::= LPAREN(LParen) expr_list(Xs) COMMA(Comma) RPAREN(RParen) .
{
    // log_trace(">>paren_expr(Paren) ::= LPAREN expr_list COMMA RPAREN");
    ParenX = calloc(sizeof(struct node_s), 1);
    ParenX->tid = TK_Paren_Expr;
    ParenX->line  = LParen->line;
    ParenX->col   = LParen->col;
    ParenX->trailing_newline = RParen->trailing_newline;
    utarray_new(ParenX->subnodes, &node_icd);
    utarray_push_back(ParenX->subnodes, LParen);
    utarray_push_back(ParenX->subnodes, Xs);
    utarray_push_back(ParenX->subnodes, Comma);
    utarray_push_back(ParenX->subnodes, RParen);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* BINARY EXPRESSIONS */
/* BinaryExpr = Test {Binop Test} . */
/* WARNING: {} means zero or more, not possible here? */
/* precedence must be higher than expr_list */
/* (FOR is highest precedence) */
binary_expr(BinExpr) ::= expr(BinExpr_rhs) binop(BinOp) expr(X_rhs) . [FOR]
{
    // log_trace(">>binary_expr ::= binary_expr binop expr");
    BinExpr = calloc(sizeof(struct node_s), 1);
    BinExpr->tid = TK_Bin_Expr;
    BinExpr->s = NULL;
    BinExpr->line  = BinExpr_rhs->line;
    BinExpr->col   = BinExpr_rhs->col;
    BinExpr->trailing_newline = X_rhs->trailing_newline;
    utarray_new(BinExpr->subnodes, &node_icd);
    utarray_push_back(BinExpr->subnodes, BinExpr_rhs);
    utarray_push_back(BinExpr->subnodes, BinOp);
    utarray_push_back(BinExpr->subnodes, X_rhs);
    /* log_debug("DUMPING BINARY_EXPR"); */
    /* dump_node(BinExpr); */
    /* log_debug("/DUMPING BINARY_EXPR"); */
}

/* Binop = 'or' */
/*       | 'and' */
/*       | '==' | '!=' | '<' | '>' | '<=' | '>=' | 'in' | 'not' 'in' */
/*       | '|' */
/*       | '^' */
/*       | '&' */
/*       | '<<' | '>>' */
/*       | '-' | '+' */
/*       | '*' | '%' | '/' | '//' */

binop ::= OR .
binop ::= AND .
binop ::= EQ2 .
binop ::= BANG_EQ .
binop ::= LANGLE .
binop ::= LE .
binop ::= RANGLE .
binop ::= GE .
binop ::= IN .
binop ::= NOT IN . [IN]
binop ::= VBAR .
binop ::= CARET .
binop ::= AMP .
binop ::= LLANGLE .
binop ::= RRANGLE .
binop ::= MINUS .
binop ::= PLUS .
binop ::= STAR .
binop ::= PCT .
binop ::= SLASH .
binop ::= SLASH2 .

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* UNARY EXPRESSIONS */
/* UnaryExpr = '+' Test */
/*           | '-' Test */
/*           | '~' Test */
/*           | 'not' Test */
/*           . */
unary_expr(X_lhs) ::= PLUS(Op) expr(X_rhs) . [LAMBDA]
{
    // log_trace(">>unary_expr(X_lhs) ::= PLUS expr(X_rhs)");
    X_lhs = calloc(sizeof(struct node_s), 1);
    X_lhs->tid = TK_Unary_Expr;
    X_lhs->line  = X_rhs->line;
    X_lhs->col   = X_rhs->col;
    utarray_new(X_lhs->subnodes, &node_icd);
    utarray_push_back(X_lhs->subnodes, Op);
    utarray_push_back(X_lhs->subnodes, X_rhs);
}
unary_expr(X_lhs) ::= MINUS(Op) expr(X_rhs) . [LAMBDA]
{
    // log_trace(">>unary_expr(X_lhs) ::= MINUS expr(X_rhs)");
    X_lhs = calloc(sizeof(struct node_s), 1);
    X_lhs->tid = TK_Unary_Expr;
    X_lhs->line  = X_rhs->line;
    X_lhs->col   = X_rhs->col;
    utarray_new(X_lhs->subnodes, &node_icd);
    utarray_push_back(X_lhs->subnodes, Op);
    utarray_push_back(X_lhs->subnodes, X_rhs);
}
unary_expr(X_lhs) ::= TILDE(Op) expr(X_rhs) .  [LAMBDA]
{
    // log_trace(">>unary_expr(X_lhs) ::= TILDE expr(X_rhs)");
    X_lhs = calloc(sizeof(struct node_s), 1);
    X_lhs->tid = TK_Unary_Expr;
    X_lhs->line  = X_rhs->line;
    X_lhs->col   = X_rhs->col;
    utarray_new(X_lhs->subnodes, &node_icd);
    utarray_push_back(X_lhs->subnodes, Op);
    utarray_push_back(X_lhs->subnodes, X_rhs);
}
unary_expr(X_lhs) ::= NOT(Op) expr(X_rhs) .  [LAMBDA]
{
    // log_trace(">>unary_expr(X_lhs) ::= NOT expr(X_rhs)");
    X_lhs = calloc(sizeof(struct node_s), 1);
    X_lhs->tid = TK_Unary_Expr;
    X_lhs->line  = X_rhs->line;
    X_lhs->col   = X_rhs->col;
    utarray_new(X_lhs->subnodes, &node_icd);
    utarray_push_back(X_lhs->subnodes, Op);
    utarray_push_back(X_lhs->subnodes, X_rhs);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* LAMBDA EXPRESSIONS */
/* LambdaExpr = 'lambda' [Parameters] ':' Test . */

lambda_expr(LambdaX) ::= LAMBDA(Lambda) COLON(Colon) expr(X) .
{
    // log_trace(">>lambda_expr ::= LAMBDA COLON expr");
    LambdaX = calloc(sizeof(struct node_s), 1);
    LambdaX->tid = TK_Lambda_Expr;
    LambdaX->line  = Lambda->line;
    LambdaX->col   = Lambda->col;
    LambdaX->trailing_newline = X->trailing_newline;
    utarray_new(LambdaX->subnodes, &node_icd);
    utarray_push_back(LambdaX->subnodes, Lambda);
    utarray_push_back(LambdaX->subnodes, Colon);
    utarray_push_back(LambdaX->subnodes, X);
}

lambda_expr(LambdaX) ::= LAMBDA(Lambda) param_list(Params) COLON(Colon) expr(X) .
{
    // log_trace(">>lambda_expr ::= LAMBDA params COLON expr");
    LambdaX = calloc(sizeof(struct node_s), 1);
    LambdaX->tid = TK_Lambda_Expr;
    LambdaX->line  = Lambda->line;
    LambdaX->col   = Lambda->col;
    LambdaX->trailing_newline = X->trailing_newline;
    utarray_new(LambdaX->subnodes, &node_icd);
    utarray_push_back(LambdaX->subnodes, Lambda);
    utarray_push_back(LambdaX->subnodes, Params);
    utarray_push_back(LambdaX->subnodes, Colon);
    utarray_push_back(LambdaX->subnodes, X);
}

/* %endif // EXPRESSIONS */
