// Bazel BUILDfile lexer
// re2c $INPUT -o $OUTPUT -i

// starlark grammar: https://github.com/bazelbuild/starlark/blob/master/spec.md#grammar-reference
// gazelle lexer:  https://github.com/bazelbuild/buildtools/blob/master/build/lex.go

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/errno.h>

#include "utarray.h"
#include "log.h"
/* #include "tokens.h" */
#include "lexis.h"

#if EXPORT_INTERFACE
#define BUFSIZE 1024
#ifndef MAX_DEPS
#define MAX_DEPS 64
#endif
#endif

int curr_tag = 0;

/*!types:re2c */

UT_icd loadsyms_icd = {sizeof(char**), NULL, NULL, NULL};

#if EXPORT_INTERFACE
struct position_s {
    int line; // line in input (starting at 1)
    int col; // col in input (starting at 0)
};

struct bf_lexer_s
{
    char *fname;
    /* from gazelle */
    struct position_s pos;  // current input position
    int extra_lines;

    /* indent blocks */
    UT_array *indent_kws;       /* stack of ints for indentation sz */
    int       indent_colons;     /* counter to track indent blocks */
    UT_array *indent;     /* stack of indentations for block content */

    /* re2c */
    /* On entry, YYCURSOR is assumed to point to the first character
       of the current token. On exit, YYCURSOR will point to the first
       character of the following token. */
    const char *sob;        /* start of buffer */
    const char *cursor;         /* YYCURSOR */
    const char *tok;            /* YYCURSOR on entry, i.e. start of tok */
    const char *sol;            /* start of line ptr */
    const char *limit;
    const char *marker;
    const char *ctx_marker;
    int mode;                   /* i.e. start condition */

    /* stags - re2c will insert const char *yyt1, yyt2, etc. */
    /*!stags:re2c format = 'const char *@@;'; */

    /* mtags - re2c will insert 'int yyt3', 'int yyt4', etc. */
    /*!mtags:re2c format = 'int @@;'; */
};

#endif

int return_token(int tok)
{
    return tok;
}

void lexer_init(char *fname,
                struct bf_lexer_s *lexer,
                const char *sob /* start of buffer */ )
{
    /* need to initialize the '@@' fields inserted by re2c */
    memset(lexer, 0, sizeof(struct bf_lexer_s));
    lexer->fname = fname;
    lexer->sob = lexer->cursor = sob;
    /* lexer->cursor = lexer->marker = lexer->tok = */
    lexer->limit = sob + strlen(sob);
    lexer->sol = sob;

    utarray_new(lexer->indent_kws, &ut_int_icd);
}

void lexer_free(bf_lexer_s *lexer)
{
    /* log_debug("lexer_free: %s", lexer->fname); */
    utarray_free(lexer->indent_kws);
}

/* static int fill(struct bf_lexer_s *lexer) */
/* { */
/*     log_debug("FILL"); */
/*     return 0; */
/* } */

/* typedef std::vector<Mtag> MtagTree; */

/* comment_push will be called once per mtag per subexpr match */
static int push_ct = 0;
static int null_ct = 0;

/* comment_push

  posn tracking: the primary token may contain newlines, as in the
  case of multi-line strings. we need to track line and col
  accordingly. we must do this here, since this routine is called
  before control passes to the primary rule.

  how? this routine is called twice per comment, once at #scmt and
  once at #ecmt. so on the second (#ecmt) call we can inspect the main
  matched string and compute the posn vals needed for the comment
  node. we count the number of newlines, and we find the last newline,
  from which we can determine the sol needed to compute the col nbr of
  the comment.

  lexer->tok: always pts to initial char of main match
  lexer->cursor: pts to current mtag (#scmt or #ecmt)
  lexer->sol:  ptr to start of line (of lexer-tok?)

  the lexer->pos.line is off-limits, since it must match lexer->tok.
  So we maintain here corresponding static vars: line, col, etc. We
  use them to track posn for the submatch tokens.
 */
static void comment_push(int type, /* 1: comment, 0: null */
                         int *idx,  // const char *t,
                         bf_lexer_s *lexer,
                         struct node_s **mtok)
{
    static const char *scmt;   /* start of tok (ptr) */
    static const char *sol;     /* start of line (ptr) */
    static int line;
    static int newlines;
    static int col;           /* start of token (column nbr, not ptr) */

    /* log_debug(""); */
    /* log_debug("new PUSH %d: idx %d, null ct: %d, push_ct: %d; line: %d newlines: %d", */
    /*           type, *idx, null_ct, push_ct, line, newlines); */
    /* /\* log_debug("\t@s1: %s, @s2: %s", s1, s2); *\/ */
    /* log_debug("idx: %p (%d)", idx, *idx); */

    /* log_debug("\tlexer: %p; lexer->pos (%d:%d)", */
    /*           lexer, lexer->pos.line, lexer->pos.col); */

    /* log_debug("\tmtok: %p (%d:%d)", *mtok, (*mtok)->line, (*mtok)->col); */
    /* log_debug("\tlexer->tok: %p: :]%s[:", lexer->tok, lexer->tok); */
    /* log_debug("\tscmt (10) %p :]%.10s[:", scmt, scmt); */
    /* log_debug("\tlexer->cursor (10) %p :]%.10s[:", lexer->cursor, lexer->cursor); */
    /* log_debug("\tlexer->marker (10) %p :]%.10s[:", lexer->marker, lexer->marker); */
    /* log_debug("\tlexer->pos: (%d:%d)", lexer->pos.line, lexer->pos.col); */
    /* log_debug("\tpos: (%d:%d)", line, col); */
    /* log_debug("\tlexer->sol (10): %p: :]%.10s:[", lexer->sol, lexer->sol); */
    /* log_debug("\tsol (10): %p: :]%.10s", sol, sol); */

    if (type == 0) {
        /* log_debug("NULL push"); */
        scmt = lexer->cursor;
        /* log_debug("lexer->tok: %p :]%s", lexer->tok, lexer->tok); */
        /* log_debug("lexer->marker: %p :]%s", lexer->marker, lexer->marker); */
        /* log_debug("lexer->cursor: %p: :]%s", lexer->cursor, lexer->cursor); */
        /* log_debug("(lexer->cursor - 1: %p: :]%s", */
        /*           lexer->cursor-2, lexer->cursor-2); */

        /* NB: idx is not reliable */
        if ( (null_ct == 0) && push_ct == 0) {
            /* log_debug("FIRST NULL, first push"); */
            /* first newline precedes comment  */
            newlines = 1;
            line        = lexer->pos.line;
            /* count (maybe escaped) newlines in string */
            /* we test against lexer->cursor - 1 instead of lexer->cursor,
               to account for case where first cmt comes after newline */
            for (const char *p = lexer->tok; p < (lexer->cursor - 1); p++) {
                if (*p == '\n') {
                    /* log_debug("    *NEWLINE (inc linect)"); */
                    line++;
                    lexer->extra_lines++;
                    /* break; */
                }
            }
            /* if ((*mtok)->comments == NULL) { */
            /*     log_debug("    creating new (*mtok)->comments array"); */
            /*     utarray_new((*mtok)->comments, &node_icd); */
            /* } */
        } else {
            /* if ( (null_ct == 0) && push_ct == 0) { */
            if ( (null_ct == 1) && (push_ct == 0) ) {
                /* log_debug("FIRST NULL, second push: 0x%" PRIx64 "\n", */
                /*           *(lexer->cursor)); */
                /* if (scmt == lexer->cursor) { */
                /* } */
                if (*(lexer->cursor) == '\n') {
                    /* log_debug("    NEWLINE"); */
                    /* (*mtok)->line++; */
                    lexer->extra_lines++;
                    /* struct node_s *n = calloc(sizeof(struct node_s), 1); */
                    /* n->type = TK_BLANK; */
                    /* n->line = line; // - newlines; */
                    /* /\* log_debug("xxxx: %d:%d", n->line, n->col); *\/ */
                    /* utarray_push_back((*mtok)->comments, n); */
                } else {
                    /* lexer->pos.line++; */
                }
            } else {
                if ( (null_ct % 2) == 0 ) { // && (push_ct != 0) ) {
                    /* log_debug("TAIL NULL, first push"); */
                } else {
                    /* log_debug("TAIL NULL, second push"); */
            if ((*mtok)->comments == NULL) {
                /* log_debug("    creating new (*mtok)->comments array"); */
                utarray_new((*mtok)->comments, &node_icd);
            }

                    /* log_debug("    creating TK_BLANK"); */
                    /* lexer->pos.line++; */
                    line++;
                    lexer->extra_lines++;
                    struct node_s *n = calloc(sizeof(struct node_s), 1);
                    n->type = TK_BLANK;
                    n->line = line; // - newlines;
                    /* log_debug("xxxx: %d:%d", n->line, n->col); */
                    utarray_push_back((*mtok)->comments, n);
                    /* lexer->pos.col = 0; */
                }
            }
            newlines++;
            (*idx)++;
        }
        /* sol = lexer->cursor; */
        null_ct++;
        return;
    }
    /* log_debug("COMMENT push (%d:%d)", line, col); */

    if ( (push_ct % 2) == 0) {
        /* log_debug("    *EVENS (%d:%d)", line, col); */

        /* line++; */
        scmt = lexer->cursor;   /* := scmt, ptr to initial char of cmt */
        if ( push_ct == 0) { /* first comment in list */
            /* log_debug("    *FIRST COMMENT"); */
            if (null_ct == 0) { /* no preceding blank line */
                line = lexer->pos.line;
                /* is 1st comment on same line as primary token? */
                int nlct=0;
                for (const char *p = lexer->tok;
                     p < lexer->cursor;
                     p++) {
                    /* log_debug("*p: %.1s", p); */
                    if (*p == '\n') {
                        /* sol = p + 1; */
                        /* log_debug("    *NEWLINE"); */
                        line++;
                        nlct++;
                        /* break; */
                    }
                }
                if (nlct > 0) {
                    /* sol = lexer->sol; */
                    /* sol++;          /\* one past newline *\/ */
                } else {
                    /* sol = lexer->sol; */
                    line = lexer->pos.line;
                    /* col  = lexer->pos.col; */
                }
            } else {
                line++;
            }

            if ((*mtok)->comments == NULL) {
                /* log_debug("\tcreating new (*mtok)->comments array"); */
                utarray_new((*mtok)->comments, &node_icd);
            }
        } else {
            line++;
        }
    } else {
        /* log_debug("    *ODDS (%d:%d)", line, col); */
        /* log_debug("(lexer->cursor - 1: %p: :]%.5s", */
        /*           lexer->cursor-2, lexer->cursor-1); */

        if ( *(lexer->cursor) == '\n') {

        } else {
        }
        /* to find start of line, start at start-of-commment and
           backup until newline or start-of-buffer */
        /* log_debug("scmt: %s", scmt); */
        /* log_debug("lexer->sob: %s", lexer->sob); */
        for (sol = scmt; // lexer->cursor - 1;
             *sol != '\n' && sol > lexer->sob;
             --sol) {
            /* log_debug("\txxxx sol: %s", sol); */
        };
        if (*sol == '\n') sol++; /* \n is end-of-line  */
        /* log_debug("SOL: %s", sol); */

        /* log_debug("\tcreating TK_COMMENT node (5): :]%.5s...", scmt); */

        /* starting at #ecmt, find first preceding newline */
        struct node_s *n = calloc(sizeof(struct node_s), 1);
        n->type = TK_COMMENT;
        /* n->line = newlines + line; */
        n->line = line; // - newlines;
        n->col  = scmt - sol; // lexer->pos.col;
        n->s    = strndup(scmt,
                          lexer->cursor /* #ecmt */ - scmt);
        /* log_debug("xxxx: %d:%d", n->line, n->col); */
        utarray_push_back((*mtok)->comments, n);
        /* log_debug("\tnew mtok: %p (%d:%d)", *mtok, (*mtok)->line, (*mtok)->col); */
        /* log_debug("yyyy"); */

        /* account for newline */
        /* line++; */
        lexer->extra_lines++;
        /* sol = lexer->sol; // cursor + 1; */
        newlines = 0;  /* why? */
    }
    /* } */
    /* log_debug("\txlexer->pos: (%d:%d)", lexer->pos.line, lexer->pos.col); */
    /* log_debug("\txpos: (%d:%d)", line, col); */
    /* log_debug("\txsol:  %p: :]%.10s[:", sol, sol); */
    /* log_debug("\txscmt: %p: :]%.5s", scmt, scmt); */
    /* log_debug("\txmtok pos: (%d:%d)", (*mtok)->line, (*mtok)->col); */
    /* log_debug("\txlexer->cursor (10) %p :]%.10s[:", */
    /*           lexer->cursor, lexer->cursor, lexer->cursor); */
    (*idx)++;
    push_ct++;
}

int get_column(struct bf_lexer_s *lexer)
{
    /* log_debug("get_column"); */
    for (lexer->sol = lexer->tok;
         *(lexer->sol) != '\n' && lexer->sol > lexer->sob;
         --(lexer->sol)) {
        ;
    };
    if (*(lexer->sol) == '\n') (lexer->sol)++; /* \n is eol, not start */
    /* log_debug("SOL: %s", lexer->sol); */
    return lexer->tok - lexer->sol;
}

int get_next_token(struct bf_lexer_s *lexer, struct node_s **mtok)
{
    /* log_debug("get_next_token"); */
    /* log_debug("lexer->limit: %p", lexer->limit); */
    /*!re2c
      re2c:api:style = free-form;
      re2c:define:YYCTYPE = char;
      re2c:define:YYCURSOR = lexer->cursor;
      re2c:define:YYLIMIT = lexer->limit;
      re2c:define:YYMARKER = lexer->marker;
      re2c:define:YYCTXMARKER = lexer->ctx_marker;
      re2c:define:YYGETCONDITION = "lexer->mode";
      re2c:define:YYSETCONDITION = "lexer->mode = @@;";

      re2c:flags:tags = 1;
      re2c:tags:expression = "lexer->@@";
     re2c:define:YYMTAGP = "comment_push(1, &@@, lexer, mtok);";
      re2c:define:YYMTAGN = "comment_push(0, &@@, lexer, mtok);";

      /* re2c:define:YYFILL   = "fill(lexer) == 0;"; */
      re2c:yyfill:enable  = 0;
    */ // end !re2c

    //int c = yycinit;
    /* lexer->mode = yycinit; */
    /* const char *YYMARKER; */

    // stag def sin bf_lexer_s
    const char *s1, *s2, *t1, *t2;

    // mtag defs are in struct bf_lexer_s
    int scmt = 0;
    int ecmt = 0;
    /* int blank1, blank2; */

    // reset comment count
    push_ct = 0;
    null_ct = 0;

loop:
    /* log_debug("loop lexmode: %d", lexer->mode); */
    curr_tag = 0;
    lexer->tok = lexer->cursor;
    /*!re2c
    //docs say !include should work, but it does not: unexpected char !
/* !include "literals.re"; */

      end    = "\x00";
      eol    = "\n";
      ws     = [ \t];
      wsnl   = [ \t\n];

      oct    = "\\" [0-9]{1,3};

      // IDENTIFIERS "an identifier is a sequence of
      // Unicode letters, decimal digits, and underscores (_), not
      // starting with a digit.

      // from https://re2c.org/manual/manual_c.html#encoding-support

      // id_start    = L | Nl | '_'; // Letters, Number letters
      id_start = [a-zA-Z_];

      // id_continue = id_start | Mn | Mc | Nd | Pc | [\u200D\u05F3];
      id_continue = id_start | [0-9];

      identifier  = id_start id_continue*;

      // NB: if blank line follows cmt, eol on cmt line will be
      // processed separately (push called with mtok == NULL)
    /* COMMENTS = ws* eol? ((ws* #scmt "#"[^\n]+ #ecmt eol) */
    /*                      | (#blank1 ws* eol #blank2))* ; */
    /* COMMENTS = (ws* ((#scmt "#"[^\n]+ #ecmt eol) | eol))*; */
    /* COMMENTS = ws* eol? (ws* (#scmt "#"[^\n]+ #ecmt)? eol)*; */

    COMMENTS = (ws* (#scmt "#"[^\n]+ #ecmt)? eol)*;

      // RULES

    // This runs _after_ mtag handlers and _before_ primary handler,
    // so lexer->tok and lexer->cursor are set. BUT, lexer->cursor
    // includes any following comments.
      <!init> {
        /* log_debug("<!init>"); */

        (*mtok)->line = lexer->pos.line;
        /* log_debug("lexer->extra_lines: %d", lexer->extra_lines); */
        if (lexer->extra_lines > 0) {
            (*mtok)->trailing_newline = true;
            lexer->pos.line += lexer->extra_lines;
            lexer->extra_lines = 0;
            lexer->pos.col = 0;
        }

        /* (*mtok)->col = lexer->pos.col; */
        (*mtok)->col = get_column(lexer);

        /* /\* to find start of line, start at start-of-commment and */
        /*    backup until newline or start-of-buffer *\/ */
        /* /\* log_debug("scmt: %s", scmt); *\/ */
        /* /\* log_debug("lexer->sob: %s", lexer->sob); *\/ */
        /* for (lexer->sol = lexer->cursor - 1; */
        /*      *(lexer->sol) != '\n' && lexer->sol > lexer->sob; */
        /*      --(lexer->sol)) { */
        /*     log_debug("lexer->SOL: %s", lexer->sol); */
        /*     /\* log_debug("\txxxx sol: %s", sol); *\/ */
        /* }; */
        /* /\* if (*(lexer->sol) == '\n') lexer->sol++; /\\* \n is end-of-line  *\\/ *\/ */
        /* (*mtok)->col  = lexer->tok - lexer->sol; */

          /* log_debug("<!init> mtok: %p (%d:%d)", */
          /*     *mtok, (*mtok)->line, (*mtok)->col); */

          /* log_debug("<!init> lexer->pos: (%d:%d)", */
          /*           lexer->pos.line, lexer->pos.col); */
          /* log_debug("<!init> lexer->tok: :]%s[:", lexer->tok); */
          /* log_debug("<!init> lexer->limit: :]%s[:", lexer->limit); */
          /* log_debug("<!init> lexer->cursor: :]%s[:", lexer->cursor); */
          /* if (lexer->marker) */
          /*     log_debug("<!init> lexer->marker[1]: %#x", *(lexer->marker)); */
          /* log_debug("<!init> lexer->marker: :]%s[:", lexer->marker); */
          /* /\* log_debug("<!init> lexer->ctx_marker[1]: %#x", *(lexer->ctx_marker)); *\/ */
          /* log_debug("<!init> lexer->ctx_marker: :]%s[:", lexer->ctx_marker); */
          // lexer->pos.col = lexer->tok - lexer->sol;
          // productions using tags must explicitly set mtok line and col
      }

    SP = " ";
    <*> @s1 SP+ @s2 {
          /* log_debug("<*> ' ': lexer->pos: %p (%d:%d)", */
          /*           &(lexer->pos), lexer->pos.line, lexer->pos.col); */
          lexer->pos.col += s2 - s1;
          /* log_debug("<*> ' ': new lexer->pos: %p (%d:%d)", */
          /*     &(lexer->pos), lexer->pos.line, lexer->pos.col); */
          goto loop;
      }

      <*> eol {
        log_debug("<*> eol (%d:%d)", lexer->pos.line, lexer->pos.col);
        log_debug("<*> eol lexer->pos %p", lexer->pos);
        lexer->sol = lexer->cursor;
        lexer->pos.line++;
        lexer->pos.col = 0;
        goto loop;
      }

// PUNCTUATION
/*
      +    -    *    //   %    **
      ~    &    |    ^    <<   >>
      .    ,    =    ;    :
      (    )    [    ]    {    }
      <    >    >=   <=   ==   !=
      +=   -=   *=   //=  %=
      &=   |=   ^=   <<=  >>=
*/

    <init> "**" COMMENTS { return return_token(TK_STAR2); }
    <init> "->" COMMENTS { return TK_ARROW; }
    <init> "<=" COMMENTS { return TK_LE; }
    <init> ">=" COMMENTS { return TK_GE; }

    <init> "." COMMENTS { return TK_DOT; }
    <init> "," COMMENTS { return TK_COMMA; }
    <init> ";" COMMENTS { return TK_SEMI; }
    <init> ":" COMMENTS {
        int kwstack_height = utarray_len(lexer->indent_kws);
        log_debug("COLON: indent_kws ct: %d", kwstack_height);
        log_debug("COLON: indent_colons: %d", lexer->indent_colons);
        if (kwstack_height > 0) {
            if (lexer->indent_colons == kwstack_height) {
                return TK_COLON;
            } else {
                if (lexer->indent_colons == kwstack_height - 1) {
                    lexer->indent_colons++;
                    return TK_COLON;
                } else {
                    log_error("indent kws and colons out of sync!");
                    exit(EXIT_FAILURE);
                }
            }
        } else {
            return TK_COLON;
        }
    }
    <init> "!=" COMMENTS { return TK_BANG_EQ; }
    <init> "!"  COMMENTS { return TK_BANG; }
    <init> "+=" COMMENTS { return TK_PLUS_EQ; }
    <init> "+" COMMENTS {
        log_debug("lex PLUS (%d:%d)", (*mtok)->line, (*mtok)->col);
        return TK_PLUS;
    }
    <init> "-=" COMMENTS { return TK_MINUS_EQ; }
    <init> "-" COMMENTS { return TK_MINUS; }
    <init> "*=" COMMENTS { return TK_STAR_EQ; }
    <init> "*" COMMENTS { return TK_STAR; }
    <init> "//=" COMMENTS { return TK_SLASH2_EQ; }
    <init> "//" COMMENTS { return TK_SLASH2; }
    <init> "/=" COMMENTS { return TK_SLASH_EQ; }
    <init> "/" COMMENTS { return TK_SLASH; }
    <init> "%=" COMMENTS { return TK_PCT_EQ; }
    <init> "%" COMMENTS { return TK_PCT; }
    <init> "&=" COMMENTS { return TK_AMP_EQ; }
    <init> "&" COMMENTS { return TK_AMP; }
    <init> "|=" COMMENTS { return TK_VBAR_EQ; }
    <init> "|" COMMENTS { return TK_VBAR; }
    <init> "^=" COMMENTS { return TK_CARET_EQ; }
    <init> "^" COMMENTS { return TK_CARET; }
    <init> "~" COMMENTS { return TK_TILDE; }
    <init> "[" COMMENTS { return TK_LBRACK; }
    <init> "]" COMMENTS { return TK_RBRACK; }
    <init> "{" COMMENTS { return TK_LBRACE; }
    <init> "}" COMMENTS { return TK_RBRACE; }

    <init> "(" COMMENTS {
        log_debug("<init> lparen");
        (*mtok)->col = lexer->tok - lexer->sol;
        return TK_LPAREN;
    }
    <init> @s1 "#"[^\n]+ eol @s2 {
        /* n->line = line; // - newlines; */
        /* n->col  = scmt - sol; // lexer->pos.col; */
        (*mtok)->s = strndup(s1, (size_t)(s2 - s1));
        return TK_COMMENT;
    }

    <init> ")" COMMENTS {
            /* log_debug("<init> RPAREN, mode: %d", lexer->mode); */
            return TK_RPAREN;
        }

    <init> "<<=" COMMENTS { return TK_LLANGLE_EQ; }
    <init> "<<"  COMMENTS { return TK_LLANGLE; }
    <init> "<"   COMMENTS { return TK_LANGLE; }
    <init> ">>=" COMMENTS { return TK_RRANGLE_EQ; }
    <init> ">>"  COMMENTS { return TK_RRANGLE; }
    <init> ">"   COMMENTS { return TK_RANGLE; }
    <init> "=="  COMMENTS { return TK_EQ2; }
    <init> "="   COMMENTS { return TK_EQ; }
    <init> "\\"  COMMENTS { return TK_BACKSLASH2; }

    /* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
    /* LITERALS: integer, floating point, string, byte  */

    /* NUMBER  LITERALS */
    /* INTEGER literals. NB: treat '0' as decimal int  */
    /* 0                               # int */
    /* 123                             # decimal int */
    /* 0123                            # disallowed */
    /* 0x7f                            # hexadecimal int */
    /* 0o755                           # octal int */


    /* INT DECIMAL */
    <init> @s1 ( "0" | [1-9][0-9]*) @s2 COMMENTS
    {
        (*mtok)->s = strndup(s1, (size_t)(s2 - s1));
        return TK_INT; // TK_INT_DEC;
    }
    /* INT OCTAL */
    <init> @s1 "0" [oO] [0-7]+ @s2 COMMENTS
    {
        (*mtok)->s = strndup(s1, (size_t)(s2 - s1));
        return TK_INT; // TK_INT_OCT;
    }
    /* INT HEX */
    <init> @s1 "0" [xX] [0-9a-fA-F]+ @s2 COMMENTS
    {
        (*mtok)->s = strndup(s1, (size_t)(s2 - s1));
        return TK_INT; // TK_INT_HEX;
    }

    /* FLOAT literals */
    exponent = [eE] [+-]? [0-9]+ ;
    <init> @s1 [0-9]+ "." [0-9]* exponent* @s2 COMMENTS {
        (*mtok)->s = strndup(s1, (size_t)(s2 - s1));
        return TK_FLOAT;
    }

    /* STRING LITERALS */
    /* single-quote */
    <init> @t1 ([br]|"br"|"rb")? @t2 "'" @s1 ([^'\n] | "\\\n")* @s2 "'"
           COMMENTS
        {
            log_debug("matched SINGLE_QUOTE TK_STRING (%d:%d)",
                      lexer->pos.line, lexer->pos.col);
            log_debug("s1: %s; s2: %s", s1, s2);
            log_debug("\tlexer->tok: %p: :]%s[:", lexer->tok, lexer->tok);
            log_debug("\tlexer->cursor (10) %p :]%.10s[:",
                      lexer->cursor, lexer->cursor, lexer->cursor);
            /* lexer->cursor pts to one past end (including comments) */
            (*mtok)->s = strndup(s1, (size_t)(s2 - s1));

            /* if we have a comment, we need to bump lexer->pos.line */
            if ((*mtok)->comments) {
                if ( utarray_len((*mtok)->comments ) > 0) {
                    struct node_s *n = utarray_back((*mtok)->comments);
                    /* struct node_s *n=NULL; */
                    /* while( (n=(struct node_s*)utarray_next((*mtok)->comments, n))) { */
                    lexer->pos.line = n->line + 1;
                    /* lexer->pos.col  = 0; // FIXME */
                    /* } */
                    /* } else { */
                    /*     lexer->pos.col += strlen( (*mtok)->s ); */
                }
            }
            (*mtok)->col = get_column(lexer);

            (*mtok)->qtype = SQUOTE;
            if (strncmp(t1, "br", 2) == 0) {
                (*mtok)->qtype |= (RAW_STR | BINARY_STR);
                return TK_STRING;
            }
            if (strncmp(t1, "rb", 2) == 0) {
                (*mtok)->qtype |= (RAW_STR | BINARY_STR);
                return TK_STRING;
            }
            //FIXME: check len t1 == 1?
            if (*t1 == 'b') { (*mtok)->qtype |= BINARY_STR; }
            if (*t1 == 'r') { (*mtok)->qtype |= RAW_STR; }
            return TK_STRING;
      }

    /* double-quote */
    <init> @t1 ([br]|"br"|"rb")? @t2 "\"" @s1 ([^"\n] | "\\\n")* @s2 "\""
           COMMENTS
        {
            /* log_debug("matched DOUBLE_QUOTE TK_STRING (%d:%d)", */
            /*           lexer->pos.line, lexer->pos.col); */

            (*mtok)->s = strndup(s1, (size_t)(s2 - s1));

            /* if we have a comment, we need to bump lexer->pos.line */
            if ((*mtok)->comments) {
              if ( utarray_len((*mtok)->comments) > 0 ) {
                struct node_s *n = utarray_back((*mtok)->comments);
                log_debug("\tlast COMMENT posn: (%d:%d)", n->line, n->col);
                lexer->pos.line = n->line + 1;
              }
            }
            (*mtok)->col = get_column(lexer);

            (*mtok)->qtype = DQUOTE;
            if (strncmp(t1, "br", 2) == 0) {
                (*mtok)->qtype |= (RAW_STR | BINARY_STR);
                return TK_STRING;
            }
            if (strncmp(t1, "rb", 2) == 0) {
                (*mtok)->qtype |= (RAW_STR | BINARY_STR);
                return TK_STRING;
            }
            //FIXME: check len t1 == 1?
            if (*t1 == 'b') { (*mtok)->qtype |= BINARY_STR; }
            if (*t1 == 'r') { (*mtok)->qtype |= RAW_STR; }
            return TK_STRING;
      }

    // MULTI-LINE (TRIPLE-QUOTED) STRINGS
    /* single-quote */
    <init> @t1 ([br]|"br"|"rb")? @t2 "'''" @s1 [^']* @s2 "'''"
           COMMENTS
        {
            (*mtok)->s = strndup(s1, (size_t)(s2 - s1));

            (*mtok)->qtype = SQUOTE | TRIPLE;
            if (strncmp(t1, "br", 2) == 0) {
                (*mtok)->qtype |= (RAW_STR | BINARY_STR);
                return TK_STRING;
            }
            if (strncmp(t1, "rb", 2) == 0) {
                (*mtok)->qtype |= (RAW_STR | BINARY_STR);
                return TK_STRING;
            }
            //FIXME: check len t1 == 1?
            if (*t1 == 'b') { (*mtok)->qtype |= BINARY_STR; }
            if (*t1 == 'r') { (*mtok)->qtype |= RAW_STR; }
            return TK_STRING;
      }

    /* double-quote */
    <init> @t1 ([br]|"br"|"rb")? @t2 "\"\"\"" @s1 [^"]* @s2 "\"\"\""
           COMMENTS
    {
        (*mtok)->s = strndup(s1, (size_t)(s2 - s1));

        (*mtok)->qtype = DQUOTE | TRIPLE;
        if (strncmp(t1, "br", 2) == 0) {
            (*mtok)->qtype |= (RAW_STR | BINARY_STR);
            return TK_STRING;
        }
        if (strncmp(t1, "rb", 2) == 0) {
            (*mtok)->qtype |= (RAW_STR | BINARY_STR);
            return TK_STRING;
        }
        //FIXME: check len t1 == 1?
        if (*t1 == 'b') { (*mtok)->qtype |= BINARY_STR; }
        if (*t1 == 'r') { (*mtok)->qtype |= RAW_STR; }
        return TK_STRING;
   }

    /* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
    /* KEYWORDS */
    <init> "and" COMMENTS { return TK_AND; }
    <init> "else" COMMENTS { return TK_ELSE; }

    <init> "break" COMMENTS { return TK_BREAK; }
    <init> "load" COMMENTS { return TK_LOAD; }
    <init> "for" COMMENTS { return TK_FOR; }
    <init> "not" COMMENTS { return TK_NOT; }
    <init> "continue" COMMENTS { return TK_CONTINUE; }
    <init> "if" COMMENTS { return TK_IF; }
    <init> "or" COMMENTS { return TK_OR; }
    <init> @s1 "def" @s2 COMMENTS {
         int c = get_column(lexer);
         utarray_push_back(lexer->indent_kws, &c);
         return TK_DEF;
     }
    <init> "in" COMMENTS { return TK_IN; }
    <init> "pass" COMMENTS { return TK_PASS; }
    <init> "elif" COMMENTS { return TK_ELIF; }
    <init> "lambda" COMMENTS { return TK_LAMBDA; }
    <init> "return" COMMENTS { return TK_RETURN; }

        /* RESERVED */
        /* "The tokens below also may not be used as identifiers although they do not appear in the grammar; they are reserved as possible future keywords:"
         */
    <init> "as" COMMENTS { return TK_AS; }
    <init> "import" COMMENTS { return TK_IMPORT; }
    <init> "assert" COMMENTS { return TK_ASSERT; }
    <init> "is" COMMENTS { return TK_IS; }
    <init> "class" COMMENTS { return TK_CLASS; }
    <init> "nonlocal" COMMENTS { return TK_NONLOCAL; }
    <init> "del" COMMENTS { return TK_DEL; }
    <init> "raise" COMMENTS { return TK_RAISE; }
    <init> "except" COMMENTS { return TK_EXCEPT; }
    <init> "try" COMMENTS { return TK_TRY; }
    <init> "finally" COMMENTS { return TK_FINALLY; }
    <init> "while" COMMENTS { return TK_WHILE; }
    <init> "from" COMMENTS { return TK_FROM; }
    <init> "with" COMMENTS { return TK_WITH; }
    <init> "global" COMMENTS { return TK_GLOBAL; }
    <init> "yield" COMMENTS { return TK_YIELD; }

    <init> @s1 identifier @s2 COMMENTS {
                       /* log_debug("<init> IDENTIFIER"); */
            /* (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok); */
            (*mtok)->s = strndup(s1, (size_t)(s2 - s1));
        return TK_ID;
        }

    <init> end       {
        /* printf("<init> ending\n"); */
        return 0;
        }

    <*> *         {
            fprintf(stderr, "ERROR lexing: %s\n", lexer->tok);
            exit(-1);
        }

    */
}

