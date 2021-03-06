#include <assert.h>
#if INTERFACE
#include <stdbool.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <libgen.h>

#if INTERFACE
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#endif

#include <unistd.h>

#include "log.h"
#if EXPORT_INTERFACE
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"
#endif

#include "sealark_parsers.h"

/* UT_string *package; */

int line;
int col;

#if EXPORT_INTERFACE
struct parsed_pkg_s {
    char          *fname;
    struct node_s *pkg_node;
    UT_hash_handle hh;
};
#endif
struct parsed_pkg_s *parsed_pkgs = NULL; /* important! initialize to NULL */

EXPORT void sealark_dispose(struct node_s *node)
{
    assert(node->tid == TK_Package);
#if defined(DEBUG_MEM)
    log_debug("disposing of %s", node->fname);
#endif
    struct parsed_pkg_s *pkg = NULL;
    HASH_FIND_STR(parsed_pkgs, node->fname, pkg);
    if (pkg) {
#if defined(DEBUG_MEM)
        log_debug("FOUND parsed pkg %s", node->fname);
#endif
        HASH_DEL(parsed_pkgs, pkg);
        free(pkg->fname);
        /* do not delete pkg->pkg_node? s7 gc will dispose of it? */
        free(pkg);
    } else {
        log_error("???? parsed pkg %s not found", node->fname);
    }
}

#if EXPORT_INTERFACE
struct parse_state_s {
    char *fname;
    struct bf_lexer_s *lexer;
    struct node_s     *root; //FIXME: rename pkg (TK_Package)
};
#endif

bool is_empty(const char *s)
{
  while (*s) {
    if (!isspace(*s))
      return false;
    s++;
  }
  return true;
}

struct parse_state_s *parser_init(struct bf_lexer_s *lexer, struct node_s *root)
{
    struct parse_state_s *ps = calloc(sizeof(struct parse_state_s), 1);
    ps->lexer = lexer;
    ps->root  = root;
    return ps;
}

EXPORT void parser_free(parse_state_s *parser)
{
    // log_debug("parser_free %s", parser->lexer->fname);
    lexer_free(parser->lexer);
    sealark_node_free(parser->root);
    free(parser);
}

EXPORT UT_array *sealark_lex_string(const char *buffer)
{
    log_set_quiet(false);

    /* log_info("sealark_lex_string:\n%s", buffer); */

    UT_array *token_list;
    utarray_new(token_list, &node_icd);

    int tok;
    struct node_s *btok = calloc(sizeof(struct node_s), 1);

    struct bf_lexer_s * lexer = malloc(sizeof(struct bf_lexer_s));
    lexer_init(NULL, lexer, buffer);


    log_set_quiet(false);
    log_set_level(LOG_TRACE);
    /* log_info("starting lex"); */

    while ( (tok = get_next_token(lexer, &btok)) != 0 ) {
        /* log_debug("TOKEN: %s (%d) @ (%d:%d) %p", */
        /*           token_name[tok][0], */
        /*           tok, */
        /*           btok->line, btok->col, */
        /*           btok */
        /*           ); */
        /* log_debug("\tlexer posn: (%d:%d)", lexer->pos.line, lexer->pos.col); */
        /* log_debug("\tstarttok: :]%s[:", btok->s); */
        btok->tid = tok;
        /* dump_node(btok); */
        utarray_push_back(token_list, btok);

        btok = calloc(sizeof(struct node_s), 1);
    }
    return token_list;
}

EXPORT UT_array *sealark_lex_file(char *fname)
{
    log_set_quiet(false);

    UT_array *token_list;
    utarray_new(token_list, &node_icd);

    /* log_info("_lex_file: %s", fname); */
    FILE *f;

    f = fopen(fname, "r");
    if (f == NULL) {
        perror(fname);
        log_error("fopen failure for %s", fname);
        /* log_error("Value of errno: %d", errnum); */
        /* log_error("fopen error %s", strerror( errnum )); */
        exit(EXIT_FAILURE);
    }
    fseek(f, 0, SEEK_END);
    const size_t fsize = (size_t) ftell(f);
    if (fsize == 0) {
        fclose(f);
        errno = -1;
        exit(EXIT_FAILURE);
    }
    fseek(f, 0, SEEK_SET);
    char *buffer = (char*) malloc(fsize + 1);
    fread(buffer, 1, fsize, f);
    buffer[fsize] = 0;
    fclose(f);

    if (is_empty(buffer)) {
        fclose(f);
        errno = -2;
        exit(EXIT_FAILURE);
    }

    struct bf_lexer_s * lexer = malloc(sizeof(struct bf_lexer_s));
    lexer_init(fname, lexer, buffer);

    int tok;
    struct node_s *btok = calloc(sizeof(struct node_s), 1);

    log_set_quiet(false);
    log_set_level(LOG_TRACE);
    /* log_info("starting lex"); */

    while ( (tok = get_next_token(lexer, &btok)) != 0 ) {
        btok->tid = tok;
        /* log_debug("LEXED TOKEN: %s (%d/%d) @ (%d:%d) %p", */
        /*           token_name[tok][0], */
        /*           tok, btok->tid, */
        /*           btok->line, btok->col, */
        /*           btok */
        /*           ); */
        /* log_debug("\tlexer posn: (%d:%d)", lexer->pos.line, lexer->pos.col); */
        /* log_debug("\tstarttok: :]%s[:", btok->s); */
        btok->tid = tok;
        /* dump_node(btok); */

        utarray_push_back(token_list, btok);

        btok = calloc(sizeof(struct node_s), 1);
    }
    return token_list;
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
EXPORT struct node_s *sealark_parse_string(const char *buffer)
{
    log_set_quiet(false);

    /* log_info("sealark_parse_string: %s", buffer); */

    int tok;
    struct node_s *btok = calloc(sizeof(struct node_s), 1);

    struct bf_lexer_s * lexer = malloc(sizeof(struct bf_lexer_s));
    lexer_init(NULL, lexer, buffer);

    struct node_s *root = NULL;

    void* pParser = ParseAlloc (malloc);
    struct parse_state_s *parse_state = parser_init(lexer, root);

    /* log_debug("parsing %s", ast->fname); */
    /* ParseTrace(stdout, "trace_"); */

    log_set_quiet(false);
    log_set_level(LOG_TRACE);
    /* log_info("starting parse"); */

    while ( (tok = get_next_token(lexer, &btok)) != 0 ) {
        // log_debug("btok: %d, tok: %d", tok, btok->tid);
        btok->tid = tok;
        /* btok->line = lexer->pos.line; */
        /* btok->col  = lexer->pos.col; */
        /* log_debug("token code: %d, %d", tok, btok->tid); */
        /* log_debug("token name: %s", token_name[tok]); */
        /* if (btok->s != NULL) { */
        /*     log_debug("token str: %p", btok->s); */
        /* } */
        /* log_debug("TOKEN: %s (%d/%d) @ (%d:%d) %p", */
        /*           token_name[tok][0], */
        /*           tok, btok->tid, */
        /*           btok->line, btok->col, */
        /*           btok */
        /*           ); */
        /* log_debug("TOKEN: %s (%d), mode: %d, (%d:%d), str: '%s'\n", */
        /*           token_name[tok][0], */
        /*           /\* yyTokenName[tok], *\/ */
        /*           tok, */
        /*           lexer->mode, */
        /*           btok->line, btok->col, */
        /*           btok->s */
        /*           /\* lexer->clean_line, *\/ */
        /*           /\* lexer->indent *\/ */
        /*           ); */
        // log_debug("lexer posn (%d:%d)", lexer->pos.line, lexer->pos.col);

        /* log_debug("btok pos: line %d col %d", btok->pos.line, btok->pos.col); */
        /* dump_node(btok); */
        // log_debug(">>>>>>>>>>>>>>>>call parser for tok %d/%d", tok, btok->tid);
        // log_debug("root: %p", parse_state->root);
        Parse(pParser, tok, btok, parse_state);
        // log_debug(">>>>>>>>>>>>>>>>/call parser");
        btok = calloc(sizeof(struct node_s), 1);
    }
    // log_debug(">>>>>>>>>>>>>>>>FINAL call parser");
    Parse(pParser, 0, btok, parse_state);
    // log_debug(">>>>>>>>>>>>>>>>/FINAL call parser");
    /* ParseFree(pParser, free ); */

    /* fflush(stdout); */
    /* fflush(stderr); */
    return parse_state->root;
}

EXPORT struct node_s *sealark_parse_file(const char *fname)
{
    log_set_quiet(false);

#if defined(DEBUG_PARSERS)
    log_info("sealark_parse_file: %s", fname);
#endif
    FILE *f, *ftrace;

    struct parsed_pkg_s *pkg = NULL;
    HASH_FIND_STR(parsed_pkgs, fname, pkg);
    if (pkg) { /* already parsed */
        return pkg->pkg_node;
    }

    /* log_debug("CWD: %s", getcwd(NULL, 0)); */

    /* int r = access(fname, F_OK); */
    /* log_debug("access %s ? %d", fname, r); */

    f = fopen(fname, "r");
    if (f == NULL) {
        log_error("fopen failure for %s", fname);
        perror(fname);
        /* log_error("Value of errno: %d", errnum); */
        /* log_error("fopen error %s", strerror( errnum )); */
        exit(EXIT_FAILURE);
    }
    fseek(f, 0, SEEK_END);
    const size_t fsize = (size_t) ftell(f);
    if (fsize == 0) {
        fclose(f);
        errno = -1;
        exit(EXIT_FAILURE);
    }
    fseek(f, 0, SEEK_SET);
    char *buffer = (char*) malloc(fsize + 1);
    fread(buffer, 1, fsize, f);
    buffer[fsize] = 0;
    fclose(f);

    if (is_empty(buffer)) {
        fclose(f);
        errno = -2;
        exit(EXIT_FAILURE);
    }

    int tok;
    /* struct bzl_token_s *btok = calloc(sizeof(struct bzl_token_s), 1); */
    struct node_s *btok = calloc(sizeof(struct node_s), 1);
    struct bf_lexer_s * lexer = malloc(sizeof(struct bf_lexer_s));
    lexer_init(fname, lexer, buffer);

    struct node_s *root = NULL;

    void* pParser = ParseAlloc (malloc);
    struct parse_state_s *parse_state = parser_init(lexer, root);

    /* tracing - FIXME: add flag */
#if defined(YYTRACE)
    char *trace_file = "lemontrace.log";
    ftrace = fopen(trace_file, "w");
    if (ftrace == NULL) {
        log_error("fopen failure for %s", trace_file);
        perror(trace_file);
        /* log_error("Value of errno: %d", errnum); */
        /* log_error("fopen error %s", strerror( errnum )); */
        /* exit(EXIT_FAILURE); */
        return NULL;
    }

    ParseTrace(ftrace, "debug");
#endif

    /* parser_init(fname, &ast); */
    /* log_debug("parsing %s", ast->fname); */
    /* ParseTrace(stdout, "trace_"); */

    log_set_quiet(false);
    log_set_level(LOG_TRACE);
    /* log_info("starting parse"); */

    while ( (tok = get_next_token(lexer, &btok)) != 0 ) {
        btok->tid = tok;
        /* btok->line = lexer->pos.line; */
        /* btok->col  = lexer->pos.col; */
        /* log_debug("token code: %d, %d", tok, btok->tid); */
        /* log_debug("token name: %s", token_name[tok]); */
        /* if (btok->s != NULL) { */
        /*     log_debug("token str: %p", btok->s); */
        /* } */
        /* log_debug("TOKEN: %s[%d/%d] (%d:%d) %p", */
        /*           token_name[tok][0], */
        /*           tok, btok->tid, */
        /*           btok->line, btok->col, */
        /*           btok */
        /*           ); */
        /* log_debug("TOKEN: %s (%d), mode: %d, (%d:%d), str: '%s'\n", */
        /*           token_name[tok][0], */
        /*           /\* yyTokenName[tok], *\/ */
        /*           tok, */
        /*           lexer->mode, */
        /*           btok->line, btok->col, */
        /*           btok->s */
        /*           /\* lexer->clean_line, *\/ */
        /*           /\* lexer->indent *\/ */
        /*           ); */
        /* log_debug("lexer posn (%d:%d)", lexer->pos.line, lexer->pos.col); */

        /* log_debug("btok pos: line %d col %d", btok->pos.line, btok->pos.col); */
        /* dump_node(btok); */
        /* log_debug(">>>>>>>>>>>>>>>>call parser for %s[%d/%d]@(%d:%d) %s%s%s", */
        /*           token_name[btok->tid][0], */
        /*           tok, btok->tid, */
        /*           btok->line, btok->col, */
        /*           btok->s == NULL? "" : " :]", */
        /*           btok->s == NULL? "" : btok->s, */
        /*           btok->s == NULL? "" : "[:"); */

        Parse(pParser, tok, btok, parse_state);
        /* log_debug(">>>>>>>>>>>>>>>>/call parser"); */
        btok = calloc(sizeof(struct node_s), 1);
    }
    /* log_debug(">>>>>>>>>>>>>>>>FINAL call parser"); */
    Parse(pParser, 0, btok, parse_state);
    /* log_debug(">>>>>>>>>>>>>>>>/FINAL call parser"); */
    ParseFree(pParser, free );
    /* dump_node(parse_state->root); */

    free(buffer);

    pkg = calloc(1, sizeof(struct parsed_pkg_s));
    int len = strlen(fname);
    pkg->fname = calloc(len, sizeof(char));
    strncpy(pkg->fname, fname, len);
    pkg->pkg_node = parse_state->root;
    pkg->pkg_node->fname = pkg->fname;
    HASH_ADD_KEYPTR(hh, parsed_pkgs, fname, len, pkg);

    return parse_state->root;
}

EXPORT void sealark_parse_state_free(struct parse_state_s *ps)
{
    //FIXME: implement
    /* log_warn("sealark_parse_state_free NOT YET IMPLEMENTED"); */
}
