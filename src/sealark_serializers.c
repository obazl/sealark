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
#include "utarray.h"
#include "utstring.h"

/* #if INTERFACE */
/* #include "utstring.h" */
/* #endif */

#include "sealark_serializers.h"

//FIXME: rename 2string => 2starlark

/* sealark_crush_string

   removes blank lines

   returns new UT_string, which user must free
 */
EXPORT char *sealark_crush_string(UT_string *src)
{
#if defined(DEBUG_SERIALIZERS)
    log_debug("sealark_crush_string");
#endif
    char *srcptr = utstring_body(src);
    char *dstptr = calloc(utstring_len(src), sizeof(char));

    size_t wc_count;

    if(dstptr) {
        char *p2 = dstptr;
        while(*srcptr != '\0') {
            if(*srcptr == '\n') {
                /* retain a single blank line */
                *p2++ = *srcptr++;
                while(*srcptr == '\n') {
                    srcptr++;
                }
            } else {
                *p2++ = *srcptr++;
            }
        }
        *p2 = '\0';
    }

    return dstptr;
}

/** sealark_squeeze_string

    collapses consecutive blank lines  to one

    returns new UT_string, which user must free
 */
EXPORT char *sealark_squeeze_string(UT_string *src)
{
#if defined(DEBUG_SERIALIZERS)
    log_debug("sealark_squeeze_string");
#endif
    char *srcptr = utstring_body(src);
    char *dstptr = calloc(utstring_len(src), sizeof(char));

    size_t wc_count;

    if(dstptr) {
        char *p2 = dstptr;
        while(*srcptr != '\0') {
            if(*srcptr == '\n') {
                /* retain a single blank line */
                *p2++ = *srcptr++;
                if(*srcptr == '\n') {
                    *p2++ = *srcptr++;
                    while(*srcptr == '\n') {
                        srcptr++;
                    }
                }
            } else {
                *p2++ = *srcptr++;
            }
        }
        *p2 = '\0';
    }

    return dstptr;
}

//FIXME: see sealark_display_ast_outline
EXPORT void sealark_node_to_starlark(struct node_s *node, UT_string *buffer)
{
#if defined(DEBUG_SERIALIZERS)
    log_debug("sealark_node_to_starlark");
#endif
    line = col = 0;
    _node2string(node, buffer);
    if (utstring_body(buffer)[utstring_len(buffer)-1] != '\n') {
        utstring_printf(buffer, "\n");
    }
}

EXPORT void sealark_nodelist2string(UT_array *nodes, UT_string *buffer)
{
#if defined(DEBUG_SERIALIZERS)
    log_debug("sealark_nodelist2string, line %d", line);
#endif
    line = col = 0;
    _nodelist2string(nodes, buffer);
    if (utstring_body(buffer)[utstring_len(buffer)-1] != '\n') {
        utstring_printf(buffer, "\n");
    }
}

#if EXPORT_INTERFACE
#define SEALARK_STRTYPE(Q) ((Q & BINARY_STR) &&      \
                 (Q & RAW_STR))? "br" \
                : (Q & BINARY_STR)? "b" \
                : (Q & RAW_STR)? "r" \
                          : "";
#endif

EXPORT char *sealark_quote_type(struct node_s *node)
{
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
    return q;
}

LOCAL void _node2string(struct node_s *node, UT_string *buffer)
{
#if defined(DEBUG_SERIALIZERS)
    /* log_debug("_node2string %d %s", node->tid, TIDNAME(node)); */
#endif
    int i;
    /* node->line is absolute; relativize it; */
    /* int l = node->line - line; */
    for (i=line; i < node->line; i++) {
        utstring_printf(buffer, "\n");
        line++;
        col = 0;
    }

    /* if (node->tid == TK_BLANK) */
    /*     utstring_printf(buffer, "\n"); */

    for (i=col; i < node->col; i++) {
        utstring_printf(buffer, " ");
        col++;
    }

    if (node->s) {
        /* log_debug("STRINGED TOK: %d %s: :]%s[:", */
        /*           node->tid, token_name[node->tid][0], */
        /*           node->s); */
        if (node->tid == TK_ID) {
            /* if (strncmp("True", node->s, 4) == 0) { */
            /*     utstring_printf(buffer, "#t"); */
            /*     col += 2; */
            /* } else { */
            /*     if (strncmp("True", node->s, 4) == 0) { */
            /*         utstring_printf(buffer, "#f"); */
            /*         col += 2; */
            /*     } else { */
                    utstring_printf(buffer, "%s", node->s);
                    col += strlen(node->s);
            /*     } */
            /* } */
        } else {
            if (node->tid == TK_STRING) {
                //FIXME: call ast_node_printable_string?

                char *br = SEALARK_STRTYPE(node->qtype);
                char *q = sealark_quote_type(node);

                utstring_printf(buffer,
                                "%s%s%s%s",
                                br,
                                q,
                                node->s,
                                q);
                /* adjust line count for embedded escaped newlines */
                for (char *p = node->s; *p != 0; p++) {
                    if (*p == '\n') {
                        line++;
                    }
                }
                col += strlen(node->s) + 2; /* allow for quote marks */
            } else {
                utstring_printf(buffer, "%s", node->s);
                col += strlen(node->s);
            }
        }
    } else {
        /* log_debug("TOK[%d] %s: :]%s[: (len: %d)", */
        /*           node->tid, */
        /*           token_name[node->tid][0], */
        /*           token_name[node->tid][1], */
        /*           strlen(token_name[node->tid][1])); */
        if (strlen(token_name[node->tid][1]) > 0) {
            utstring_printf(buffer, "%s", token_name[node->tid][1]);
            col += strlen(token_name[node->tid][1]);
        }
    }

    if (node->comments) {
        if (utarray_len(node->comments) > 0) {
            comments2string(node->comments, buffer);
        }
    }
    if (node->subnodes) {
        if (utarray_len(node->subnodes) > 0) {
            _nodelist2string(node->subnodes, buffer);
        }
    }
}

LOCAL void _nodelist2string(UT_array *nodes, UT_string *buffer)
{
#if defined(DEBUG_SERIALIZERS)
    /* log_debug("nodelist2string"); */
#endif
    /* line = col = 0; */
    struct node_s *node=NULL;
    while( (node=(struct node_s*)utarray_next(nodes, node))) {
        _node2string(node, buffer);
    }
}

LOCAL void comments2string(UT_array *nodes, UT_string *buffer)
{
#if defined(DEBUG_SERIALIZERS)
    /* log_debug("comments2string"); */
#endif
    struct node_s *node=NULL;
    while( (node=(struct node_s*)utarray_next(nodes, node))) {
        _node2string(node, buffer);
    }
}

/* **************************************************************** */
//FIXME: handle large files. use dynamic alloc
EXPORT void sealark_display_node(struct node_s *nd,
                                 UT_string *buffer, int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("sealark_display_node: %d %s", nd->tid, TIDNAME(nd));
    if (nd->tid == TK_ID)
        log_debug("ID: %s", nd->s);
#endif

    // check display_buf size, expand if needed

    /* char buf[128]; */
    /* UT_string *buf; */
    /* utstring_new(buf); */
    int len;

    if (level == 0) utstring_printf(buffer, "\n");

    utstring_printf(buffer, "%*.s#<node ",
                    (level==0)? 0 : (level+1)*2+level+1, ".");
    /* sprintf(buf, "#ast_node<\n"); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    utstring_printf(buffer, "tid=%d", nd->tid);
    /* sprintf(buf, " tid  = %d,\n", nd->tid); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    utstring_printf(buffer, " tnm=%s", token_name[nd->tid][0]);
    /* sprintf(buf, " tnm  = %s,\n", token_name[nd->tid][0]); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    utstring_printf(buffer, " line=%d", nd->line);
    /* sprintf(buf, " line  = %d,\n", nd->line); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    utstring_printf(buffer, " col=%d", nd->col);
    /* sprintf(buf, " col   = %d,\n", nd->col); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    utstring_printf(buffer, " trailing_newline=%d", nd->trailing_newline);
    /* sprintf(buf, " trailing_newline = %d,\n", nd->trailing_newline); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    if (nd->tid == TK_STRING) {
        utstring_printf(buffer, " qtype=%#04x", nd->qtype);
        /* sprintf(buf, " qtype = #x%#X,\n", nd->qtype); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */
    }

    if (nd->s) {
        char *br = SEALARK_STRTYPE(nd->qtype);
        char *q = sealark_quote_type(nd);

        /* for debugging, just print the string w/o qtype meta stuff */
        /* i.e. print r'foo' as foo*/
        /* utstring_printf(buffer, " s=%s%s%s%s", */
        /*                 br, q, nd->s, q); */
        utstring_printf(buffer, " s=\"%s\"", nd->s);

        /* sprintf(buf, " s     = %s,\n", nd->s); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */
    }

    if (nd->comments) {
        utstring_printf(buffer, " comments= ");
        /* sprintf(buf, " comments = "); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */

        /* updates global display_buf */
        //FIXME sunlark_nodelist_display(s7, (UT_array*)nd->comments);

        utstring_printf(buffer, ",");
        /* sprintf(buf, ",\n"); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */
    }

    if (nd->subnodes) {
        utstring_printf(buffer, "\n%*.ssubnodes=[\n",
                        (level==0)? 2 : (level+2)*2+level+1, ".");
                        /* (level==0)? level+2 :  level+2, " "); */
        /* sprintf(buf, " subnodes =\n\t"); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */

        //FIXME sunlark_nodelist_display(s7, (UT_array*)nd->subnodes);
        struct node_s *subn = NULL;
        int lvl = ++level;
        while((subn=(struct node_s*)utarray_next(nd->subnodes,
                                                 subn))) {
            sealark_display_node(subn, buffer, lvl);
        }

        utstring_printf(buffer, "%*.s]", (level+1)*2+1, " ");
        /* sprintf(buf, ",\n"); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */
    }

    utstring_printf(buffer, ">\n");
    /* sprintf(display_ptr - 2, ">,\n"); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr++; // -= 1; */

    /* return display_buf; */
}

LOCAL void _display_ast_outline(struct node_s *node,
                                      UT_string *buf, /* update in place */
                                      int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("sealark_display_ast_outline");
#endif

   switch(node->tid) {
    case TK_STRING: {
        char *br = SEALARK_STRTYPE(node->qtype);
        char *q = sealark_quote_type(node);

        utstring_printf(buf, "%*.s%d: %s[%d] @%d:%d    %s%s%s%s\n",
                  2*level, " ", level, TIDNAME(node), node->tid,
                  node->line, node->col,
                  br, q, node->s, q);
                  /* node->index); */
    }
        break;
    case TK_INT:
        utstring_printf(buf, "%*.s%d: %s[%d] @%d:%d        %s\n",
                  2*level, " ", level, TIDNAME(node), node->tid,
                  node->line, node->col, node->s);
        break;
    case TK_ID:
        utstring_printf(buf, "%*.s%d: %s[%d] @%d:%d    %s\n",
                  2*level, " ", level, TIDNAME(node), node->tid,
                  node->line, node->col, node->s);
        break;
    default:
        utstring_printf(buf, "%*.s%d: %s[%d] @%d:%d\n",
                  2*level, " ", level, TIDNAME(node), node->tid,
                  node->line, node->col);
        if (node->subnodes) {
            struct node_s *subnode = NULL;
            while((subnode=(struct node_s*)utarray_next(node->subnodes,
                                                        subnode))) {
                _display_ast_outline(subnode, buf, level+1);
            }
        }
    }
}

/* **************************************************************** */
EXPORT UT_string *sealark_display_ast_outline(struct node_s *nd,
                                              int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("sealark_display_node: %d %s", nd->tid, TIDNAME(nd));
    if (nd->tid == TK_ID)
        log_debug("ID: %s", nd->s);
#endif

    UT_string *buf;
    utstring_new(buf); // caller must free

    _display_ast_outline(nd, buf, level);
    return buf;
 }
