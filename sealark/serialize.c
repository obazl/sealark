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

#include "serialize.h"

//FIXME: rename 2string => 2starlark

EXPORT void starlark_node2string(struct node_s *node, UT_string *buffer)
{
    /* log_debug("starlark_node2string"); */
    line = col = 0;
    _node2string(node, buffer);
    if (utstring_body(buffer)[utstring_len(buffer)-1] != '\n') {
        utstring_printf(buffer, "\n");
    }
}

EXPORT void starlark_nodelist2string(UT_array *nodes, UT_string *buffer)
{
    /* log_debug("rootlist2string, line %d", line); */
    line = col = 0;
    _nodelist2string(nodes, buffer);
    if (utstring_body(buffer)[utstring_len(buffer)-1] != '\n') {
        utstring_printf(buffer, "\n");
    }
}

LOCAL void _node2string(struct node_s *node, UT_string *buffer)
{
    /* log_debug("_node2string, line %d", line); */
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
        if (node->tid == TK_STRING) {
            //FIXME: call ast_node_printable_string?
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
                if (node->qtype & DQUOTE) {
                    if (node->qtype & TRIPLE) {
                        q = "\"\"\"";
                    } else {
                        q = "\"";
                    }
                } else {
                    q = "";
                }
            }

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
    /* log_debug("nodelist2string"); */
    /* line = col = 0; */
    struct node_s *node=NULL;
    while( (node=(struct node_s*)utarray_next(nodes, node))) {
        _node2string(node, buffer);
    }
}

LOCAL void comments2string(UT_array *nodes, UT_string *buffer)
{
    struct node_s *node=NULL;
    while( (node=(struct node_s*)utarray_next(nodes, node))) {
        _node2string(node, buffer);
    }
}
