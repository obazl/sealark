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

#if INTERFACE
#include "utstring.h"
#endif

#include "obazl_starlark_serialize.h"

void root2string(struct node_s *node, UT_string *buffer)
{
    /* log_debug("root2string, line %d", line); */
    line = col = 0;
    node2string(node, buffer);
    /* if (utstring_body(buffer)[utstring_len(buffer)-1] != '\n') { */
        utstring_printf(buffer, "\n");
    /* } */
}

void rootlist2string(UT_array *nodes, UT_string *buffer)
{
    /* log_debug("rootlist2string, line %d", line); */
    line = col = 0;
    nodelist2string(nodes, buffer);
    if (utstring_body(buffer)[utstring_len(buffer)-1] != '\n') {
        utstring_printf(buffer, "\n");
    }
}

void node2string(struct node_s *node, UT_string *buffer)
{
    /* log_debug("node2string, line %d", line); */
    int i;
    /* node->line is absolute; relativize it; */
    /* int l = node->line - line; */
    for (i=line; i < node->line; i++) {
        utstring_printf(buffer, "\n");
        line++;
        col = 0;
    }

    /* if (node->type == TK_BLANK) */
    /*     utstring_printf(buffer, "\n"); */

    for (i=col; i < node->col; i++) {
        utstring_printf(buffer, " ");
        col++;
    }

    if (node->s) {
        /* log_debug("STRINGED TOK: %d %s: :]%s[:", */
        /*           node->type, token_name[node->type][0], */
        /*           node->s); */
        if (node->type == TK_STRING) {
            utstring_printf(buffer, "%c%s%c",
                            node->q, node->s, node->q);
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
        /*           node->type, */
        /*           token_name[node->type][0], */
        /*           token_name[node->type][1], */
        /*           strlen(token_name[node->type][1])); */
        if (strlen(token_name[node->type][1]) > 0) {
            utstring_printf(buffer, "%s", token_name[node->type][1]);
            col += strlen(token_name[node->type][1]);
        }
    }

    if (node->comments) {
        if (utarray_len(node->comments) > 0) {
            comments2string(node->comments, buffer);
        }
    }
    if (node->subnodes) {
        if (utarray_len(node->subnodes) > 0) {
            nodelist2string(node->subnodes, buffer);
        }
    }
}

void comments2string(UT_array *nodes, UT_string *buffer)
{
    struct node_s *node=NULL;
    while( (node=(struct node_s*)utarray_next(nodes, node))) {
        node2string(node, buffer);
    }
}

void nodelist2string(UT_array *nodes, UT_string *buffer)
{
    /* log_debug("nodelist2string"); */
    /* line = col = 0; */
    struct node_s *node=NULL;
    while( (node=(struct node_s*)utarray_next(nodes, node))) {
        node2string(node, buffer);
    }
}
