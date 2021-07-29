#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "sealark_debug.h"

/* ******************************** */
/* recursively print outline */
EXPORT void sealark_debug_print_ast_outline(struct node_s *node, int level)
{
    if (node->s)
        log_debug("%*.s%d: %s %d: %s",
                  2*level, " ", level, TIDNAME(node), node->tid, node->s);
    else
        log_debug("%*.s%d: %s %d",
                  2*level, " ", level, TIDNAME(node), node->tid);

    if (node->subnodes) {
        struct node_s *subnode = NULL;
        while((subnode=(struct node_s*)utarray_next(node->subnodes,
                                                    subnode))) {
            sealark_debug_print_ast_outline(subnode, level+1);
        }
     }
}

EXPORT void sealark_debug_print_node_starlark(struct node_s *node,
                                              bool crush)
{
    UT_string *buf;
    char *str;
    utstring_new(buf);
    sealark_node_to_starlark(node, buf);
    if (crush)
        str = sealark_crush_string(buf);
    else
        str = utstring_body(buf);
    log_debug("%s", str);
    if (crush) free(str);
    utstring_free(buf);
}

EXPORT void sealark_dump_node(struct node_s *node)
{
    log_debug("dump_node: %p", node);
    log_debug("%s[%d] %c (%d:%d)",
              token_name[node->tid][0],
              node->tid,
              /* node->tid == TK_STRING? node->q: ' ', */
              (node->qtype == SQUOTE)? '\''
              : (node->qtype == DQUOTE)? '"'
              : ' ',
              node->line, node->col);
    switch (node->tid) {
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

EXPORT void dump_nodes(UT_array *nodes)
{
    log_debug("dump_nodes: %p, ct: %d", nodes, utarray_len(nodes));

    struct node_s *node=NULL;
    char *q;
    while( (node=(struct node_s*)utarray_next(nodes, node))) {
        /* log_debug("qtype: %#x", node->qtype); */
        /*           node->tid, */
        /*           token_name[node->tid][0]); */
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

        log_debug("%s[%d] %s %s (%d:%d)",
                  token_name[node->tid][0],
                  node->tid,

                  ((node->qtype & BINARY_STR) &&
                    (node->qtype & RAW_STR))? "br"
                  : (node->qtype & BINARY_STR)? "b"
                  : (node->qtype & RAW_STR)? "r"
                  : "",

                  q,
                  node->line, node->col);

        /* if (node->s) log_debug("\tstarttok: :]%s[:", node->s); */
        switch (node->tid) {
        case TK_COMMENT: log_debug("\tstarttok: %s", node->s); break;
        case TK_ID: log_debug("\tstarttok: :]%s[:", node->s); break;
        case TK_INT: log_debug("\tstarttok: :]%s[:", node->s); break;
        case TK_STRING: log_debug("\tstarttok: :]%s[:", node->s); break;
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
                log_debug("  /subnodes %s[%d]",
                          token_name[node->tid][0], node->tid);
            }
        }
    }
    log_debug("/dump_nodes");
}
