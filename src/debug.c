#include "log.h"
#include "utarray.h"
#include "debug.h"

EXPORT void dump_node(struct node_s *node)
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

EXPORT void dump_nodes(UT_array *nodes)
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
