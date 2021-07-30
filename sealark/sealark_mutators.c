#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_mutators.h"

/* **************** */
EXPORT struct node_s *sealark_set_int(struct node_s *node,
                                      const int newint)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sunlark_set_int: %d", newint);
#endif

    assert(node->tid ==  TK_INT);

    /* int d = s7_integer(newint); */
    char buf[128];
    snprintf(buf, 128, "%d", newint);
    int len = strlen(buf);
    log_debug("new val: %d", newint);
    node->tid = TK_INT;
    node->s = calloc(len, sizeof(char));
    strncpy(node->s, buf, len);
    return node;
}

/* **************** */
/* string flags in sealark_nodes.c */
EXPORT struct node_s *sealark_set_string(struct node_s *node,
                                         int qtype,
                                         const char *newstr)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sunlark_set_string: %s", newstr);
#endif

    if (node->tid != TK_STRING) {
        if (node->subnodes) {
            utarray_free(node->subnodes);
            node->subnodes = NULL;
        }
    }
    node->tid = TK_STRING;
    node->qtype = qtype; //(qmark | qqq | qtype);
    log_debug("qtype: %#04x", node->qtype);

    int len = strlen(newstr);
    node->s = calloc(len, sizeof(char));
    strncpy(node->s, newstr, len);
    return node;
}

EXPORT struct node_s *sealark_set_string_c_object(struct node_s *node,
                                                  struct node_s *newstr)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sunlark_set_string_c_object");
#endif

    if (node->tid != TK_STRING) {
        if (node->subnodes) {
            utarray_free(node->subnodes);
            node->subnodes = NULL;
        }
    }
    node->tid = TK_STRING;
    node->qtype = newstr->qtype;
    log_debug("qtype: %#04x", node->qtype);

    int len = strlen(newstr->s);
    node->s = calloc(len, sizeof(char));
    strncpy(node->s, newstr->s, len);
    return node;
}

/* **************** */
EXPORT struct node_s *sealark_set_symbol(struct node_s *node,
                                         const char *newstr)
{
#if defined(DEBUG_TRACE) || defined(DEBUG_MUTATE)
    log_debug("sunlark_set_symbol: %s", newstr);
#endif

    if (node->tid != TK_ID) {
        if (node->subnodes) {
            utarray_free(node->subnodes);
            node->subnodes = NULL;
        }
    }
    node->qtype = 0;
    node->tid = TK_ID;
    int len = strlen(newstr);
    node->s = calloc(len, sizeof(char));
    strncpy(node->s, newstr, len);
    return node;
}

