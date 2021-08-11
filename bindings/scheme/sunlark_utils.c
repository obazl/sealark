#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "log.h"
/* #include "utarray.h" */
#include "utstring.h"

#include "s7.h"

#include "sunlark_utils.h"

/* **************************************************************** */
EXPORT const char* sunlark_node_to_string(s7_pointer nd)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("sunlark_to_string");
#endif

    if (s7_is_c_object(nd)) {
        struct node_s *node = s7_c_object_value(nd);
        if (sealark_is_printable(node)) {
            return sealark_node_printable_string(node);
        } else {
            return NULL;
        }
    }
}

EXPORT int sealark_normalize_index(struct node_s *node, int index)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_VECTORS)
    log_debug("sealark_normalize_index: %d", index);
#endif

    struct node_s *nodes;
    int subnode_ct;
    int item_ct;

    if(node->tid == TK_List_Expr) {
        nodes = utarray_eltptr(node->subnodes, 1);
        subnode_ct = utarray_len(nodes->subnodes);
        item_ct = (subnode_ct + 1) / 2;
    } else {
        if(node->tid == TK_Arg_List) {
            subnode_ct = utarray_len(node->subnodes);
            item_ct = (subnode_ct + 1) / 2;
        } else {
            if(node->tid == TK_Package) {
                item_ct = sealark_pkg_targets_count(node);
            } else {
                log_error("normalize idx unhandled node type: %d %s",
                          node->tid, TIDNAME(node));
            }
        }
    }
    /* reverse indexing */
    if (index < 0) {
        if (abs(index) > item_ct) {
            log_error("abs(%d) > item_ct", index, item_ct);
            errno = EINDEX_OUT_OF_BOUNDS;
            /* return NULL; */
        } else {
            index = item_ct + index;
            /* log_debug("recurring..."); */
            /* return sealark_vector_item_for_int(node, index); */
        }
    }

    if (index > item_ct-1) {
        log_error("index > target count");
        errno = EINDEX_OUT_OF_BOUNDS;
        /* return NULL; */
    }
    return index;
}

int sunlark_kwindex_to_int(s7_scheme *s7, s7_pointer arg)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_kwindex_to_int: %s", s7_object_to_c_string(s7, arg));
#endif

    int idx;
    if (s7_is_integer(arg))
        return s7_integer(arg);

    errno = 0;
    /* sunlark_is_nbr_kw will set errno */
    return sunlark_is_nbr_kw(s7, arg);
}

int sunlark_is_nbr_kw(s7_scheme *s7, s7_pointer arg)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_is_nbr_kw: %s", s7_object_to_c_string(s7, arg));
#endif

    if (s7_is_keyword(arg)) {
        s7_pointer sym = s7_keyword_to_symbol(s7, arg);
        const char *kw = s7_symbol_name(sym);
        int len = strlen(kw); // very unlikely to be more than 2
        int i = (kw[0] == '-')? 1 : 0;
        for ( ; i < len; i++) {
            if ( isdigit((int)kw[i]) == 0 ) {
                errno = EKW_NOT_NBR;
                return -1;
            }
        }
        errno = 0;
        return atoi(kw);
    } else {
        errno = ENOT_A_KW;
        return -1;
    }
}

s7_pointer node_to_scheme(s7_scheme *s7, struct node_s *node)
{
#ifdef DEBUG_TRACE
    log_debug("node_to_scheme tid %d %s", node->tid, TIDNAME(node));
#endif
    s7_pointer val;
    switch(node->tid) {
    case TK_STRING: {
        UT_string *buf;
        utstring_new(buf);
        char *br = SEALARK_STRTYPE(node->qtype);
        char *q = sealark_quote_type(node);
        utstring_printf(buf,
                        "%s%s%s%s",
                        br,
                        q,
                        node->s,
                        q);
        val =  s7_make_string(s7, utstring_body(buf));
        utstring_free(buf);
    }
        break;
    case TK_ID:
        if (strncmp("True", node->s, 4) == 0) {
            val = s7_t(s7);
        } else {
            if (strncmp("False", node->s, 4) == 0) {
                val = s7_make_symbol(s7, "#f");
            } else {
                val =  s7_make_symbol(s7, node->s);
            }
        }
        break;
    case TK_INT:
        val =  s7_make_integer(s7, atoi(node->s));
        break;
    case TK_Binding: {
        struct node_s *v = sealark_value_for_binding(node);
        return sunlark_new_node(s7, v);
    }
        break;
    case TK_List_Expr:
        log_error("dollar list expr");
    default:
        log_error("FIXME:  deal with :$");
    }
    return val;
    /* } */
}
