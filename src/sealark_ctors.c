#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "sealark_ctors.h"

struct node_s *sealark_make_binding_bool(char *key, bool val)
{
}

EXPORT struct node_s *sealark_make_binding_int(const char *key, int val)
{
    struct node_s *binding = sealark_new_node(TK_Binding, with_subnodes);
    struct node_s *keynode = sealark_new_node(TK_ID, without_subnodes);
    int len = strlen(key);
    keynode->s = calloc(len, sizeof(char));
    strncpy(keynode->s, key, len+1);
    utarray_push_back(binding->subnodes, keynode);

    struct node_s *eq = sealark_new_node(TK_EQ, without_subnodes);
    utarray_push_back(binding->subnodes, eq);

    struct node_s *valnd = sealark_new_node(TK_INT, without_subnodes);
    char buf[32];
    snprintf(buf, 32, "%d", val);
    valnd->s = calloc(strlen(buf)+1, sizeof(char));
    snprintf(valnd->s, strlen(buf)+1, buf);

    utarray_push_back(binding->subnodes, valnd);

    return binding;
}

struct node_s *sealark_make_binding_int_list(char *key, UT_array *vals)
{
}

struct node_s *sealark_make_binding_string(char *key, char *val)
{
}

struct node_s *sealark_make_binding_string_list(char *key, UT_array *vals)
{
}

struct node_s *sealark_make_binding_sym(char *key, char *val)
{
}

