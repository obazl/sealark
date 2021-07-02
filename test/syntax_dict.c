#include "log.h"
#include "utarray.h"

#include "unity.h"
#include "syntax_dict.h"

UT_string *buf;
const char *test_str;
struct node_s *root;

LOCAL const char *dict_comp[] = {
    "{x: len(x) for x in words}\n",
    "{x: x*2 for x in [1, 2]}\n",
    "{x: x*2 for x in f(a)}\n",
    "{x: x*2 for x in f(a[3:15:2])}\n",
    NULL
};

LOCAL const char *dict_expr[] = {
    "{a: b}\n",
    NULL
};

void setUp(void) {
    utstring_new(buf);
}

void tearDown(void) {
    utstring_free(buf);
}

void test_dict_comp(void) {
    int ct;
    for (ct=0; dict_comp[ct] != NULL; ct++); ct--;
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, dict_comp[i]); */
        test_str = dict_comp[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_dict_expr(void) {
    int ct;
    for (ct=0; dict_expr[ct] != NULL; ct++); ct--;
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, dict_expr[i]); */
        test_str = dict_expr[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

 main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_dict_comp);
    /* RUN_TEST(test_dict_expr); */
    return UNITY_END();
}
