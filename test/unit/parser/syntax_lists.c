#include "log.h"
#include "utarray.h"
#include "unity.h"
#include "sealark.h"

#include "syntax_lists.h"

UT_string *buf;
LOCAL char *test_str;
struct node_s *root;

LOCAL char *list_expr[] = {
    "[a]\n",
    "[a, b]\n",
    "[1, 2]\n",
    "[a, 2]\n",

    "[a, f()]\n",
    NULL
};

LOCAL char *list_comp[] = {
    "[x*x for x in range(5)]\n",
    "[x*x for x in range(5) if x%2 == 0]\n",
    "[x*y+z for (x, y), z in [((2, 3), 5), ((\"o\", 2), \"!\")]]\n",
    "[x for x in range(5)\n"
    "    if x%2 == 0]\n",

    "[x for x in range(5)\n"
    "     for y in range(6)\n"
    "       for z in range(7)]\n",

    "[x for x in range(5)\n"
    "    if x%2 == 0\n"
    "    for y in range(6)]\n",

    "[x for x in range(5)\n"
    "    if x%2 == 0\n"
    "    for y in range(5)\n"
    "    if y > x]\n",

    NULL
};

void setUp(void) {
    utstring_new(buf);
}

void tearDown(void) {
    utstring_free(buf);
}

void test_list_comp(void) {
    int ct;
    for (ct=0; list_comp[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, list_comp[i]); */
        test_str = list_comp[i];
        root = sealark_parse_string(test_str);
        utstring_renew(buf);
        sealark_node_to_starlark(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        sealark_node_free(root);
    }
}

void test_list_expr(void) {
    int ct;
    for (ct=0; list_expr[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, list_expr[i]); */
        test_str = list_expr[i];
        root = sealark_parse_string(test_str);
        utstring_renew(buf);
        sealark_node_to_starlark(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        sealark_node_free(root);
    }
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_list_expr);
    RUN_TEST(test_list_comp);
    return UNITY_END();
}
