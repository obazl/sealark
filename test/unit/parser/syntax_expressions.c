#include "log.h"
#include "utarray.h"
#include "unity.h"
#include "sealark.h"

#include "syntax_expressions.h"

UT_string *buf;
LOCAL char *test_str;
struct node_s *root;

LOCAL char *if_expr[] = {
    "\"yes\" if enabled else \"no\"\n",
    "a if enabled else b\n",
    "a if b else (c if d else e)\n",
    "(a if b else c) if d else e\n",
    "lambda: (a if b else c)\n",
    "(lambda: a) if b else c\n",
    "a if b else lambda: (c if d else e)\n",
    "a if b else (lambda: c if d else e)\n",
    "(a if b else lambda: c) if d else e\n",
    NULL
};

LOCAL char *paren_expr[] = {
    "a * (b + c)\n",
    "1 + 2 * 3 + 4\n",
    "(1 + 2) * (3 + 4)\n",
    NULL
};

LOCAL char *dot_expr[] = {
    "a.b\n",
    "a.b.c\n",
    NULL
};

LOCAL char *lambda[] = {
    "lambda x: x\n",
    "lambda x: 2*x\n",
    "map(lambda x: 2*x, range(3))\n",
    "map(lambda x: (2+x)*(3+x), range(3))\n",
    "lambda: (a if b else c)\n",
    "(lambda: a) if b else c\n",
    NULL
};

void setUp(void) {
    utstring_new(buf);
}

void tearDown(void) {
    utstring_free(buf);
}

void test_if_expr(void) {
    int ct;
    for (ct=0; if_expr[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, if_expr[i]); */
        test_str = if_expr[i];
        root = sealark_parse_string(test_str);
        utstring_renew(buf);
        sealark_node_to_starlark(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        sealark_node_free(root);
    }
}

void test_lambda(void) {
    int ct;
    for (ct=0; lambda[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, lambda[i]); */
        test_str = lambda[i];
        root = sealark_parse_string(test_str);
        utstring_renew(buf);
        sealark_node_to_starlark(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        sealark_node_free(root);
    }
}

void test_paren_expr(void) {
    int ct;
    for (ct=0; paren_expr[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, paren_expr[i]); */
        test_str = paren_expr[i];
        root = sealark_parse_string(test_str);
        utstring_renew(buf);
        sealark_node_to_starlark(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        sealark_node_free(root);
    }
}

void test_dot_expr(void) {
    int ct;
    for (ct=0; dot_expr[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, dot_expr[i]); */
        test_str = dot_expr[i];
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
    RUN_TEST(test_if_expr);
    RUN_TEST(test_lambda);
    RUN_TEST(test_paren_expr);
    RUN_TEST(test_dot_expr);
    return UNITY_END();
}
