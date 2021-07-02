#include "log.h"
#include "utarray.h"

#include "unity.h"
#include "syntax_call_expr.h"

UT_string *buf;
const char *test_str;
struct node_s *root;

LOCAL const char *call_a[] = {
    "foo()\n",
    "foo(a)\n",
    "foo(a, b)\n",
    "foo(3)\n",
    "foo(3.14)\n",
    "foo(1.2e3)\n",
    "foo(1,2,3)\n",
    "foo(1.1, 2.2, 3.3)\n",
    "foo(a,1,b,2)\n",
    "foo( a , 1  ,  b    ,    2    )\n",
    "foo(a, bar(1), baz(1.2))\n",
    "foo(bar[:])\n",
    "foo(bar[f:g()])\n",
    /* optional trailing comma */
    "foo(a,)\n",
    "foo(3,)\n",
    "foo(3.14,)\n",
    "foo(1.2e3,)\n",
    "foo(1,2,3,)\n",
    "foo(1.1, 2.2, 3.3,)\n",
    "foo(a,1,b,2,)\n",
    "foo( a , 1  ,  b    ,    2  ,  )\n",
    "foo(a, bar(1), baz(1.2) ,)\n",
    "foo(bar[:],)\n",
    "foo(bar[f:g()],)\n",
    NULL
};

LOCAL const char *call_rules[] = {
    "foo(a='b')\n",

    "ocaml_module(\n"
    "    name   = \"Uri\",\n"
    "    struct = \"uri.ml\",\n"
    "    sig    = \"Uri.cmi\",\n"
    "    deps   = [\n"
    "        \"@opam//lib/angstrom\",\n"
    "        \"@opam//lib/stringext\",\n"
    "        \"@opam//lib/yojson\",\n"
    "    ]\n"
    ")\n",
    NULL
};

void setUp(void) {
    utstring_new(buf);
}

void tearDown(void) {
    utstring_free(buf);
}

void test_call_a(void) {
    int ct;
    for (ct=0; call_a[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, call_a[i]); */
        test_str = call_a[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_call_rules(void) {
    int ct;
    for (ct=0; call_rules[ct] != NULL; ct++); ct--;
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, call_rules[i]); */
        test_str = call_rules[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_call_a);
    /* RUN_TEST(test_call_rules); */
    return UNITY_END();
}
