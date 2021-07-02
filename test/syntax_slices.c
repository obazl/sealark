#include "log.h"
#include "utarray.h"

#include "unity.h"
#include "syntax_slices.h"

UT_string *buf;
const char *test_str;
struct node_s *root;

LOCAL const char *slice_a[] = {
    "foo[]\n",
    "foo[:]\n",
    "foo[::]\n",
    "foo[a]\n",
    "foo[a:]\n",
    "foo[a::]\n",
    "foo[a:b]\n",
    "foo[a:b:]\n",
    "foo[a::c]\n",
    "foo[a:b:c]\n",
    "foo[:b]\n",
    "foo[:b:]\n",
    "foo[:b:c]\n",
    "foo[::c]\n",
    NULL
};

LOCAL const char *slice_embed_a[] = {
    "foo[bar[]]\n",
    "foo[bar[:]]\n",
    "foo[bar[::]]\n",

    "foo[bar[]:]\n",
    "foo[bar[:]::]\n",

    "foo[bar[]:b]\n",
    "foo[bar[:]:b]\n",
    "foo[bar[::]:b]\n",

    "foo[bar[]:]\n",
    "foo[bar[]::]\n",
    "foo[bar[]:b]\n",
    "foo[bar[]:b:]\n",
    "foo[bar[]::c]\n",
    "foo[bar[]:b:c]\n",
    "foo[:b]\n",
    "foo[:b:]\n",
    "foo[:b:c]\n",
    "foo[::c]\n",

    "foo[bar(baz[])]\n",
    "foo[bar(baz[buz()])]\n",
    "foo[bar(baz[a])]\n",

    "foo[bar(baz[] #cmt\n)]\n",
    "foo[bar(baz[buz(a,1,2.0)])]\n",
    "foo[bar(\n    baz[\n    a\n]\n)\n]\n",
    NULL
};

LOCAL const char *slice_a_cmt[] = {
    "foo[] #cmt1\n",
    "foo[] #cmt1\n\n\n\n",
    "foo[]\n #cmt1\n",
    "foo[]\n\n\n #cmt1\n",
    "foo[] #cmt1\n#cmt2\n",
    "foo[] #cmt1\n\n#cmt2\n",

    "foo[\n #cmt1\n]\n",
    "foo[\n #cmt1\n #cmt2\n]\n",
    "foo[\n\n #cmt1\n #cmt2\n]\n",
    "foo[\n\n #cmt1\n\n #cmt2\n]\n",
    "foo[\n\n #cmt1\n\n #cmt2\n\n]\n",
    "foo[\n #cmt1\n]\n #cmt2\n",

    "foo[:] #cmt1\n",
    "foo[ #cmt1\n:]\n",
    "foo[ #cmt1\n:\n#cmt2\n]\n",
    "foo[ #cmt1\n:\n#cmt2\n] #cmt3\n",

    "foo[::] #cmt1\n",
    "foo[ #cmt1\n::]\n",
    "foo[ #cmt1\n:: #cmt2\n]\n",
    "foo[ #cmt1\n: #cmt2\n:]\n",
    "foo[ #cmt1\n:\n #cmt2\n: #cmt3\n]\n",

    "foo[a] #cmt1\n",
    "foo[a:] #cmt1\n",

    "foo[a::] #cmt1\n",
    "foo[a #cmt1\n    ::] #cmt2\n",
    "foo[a:: #cmt1\n]\n",
    "foo[\n #cmt1\na::\n #cmt2\n] #cmt3\n",

    "foo[a:b] #cmt1\n",
    "foo[a:b:] #cmt1\n",
    "foo[a::c] #cmt1\n",

    "foo[a:b:c] #cmt1\n",
    "foo[a #cmt1\n:b #cmt2\:c #cmt3\n] #cmt4\n",
    "foo[ #cmt1\a:b:c #cmt2\n] #cmt3\n",
    "foo[a: #cmt1\nb: #cmt2\nc] #cmt1\n",

    "foo[:b] #cmt1\n",
    "foo[:b:] #cmt1\n",
    "foo[:b:c] #cmt1\n",
    "foo[::c] #cmt1\n",
    NULL
};

LOCAL const char *slice_int[] = {
    "foo[1]\n",
    "foo[1:]\n",
    "foo[1::]\n",
    "foo[1:2]\n",
    "foo[1:2:]\n",
    "foo[1::3]\n",
    "foo[1:2:3]\n",
    "foo[:2]\n",
    "foo[:2:]\n",
    "foo[:2:3]\n",
    "foo[::3]\n",
    NULL
};

LOCAL const char *slice_a_int[] = {
    "foo[1:a]\n",
    "foo[1:a:]\n",
    "foo[a:1]\n",
    "foo[a:1:]\n",
    "foo[a::3]\n",
    "foo[1::c]\n",
    "foo[a:2:3]\n",
    "foo[1:b:3]\n",
    "foo[1:2:c]\n",
    "foo[:b:3]\n",
    "foo[:2:c]\n",
    NULL
};

LOCAL const char *slice_float[] = {
    "foo[1.]\n",
    "foo[1.:]\n",
    "foo[1.::]\n",
    "foo[1.:2.]\n",
    "foo[1.:2.:]\n",
    "foo[1.::3.]\n",
    "foo[1.:2.:3.]\n",
    "foo[:2.]\n",
    "foo[:2.:]\n",
    "foo[:2.:3.]\n",
    "foo[::3.]\n",

    "foo[1.0]\n",
    "foo[1.0:]\n",
    "foo[1.0::]\n",
    "foo[1.0:2.0]\n",
    "foo[1.0:2.0:]\n",
    "foo[1.0::3.0]\n",
    "foo[1.0:2.0:3.0]\n",
    "foo[:2.0]\n",
    "foo[:2.0:]\n",
    "foo[:2.0:3.0]\n",
    "foo[::3.0]\n",

    "foo[1.9e123]\n",
    "foo[1.9e123:]\n",
    "foo[1.9e123::]\n",
    "foo[1.9e123:2.9e123]\n",
    "foo[1.9e123:2.9e123:]\n",
    "foo[1.9e123::3.9e123]\n",
    "foo[1.9e123:2.9e123:3.9e123]\n",
    "foo[:2.9e123]\n",
    "foo[:2.9e123:]\n",
    "foo[:2.9e123:3.9e123]\n",
    "foo[::3.9e123]\n",
    NULL
};

LOCAL const char *slice_a_float[] = {
    "foo[a:2.]\n",
    "foo[a:2.:]\n",
    "foo[a::3.]\n",
    "foo[a:2.:3.]\n",
    "foo[:b:3.]\n",
    "foo[::3.]\n",

    "foo[1.0:b]\n",
    "foo[1.0:b:]\n",
    "foo[1.0::3.0]\n",
    "foo[1.0:b:3.0]\n",
    "foo[:b]\n",
    "foo[:b:]\n",
    "foo[:b:3.0]\n",
    "foo[::3.0]\n",

    "foo[1.9e123]\n",
    "foo[1.9e123:]\n",
    "foo[1.9e123::]\n",
    "foo[1.9e123:2.9e123]\n",
    "foo[1.9e123:2.9e123:]\n",
    "foo[1.9e123::3.9e123]\n",
    "foo[1.9e123:2.9e123:3.9e123]\n",
    "foo[:2.9e123]\n",
    "foo[:2.9e123:]\n",
    "foo[:2.9e123:3.9e123]\n",
    "foo[::3.9e123]\n",
    NULL
};

LOCAL const char *slice_fn[] = {
    "foo[f()]\n",
    "foo[f(a)]\n",
    "foo[f(1)]\n",
    "foo[f(a,1)]\n",
    "foo[f(a,1.0)]\n",

    "foo[f():]\n",
    "foo[f(a):]\n",
    "foo[f(1):]\n",
    "foo[f(a,1):]\n",
    "foo[f(a,1.0):]\n",

    "foo[f()::]\n",
    "foo[f(a)::]\n",
    "foo[f(1)::]\n",
    "foo[f(a,1)::]\n",
    "foo[f(a,1.0)::]\n",

    "foo[f():g()]\n",
    "foo[f(a):g()]\n",
    "foo[f(1):g()]\n",
    "foo[f(a,1):g()]\n",
    "foo[f(a,1.0):g()]\n",

    "foo[f():g():]\n",
    "foo[f(a):g():]\n",
    "foo[f(1):g():]\n",
    "foo[f(a,1):g():]\n",
    "foo[f(a,1.0):g():]\n",

    "foo[f():g():h()]\n",
    "foo[f(a):g():h()]\n",
    "foo[f(1):g():h()]\n",
    "foo[f(a,1):g():h()]\n",
    "foo[f(a,1.0):g():h()]\n",

    "foo[f():g(b):h(c)]\n",
    "foo[f(a):g(b):h(c)]\n",
    "foo[f(1):g(b):h(c)]\n",
    "foo[f(a,1):g(b):h(c)]\n",
    "foo[f(a,1.0):g(b):h(c)]\n",
    NULL
};

void setUp(void) {
    utstring_new(buf);
}

void tearDown(void) {
    utstring_free(buf);
}

void test_slice_a(void) {
    int ct;
    for (ct=0; slice_a[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, slice_a[i]); */
        test_str = slice_a[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_slice_embed_a(void) {
    int ct;
    for (ct=0; slice_embed_a[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        printf("case %d: :]%s[:\n", i, slice_embed_a[i]);
        test_str = slice_embed_a[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        printf(":]%s[:\n", utstring_body(buf));
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_slice_a_cmt(void) {
    int ct;
    for (ct=0; slice_a_cmt[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        printf("case %d: :]%s[:\n", i, slice_a_cmt[i]);
        test_str = slice_a_cmt[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        printf(":]%s[:\n", utstring_body(buf));
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_slice_int(void) {
    int ct;
    for (ct=0; slice_int[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        printf("case %d: :]%s[:\n", i, slice_int[i]);
        test_str = slice_int[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        printf(":]%s[:\n", utstring_body(buf));
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_slice_float(void) {
    int ct;
    for (ct=0; slice_float[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        printf("case %d: :]%s[:\n", i, slice_float[i]);
        test_str = slice_float[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        printf(":]%s[:\n", utstring_body(buf));
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_slice_a_float(void) {
    int ct;
    for (ct=0; slice_a_float[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        printf("case %d: :]%s[:\n", i, slice_a_float[i]);
        test_str = slice_a_float[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        printf(":]%s[:\n", utstring_body(buf));
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_slice_a_int(void) {
    int ct;
    for (ct=0; slice_a_int[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        printf("case %d: :]%s[:\n", i, slice_a_int[i]);
        test_str = slice_a_int[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        printf(":]%s[:\n", utstring_body(buf));
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_slice_fn(void) {
    int ct;
    for (ct=0; slice_fn[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        printf("case %d: :]%s[:\n", i, slice_fn[i]);
        test_str = slice_fn[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        printf(":]%s[:\n", utstring_body(buf));
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_slice_a);
    RUN_TEST(test_slice_embed_a);
    RUN_TEST(test_slice_a_cmt);
    RUN_TEST(test_slice_int);
    RUN_TEST(test_slice_a_int);
    RUN_TEST(test_slice_float);
    RUN_TEST(test_slice_a_float);
    RUN_TEST(test_slice_fn);
    return UNITY_END();
}
