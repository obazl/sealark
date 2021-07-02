#include "log.h"
#include "utarray.h"

#include "unity.h"
#include "syntax_statements.h"

UT_string *buf;
const char *test_str;
struct node_s *root;

LOCAL const char *def_stmts[] = {
    "def f(): pass\n",
    "def f(a, b, c): pass\n",
    "def f(a, b, c=1): pass\n",
    "def f(a, b, c=1, *args): pass\n",
    "def f(a, b, c=1, *args, **kwargs): pass\n",
    "def f(**kwargs): pass\n",

    "def list_to_dict(items):\n"
    "    # Convert list of tuples to dict\n"
    "    m = {}\n"
    "    for k, m[k] in items:\n"
    "        pass\n"
    "    return m\n",

    "def list_to_dict(items):\n"
    "    # Convert list of tuples to dict\n"
    "    m = {}\n"
    "    for k, m[k] in items: pass; return m\n",

    NULL
};

LOCAL const char *if_stmts[] = {
    "if x%2 == 1: continue\n",

    "if x%2 == 1:\n"
    "    continue\n",

    "if x%2 == 1: # skip evens\n"
    "    continue\n",

    "if x > 7: break\n",

    "if x > 7:\n"
    "    break\n",

    "if x > 7:  # stop at 8\n"
    "    break\n",

    NULL
};

LOCAL const char *for_stmts[] = {
    "for a in [1, 2, 3]: print(a)\n",
    "for a, i in [[\"a\", 1], [\"b\", 2], [\"c\", 3]]: print(a, i)\n",
    NULL
};

/* SimpleStmt = SmallStmt {';' SmallStmt} [';'] '\n' . */
/* SmallStmt = ReturnStmt */
/*           | BreakStmt | ContinueStmt | PassStmt */
/*           | AssignStmt */
/*           | ExprStmt */
/*           | LoadStmt */

LOCAL const char *simple[] = {
    "return\n",
    "break\n",
    "continue\n",
    "pass\n",

    NULL
};

LOCAL const char *assignments[] = {
    "k = 1\n",
    "a[i] = v\n",
    "m.f = \"\"\n",
    "a, b = 2, 3\n",
    "(x, y) = f()\n",
    "[zero, one, two] = range(3)\n",
    "[] = ()\n",
    "[(a, b), (c, d)] = (\"ab\", \"cd\")\n",
    NULL
};

LOCAL const char *multi_assignments[] = {
    "a,b = c,d\n",
    "a,b = (c,d)\n",
    "a,b = (c,d,)\n",
    "(a, b), (c, d) = (\"ab\", \"cd\")\n",
    "(a, b), (c, d) = ('ab', 'cd'), (1, 2)\n",
    /* "a,b = c,\n",               /\* illegal *\/ */
    NULL
};

LOCAL const char *augmented_assignments[] = {
    "x += 1\n",
    "x -= 1\n",
    "x *= 1\n",
    "x /= 1\n",
    "x //= 1\n",
    "x %= 1\n",
    "x &= 1\n",
    "x |= 1\n",
    "x ^= 1\n",
    "x <<= 1\n",
    "x >>= 1\n",

    "x -= 1\n",
    "x = x - 1\n",

    "x.filename += \".sky\"\n",
    "x.filename = x.filename + \".sky\"\n",
    "a[index()] *= 2\n",

    "i = index()\n",
    "a[i] = b[j] * 2\n",

    NULL
};

LOCAL const char *load_stmts[] = {
    "load(\"module.sky\", \"x\", \"y\", \"z\")\n",
    /* # assigns x, y, and z */
    "load(\"module.sky\", \"x\", y2=\"y\", \"z\")\n",
    /* # assigns x, y2, and z */
    NULL
};

void setUp(void) {
    utstring_new(buf);
}

void tearDown(void) {
    utstring_free(buf);
}

void test_simple(void) {
    int ct;
    for (ct=0; simple[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, simple[i]); */
        test_str = simple[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_def_stmts(void) {
    int ct;
    for (ct=0; def_stmts[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, def_stmts[i]); */
        test_str = def_stmts[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_if_stmts(void) {
    int ct;
    for (ct=0; if_stmts[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, if_stmts[i]); */
        test_str = if_stmts[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_for_stmts(void) {
    int ct;
    for (ct=0; for_stmts[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, for_stmts[i]); */
        test_str = for_stmts[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_assignments(void) {
    int ct;
    for (ct=0; assignments[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, assignments[i]); */
        test_str = assignments[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_multi_assignments(void) {
    int ct;
    for (ct=0; multi_assignments[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, multi_assignments[i]); */
        test_str = multi_assignments[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_augmented_assignments(void) {
    int ct;
    for (ct=0; augmented_assignments[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, augmented_assignments[i]); */
        test_str = augmented_assignments[i];
        root = obazl_starlark_parse_string(test_str);
        utstring_renew(buf);
        root2string(root, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        node_dtor(root);
    }
}

void test_load_stmts(void) {
    int ct;
    for (ct=0; load_stmts[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: :]%s[:\n", i, load_stmts[i]); */
        test_str = load_stmts[i];
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
    RUN_TEST(test_simple);
    RUN_TEST(test_def_stmts);
    RUN_TEST(test_if_stmts);
    RUN_TEST(test_for_stmts);
    RUN_TEST(test_assignments);
    RUN_TEST(test_multi_assignments);
    RUN_TEST(test_augmented_assignments);
    RUN_TEST(test_load_stmts);
    return UNITY_END();
}
