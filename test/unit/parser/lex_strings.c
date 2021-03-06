#include "utarray.h"
#include "unity.h"
#include "sealark.h"

#include "lex_strings.h"

UT_string *buf;
LOCAL char *test_str;
UT_array *result;

LOCAL char *quotes_singlets[] = {
    "\"I am a plain double-quoted string\"\n",
    "'I am a plain single-quoted string'\n",
    "r\"I am a raw double-quoted string\"\n",
    "r'I am a raw single-quoted string'\n",
    "b\"I am a binary double-quoted string\"\n",
    "b'I am a binary single-quoted string'\n",
    "br\"I am a binary raw double-quoted string\"\n",
    "br'I am a binary raw double-quoted string'\n"
};

LOCAL char *quotes_triplets[] = {
    "\"\"\"I am a plain triple double-quoted string\"\"\"\n",
    "'''I am a plain triple single-quoted string'''\n",
    "r\"\"\"I am a raw double-quoted string\"\"\"\n",
    "r'''I am a raw single-quoted string'''\n",
    "b\"\"\"I am a binary double-quoted string\"\"\"\n",
    "b'''I am a binary single-quoted string'''\n",
    "br\"\"\"I am a binary raw double-quoted string\"\"\"\n",
    "br'''I am a binary raw double-quoted string'''\n"
};

LOCAL char *multicomments[] = {
    "'hello' #cmt1\n",
    "\"hello\" #cmt1\n",
    "'hello'\n    #cmt1\n",
    "\"hello\"\n    #cmt1\n",
    "'hello' #cmt1\n #cmt2\n",
    "\"hello\" #cmt1\n #cmt2\n",
    "'hello'    #cmt1\n    #cmt2\n",
    "\"hello\"    #cmt1\n    #cmt2\n",

    "'hello' #cmt1\n'bye' #cmt2\n",
    "\"hello\" #cmt1\n'bye' #cmt2\n"
    "'hello' #cmt1\n #cmt2\n'bye' #cmt3\n",
    "\"hello\" #cmt1\n #cmt2\n'bye' #cmt3\n",
    NULL
};

LOCAL char *mixed[] = {
    "'hello'\n",
    "\"hello\"\n",

    "'hello' 'foo'\n",
    "\"hello\" \"foo\"\n",
    "'hello' \"foo\"\n",        /* 5 */
    "\"hello\" 'foo'\n",

    "'hello'\n'foo'\n",
    "\"hello\"\n\"foo\"\n",
    "'hello'\n\"foo\"\n",
    "\"hello\"\n'foo'\n",        /* 10 */

    "'hello'\n    'foo'\n",
    "\"hello\"\n    \"foo\"\n",
    "'hello'\n    \"foo\"\n",
    "\"hello\"\n    'foo'\n",

    "\"hello\" #cmt1\n",              /* 15 */
    "'hello' #cmt1\n",

    "'hello' 'foo'\n",
    "\"hello\" \"foo\"\n",
    "'hello' \"foo\"\n",
    "\"hello\" 'foo'\n",        /* 20 */

    "'hello'\n'foo'\n",
    "\"hello\"\n\"foo\"\n",
    "'hello'\n\"foo\"\n",
    "\"hello\"\n'foo'\n",

    "'hello'\n    'foo'\n",     /* 25 */
    "\"hello\"\n    \"foo\"\n",
    "'hello'\n    \"foo\"\n",
    "\"hello\"\n    'foo'\n",
    NULL
};

LOCAL char *escaped_nls[] = {
    "'hel\\n\
lo'\n",
    "\"hel\\n\
lo\"\n",

    "'hel\
lo'\n",
    "\"hel\
lo\"\n",

    "'hel\
lo' #cmt1\n",
    "\"hel\
lo\" #cmt1\n",

    /* %%%%%%%%%%%%%%%% */
    "'hel\
lo' 'foo' #cmt1\n",
    "\"hel\
lo\" \"foo\" #cmt1\n",
    "'hel\
lo' \"foo\" #cmt1\n",
    "\"hel\
lo\" 'foo' #cmt1\n",

    "'hel\
lo' 'foo\
bar' #cmt1\n",
    "\"hel\
lo\" \"foo\
bar\" #cmt1\n",
    "'hel\
lo' \"foo\
bar\" #cmt1\n",
    "\"hel\
lo\" 'foo\
bar' #cmt1\n",
    /* %%%%%%%%%%%%%%%% */
    "'hel\
lo'    #cmt1\n'foo' #cmt2\n",
    "\"hel\
lo\"    #cmt1\n\"foo\" #cmt2\n",
    "'hel\
lo'    #cmt1\n\"foo\" #cmt2\n",
    "\"hel\
lo\"    #cmt1\n'foo' #cmt2\n",

    /* %%%%%%%%%%%%%%%% */
    "'hel\
lo'\n    #cmt1\n'foo' #cmt2\n",
    "\"hel\
lo\"\n    #cmt1\n\"foo\" #cmt2\n",
    "'hel\
lo'\n    #cmt1\n\"foo\" #cmt2\n",
    "\"hel\
lo\"\n    #cmt1\n'foo' #cmt2\n",

    /* %%%%%%%%%%%%%%%% */
    "'hel\
lo'\n    #cmt1\n'foo\
bar' #cmt2\n",
    "\"hel\
lo\"\n    #cmt1\n\"foo\
bar\" #cmt2\n",
    "'hel\
lo'\n    #cmt1\n\"foo\
bar\" #cmt2\n",
    "\"hel\
lo\"\n    #cmt1\n'foo\
bar' #cmt2\n",

    NULL
};

void setUp(void) {
    utstring_new(buf);
}

void tearDown(void) {
    utstring_free(buf);
}

void test_single_line_single_quote(void) {
    test_str = "'hello'\n";
    result = sealark_lex_string(test_str);
    utstring_renew(buf);
    sealark_nodelist2string(result, buf);
    /* printf("t :]%s[:\n", test_str); */
    /* printf("buf :]%s[:\n", utstring_body(buf)); */
    int eq = strcmp(test_str, utstring_body(buf));
    /* printf("compare: %d\n", eq); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
    utarray_free(result);

    /* test_str = "'hello' #cmt1\n"; */
    /* result = sealark_lex_string(test_str); */
    /* utstring_renew(buf); */
    /* sealark_nodelist2string(result, buf); */
    /* /\* printf(":]%s[:\n", utstring_body(buf)); *\/ */
    /* TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf)); */
    /* utarray_free(result); */

    /* test_str = "'hello' #cmt1\n"; */
    /* result = sealark_lex_string(test_str); */
    /* utstring_renew(buf); */
    /* sealark_nodelist2string(result, buf); */
    /* /\* printf(":]%s[:\n", utstring_body(buf)); *\/ */
    /* TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf)); */
    /* utarray_free(result); */
}

void test_single_line_single_quote_cmt1_nl_cmt2a(void) {
    test_str = "'hello' #cmt1\n#cmt2a\n";
    result = sealark_lex_string(test_str);
    utstring_renew(buf);
    sealark_nodelist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
    utarray_free(result);
}
void test_single_line_single_quote_cmt1_nl_cmt2b(void) {
    test_str = "'hello' #cmt1\n    #cmt2b\n";
    result = sealark_lex_string(test_str);
    sealark_nodelist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
    utarray_free(result);
}

void test_single_line_single_quote_cmt1_nl2_cmt2a(void) {
    test_str = "'hello' #cmt1\n\n#cmt2a\n";
    result = sealark_lex_string(test_str);
    sealark_nodelist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
    utarray_free(result);
}
void test_single_line_single_quote_cmt1_nl2_cmt2b(void) {
    test_str = "'hello' #cmt1\n\n    #cmt2b\n";
    result = sealark_lex_string(test_str);
    sealark_nodelist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
    utarray_free(result);
}

void test_single_line_single_quote_nl_cmt1(void) {
    test_str = "'hel\\\nlo'\n#cmt1\n";
    result = sealark_lex_string(test_str);
    sealark_nodelist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
    utarray_free(result);
}
void test_single_line_single_quote_nl_cmt1_nl_cmt2a(void) {
    test_str = "'hello'\n#cmt1\n#cmt2a\n";
    result = sealark_lex_string(test_str);
    sealark_nodelist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
    utarray_free(result);
}
void test_single_line_single_quote_nl_cmt1_nl_cmt2b(void) {
    test_str = "'hello'\n#cmt1\n    #cmt2b\n";
    result = sealark_lex_string(test_str);
    /* dump_nodes(result); */
    sealark_nodelist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
    utarray_free(result);
}

void test_mix1(void) {
    test_str =
"'hello' #cmt1\n\
'bye'  #cmt2\n\
'foo' #cmt3\n";
    result = sealark_lex_string(test_str);
    /* dump_nodes(result); */
    utstring_renew(buf);
    sealark_nodelist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
    utarray_free(result);

    test_str =
"'hello' 'bye' #cmt1\n\
'foo' 'bar' #cmt2\n";
    result = sealark_lex_string(test_str);
    /* dump_nodes(result); */
    utstring_renew(buf);
    sealark_nodelist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
    utarray_free(result);
}

void test_quotes_singlets(void) {
    int ct;
    for (ct=0; quotes_singlets[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: %s\n", i, quotes_singlets[i]); */
        test_str = quotes_singlets[i];
        result = sealark_lex_string(test_str);
        /* printf("\n"); */
        /* dump_nodes(result); */
        utstring_renew(buf);
        sealark_nodelist2string(result, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        utarray_free(result);
    }
}

void test_quotes_triplets(void) {
    int ct;
    for (ct=0; quotes_triplets[ct] != NULL; ct++);
    for (int i=0; i < ct; i++) {
        /* printf("case %d: %s\n", i, quotes_triplets[i]); */
        test_str = quotes_triplets[i];
        result = sealark_lex_string(test_str);
        /* printf("\n"); */
        /* dump_nodes(result); */
        utstring_renew(buf);
        sealark_nodelist2string(result, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        utarray_free(result);
    }
}

void test_multicomments(void) {
    int ct;
    for (ct=0; multicomments[ct] != NULL; ct++); ct--;
    for (int i=0; i < ct; i++) {
        /* printf("case %d: %s\n", i, multicomments[i]); */
        test_str = multicomments[i];
        result = sealark_lex_string(test_str);
        /* printf("\n"); */
        /* dump_nodes(result); */
        utstring_renew(buf);
        sealark_nodelist2string(result, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        utarray_free(result);
    }
}

void test_mix_quotes(void) {
    int ct;
    for (ct=0; mixed[ct] != NULL; ct++); ct--;
    for (int i=0; i < ct; i++) {
        /* printf("case %d: %s\n", i, mixed[i]); */
        test_str = mixed[i];
        result = sealark_lex_string(test_str);
        /* printf("\n"); */
        /* dump_nodes(result); */
        utstring_renew(buf);
        sealark_nodelist2string(result, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        utarray_free(result);
    }
}

void test_escaped_nls(void) {
    int ct;
    for (ct=0; escaped_nls[ct] != NULL; ct++); ct--;
    for (int i=0; i < ct; i++) {
        /* printf("case %d\n", i); */
        test_str = escaped_nls[i];
        /* printf("test_str: %s", escaped_nls[i]); */
        result = sealark_lex_string(test_str);
        /* printf("\n"); */
        /* dump_nodes(result); */
        utstring_renew(buf);
        sealark_nodelist2string(result, buf);
        /* printf(":]%s[:\n", utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
        utarray_free(result);
    }
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_single_line_single_quote);
    RUN_TEST(test_single_line_single_quote_cmt1_nl_cmt2a);
    RUN_TEST(test_single_line_single_quote_cmt1_nl_cmt2b);
    RUN_TEST(test_single_line_single_quote_cmt1_nl2_cmt2a);
    RUN_TEST(test_single_line_single_quote_cmt1_nl2_cmt2b);

    RUN_TEST(test_single_line_single_quote_nl_cmt1);
    RUN_TEST(test_single_line_single_quote_nl_cmt1_nl_cmt2a);
    RUN_TEST(test_single_line_single_quote_nl_cmt1_nl_cmt2b);

    RUN_TEST(test_quotes_singlets);
    RUN_TEST(test_quotes_triplets);

    RUN_TEST(test_multicomments);
    RUN_TEST(test_mix1);
    RUN_TEST(test_mix_quotes);
    RUN_TEST(test_escaped_nls);
    return UNITY_END();
}
