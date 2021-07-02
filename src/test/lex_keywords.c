#include "utarray.h"
#include "utstring.h"

#include "unity.h"
#include "lex_keywords.h"

UT_string *buf;
UT_string *test_s;
UT_array  *result;

LOCAL const int tk[16] = {
    TK_AND, TK_OR,
    TK_IF, TK_ELSE, TK_ELIF,
    TK_LOAD, TK_FOR,
    TK_NOT, TK_CONTINUE,
    TK_DEF, TK_IN,
    TK_PASS, TK_BREAK,
    TK_LAMBDA, TK_RETURN,
    /* RESERVED */
    TK_AS, TK_IMPORT,
    TK_ASSERT, TK_IS,
    TK_CLASS, TK_NONLOCAL,
    TK_DEL, TK_RAISE,
    TK_EXCEPT, TK_TRY,
    TK_FINALLY, TK_WHILE,
    TK_FROM, TK_WITH,
    TK_GLOBAL, TK_YIELD
};

void setUp(void) {
    utstring_new(test_s);
    utstring_new(buf);
}

void tearDown(void) {
    utstring_free(test_s);
    utstring_free(buf);
}

void test_kw(void) {
    for (int i=0; i < 16; i++) {
        /* printf("punct %s: :]%s[:\n", */
        /*        token_name[tk[i]][0], token_name[tk[i]][1]); */
        utstring_renew(test_s);
        utstring_printf(test_s, "%s\n",
                        token_name[tk[i]][1]);
        /* printf(utstring_body(test_s)); */
        result = obazl_starlark_lex_string(utstring_body(test_s));
        utstring_renew(buf);
        rootlist2string(result, buf);
        /* printf(utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(utstring_body(test_s), utstring_body(buf));
        utarray_free(result);
    }
}

void test_kw_cmt(void) {
    for (int i=0; i < 16; i++) {
        /* printf("tok %s: :]%s[:\n", */
        /*        token_name[tk[i]][0], token_name[tk[i]][1]); */
        utstring_renew(test_s);
        utstring_printf(test_s, "%s%s",
                        token_name[tk[i]][1],
                        " #cmt1\n");
        /* printf(utstring_body(test_s)); */
        result = obazl_starlark_lex_string(utstring_body(test_s));
        utstring_renew(buf);
        rootlist2string(result, buf);
        /* printf(utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(utstring_body(test_s), utstring_body(buf));
        utarray_free(result);
    }
}

void test_kw_nl_cmt(void) {
    for (int i=0; i < 16; i++) {
        /* printf("punct %s: :]%s[:\n", */
        /*        token_name[tk[i]][0], token_name[tk[i]][1]); */
        utstring_renew(test_s);
        utstring_printf(test_s, "%s%s",
                        token_name[tk[i]][1],
                        "\n    #cmt1\n");
        /* printf(utstring_body(test_s)); */
        result = obazl_starlark_lex_string(utstring_body(test_s));
        utstring_renew(buf);
        rootlist2string(result, buf);
        /* printf(utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(utstring_body(test_s), utstring_body(buf));
        utarray_free(result);
    }
}

void test_kw_cmt_nl2_cmt(void) {
    for (int i=0; i < 16; i++) {
        /* printf("punct %s: :]%s[:\n", */
        /*        token_name[tk[i]][0], token_name[tk[i]][1]); */
        utstring_renew(test_s);
        utstring_printf(test_s, "%s%s",
                        token_name[tk[i]][1],
                        "\n\n    #cmt1\n");
        /* printf(utstring_body(test_s)); */
        result = obazl_starlark_lex_string(utstring_body(test_s));
        utstring_renew(buf);
        rootlist2string(result, buf);
        /* printf(utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(utstring_body(test_s), utstring_body(buf));
        utarray_free(result);
    }
}

void test_kw_nl_cmt_nl_cmt(void) {
    for (int i=0; i < 16; i++) {
        /* printf("punct %s: :]%s[:\n", */
        /*        token_name[tk[i]][0], token_name[tk[i]][1]); */
        utstring_renew(test_s);
        utstring_printf(test_s, "%s%s",
                        token_name[tk[i]][1],
                        "\n    #cmt1\n    #cmt2\n");
        /* printf(utstring_body(test_s)); */
        result = obazl_starlark_lex_string(utstring_body(test_s));
        utstring_renew(buf);
        rootlist2string(result, buf);
        /* printf(utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(utstring_body(test_s), utstring_body(buf));
        utarray_free(result);
    }
}

void test_kw_nl2_cmt_nl_cmt(void) {
    for (int i=0; i < 16; i++) {
        /* printf("punct %s: :]%s[:\n", */
        /*        token_name[tk[i]][0], token_name[tk[i]][1]); */
        utstring_renew(test_s);
        utstring_printf(test_s, "%s%s",
                        token_name[tk[i]][1],
                        "\n\n    #cmt1\n    #cmt2\n");
        /* printf(utstring_body(test_s)); */
        result = obazl_starlark_lex_string(utstring_body(test_s));
        utstring_renew(buf);
        rootlist2string(result, buf);
        /* printf(utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(utstring_body(test_s), utstring_body(buf));
        utarray_free(result);
    }
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_kw);
    RUN_TEST(test_kw_cmt);
    RUN_TEST(test_kw_nl_cmt);
    RUN_TEST(test_kw_cmt_nl2_cmt);
    RUN_TEST(test_kw_nl_cmt_nl_cmt);
    RUN_TEST(test_kw_nl2_cmt_nl_cmt);
    return UNITY_END();
}
