#include "utarray.h"
#include "utstring.h"

#include "unity.h"
#include "lex_punctuation.h"

UT_string *buf;
UT_string *test_s;
UT_array *result;

LOCAL const int tk[42] = {
    TK_STAR2, TK_ARROW,
    TK_LE, TK_GE,
    TK_SEMI, TK_COLON,
    TK_BANG_EQ, TK_BANG,
    TK_PLUS_EQ, TK_PLUS,        /* 10 */
    TK_MINUS_EQ, TK_MINUS,
    TK_STAR_EQ, TK_STAR,
    TK_SLASH2_EQ, TK_SLASH2,
    TK_DIV_EQ, TK_SLASH,
    TK_PCT_EQ, TK_PCT,          /* 20 */
    TK_AMP_EQ, TK_AMP,
    TK_VBAR_EQ, TK_VBAR,
    TK_CARET_EQ, TK_CARET,
    TK_LBRACK, TK_RBRACK,
    TK_LBRACE, TK_RBRACE,       /* 30 */
    TK_LPAREN, TK_RPAREN,
    TK_TILDE,

    TK_LLANGLE_EQ, TK_LLANGLE, TK_LANGLE,
    TK_RRANGLE_EQ, TK_RRANGLE, TK_RANGLE,
    TK_EQ2, TK_EQ,
    TK_ESC_BACKSLASH /* 42 */
};

void setUp(void) {
    utstring_new(test_s);
    utstring_new(buf);
}

void tearDown(void) {
    utstring_free(test_s);
    utstring_free(buf);
}

void test_p(void) {
    for (int i=0; i < 42; i++) {
        printf("punct %s: :]%s[:\n",
               token_name[tk[i]][0], token_name[tk[i]][1]);
        utstring_renew(test_s);
        utstring_printf(test_s, "%s%s",
                        token_name[tk[i]][1],
                        " #cmt1\n    #cmt2\n");
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
    /* RUN_TEST(test_punctuation); */
    RUN_TEST(test_p);
    return UNITY_END();
}
