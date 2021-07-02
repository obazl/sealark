#include "utarray.h"

#include "unity.h"
#include "lex_numbers.h"

UT_string *buf;
const char *test_str;
UT_array *result;

void setUp(void) {
    utstring_new(buf);
}

void tearDown(void) {
    utstring_free(buf);
}

void test_int_dec(void) {
    test_str = "0\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1234567890\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1234567890 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1234567890\n#cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1234567890\n    #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
}

void test_int_hex(void) {
    test_str = "0x0\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0x1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0xf\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0xF\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0x0123456789aAbBcCdDeEfF\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0x0123456789aAbBcCdDeEfF #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0x0123456789aAbBcCdDeEfF\n#cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0x0123456789aAbBcCdDeEfF\n    #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
}

void test_int_oct(void) {
    test_str = "0o0\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0o1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0o7\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0o01234567\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0o01234567 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0o01234567\n#cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "0o01234567\n    #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
}

void test_float(void) {
    test_str = "1.\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.0\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e+1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E+1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e-1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E-1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e0\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E0\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E-0\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e-0\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e+0\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E+0\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
}

void test_float_cmt(void) {
    test_str = "1. #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.0 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e1 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E1 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e+1 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E+1 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e-1 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E-1 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e0 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E0 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E-0 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e-0 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e+0 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E+0 #cmt1\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
}

void test_float_cmt_nl_cmt(void) {
    test_str = "1. #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.0 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e1 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E1 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e+1 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E+1 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e-1 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E-1 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e0 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E0 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E-0 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e-0 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e+0 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E+0 #cmt1\n#cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
}

void test_float_cmt_nl_sp4_cmt(void) {
    test_str = "1. #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.0 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e1 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E1 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e+1 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E+1 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e-1 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E-1 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e0 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E0 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E-0 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e-0 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e+0 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E+0 #cmt1\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
}

void test_float_cmt_nl2_sp4_cmt(void) {
    test_str = "1. #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    /* printf(":]%s[:\n", utstring_body(buf)); */
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.0 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e1 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E1 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e+1 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E+1 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e-1 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E-1 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e0 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E0 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E-0 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e-0 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1e+0 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));

    test_str = "1.1E+0 #cmt1\n\n    #cmt2\n";
    result = obazl_starlark_lex_string(test_str);
    utstring_renew(buf);
    rootlist2string(result, buf);
    TEST_ASSERT_EQUAL_STRING(test_str, utstring_body(buf));
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_int_dec);
    RUN_TEST(test_int_hex);
    RUN_TEST(test_int_oct);
    RUN_TEST(test_float);
    RUN_TEST(test_float_cmt);
    RUN_TEST(test_float_cmt_nl_cmt);
    RUN_TEST(test_float_cmt_nl_sp4_cmt);
    RUN_TEST(test_float_cmt_nl2_sp4_cmt);
    return UNITY_END();
}
