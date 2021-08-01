#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_set_strings.h"

UT_string *buf;
UT_string *test_s;
/* UT_array  *result; */

char *build_file = "test/unit/sunlark/BUILD.strings";

s7_scheme *s7;

/* struct parse_state_s *parse_state; */

static s7_pointer ast;
struct node_s *root;

s7_pointer is_eq_s7;
s7_pointer is_equal_s7;

void setUp(void) {
    s7 = sunlark_init();
    is_eq_s7 = s7_name_to_value(s7, "eq?");
    is_equal_s7 = s7_name_to_value(s7, "equal?");
    ast = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    root = s7_c_object_value(ast);
}

void tearDown(void) {
    s7_quit(s7);
}

/* **************** singleton quotation **************** */
void test_binding_value_string_plain_dq(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 0 :@ plaindq :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote plain */
    TEST_ASSERT( !(bvalue_node->qtype & SQUOTE) );

    /* use :$ to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue, s7_eval_c_string(s7, "'(:$)"));
                            /* s7_cons(s7, s7_make_keyword(s7, "'(:$)"), */
                            /*         s7_nil(s7))); */
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "\"I am a plain double-quoted string\"",
                              s7_string(bvalue_str) );
}

void test_binding_value_string_plain_sq(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 0 :@ plainsq :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote plain */
    TEST_ASSERT( bvalue_node->qtype & SQUOTE );

    /* use :$ to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue, s7_eval_c_string(s7, "'(:$)"));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    //NB: string test compares only string content, not quote mark types
    TEST_ASSERT_EQUAL_STRING( "'I am a plain single-quoted string'",
                              s7_string(bvalue_str) );
    //FIXME: deal with qmarks
}

void test_binding_value_string_raw_dq(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 0 :@ rawdq :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote plain */
    TEST_ASSERT( !(bvalue_node->qtype & SQUOTE) );
    
    /* use :$ to get a string value */
    s7_pointer bvalue_str = s7_apply_function(s7, bvalue, s7_eval_c_string(s7, "'(:$)"));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "r\"I am a raw double-quoted string\"",
                              s7_string(bvalue_str) );
}

void test_binding_value_string_raw_sq(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 0 :@ rawsq :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote raw */
    TEST_ASSERT( bvalue_node->qtype & SQUOTE );

    /* use :$ to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue, s7_eval_c_string(s7, "'(:$)"));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "r'I am a raw single-quoted string'",
                              s7_string(bvalue_str) );
}

void test_binding_value_string_bin_dq(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 0 :@ bindq :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote bin */
    TEST_ASSERT( !(bvalue_node->qtype & SQUOTE) );

    /* use :$ to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue, s7_eval_c_string(s7, "'(:$)"));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "b\"I am a binary double-quoted string\"",
                              s7_string(bvalue_str) );
}

void test_binding_value_string_bin_sq(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 0 :@ binsq :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote bin */
    TEST_ASSERT( bvalue_node->qtype & SQUOTE );

    /* use :$ to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue, s7_eval_c_string(s7, "'(:$)"));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "b'I am a binary single-quoted string'",
                              s7_string(bvalue_str) );
}

/* **************** triple quotation **************** */
void test_binding_value_string_plain_dq3(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 1 :@ plaindq3 :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote plain */
    TEST_ASSERT( !(bvalue_node->qtype & SQUOTE) );

    /* use :$ to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue, s7_eval_c_string(s7, "'(:$)"));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "\"\"\"I am a plain triple double-quoted string\"\"\"",
                              s7_string(bvalue_str) );
}

void test_binding_value_string_plain_sq3(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 1 :@ plainsq3 :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote plain */
    TEST_ASSERT( bvalue_node->qtype & SQUOTE );

    /* use :$ to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue, s7_eval_c_string(s7, "'(:$)"));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "'''I am a plain triple single-quoted string'''",
                              s7_string(bvalue_str) );
}

void test_binding_value_string_raw_dq3(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 1 :@ rawdq3 :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote plain */
    TEST_ASSERT( !(bvalue_node->qtype & SQUOTE) );

    /* use :$ to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue, s7_eval_c_string(s7, "'(:$)"));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "r\"\"\"I am a raw triple double-quoted string\"\"\"",
                              s7_string(bvalue_str) );
}

void test_binding_value_string_raw_sq3(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 1 :@ rawsq3 :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote raw */
    TEST_ASSERT( bvalue_node->qtype & SQUOTE );

    /* use :$ to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue, s7_eval_c_string(s7, "'(:$)"));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "r'''I am a raw triple single-quoted string'''",
                              s7_string(bvalue_str) );
}

void test_binding_value_string_bin_dq3(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 1 :@ bindq3 :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote bin */
    TEST_ASSERT( !(bvalue_node->qtype & SQUOTE) );

    /* use :$ to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue, s7_eval_c_string(s7, "'(:$)"));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "b\"\"\"I am a binary triple double-quoted string\"\"\"",
                              s7_string(bvalue_str) );
}

void test_binding_value_string_bin_sq3(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 1 :@ binsq3 :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote bin */
    TEST_ASSERT( bvalue_node->qtype & SQUOTE );
    TEST_ASSERT( bvalue_node->qtype & TRIPLE );

    /* starlark print value */
    s7_pointer bvalue_str
        = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark->starlark"),
                            s7_list(s7, 1, bvalue));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    bvalue_str
        = s7_apply_function(s7, bvalue,
                            s7_eval_c_string(s7, "'(:$)"));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "b'''I am a binary triple single-quoted string'''",
                              s7_string(bvalue_str) );
}

/* **************** updates **************** */
void test_set_string_plain_dq(void) {
    char *starlark = "I am a plain dq string";
    s7_pointer str = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark-make-string"),
                            s7_list(s7, 1,
                                    s7_make_string(s7, starlark)));
    /* log_debug(":\n%s", s7_object_to_c_string(s7, str)); */
    s7_pointer str_str
        = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(str_str) );
    TEST_ASSERT( s7_is_string(str_str) );
    TEST_ASSERT_EQUAL_STRING( "\"I am a plain dq string\"",
                              s7_string(str_str) );

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(str) );
    TEST_ASSERT( sunlark_node_tid(s7, str) == TK_STRING );

    struct node_s *str_node = s7_c_object_value(str);
    TEST_ASSERT( str_node->tid == TK_STRING );

    /* verify qtype: single quote plain */
    TEST_ASSERT( !(str_node->qtype & SQUOTE) );

    /* (set! (str) "hello") */
    s7_pointer newstr = s7_apply_function(s7,
                          s7_name_to_value(s7, "set!"),
                           s7_list(s7, 2,
                                   s7_list(s7, 1, str),
                                    s7_make_string(s7, "hello")));
    /* log_debug(":\n%s", s7_object_to_c_string(s7, str)); */

    s7_pointer newstr_str
        = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(newstr_str) );
    TEST_ASSERT( s7_is_string(newstr_str) );
    TEST_ASSERT_EQUAL_STRING( "\"hello\"",
                              s7_string(newstr_str) );

    s7_pointer same = s7_apply_function(s7,
                          s7_name_to_value(s7, "equal?"),
                                        s7_list(s7, 2, str, newstr));
    TEST_ASSERT( same == s7_t(s7) );

    /* s7_pointer str_str */
    /*     = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)")); */
    /*                         /\* s7_cons(s7, s7_make_keyword(s7, "'(:$)"), *\/ */
    /*                         /\*         s7_nil(s7))); *\/ */
    /* log_debug("str_s: %s", s7_object_to_c_string(s7, str_str)); */
    /* TEST_ASSERT( !s7_is_c_object(str_str) ); */
    /* TEST_ASSERT( s7_is_string(str_str) ); */
    /* TEST_ASSERT_EQUAL_STRING( "\"I am a plain double-quoted string\"", */
    /*                           s7_string(str_str) ); */
}

void test_set_string_raw_sq(void) {
    char *starlark = "I am a plain dq string";
    s7_pointer str = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark-make-string"),
                            s7_list(s7, 1,
                                    s7_make_string(s7, starlark)));
    /* log_debug(":\n%s", s7_object_to_c_string(s7, str)); */
    s7_pointer str_str = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(str_str) );
    TEST_ASSERT( s7_is_string(str_str) );
    TEST_ASSERT_EQUAL_STRING( "\"I am a plain dq string\"", s7_string(str_str) );

    /* verify qtype: single quote plain */
    /* TEST_ASSERT_EQUAL( str_str->qtype, 0 ); */

    /* (set! (str) r'''hello''') */
    s7_pointer rsq3 = s7_apply_function(s7,
                          s7_name_to_value(s7, "sunlark-make-string"),
                           s7_list(s7, 7,
                                   s7_make_string(s7, "hello"),
                                   s7_make_keyword(s7, "q"),
                                   s7_make_character(s7, '\''),
                                   s7_make_keyword(s7, "qqq"),
                                   s7_t(s7),
                                   s7_make_keyword(s7, "t"),
                                   s7_make_keyword(s7, "r")));

    s7_pointer newstr = s7_apply_function(s7,
                          s7_name_to_value(s7, "set!"),
                           s7_list(s7, 2,
                                   s7_list(s7, 1, str),
                                   rsq3));

    s7_pointer newstr_str = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    log_debug(":\n%s", s7_object_to_c_string(s7, newstr_str));
    TEST_ASSERT( !s7_is_c_object(newstr_str) );
    TEST_ASSERT( s7_is_string(newstr_str) );
    TEST_ASSERT_EQUAL_STRING( "r'''hello'''", s7_string(newstr_str) );

    s7_pointer same = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                                        s7_list(s7, 2, str, newstr));
    TEST_ASSERT( same == s7_t(s7) );
}


int main(void) {
    UNITY_BEGIN();

    RUN_TEST(test_binding_value_string_plain_dq);
    RUN_TEST(test_binding_value_string_plain_sq);
    RUN_TEST(test_binding_value_string_raw_dq);
    RUN_TEST(test_binding_value_string_raw_sq);
    RUN_TEST(test_binding_value_string_bin_dq);
    RUN_TEST(test_binding_value_string_bin_sq);

    RUN_TEST(test_binding_value_string_plain_dq3);
    RUN_TEST(test_binding_value_string_plain_sq3);
    RUN_TEST(test_binding_value_string_raw_dq3);
    RUN_TEST(test_binding_value_string_raw_sq3);
    RUN_TEST(test_binding_value_string_bin_dq3);
    RUN_TEST(test_binding_value_string_bin_sq3);

    RUN_TEST(test_set_string_plain_dq);
    /* /\* RUN_TEST(test_set_string_plain_sq); *\/ */
    /* /\* RUN_TEST(test_set_string_raw_dq); *\/ */
    RUN_TEST(test_set_string_raw_sq);
    /* /\* RUN_TEST(test_set_string_bin_dq); *\/ */
    /* /\* RUN_TEST(test_set_string_bin_sq); *\/ */

    return UNITY_END();
}
