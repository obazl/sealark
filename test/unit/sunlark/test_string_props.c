#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_string_props.h"

UT_string *buf;
UT_string *test_s;

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

/* **************** properties **************** */
void test_string_props_pdq1(void) {
    char *pdq1_s = "hello pdq1";
    s7_pointer plain = s7_apply_function(s7, s7_name_to_value(s7, "sunlark-make-string"),
                                       s7_list(s7, 1, s7_make_string(s7, pdq1_s)));
    s7_pointer plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( plain_prop == s7_make_keyword(s7, "plain") );
    plain_prop = NULL;
    plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( plain_prop == s7_make_character(s7, '\"') );
    plain_prop = NULL;
    plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:qqq)"));
    TEST_ASSERT( plain_prop == s7_f(s7) );
    s7_pointer plain_str = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "\"hello pdq1\"", s7_string(plain_str) );
}

void test_string_props_psq1(void) {
    char *plain_s = "hello psq1";
    s7_pointer plain = s7_apply_function(s7, s7_name_to_value(s7, "sunlark-make-string"),
                                         s7_list(s7, 3,
                                                 s7_make_string(s7, plain_s),
                                                 s7_make_keyword(s7, "t"),
                                                 s7_make_keyword(s7, "r")));
    s7_pointer plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( plain_prop == s7_make_keyword(s7, "r") );
    plain_prop = NULL;
    plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( plain_prop == s7_make_character(s7, '\"') );
    plain_prop = NULL;
    plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:qqq)"));
    TEST_ASSERT( plain_prop == s7_f(s7) );
    s7_pointer plain_str = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "r\"hello psq1\"", s7_string(plain_str) );
}

void test_string_props_pdq3(void) {
    char *plain_s = "hello pdq3";
    s7_pointer plain = s7_apply_function(s7, s7_name_to_value(s7, "sunlark-make-string"),
                                         s7_list(s7, 3,
                                                 s7_make_string(s7, plain_s),
                                                 s7_make_keyword(s7, "qqq"),
                                                 s7_t(s7)));
    s7_pointer plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( plain_prop == s7_make_keyword(s7, "plain") );
    plain_prop = NULL;
    plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( plain_prop == s7_make_character(s7, '"') );
    plain_prop = NULL;
    plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:qqq)"));
    TEST_ASSERT( plain_prop == s7_t(s7) );
    s7_pointer plain_str = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "\"\"\"hello pdq3\"\"\"", s7_string(plain_str) );
}

/* **************** */
void test_string_props_psq3(void) {
    char *plain_s = "hello psq3";
    s7_pointer plain = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark-make-string"),
                            s7_list(s7, 5,
                                    s7_make_string(s7, plain_s),
                                    s7_make_keyword(s7, "q"),
                                    s7_make_character(s7, '\''),
                                    s7_make_keyword(s7, "qqq"),
                                    s7_t(s7)));
    s7_pointer plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( plain_prop == s7_make_keyword(s7, "plain") );
    plain_prop = NULL;
    plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( plain_prop == s7_make_character(s7, '\'') );
    plain_prop = NULL;
    plain_prop = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:qqq)"));
    sunlark_debug_print_node(s7, plain);
    TEST_ASSERT( plain_prop == s7_t(s7) );
    s7_pointer plain_str = s7_apply_function(s7, plain, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "'''hello psq3'''", s7_string(plain_str) );
}

/* **************** */
void test_string_props_bdq1(void) {
    char *starlark = "hello bdq1";
    s7_pointer str = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark-make-string"),
                            s7_list(s7, 3,
                                    s7_make_string(s7, starlark),
                                    s7_make_keyword(s7, "t"),
                                    s7_make_keyword(s7, "b")));
    s7_pointer str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( str_prop == s7_make_keyword(s7, "b") );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( str_prop == s7_make_character(s7, '"') );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:qqq)"));
    TEST_ASSERT( str_prop == s7_f(s7) );

    s7_pointer str_str = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "b\"hello bdq1\"", s7_string(str_str) );
}

/* **************** */
void test_string_props_bsq1(void) {
    char *starlark = "hello bsq1";
    s7_pointer str = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark-make-string"),
                            s7_list(s7, 5,
                                    s7_make_string(s7, starlark),
                                    s7_make_keyword(s7, "q"),
                                    s7_make_character(s7, '\''),
                                    s7_make_keyword(s7, "t"),
                                    s7_make_keyword(s7, "b")));
    s7_pointer str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( str_prop == s7_make_keyword(s7, "b") );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( str_prop == s7_make_character(s7, '\'') );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:qqq)"));
    TEST_ASSERT( str_prop == s7_f(s7) );

    s7_pointer str_str = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "b'hello bsq1'", s7_string(str_str) );
}

/* **************** */
void test_string_props_bdq3(void) {
    char *starlark = "hello bdq3";
    s7_pointer str = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark-make-string"),
                            s7_list(s7, 5,
                                    s7_make_string(s7, starlark),
                                    s7_make_keyword(s7, "qqq"),
                                    s7_t(s7),
                                    s7_make_keyword(s7, "t"),
                                    s7_make_keyword(s7, "b")));
    s7_pointer str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( str_prop == s7_make_keyword(s7, "b") );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( str_prop == s7_make_character(s7, '"') );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:qqq)"));
    TEST_ASSERT( str_prop == s7_t(s7) );

    s7_pointer str_str = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "b\"\"\"hello bdq3\"\"\"", s7_string(str_str) );
}

/* **************** */
void test_string_props_bsq3(void) {
    char *starlark = "hello bsq3";
    s7_pointer str = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark-make-string"),
                            s7_list(s7, 7,
                                    s7_make_string(s7, starlark),
                                    s7_make_keyword(s7, "q"),
                                    s7_make_character(s7, '\''),
                                    s7_make_keyword(s7, "qqq"),
                                    s7_t(s7),
                                    s7_make_keyword(s7, "t"),
                                    s7_make_keyword(s7, "b")));
    s7_pointer str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( str_prop == s7_make_keyword(s7, "b") );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( str_prop == s7_make_character(s7, '\'') );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:qqq)"));
    TEST_ASSERT( str_prop == s7_t(s7) );

    s7_pointer str_str = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "b'''hello bsq3'''", s7_string(str_str) );
}

/* **************** */
void test_string_props_rdq1(void) {
    char *starlark = "hello rdq1";
    s7_pointer str = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark-make-string"),
                            s7_list(s7, 3,
                                    s7_make_string(s7, starlark),
                                    s7_make_keyword(s7, "t"),
                                    s7_make_keyword(s7, "r")));
    s7_pointer str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( str_prop == s7_make_keyword(s7, "r") );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( str_prop == s7_make_character(s7, '"') );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:qqq)"));
    TEST_ASSERT( str_prop == s7_f(s7) );

    s7_pointer str_str = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "r\"hello rdq1\"", s7_string(str_str) );
}

/* **************** */
void test_string_props_rsq1(void) {
    char *starlark = "hello rsq1";
    s7_pointer str = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark-make-string"),
                            s7_list(s7, 5,
                                    s7_make_string(s7, starlark),
                                    s7_make_keyword(s7, "q"),
                                    s7_make_character(s7, '\''),
                                    s7_make_keyword(s7, "t"),
                                    s7_make_keyword(s7, "r")));
    s7_pointer str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( str_prop == s7_make_keyword(s7, "r") );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( str_prop == s7_make_character(s7, '\'') );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:qqq)"));
    TEST_ASSERT( str_prop == s7_f(s7) );

    s7_pointer str_str = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "r'hello rsq1'", s7_string(str_str) );
}

/* **************** */
void test_string_props_rdq3(void) {
    char *starlark = "hello rdq3";
    s7_pointer str = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark-make-string"),
                            s7_list(s7, 5,
                                    s7_make_string(s7, starlark),
                                    s7_make_keyword(s7, "qqq"),
                                    s7_t(s7),
                                    s7_make_keyword(s7, "t"),
                                    s7_make_keyword(s7, "r")));
    s7_pointer str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( str_prop == s7_make_keyword(s7, "r") );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( str_prop == s7_make_character(s7, '"') );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:qqq)"));
    TEST_ASSERT( str_prop == s7_t(s7) );

    s7_pointer str_str = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "r\"\"\"hello rdq3\"\"\"", s7_string(str_str) );
}

/* **************** */
void test_string_props_rsq3(void) {
    char *starlark = "hello rsq3";
    s7_pointer str = s7_apply_function(s7,
                            s7_name_to_value(s7, "sunlark-make-string"),
                            s7_list(s7, 7,
                                    s7_make_string(s7, starlark),
                                    s7_make_keyword(s7, "q"),
                                    s7_make_character(s7, '\''),
                                    s7_make_keyword(s7, "qqq"),
                                    s7_t(s7),
                                    s7_make_keyword(s7, "t"),
                                    s7_make_keyword(s7, "r")));
    s7_pointer str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:t)"));
    TEST_ASSERT( str_prop == s7_make_keyword(s7, "r") );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:q)"));
    TEST_ASSERT( str_prop == s7_make_character(s7, '\'') );
    str_prop = NULL;
    str_prop = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:qqq)"));
    TEST_ASSERT( str_prop == s7_t(s7) );

    s7_pointer str_str = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "r'''hello rsq3'''", s7_string(str_str) );
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
    s7_pointer str_str
        = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(str_str) );
    TEST_ASSERT( s7_is_string(str_str) );
    TEST_ASSERT_EQUAL_STRING( "\"I am a plain dq string\"",
                              s7_string(str_str) );

    /* verify qtype: single quote plain */
    /* TEST_ASSERT_EQUAL( str_str->qtype, 0 ); */

    /* (set! (str) r'''hello''') */
    s7_pointer rhello3 = s7_apply_function(s7,
                          s7_name_to_value(s7, "sunlark-make-string"),
                           s7_list(s7, 7,
                                   s7_make_string(s7, "hello"),
                                   s7_make_keyword(s7, "q"),
                                   s7_make_character(s7, '\''),
                                   s7_make_keyword(s7, "qqq"),
                                   s7_t(s7),
                                   s7_make_keyword(s7, "type"),
                                   s7_make_keyword(s7, "raw")));

    s7_pointer newstr = s7_apply_function(s7,
                          s7_name_to_value(s7, "set!"),
                           s7_list(s7, 2,
                                   s7_list(s7, 1, str),
                                   rhello3));
    log_debug(":\n%s", s7_object_to_c_string(s7, str));

    s7_pointer newstr_str
        = s7_apply_function(s7, str, s7_eval_c_string(s7, "'(:$)"));
    log_debug(":\n%s", s7_object_to_c_string(s7, newstr_str));
    TEST_ASSERT( !s7_is_c_object(newstr_str) );
    TEST_ASSERT( s7_is_string(newstr_str) );
    TEST_ASSERT_EQUAL_STRING( "r'''hello'''",
                              s7_string(newstr_str) );

    s7_pointer same = s7_apply_function(s7,
                          s7_name_to_value(s7, "equal?"),
                                        s7_list(s7, 2, str, newstr));
    TEST_ASSERT( same == s7_t(s7) );
}


int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_string_props_pdq1);
    RUN_TEST(test_string_props_psq1);
    RUN_TEST(test_string_props_pdq3);
    RUN_TEST(test_string_props_psq3);

    RUN_TEST(test_string_props_bdq1);
    RUN_TEST(test_string_props_bdq3);
    RUN_TEST(test_string_props_bsq1);
    RUN_TEST(test_string_props_bsq3);

    RUN_TEST(test_string_props_rdq1);
    RUN_TEST(test_string_props_rdq3);
    RUN_TEST(test_string_props_rsq1);
    RUN_TEST(test_string_props_rsq3);

    /* RUN_TEST(test_raw_string_props); */
    /* RUN_TEST(test_binary_string_props); */
    /* RUN_TEST(test_rawbinary_string_props); */

    /* RUN_TEST(test_binding_value_string_plain_dq); */
    /* RUN_TEST(test_binding_value_string_plain_sq); */
    /* RUN_TEST(test_binding_value_string_raw_dq); */
    /* RUN_TEST(test_binding_value_string_raw_sq); */
    /* RUN_TEST(test_binding_value_string_bin_dq); */
    /* RUN_TEST(test_binding_value_string_bin_sq); */

    /* RUN_TEST(test_binding_value_string_plain_dq3); */
    /* RUN_TEST(test_binding_value_string_plain_sq3); */
    /* RUN_TEST(test_binding_value_string_raw_dq3); */
    /* RUN_TEST(test_binding_value_string_raw_sq3); */
    /* RUN_TEST(test_binding_value_string_bin_dq3); */
    /* RUN_TEST(test_binding_value_string_bin_sq3); */

    /* RUN_TEST(test_set_string_plain_dq); */
    /* /\* RUN_TEST(test_set_string_plain_sq); *\/ */
    /* /\* RUN_TEST(test_set_string_raw_dq); *\/ */
    /* RUN_TEST(test_set_string_raw_sq); */
    /* /\* RUN_TEST(test_set_string_bin_dq); *\/ */
    /* /\* RUN_TEST(test_set_string_bin_sq); *\/ */

    return UNITY_END();
}
