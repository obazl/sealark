#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_make_target.h"

s7_scheme *s7;

static s7_pointer tgt;

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_with_null_attrs);
    /* RUN_TEST(test_macro_with_null_attrs); */

    RUN_TEST(test_with_3_attrs);
    /* RUN_TEST(test_macro_with_3_attrs); */

    return UNITY_END();
}
/* **************************************************************** */
/* **************************************************************** */
/* **************************************************************** */
void test_with_null_attrs(void) {
    char *s =
        "(make-target :name \"tgt1\" :rule 'cc_library"
        "             :attrs '())";
    s7_pointer target = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Call_Expr );

    /* _validate_key(binding); */

    /* verify key == "tgt1" */
    s7_pointer key
        = s7_apply_function(s7, target,
                            s7_list(s7,1,s7_make_keyword(s7,"key")));
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT_EQUAL_INT( TK_STRING, sunlark_node_tid(s7, key) );
    TEST_ASSERT_EQUAL_STRING( "\"tgt1\"", sunlark_node_to_string(key) );

    s7_pointer rule
        = s7_apply_function(s7, target,
                            s7_list(s7,1,s7_make_keyword(s7,"rule")));
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT_EQUAL_INT( TK_ID, sunlark_node_tid(s7, rule) );
    TEST_ASSERT_EQUAL_STRING( "cc_library", sunlark_node_to_string(rule) );

    s7_pointer binding_ct
        = s7_apply_function(s7, target,
                            s7_list(s7,1,s7_make_keyword(s7,"length")));
    TEST_ASSERT( !s7_is_c_object(binding_ct) );
    TEST_ASSERT( s7_is_integer(binding_ct) );
    /* only 'name' binding */
    TEST_ASSERT_EQUAL_INT( 1, s7_integer(binding_ct) );
}

void test_macro_with_null_attrs(void) {
    char *s =
        "#>(cc_library \"tgt1\" '())";
    s7_pointer target = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Call_Expr );

    /* _validate_key(binding); */

    /* verify key == "tgt1" */
    s7_pointer key
        = s7_apply_function(s7, target,
                            s7_list(s7,1,s7_make_keyword(s7,"key")));
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT_EQUAL_INT( TK_STRING, sunlark_node_tid(s7, key) );
    TEST_ASSERT_EQUAL_STRING( "\"tgt1\"", sunlark_node_to_string(key) );

    s7_pointer rule
        = s7_apply_function(s7, target,
                            s7_list(s7,1,s7_make_keyword(s7,"rule")));
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT_EQUAL_INT( TK_ID, sunlark_node_tid(s7, rule) );
    TEST_ASSERT_EQUAL_STRING( "cc_library", sunlark_node_to_string(rule) );

    s7_pointer binding_ct
        = s7_apply_function(s7, target,
                            s7_list(s7,1,s7_make_keyword(s7,"length")));
    TEST_ASSERT( !s7_is_c_object(binding_ct) );
    TEST_ASSERT( s7_is_integer(binding_ct) );
    /* only 'name' binding */
    TEST_ASSERT_EQUAL_INT( 1, s7_integer(binding_ct) );
}

/* **************************************************************** */
void test_with_3_attrs(void) {
    char *s =
        "(make-target :name \"tgt1\" :rule 'cc_library"
        "             :attrs (list "
        "                     #@(a_false #f)"
        "                     #@(an_int 7)"
        "                     #@(an_intlist #(1 2 3))))";
    s7_pointer target = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Call_Expr );

    /* _validate_key(binding); */

    /* verify key == "tgt1" */
    s7_pointer key
        = s7_apply_function(s7, target,
                            s7_list(s7,1,s7_make_keyword(s7,"key")));
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT_EQUAL_INT( TK_STRING, sunlark_node_tid(s7, key) );
    TEST_ASSERT_EQUAL_STRING( "\"tgt1\"", sunlark_node_to_string(key) );

    s7_pointer rule
        = s7_apply_function(s7, target,
                            s7_list(s7,1,s7_make_keyword(s7,"rule")));
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT_EQUAL_INT( TK_ID, sunlark_node_tid(s7, rule) );
    TEST_ASSERT_EQUAL_STRING( "cc_library", sunlark_node_to_string(rule) );

    s7_pointer binding_ct
        = s7_apply_function(s7, target,
                            s7_list(s7,1,s7_make_keyword(s7,"length")));
    TEST_ASSERT( !s7_is_c_object(binding_ct) );
    TEST_ASSERT( s7_is_integer(binding_ct) );
    /* 'name' binding plus 3 */
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(binding_ct) );
}

/* **************************************************************** */
void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
}

void tearDown(void) {
    s7_quit(s7);
}

