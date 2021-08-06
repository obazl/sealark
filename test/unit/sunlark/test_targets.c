#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_targets.h"

static s7_pointer tgt;

int main(void) {
    build_file = "test/unit/sunlark/BUILD.targets";

    UNITY_BEGIN();
    RUN_TEST(test_target);      /* verify setUp worked */
    RUN_TEST(test_tgt_rule);        /* (:rule) */
    RUN_TEST(test_tgt_name);        /* (:name) */

    RUN_TEST(test_tgt_attr_sym);    /* (:attr srcs) */
    RUN_TEST(test_tgt_binding_sym); /* (:binding srcs) */
    RUN_TEST(test_tgt_at_sym);      /* (:@ srcs) */

    RUN_TEST(test_tgt_at_sym_key);      /* (:@ srcs :key) */
    RUN_TEST(test_tgt_at_sym_value);    /* (:@ srcs :value) */

    RUN_TEST(test_tgt_at_sym_value_i);    /* (:@ srcs :value 0) */
    RUN_TEST(test_tgt_at_int_value_i);    /* (:@ 1 :value 0) */
    RUN_TEST(test_tgt_at_int_dollar_i);    /* (:@ 1 :$ 0) */

    RUN_TEST(test_tgt_attr_sym_value_string); /* (:attr srcs :value "hello-lib.cc") */
    RUN_TEST(test_tgt_attr_sym_dollar_string); /* (:attr srcs :$ "hello-lib.cc") */

    RUN_TEST(test_tgt_at_sym_dollar_string); /* (:@ srcs :value "hello-lib.cc") */
    RUN_TEST(test_tgt_at_sym_dollar_string); /* (:@ srcs :$ "hello-lib.cc") */

    RUN_TEST(test_tgt_at_int_value_string); /* (:@ 1 :value "hello-lib.cc") */
    RUN_TEST(test_tgt_at_int_dollar_string); /* (:@ 1 :$ "hello-lib.cc") */

    return UNITY_END();
}

/* **************************************************************** */
void test_target(void) {
    validate_hello_world_target(tgt);
}

/* **************************************************************** */
void _validate_rule(s7_pointer rule) {
    s7_pointer pred = s7_apply_function(s7, rule,
                                        s7_eval_c_string(s7, "'(:id?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    TEST_ASSERT( sunlark_node_tid(s7, rule) == TK_ID );
    TEST_ASSERT( s7_is_c_object(rule) );
    TEST_ASSERT( !s7_is_string(rule) );
    s7_pointer rule_sym = s7_apply_function(s7, rule,
                                            s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(rule_sym) );
    TEST_ASSERT( s7_is_symbol(rule_sym) );
    pred = NULL;
    pred = s7_apply_function(s7, s7_name_to_value(s7,"symbol?"),
                             s7_list(s7, 1, rule_sym));
    TEST_ASSERT( pred == s7_t(s7) );

    TEST_ASSERT_EQUAL_STRING( "cc_library", s7_symbol_name(rule_sym) );

}

void test_tgt_rule(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:rule)");
    s7_pointer rule = s7_apply_function(s7, tgt, path);
    _validate_rule(rule);
}

/* **************************************************************** */
void _validate_name(s7_pointer nm) {
    s7_pointer is_string
        = s7_apply_function(s7, nm, s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( s7_t(s7) == is_string );
    TEST_ASSERT( s7_is_c_object(nm) );
    TEST_ASSERT( !s7_is_string(nm) );
    TEST_ASSERT( sunlark_node_tid(s7, nm) == TK_STRING );

    s7_pointer sym = s7_apply_function(s7, nm,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(sym) );
    TEST_ASSERT( s7_is_string(sym) );
    TEST_ASSERT_EQUAL_STRING( "\"hello-lib\"", s7_string(sym) );
}

void test_tgt_name(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:name)");
    s7_pointer name = s7_apply_function(s7, tgt, path);
    /* validate_hello_world_target(target); */
}

/* **************************************************************** */
void test_tgt_attr_sym(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:attr srcs)");
    s7_pointer attr = s7_apply_function(s7, tgt, path);
    validate_attr_srcs(attr);
}

void test_tgt_binding_sym(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:binding srcs)");
    s7_pointer attr = s7_apply_function(s7, tgt, path);
    validate_attr_srcs(attr);
}

void test_tgt_at_sym(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:@ srcs)");
    s7_pointer attr = s7_apply_function(s7, tgt, path);
    validate_attr_srcs(attr);
}

void test_tgt_at_sym_key(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:@ srcs :key)");
    s7_pointer key_node = s7_apply_function(s7, tgt, path);
    TEST_ASSERT( s7_is_c_object(key_node) );
    s7_pointer pred = NULL;
    pred = s7_apply_function(s7, key_node,
                             s7_eval_c_string(s7, "'(:node?)"));
    TEST_ASSERT( s7_t(s7) == pred );
    /* Convert to Scheme datum */
    s7_pointer key_str = s7_apply_function(s7, key_node,
                                           s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(key_str) );
    TEST_ASSERT( s7_is_symbol(key_str) );
    TEST_ASSERT_EQUAL_STRING( "srcs", s7_symbol_name(key_str) );
}

void test_tgt_at_sym_value(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:@ srcs :value)");
    s7_pointer val_node = s7_apply_function(s7, tgt, path);
    TEST_ASSERT( s7_is_c_object(val_node) );
    s7_pointer pred = s7_apply_function(s7, val_node,
                                        s7_eval_c_string(s7, "'(:node?)"));
    TEST_ASSERT( s7_t(s7) == pred );

    /* in this case the value is a string list */
    pred = NULL;
    pred = s7_apply_function(s7, val_node,
                             s7_eval_c_string(s7, "'(:list-expr?)"));
    TEST_ASSERT( s7_t(s7) == pred );
    /* of length 1 */
    s7_pointer len = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                                       s7_list(s7, 1, val_node));
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(len) );

    /* whose 0 item is "hello-lib.cc" */
    s7_pointer item = s7_apply_function(s7, val_node,
                             s7_eval_c_string(s7, "'(:0)"));
    /* which is a string node */
    pred = s7_apply_function(s7, item,
                             s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    /* whose Scheme value is string "hello-lib.cc" */
    s7_pointer sval = s7_apply_function(s7, item,
                                        s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "\"hello-lib.cc\"", s7_string(sval) );
}

void test_tgt_at_sym_value_i(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:@ srcs :value :0)");
    s7_pointer item = s7_apply_function(s7, tgt, path);
    s7_pointer pred = s7_apply_function(s7, item,
                                        s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    /* whose Scheme value is string "hello-lib.cc" */
    s7_pointer sval = s7_apply_function(s7, item,
                                        s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "\"hello-lib.cc\"", s7_string(sval) );
}

void test_tgt_at_int_value_i(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:@ 1 :value :0)");
    s7_pointer item = s7_apply_function(s7, tgt, path);
    s7_pointer pred = s7_apply_function(s7, item,
                                        s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    /* whose Scheme value is string "hello-lib.cc" */
    s7_pointer sval = s7_apply_function(s7, item,
                                        s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "\"hello-lib.cc\"", s7_string(sval) );
}

void test_tgt_at_int_dollar_i(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:@ 1 :$ :0)");
    s7_pointer item = s7_apply_function(s7, tgt, path);
    s7_pointer pred = s7_apply_function(s7, item,
                                        s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    /* whose Scheme value is string "hello-lib.cc" */
    s7_pointer sval = s7_apply_function(s7, item,
                                        s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "\"hello-lib.cc\"", s7_string(sval) );
}

/* **************************************************************** */
/* NB: the main purpose of :value "string" projector is to support set! ops */
void test_tgt_attr_sym_value_string(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:attr srcs :value \"howdy.cc\")");
    s7_pointer item_list = s7_apply_function(s7, tgt, path);
    _validate_value_string(item_list);
}
void test_tgt_attr_sym_dollar_string(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:attr srcs :$ \"howdy.cc\")");
    s7_pointer item_list = s7_apply_function(s7, tgt, path);
    _validate_value_string(item_list);
}
void test_tgt_at_sym_value_string(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:@ srcs :value \"howdy.cc\")");
    s7_pointer item_list = s7_apply_function(s7, tgt, path);
    _validate_value_string(item_list);
}
void test_tgt_at_sym_dollar_string(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:@ srcs :$ \"howdy.cc\")");
    s7_pointer item_list = s7_apply_function(s7, tgt, path);
    _validate_value_string(item_list);
}
void test_tgt_at_int_value_string(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:@ 1 :value \"howdy.cc\")");
    s7_pointer item_list = s7_apply_function(s7, tgt, path);
    _validate_value_string(item_list);
}
void test_tgt_at_int_dollar_string(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:@ 1 :$ \"howdy.cc\")");
    s7_pointer item_list = s7_apply_function(s7, tgt, path);
    _validate_value_string(item_list);
}

void _validate_value_string(s7_pointer item_list)
{
    TEST_ASSERT( s7_is_list(s7, item_list));
    s7_pointer len = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                                       s7_list(s7, 1, item_list));
    TEST_ASSERT_EQUAL_INT( 1, s7_integer(len) );

    s7_pointer item1 = s7_apply_function(s7, s7_name_to_value(s7, "car"),
                                       s7_list(s7, 1, item_list));
    TEST_ASSERT( s7_is_c_object(item1));
    s7_pointer pred = s7_apply_function(s7, item1,
                                        s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    s7_pointer sval = s7_apply_function(s7, item1,
                                        s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "\"howdy.cc\"", s7_string(sval) );
}

/* **************************************************************** */
UT_string *buf;
UT_string *test_s;

char *build_file;

s7_scheme *s7;

struct parse_state_s *parse_state;
struct node_s *root;

void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
    s7_pointer pkg = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    root = s7_c_object_value(pkg);
    s7_pointer path = s7_eval_c_string(s7, "'(:target \"hello-lib\")");
    tgt = s7_apply_function(s7, pkg, path);
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);
    s7_quit(s7);
}

