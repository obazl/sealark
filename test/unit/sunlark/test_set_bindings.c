#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

#include "sealark.h"
#include "sunlark.h"

#include "test_set_bindings.h"

UT_string *buf;
UT_string *test_s;

char *build_file = "test/unit/sunlark/BUILD.set_bindings";

s7_scheme *s7;

/* struct parse_state_s *parse_state; */

static s7_pointer ast;
struct node_s *root;

static s7_pointer result;
static s7_pointer item, val, count;

void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);

    ast = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    s7_define_variable(s7, "ast", ast);
    root = s7_c_object_value(ast);
}

void tearDown(void) {
    s7_quit(s7);
}

void test_set_bool_to_int(void) {
    /* bool_attr = True */
    s7_pointer path = s7_eval_c_string(s7, "'(:> 0 :@ 1)");
    s7_pointer item = s7_apply_function(s7, ast, path);
    /* :value of item == ID node; :$ of node == symbol 'True */
    s7_pointer valnode= s7_apply_function(s7, item,
                                      s7_eval_c_string(s7, "'(:value)"));
    s7_pointer val = s7_apply_function(s7, valnode,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "True", s7_symbol_name(val));
    /* getter: (item :value) */
    s7_pointer getter = s7_list(s7, 2, item, s7_make_keyword(s7, "value"));

    /* (result (set! (item :value) 99)) */
    s7_pointer newval = s7_make_integer(s7, 99);
    result = s7_apply_function(s7, set_bang, s7_list(s7, 2, getter, newval));
    val = s7_apply_function(s7, result, s7_eval_c_string(s7, "'(:$)"));
    val = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 99, s7_integer(val));
}

void test_set_bool_to_int_list1(void) {
    /* bool_attr = True */
    s7_pointer path = s7_eval_c_string(s7, "'(:> 0 :@ 1)");
    s7_pointer binding = s7_apply_function(s7, ast, path);
    /* :value of binding == ID node; :$ of node == symbol 'True */
    s7_pointer valnode= s7_apply_function(s7, binding,
                                      s7_eval_c_string(s7, "'(:value)"));
    s7_pointer val = s7_apply_function(s7, valnode,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "True", s7_symbol_name(val));
    s7_pointer getter = s7_list(s7, 2, binding, s7_make_keyword(s7, "value"));

    /* (result (set! (binding :value) '(21)) */
    s7_pointer newval = s7_list(s7, 2,
                                s7_make_symbol(s7, "quote"),
                                s7_list(s7, 1,
                                        s7_make_integer(s7, 21)));
    result = s7_apply_function(s7, set_bang, s7_list(s7, 2, getter, newval));
    val = s7_apply_function(s7, binding, s7_eval_c_string(s7, "'(:value)"));
    s7_pointer item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(0)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 21, s7_integer(item));
}

void test_set_bool_to_int_list2(void) {
    /* bool_attr = True */
    s7_pointer path = s7_eval_c_string(s7, "'(:> 0 :@ 1)");
    s7_pointer binding = s7_apply_function(s7, ast, path);
    /* :value of binding == ID node; :$ of node == symbol 'True */
    s7_pointer valnode= s7_apply_function(s7, binding,
                                      s7_eval_c_string(s7, "'(:value)"));
    val = s7_apply_function(s7, valnode,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "True", s7_symbol_name(val));
    s7_pointer getter = s7_list(s7, 2, binding, s7_make_keyword(s7, "value"));

    /* (result (set! (binding :value) '(31 32)) */
    s7_pointer newval = s7_list(s7, 2,
                                s7_make_symbol(s7, "quote"),
                                s7_list(s7, 2,
                                        s7_make_integer(s7, 31),
                                        s7_make_integer(s7, 32)));
    result = s7_apply_function(s7, set_bang, s7_list(s7, 2, getter, newval));
    log_debug("result %s", s7_object_to_c_string(s7, result));
    val = s7_apply_function(s7, binding, s7_eval_c_string(s7, "'(:value)"));
    count = s7_apply_function(s7, length_s7, s7_list(s7, 1, binding));
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(count));
    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(0)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 31, s7_integer(item));
    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(1)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 32, s7_integer(item));
}

void test_set_bool_to_int_list4(void) {
    /* bool_attr = True */
    s7_pointer path = s7_eval_c_string(s7, "'(:> 0 :@ 1)");
    s7_pointer binding = s7_apply_function(s7, ast, path);
    /* :value of binding == ID node; :$ of node == symbol 'True */
    s7_pointer valnode= s7_apply_function(s7, binding,
                                      s7_eval_c_string(s7, "'(:value)"));
    val = s7_apply_function(s7, valnode,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "True", s7_symbol_name(val));
    s7_pointer getter = s7_list(s7, 2, binding, s7_make_keyword(s7, "value"));

    /* (result (set! (binding :value) '(41 42 43 44)) */
    s7_pointer newval = s7_list(s7, 2,
                                s7_make_symbol(s7, "quote"),
                                s7_list(s7, 4,
                                        s7_make_integer(s7, 41),
                                        s7_make_integer(s7, 42),
                                        s7_make_integer(s7, 43),
                                        s7_make_integer(s7, 44)));
    result = s7_apply_function(s7, set_bang, s7_list(s7, 2, getter, newval));
    log_debug("result %s", s7_object_to_c_string(s7, binding));
    val = s7_apply_function(s7, binding, s7_eval_c_string(s7, "'(:value)"));
    count = s7_apply_function(s7, length_s7, s7_list(s7, 1, val));
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(count));
    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(0)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 41, s7_integer(item));
    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(1)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 42, s7_integer(item));
    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(2)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 43, s7_integer(item));
    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(3)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 44, s7_integer(item));
}

/* **************************************************************** */
void test_set_string_list_to_bool(void) {
    /* int_list = [1, 2, 3], */
    char *cmd = "(set! (ast :> 0 :@ 'int_list :value) #t)";
    s7_pointer r = s7_eval_c_string(s7, cmd);
    log_debug("r: %s", s7_object_to_c_string(s7, r));
    s7_pointer path = s7_eval_c_string(s7, "'(:> 0 :@ int_list)");
    s7_pointer binding = s7_apply_function(s7, ast, path);
    log_debug("binding: %s", s7_object_to_c_string(s7, binding));
    s7_pointer valnode= s7_apply_function(s7, binding,
                                          s7_eval_c_string(s7, "'(:value)"));
    s7_pointer val= s7_apply_function(s7, valnode,
                                      s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "True", s7_symbol_name(val));

    /* val = s7_apply_function(s7, valnode, s7_eval_c_string(s7, "'(:$)")); */
    /* TEST_ASSERT_EQUAL_STRING( "True", s7_symbol_name(val)); */
    /* s7_pointer getter = s7_list(s7, 2, binding, s7_make_keyword(s7, "value")); */

    /* /\* (result (set! (binding :value) '(41 42 43 44)) *\/ */
    /* s7_pointer newval = s7_list(s7, 2, */
    /*                             s7_make_symbol(s7, "quote"), */
    /*                             s7_list(s7, 4, */
    /*                                     s7_make_integer(s7, 41), */
    /*                                     s7_make_integer(s7, 42), */
    /*                                     s7_make_integer(s7, 43), */
    /*                                     s7_make_integer(s7, 44))); */
    /* result = s7_apply_function(s7, set_bang, s7_list(s7, 2, getter, newval)); */
    /* log_debug("result %s", s7_object_to_c_string(s7, binding)); */
    /* val = s7_apply_function(s7, binding, s7_eval_c_string(s7, "'(:value)")); */
    /* count = s7_apply_function(s7, length_s7, s7_list(s7, 1, val)); */
    /* TEST_ASSERT_EQUAL_INT( 4, s7_integer(count)); */
    /* item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(0)")); */
    /* item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)")); */
    /* TEST_ASSERT_EQUAL_INT( 41, s7_integer(item)); */
    /* item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(1)")); */
    /* item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)")); */
    /* TEST_ASSERT_EQUAL_INT( 42, s7_integer(item)); */
    /* item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(2)")); */
    /* item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)")); */
    /* TEST_ASSERT_EQUAL_INT( 43, s7_integer(item)); */
    /* item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(3)")); */
    /* item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)")); */
    /* TEST_ASSERT_EQUAL_INT( 44, s7_integer(item)); */
}

/**************/
int main(void) {
    UNITY_BEGIN();
    /* RUN_TEST(test_set_bool_to_int); */
    /* RUN_TEST(test_set_bool_to_int_list1); */
    /* RUN_TEST(test_set_bool_to_int_list2); */
    /* RUN_TEST(test_set_bool_to_int_list4); */

    RUN_TEST(test_set_string_list_to_bool);
    return UNITY_END();
}
