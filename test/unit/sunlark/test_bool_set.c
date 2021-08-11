#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

#include "sealark.h"
#include "sunlark.h"

#include "test_bool_set.h"

UT_string *buf;
UT_string *test_s;

char *build_file = "test/unit/sunlark/BUILD.bool_set";

s7_scheme *s7;

/**************/
int main(void) {
    UNITY_BEGIN();
    /* RUN_TEST(test_set_bool_to_int); */
    /* RUN_TEST(test_set_bool_to_int_list1); */
    RUN_TEST(test_set_bool_to_int_list2);
    /* RUN_TEST(test_set_bool_to_int_list4); */

    /* RUN_TEST(test_set_bool_to_string); */
    /* RUN_TEST(test_set_bool_to_string_list1); */
    return UNITY_END();
}

/* **************************************************************** */
void test_set_bool_to_int(void) {
    /* binding1 = True */
    char *s = "'(:> \"bindings-set-1\" :@ binding1)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer item = s7_apply_function(s7, pkg, path);
    /* :value of item == ID node; :$ of node == bool #t (True in PKG) */
    s7_pointer val_s7= s7_apply_function(s7, item,
                                      s7_eval_c_string(s7, "'(:value)"));
    struct node_s *valnode = s7_c_object_value(val_s7);
    s7_pointer val = s7_apply_function(s7, val_s7,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_is_boolean(val));
    TEST_ASSERT( s7_t(s7) == val ); /* Scheme boolean */
    TEST_ASSERT_EQUAL_INT( 1, s7_boolean(s7,val)); /* C bool */
    TEST_ASSERT_EQUAL_STRING( "True", valnode->s ); /* Starlark bool */

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
    /* binding1 = True */
    s7_pointer path = s7_eval_c_string(s7, "'(:> 0 :@ 1)");
    s7_pointer binding = s7_apply_function(s7, pkg, path);
    /* :value of binding == ID node; :$ of node == symbol '#t */
    s7_pointer valnode= s7_apply_function(s7, binding,
                                      s7_eval_c_string(s7, "'(:value)"));
    s7_pointer val = s7_apply_function(s7, valnode,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_t(s7) == val );
    TEST_ASSERT_EQUAL_INT( 1, s7_boolean(s7, val));
    s7_pointer getter = s7_list(s7, 2, binding, s7_make_keyword(s7, "value"));

    /* (set! (binding :value) '(21)) */
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
    /* binding1 = True */
    char *s = "'(:> \"bindings-set-1\" :@ binding1)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer binding = s7_apply_function(s7, pkg, path);
    /* :value of binding == ID node; :$ of node == symbol '#t */
    s7_pointer valnode= s7_apply_function(s7, binding,
                                      s7_eval_c_string(s7, "'(:value)"));
    val = s7_apply_function(s7, valnode,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_t(s7) == val );
    TEST_ASSERT_EQUAL_INT( 1, s7_boolean(s7,val));

    /* REMINDER: lval getter must be a fn application, in parens */
    s7_pointer getter = s7_list(s7, 1,
                                s7_list(s7, 2,
                                        binding,
                                        s7_make_keyword(s7, "value")));

    /* (set! (binding :value) '(31 32)) */
    s7_pointer newval = s7_list(s7, 2,
                                s7_make_symbol(s7, "quote"),
                                s7_list(s7, 2,
                                        s7_make_integer(s7, 31),
                                        s7_make_integer(s7, 32)));
    result = s7_apply_function(s7, set_bang, s7_list(s7, 2, getter, newval));
    log_debug("result %s", s7_object_to_c_string(s7, result));
    val = s7_apply_function(s7, binding, s7_eval_c_string(s7, "'(:value)"));
    count = s7_apply_function(s7, length_proc, s7_list(s7, 1, binding));
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(count));
    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(0)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 31, s7_integer(item));

    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(:1)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 32, s7_integer(item));
}

void test_set_bool_to_int_list4(void) {
    /* binding1 = True */
    s7_pointer path = s7_eval_c_string(s7, "'(:> 0 :@ 1)");
    s7_pointer binding = s7_apply_function(s7, pkg, path);
    /* :value of binding == ID node; :$ of node == symbol '#t */
    s7_pointer valnode= s7_apply_function(s7, binding,
                                      s7_eval_c_string(s7, "'(:value)"));
    val = s7_apply_function(s7, valnode,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_t(s7) == val );
    TEST_ASSERT_EQUAL_INT( 1, s7_boolean(s7, val));
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
    s7_pointer count = s7_apply_function(s7, length_proc, s7_list(s7, 1, val));
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(count));
    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(0)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 41, s7_integer(item));
    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(:1)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 42, s7_integer(item));
    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(:2)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 43, s7_integer(item));
    item = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(:3)"));
    item = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_INT( 44, s7_integer(item));
}

/* **************************************************************** */
void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);

    pkg = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    s7_define_variable(s7, "pkg", pkg);
}

void tearDown(void) {
    s7_quit(s7);
}
