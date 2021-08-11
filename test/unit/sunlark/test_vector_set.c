#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

#include "sealark.h"
#include "sunlark.h"

#include "test_vector_set.h"

char *build_file = "test/unit/sunlark/BUILD.vector_set";

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_path_5_ivec_set_first_item);
    RUN_TEST(test_path_5_ivec_set_item_1);
    RUN_TEST(test_path_5_ivec_set_last_item);

    /* RUN_TEST(test_path_5_svec_set_item_1); */
    /* RUN_TEST(test_path_5_svec_set_last_item); */
    return UNITY_END();
}

/* int_veca = [1, 2, 3], */
static char *int_veca = "'(:> \"int-vectors\" :@ int_veca :$)";
/* **************************************************************** */
/* **************************************************************** */
void test_path_5_ivec_set_first_item(void) {
    path = s7_eval_c_string(s7, int_veca);
    s7_pointer vec = s7_apply_function(s7, pkg, path);

    /* (sunlark-node? vec) */
    pred = s7_apply_function(s7, is_sunlark_node_proc,
                             s7_list(s7, 1, vec));
    TEST_ASSERT( pred == s7_t(s7) );

    /* (vec :list-expr?) */
    pred = s7_apply_function(s7, vec, list_expr_p);
    TEST_ASSERT( pred == s7_t(s7) );

    /* (vec :length) */
    s7_pointer count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(count) );

    /* verify all 3 items before set! */
    item = s7_apply_function(s7, vec, item_0_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 1, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_1_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_2_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(itemval) );

    getter = s7_list(s7, 2, vec, s7_make_keyword(s7, "0"));

    /* (set! (vec :1) :null) */
    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_integer(s7, 99)));

    /* (vec :length): same as before set! */
    count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(count) );

    /* verify new value found */
    char *verify   = "'(:> \"int-vectors\" :@ int_veca :$ 99)";
    s7_pointer get = s7_eval_c_string(s7, verify);
    path = s7_eval_c_string(s7, verify);
    s7_pointer items = s7_apply_function(s7, pkg, path);
    TEST_ASSERT_EQUAL_INT( 1, s7_list_length(s7, items) );

    /* verify all 3 items after set! */
    item = s7_apply_function(s7, vec, item_0_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 99, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_1_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_2_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(itemval) );
}

void test_path_5_ivec_set_item_1(void) {
    path = s7_eval_c_string(s7, int_veca);
    s7_pointer vec = s7_apply_function(s7, pkg, path);

    /* (sunlark-node? vec) */
    pred = s7_apply_function(s7, is_sunlark_node_proc,
                             s7_list(s7, 1, vec));
    TEST_ASSERT( pred == s7_t(s7) );

    /* (vec :list-expr?) */
    pred = s7_apply_function(s7, vec, list_expr_p);
    TEST_ASSERT( pred == s7_t(s7) );

    /* (vec :length) */
    s7_pointer count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(count) );

    /* verify all 3 items before set! */
    item = s7_apply_function(s7, vec, item_0_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 1, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_1_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_2_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(itemval) );

    getter = s7_list(s7, 2, vec, s7_make_keyword(s7, "1"));

    /* (set! (vec :1) :null) */
    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_integer(s7, 99)));

    /* (vec :length): same as before set! */
    count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(count) );

    /* verify new value found */
    char *verify   = "'(:> \"int-vectors\" :@ int_veca :$ 99)";
    s7_pointer get = s7_eval_c_string(s7, verify);
    path = s7_eval_c_string(s7, verify);
    s7_pointer items = s7_apply_function(s7, pkg, path);
    TEST_ASSERT_EQUAL_INT( 1, s7_list_length(s7, items) );

    /* verify all 3 items after set! */
    item = s7_apply_function(s7, vec, item_0_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 1, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_1_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 99, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_2_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(itemval) );
}

/* **************************************************************** */
/* using negative indexing: last item == -1 */
void test_path_5_ivec_set_last_item(void) {
    path = s7_eval_c_string(s7, int_veca);
    s7_pointer vec = s7_apply_function(s7, pkg, path);

    /* (sunlark-node? vec) */
    pred = s7_apply_function(s7, is_sunlark_node_proc,
                             s7_list(s7, 1, vec));
    TEST_ASSERT( pred == s7_t(s7) );

    /* (vec :list-expr?) */
    pred = s7_apply_function(s7, vec, list_expr_p);
    TEST_ASSERT( pred == s7_t(s7) );

    /* (vec :length) */
    s7_pointer count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(count) );

    /* verify all 3 items before set! */
    item = s7_apply_function(s7, vec, item_0_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 1, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_1_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_2_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(itemval) );

    /* last item index: -1 */
    getter = s7_list(s7, 2, vec, s7_make_keyword(s7, "-1"));

    /* (set! (vec :1) :null) */
    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_integer(s7, 99)));

    /* (vec :length): same as before set! */
    count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(count) );

    /* verify new value found */
    char *verify   = "'(:> \"int-vectors\" :@ int_veca :$ 99)";
    s7_pointer get = s7_eval_c_string(s7, verify);
    path = s7_eval_c_string(s7, verify);
    s7_pointer items = s7_apply_function(s7, pkg, path);
    TEST_ASSERT_EQUAL_INT( 1, s7_list_length(s7, items) );

    /* verify all 3 items after set! */
    item = s7_apply_function(s7, vec, item_0_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 1, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_1_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(itemval) );

    item = NULL; item = s7_apply_function(s7, vec, item_2_op);
    itemval = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_INT( 99, s7_integer(itemval) );
}

/* **************************************************************** */
/* string_veca = ["a0", "a1", "a2"], */
static char *string_veca = "'(:> \"string-vectors\" :@ string_veca :$)";
/* **************************************************************** */
/* **************************************************************** */
void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
    pkg = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
}

void tearDown(void) {
    s7_quit(s7);
}

