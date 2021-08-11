#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

#include "sealark.h"
#include "sunlark.h"

#include "test_vector_rm.h"

char *build_file = "test/unit/sunlark/BUILD.vector_set";

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_path_5_ivec_rm_first_item);
    RUN_TEST(test_path_5_ivec_rm_item_1);
    RUN_TEST(test_path_5_ivec_rm_last_item);
    RUN_TEST(test_path_5_ivec_rm_all);

    RUN_TEST(test_path_5_svec_rm_first_item);
    RUN_TEST(test_path_5_svec_rm_item_1);
    RUN_TEST(test_path_5_svec_rm_last_item);
    RUN_TEST(test_path_5_svec_rm_all);
    return UNITY_END();
}

/* int_veca = [1, 2, 3], */
static char *int_veca = "'(:> \"int-vectors\" :@ int_veca :$)";
/* **************************************************************** */
void test_path_5_ivec_rm_first_item(void) {
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

    /* first item: :0 */
    getter = s7_list(s7, 2, vec, s7_make_keyword(s7, "0"));

    /* (set! (vec :1) :null) */
    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_keyword(s7, "null")));

    /* (vec :length) */
    count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(count) );

    /* deleted item not found */
    char *verify   = "'(:> \"int-vectors\" :@ int_veca :$ 1)";
    s7_pointer get = s7_eval_c_string(s7, verify);
    path = s7_eval_c_string(s7, verify);
    s7_pointer items = s7_apply_function(s7, pkg, path);
    TEST_ASSERT_EQUAL_INT( 0, s7_list_length(s7, items) );
}

void test_path_5_ivec_rm_item_1(void) {
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

    getter = s7_list(s7, 2, vec, s7_make_keyword(s7, "1"));

    /* (set! (vec :1) :null) */
    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_keyword(s7, "null")));

    /* (vec :length) */
    count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(count) );

    /* deleted item not found */
    char *verify   = "'(:> \"int-vectors\" :@ int_veca :$ 2)";
    s7_pointer get = s7_eval_c_string(s7, verify);
    path = s7_eval_c_string(s7, verify);
    s7_pointer items = s7_apply_function(s7, pkg, path);
    TEST_ASSERT_EQUAL_INT( 0, s7_list_length(s7, items) );
}

void test_path_5_ivec_rm_last_item(void) {
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

    getter = s7_list(s7, 2, vec, s7_make_keyword(s7, "-1"));

    /* (set! (vec :1) :null) */
    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_keyword(s7, "null")));

    /* (vec :length) */
    count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(count) );

    /* deleted item not found */
    char *verify   = "'(:> \"int-vectors\" :@ int_veca :$ 3)";
    s7_pointer get = s7_eval_c_string(s7, verify);
    path = s7_eval_c_string(s7, verify);
    s7_pointer items = s7_apply_function(s7, pkg, path);
    TEST_ASSERT_EQUAL_INT( 0, s7_list_length(s7, items) );
}

void test_path_5_ivec_rm_all(void) {
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

    /* val :* selects all items */
    getter = s7_list(s7, 2, vec, s7_make_keyword(s7, "*"));

    /* (set! (vec :1) :null) */
    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_keyword(s7, "null")));

    /* (vec :length) */
    count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 0, s7_integer(count) );

    /* deleted item not found */
    char *verify   = "'(:> \"int-vectors\" :@ int_veca :$ 2)";
    s7_pointer get = s7_eval_c_string(s7, verify);
    path = s7_eval_c_string(s7, verify);
    s7_pointer items = s7_apply_function(s7, pkg, path);
    TEST_ASSERT_EQUAL_INT( 0, s7_list_length(s7, items) );
}

/* **************************************************************** */
/* string_veca = ["a0", "a1", "a2"], */
static char *string_veca = "'(:> \"string-vectors\" :@ string_veca :$)";
/* **************************************************************** */
void test_path_5_svec_rm_first_item(void) {
    path = s7_eval_c_string(s7, string_veca);
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

    /* first item: "a0" ast posn 0 */
    getter = s7_list(s7, 2, vec, s7_make_keyword(s7, "0"));

    /* (set! (vec :1) :null) */
    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_keyword(s7, "null")));

    /* (vec :length) */
    count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(count) );

    /* deleted item not found */
    char *verify   = "'(:> \"string-vectors\" :@ string_veca :$ \"a0\")";
    s7_pointer get = s7_eval_c_string(s7, verify);
    path = s7_eval_c_string(s7, verify);
    s7_pointer items = s7_apply_function(s7, pkg, path);
    TEST_ASSERT_EQUAL_INT( 0, s7_list_length(s7, items) );
}

void test_path_5_svec_rm_item_1(void) {
    path = s7_eval_c_string(s7, string_veca);
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

    /* item 1: "a1" at posn :1 */
    getter = s7_list(s7, 2, vec, s7_make_keyword(s7, "1"));

    /* (set! (vec :1) :null) */
    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_keyword(s7, "null")));

    /* (vec :length) */
    count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(count) );

    /* deleted item not found */
    char *verify   = "'(:> \"string-vectors\" :@ string_veca :$ \"a1\")";
    s7_pointer get = s7_eval_c_string(s7, verify);
    path = s7_eval_c_string(s7, verify);
    s7_pointer items = s7_apply_function(s7, pkg, path);
    TEST_ASSERT_EQUAL_INT( 0, s7_list_length(s7, items) );
}

void test_path_5_svec_rm_last_item(void) {
    path = s7_eval_c_string(s7, string_veca);
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

    /* last item: "a2" at posn :-1 */
    getter = s7_list(s7, 2, vec, s7_make_keyword(s7, "-1"));

    /* (set! (vec :1) :null) */
    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_keyword(s7, "null")));

    /* (vec :length) */
    count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(count) );

    /* deleted item not found */
    char *verify   = "'(:> \"string-vectors\" :@ string_veca :$ \"a2\")";
    s7_pointer get = s7_eval_c_string(s7, verify);
    path = s7_eval_c_string(s7, verify);
    s7_pointer items = s7_apply_function(s7, pkg, path);
    TEST_ASSERT_EQUAL_INT( 0, s7_list_length(s7, items) );
}

/* ****************************** */
void test_path_5_svec_rm_all(void) {
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

    /* val :* selects all items */
    getter = s7_list(s7, 2, vec, s7_make_keyword(s7, "*"));

    /* (set! (vec :1) :null) */
    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_keyword(s7, "null")));

    /* (vec :length) */
    count = s7_apply_function(s7, vec, length_op);
    TEST_ASSERT_EQUAL_INT( 0, s7_integer(count) );

    /* deleted item not found */
    char *verify   = "'(:> \"int-vectors\" :@ int_veca :$ 2)";
    //s7_pointer get = s7_eval_c_string(s7, verify);
    s7_pointer get = s7_eval_c_string(s7, verify);
    s7_pointer items = s7_apply_function(s7, pkg, get);
    TEST_ASSERT_EQUAL_INT( 0, s7_list_length(s7, items) );
}

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

