#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "paths_tetradic.h"

UT_string *buf;
UT_string *test_s;
static UT_array  *result;

char *build_file = "test/unit/sunlark/BUILD.test1";

s7_scheme *s7;

struct parse_state_s *parse_state;

static s7_pointer ast;
struct node_s *root;

void setUp(void) {
    s7 = sunlark_init();
    /* parse_state = sealark_parse_file(build_file); */
    /* ast = parse_state->root; */
    ast = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    root = s7_c_object_value(ast);
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);
    s7_quit(s7);
}

void test_target_for_index(void) {
    // (:targets 1 :bindings))
    struct node_s *target = sealark_target_for_index(root, 0);
    TEST_ASSERT_EQUAL_INT(4, sealark_target_bindings_count(target));
    TEST_ASSERT_EQUAL_INT(target->tid, TK_Call_Expr);

    target = sealark_target_for_index(root, 1);
    TEST_ASSERT_EQUAL_INT(3, sealark_target_bindings_count(target));
    TEST_ASSERT_EQUAL_INT(target->tid, TK_Call_Expr);
}

void test_s7(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 1 :bindings srcs :value 0)");
    s7_pointer result = s7_apply_function(s7, ast, path);
    TEST_ASSERT( s7_is_c_object(result));

    s7_pointer is_s = s7_apply_function(s7, result,
                                     s7_make_keyword(s7, "string?"));
    TEST_ASSERT(is_s == s7_t(s7));

    struct node_s *snode = s7_c_object_value(result);
    TEST_ASSERT(snode->tid == TK_STRING);
    TEST_ASSERT_EQUAL_STRING(snode->s, "hello-world.cc");

    /* char *s = sunlark_s_value_to_c_string(s7, result); */
    /* sunlark_to_c_string - straight concat, no whitespace  */

}


int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_target_for_index);
    /* RUN_TEST(test_s7); */
    return UNITY_END();
}
