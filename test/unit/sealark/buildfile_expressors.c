#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "sealark.h"

#include "buildfile_expressors.h"

UT_string *buf;
UT_string *test_s;
UT_array  *result;

char *build_file = "test/data/cc/BUILD.test";

struct node_s *parse_state;

struct node_s *ast;

void setUp(void) {
    ast = sealark_parse_file(build_file);
    /* ast = parse_state->root; */
}

void tearDown(void) {
    /* sealark_parse_state_free(parse_state); */
    sealark_node_free(ast);
}

void test_target_for_index(void) {
    // (:targets 1 :bindings))
    struct node_s *target = sealark_target_for_index(ast, 0);
    TEST_ASSERT_EQUAL_INT(4, sealark_target_bindings_count(target));
    TEST_ASSERT_EQUAL_INT(target->tid, TK_Call_Expr);

    target = sealark_target_for_index(ast, 1);
    TEST_ASSERT_EQUAL_INT(3, sealark_target_bindings_count(target));
    TEST_ASSERT_EQUAL_INT(target->tid, TK_Call_Expr);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_target_for_index);
    return UNITY_END();
}
