#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "targets.h"

UT_string *buf;
UT_string *test_s;
UT_array  *result;

char *build_file = "test/unit/sunlark/BUILD.test1";

s7_scheme *s7;

struct parse_state_s *parse_state;

s7_pointer ast;
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

void test_targets(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets)");
    s7_pointer targets = s7_apply_function(s7, ast, path);

    /* check type, tid */
    TEST_ASSERT( !s7_is_c_object(targets) );
    TEST_ASSERT( s7_is_list(s7, targets) );
    TEST_ASSERT( s7_list_length(s7, targets) == 5 );

    /* we can access a single target using a path expression, as in
       test_target below. but here, since targets is a Scheme list,
       we must use Scheme list operations: */
    s7_pointer target;
    while ( !s7_is_null(s7, targets) ) {
        target = s7_car(targets);
        TEST_ASSERT( s7_is_c_object(target) );
        TEST_ASSERT( s7_c_object_type(target) == AST_NODE_T );
        targets = s7_cdr(targets);
    }
}

/* :target 1:
cc_binary(
    name = "hello-world",
    srcs = ["hello-world.cc"],
    deps = [":hello-lib"],
)
*/
void test_target(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 1)");
    s7_pointer target = s7_apply_function(s7, ast, path);

    /* check type, tid. We use :target, but in the ast TK_Call_Expr */
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Call_Expr );

    struct node_s *target_node = s7_c_object_value(target);
    TEST_ASSERT( target_node->tid == TK_Call_Expr );

    /* now check rule == "cc_binary" */
      s7_pointer rule
        = s7_apply_function(s7, target,
                            s7_cons(s7, s7_make_keyword(s7,"rule"),
                                    s7_nil(s7)));
    log_debug("rule: %s", s7_object_to_c_string(s7, rule));
    TEST_ASSERT( s7_is_c_object(rule) );
    TEST_ASSERT( !s7_is_string(rule) );
    TEST_ASSERT( sunlark_node_tid(s7, rule) == TK_ID );

    s7_pointer rule_str
        = s7_apply_function(s7, rule,
                            s7_cons(s7, s7_make_keyword(s7,"print"),
                                    s7_nil(s7)));
    log_debug("rule_str: %s", s7_object_to_c_string(s7, rule_str));
    log_debug("rule_strx: %s", s7_string(rule_str));

    TEST_ASSERT( !s7_is_c_object(rule_str) );
    TEST_ASSERT( s7_is_string(rule_str) );
    TEST_ASSERT_EQUAL_STRING( "cc_binary", s7_string(rule_str) );

    struct node_s *id_node = utarray_eltptr(target_node->subnodes, 0);
    TEST_ASSERT( id_node->tid == TK_ID );
    TEST_ASSERT( strncmp(id_node->s, "cc_binary", 9) == 0);
    TEST_ASSERT( strlen(id_node->s) == 9);

    /* check target name == "hello-world" */
    s7_pointer name
        = s7_apply_function(s7, target,
                            s7_cons(s7, s7_make_keyword(s7,"name"),
                                    s7_nil(s7)));
    log_debug("name: %s", s7_object_to_c_string(s7, name));
    TEST_ASSERT( s7_is_c_object(name) );
    TEST_ASSERT( sunlark_node_tid(s7, name) == TK_STRING );

    /* bindings */
    s7_pointer bindings
        = s7_apply_function(s7, target,
                            s7_cons(s7, s7_make_keyword(s7,"bindings"),
                                    s7_nil(s7)));
    TEST_ASSERT( !s7_is_c_object(bindings) );
    TEST_ASSERT( s7_is_list(s7, bindings) );
    TEST_ASSERT( s7_list_length(s7, bindings) == 3 );
}

int main(void) {
    UNITY_BEGIN();
    /* RUN_TEST(test_targets); */
    RUN_TEST(test_target);
    return UNITY_END();
}
