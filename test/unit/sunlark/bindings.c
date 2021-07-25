#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "bindings.h"

UT_string *buf;
UT_string *test_s;
UT_array  *result;

char *build_file = "test/unit/sunlark/BUILD.paths_tetradic";

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

/* :target 1:
cc_binary(
    name = "hello-world",
    srcs = ["hello-world.cc"],
    deps = [":hello-lib"],
)
*/
void test_bindings(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 1 :bindings srcs)");
    s7_pointer binding = s7_apply_function(s7, ast, path);

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );

    struct node_s *binding_node = s7_c_object_value(binding);
    TEST_ASSERT( binding_node->tid == TK_Binding );

    /* now check key == "srcs" */
    /* ((binding :key) :tid->kw) => :id */
    s7_pointer key
        = s7_apply_function(s7, binding,
                            s7_cons(s7, s7_make_keyword(s7,"key"),
                                    s7_nil(s7)));
    log_debug("key: %s", s7_object_to_c_string(s7, key));
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT( sunlark_node_tid(s7, key) == TK_ID );

    struct node_s *id_node = utarray_eltptr(binding_node->subnodes, 0);
    TEST_ASSERT( id_node->tid == TK_ID );
    TEST_ASSERT( strncmp(id_node->s, "srcs", 4) == 0);
    TEST_ASSERT( strlen(id_node->s) == 4);

    /* check val == ["hello-world.cc"] */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_cons(s7, s7_make_keyword(s7,"value"),
                                    s7_nil(s7)));
    log_debug("val: %s", s7_object_to_c_string(s7, val));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_List_Expr );

    /* in this case val is a vector; index into it */
    s7_pointer item
        = s7_apply_function(s7, val,
                            s7_cons(s7, s7_make_integer(s7, 0),
                                    s7_nil(s7)));
    log_debug("item: %s", s7_object_to_c_string(s7, item));
    TEST_ASSERT( s7_is_c_object(item) );
    TEST_ASSERT( sunlark_node_tid(s7, item) == TK_STRING );

    /* use :print to get a string value */
    s7_pointer sval
        = s7_apply_function(s7, item,
                            s7_cons(s7, s7_make_keyword(s7, "print"),
                                    s7_nil(s7)));
    log_debug("sval: %s", s7_object_to_c_string(s7, sval));
    TEST_ASSERT( !s7_is_c_object(sval) );
    TEST_ASSERT( s7_is_string(sval) );
}

int main(void) {
    UNITY_BEGIN();
    /* RUN_TEST(test_target_for_index); */
    /* RUN_TEST(test_s7); */
    RUN_TEST(test_bindings);
    return UNITY_END();
}
