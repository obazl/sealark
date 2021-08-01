#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "definitions.h"

UT_string *buf;
UT_string *test_s;

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

void test_bindings(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 1 :bindings)");
    s7_pointer bindings = s7_apply_function(s7, ast, path);

    /* check type, tid */
    TEST_ASSERT( !s7_is_c_object(bindings) );
    TEST_ASSERT( s7_is_list(s7, bindings) );
    TEST_ASSERT( s7_list_length(s7, bindings) == 3 );

    /* we can access a binding using a path expression, as in
       test_binding below. but here, since bindings is a Scheme
       list, we must use Scheme list operations: */
    s7_pointer binding;
    while ( !s7_is_null(s7, bindings) ) {
        binding = s7_car(bindings);
        TEST_ASSERT( s7_is_c_object(binding) );
        TEST_ASSERT( s7_c_object_type(binding) == AST_NODE_T );
        bindings = s7_cdr(bindings);
    }
}

/* :target 1:
cc_binary(
    name = "hello-world",
    srcs = ["hello-world.cc"],
    deps = [":hello-lib"],
)
*/
void test_binding(void) {
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
    RUN_TEST(test_bindings);
    /* RUN_TEST(test_binding); */
    return UNITY_END();
}
