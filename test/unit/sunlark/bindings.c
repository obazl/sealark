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

char *build_file = "test/unit/sunlark/BUILD.test1";

s7_scheme *s7;

/* struct parse_state_s *parse_state; */

s7_pointer ast;
struct node_s *root;

s7_pointer is_eq_s7;
s7_pointer is_equal_s7;

void setUp(void) {
    s7 = sunlark_init();
    is_eq_s7 = s7_name_to_value(s7, "eq?");
    is_equal_s7 = s7_name_to_value(s7, "equal?");
    ast = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    root = s7_c_object_value(ast);
}

void tearDown(void) {
    s7_quit(s7);
}

void test_bindings(void) {
    s7_pointer path1 = s7_eval_c_string(s7, "'(:targets 1 :bindings)");
    s7_pointer bindings1 = s7_apply_function(s7, ast, path1);
    /* check type, tid */
    TEST_ASSERT( !s7_is_c_object(bindings1) );
    TEST_ASSERT( s7_is_list(s7, bindings1) );
    TEST_ASSERT( s7_list_length(s7, bindings1) == 3 );

    s7_pointer path2 = s7_eval_c_string(s7, "'(:targets 1 :bindings)");
    s7_pointer bindings2 = s7_apply_function(s7, ast, path2);
    TEST_ASSERT( !s7_is_c_object(bindings2) );
    TEST_ASSERT( s7_is_list(s7, bindings2) );
    TEST_ASSERT( s7_list_length(s7, bindings2) == 3 );

    /* from same path we get equal? but not eq? */
    s7_pointer are_eq = s7_apply_function(s7, is_eq_s7,
                                          s7_list(s7, 2,
                                                  bindings1, bindings2));
    TEST_ASSERT( are_eq == s7_f(s7) );
    s7_pointer are_equal = s7_apply_function(s7, is_equal_s7,
                                          s7_list(s7, 2,
                                                  bindings1, bindings2));
    TEST_ASSERT( are_equal == s7_t(s7) );

    /* underlying c-objects: different arrays with same vals */
    /* equality over list elements (nodes) */
    s7_pointer binding1, binding2;
    struct node_s *binding1_o;
    struct node_s *binding2_o;
    while ( !s7_is_null(s7, bindings1) ) {
        binding1 = s7_car(bindings1);
        binding2 = s7_car(bindings2);
        TEST_ASSERT( s7_is_c_object(binding1) );
        TEST_ASSERT( s7_is_c_object(binding2) );
        TEST_ASSERT( s7_c_object_type(binding1) == AST_NODE_T );
        TEST_ASSERT( s7_c_object_type(binding2) == AST_NODE_T );

        are_eq = s7_apply_function(s7, is_eq_s7,
                                   s7_list(s7, 2,
                                           binding1, binding2));
        TEST_ASSERT( are_eq == s7_f(s7) ); /* not eq? */

        are_equal = s7_apply_function(s7, is_equal_s7,
                                      s7_list(s7, 2,
                                              binding1, binding2));
        TEST_ASSERT( are_equal == s7_t(s7) ); /* are equal? */

        binding1_o = s7_c_object_value(binding1);
        binding2_o = s7_c_object_value(binding2);
        TEST_ASSERT( binding1_o != binding2_o );

        TEST_ASSERT( binding1_o->tid == binding2_o->tid );
        TEST_ASSERT( &(binding1_o->tid) != &(binding2_o->tid) );

        TEST_ASSERT( binding1_o->subnodes == binding2_o->subnodes );
        TEST_ASSERT( utarray_len(binding1_o->subnodes)
                     == utarray_len(binding2_o->subnodes) );

        bindings1 = s7_cdr(bindings1);
        bindings2 = s7_cdr(bindings2);
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

void test_binding_key(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 1 :bindings srcs :key)");
    s7_pointer bkey = s7_apply_function(s7, ast, path);

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bkey) );
    TEST_ASSERT( sunlark_node_tid(s7, bkey) == TK_ID );
    log_debug("bkey: %s", s7_object_to_c_string(s7, bkey));

    struct node_s *bkey_node = s7_c_object_value(bkey);
    TEST_ASSERT( bkey_node->tid == TK_ID );
    TEST_ASSERT( strncmp(bkey_node->s, "srcs", 4) == 0);
    TEST_ASSERT( strlen(bkey_node->s) == 4);

}

void test_binding_value_string_plain_dq(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 2 :bindings plaindq :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote plain */
    TEST_ASSERT( bvalue_node->qtype & DQUOTE );

    /* use :print to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue,
                            s7_cons(s7, s7_make_keyword(s7, "print"),
                                    s7_nil(s7)));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "\"I am a plain double-quoted string\"",
                              s7_string(bvalue_str) );
}

void test_binding_value_string_raw_dq(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 2 :bindings plaindq :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote plain */
    TEST_ASSERT( bvalue_node->qtype & DQUOTE );

    /* use :print to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue,
                            s7_cons(s7, s7_make_keyword(s7, "print"),
                                    s7_nil(s7)));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "\"I am a plain double-quoted string\"",
                              s7_string(bvalue_str) );
}

void test_binding_value_string_plain_sq(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 2 :bindings plainsq :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_STRING );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_STRING );

    /* verify qtype: single quote plain */
    TEST_ASSERT( bvalue_node->qtype & SQUOTE );

    /* use :print to get a string value */
    s7_pointer bvalue_str
        = s7_apply_function(s7, bvalue,
                            s7_cons(s7, s7_make_keyword(s7, "print"),
                                    s7_nil(s7)));
    log_debug("bvalue_s: %s", s7_object_to_c_string(s7, bvalue_str));
    TEST_ASSERT( !s7_is_c_object(bvalue_str) );
    TEST_ASSERT( s7_is_string(bvalue_str) );
    TEST_ASSERT_EQUAL_STRING( "'I am a plain single-quoted string'",
                              s7_string(bvalue_str) );
}

void test_binding_value_vector(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 1 :bindings srcs :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_List_Expr );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_List_Expr );
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_bindings);
    RUN_TEST(test_binding);
    RUN_TEST(test_binding_key);
    RUN_TEST(test_binding_value_string_plain_dq);
    RUN_TEST(test_binding_value_string_plain_sq);
    /* RUN_TEST(test_binding_value_vector); */
    return UNITY_END();
}
