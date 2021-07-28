#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "vectors.h"

UT_string *buf;
UT_string *test_s;
UT_array  *result;

char *build_file = "test/unit/sunlark/BUILD.vectors";

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

void test_int_vector(void) {
    s7_pointer vec = sunlark_parse_string(s7,
                                          s7_make_string(s7, "[1, 2,\n\
 3\n\
]\n"));

    s7_pointer path = s7_eval_c_string(s7,
                       "'(0)");
    s7_pointer item = s7_apply_function(s7, vec, path);

    log_debug("item:\n%s", s7_object_to_c_string(s7, item));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(item) );
    TEST_ASSERT( sunlark_node_tid(s7, item) == TK_INT );

    struct node_s *item_node = s7_c_object_value(item);
    TEST_ASSERT( item_node->tid == TK_INT );
    TEST_ASSERT( strncmp(item_node->s, "1", 1) == 0 );
    TEST_ASSERT( strlen(item_node->s) == 1 );

    /* (eq? item 1) => #f, but (eq? (item :val) 1) => #t */
    s7_pointer eq_1 = s7_apply_function(s7, is_eq_s7,
                                        s7_list(s7, 2,
                                                item,
                                                s7_make_integer(s7, 1)));
    TEST_ASSERT( eq_1 == s7_t(s7) );
}

/* labels are syntactically same strings, but just in case we add
   label-aware ops */
void test_label_vector(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 0 :bindings string_vec :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_List_Expr );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_List_Expr );
}

void test_string_vector(void) {
    /* access a string-vector value */
    s7_pointer path = s7_eval_c_string(s7,
        "'(:target \"string-vectors\" :bindings string_veca :value)");
    s7_pointer svector = s7_apply_function(s7, ast, path);

    log_debug("svector:\n%s", s7_object_to_c_string(s7, svector));

    /* result is a Scheme vector of sunlark string c-objects */
    TEST_ASSERT( !s7_is_c_object(svector) );
    TEST_ASSERT( s7_is_vector(svector) );

    /* vector-ref - returns sunlark :string, not Scheme string */
    /* (svector 1) => sunlark :string */
    s7_pointer item1 = s7_apply_function(s7, svector,
                                     s7_cons(s7,
                                             s7_make_integer(s7, 1),
                                             s7_nil(s7)));
    TEST_ASSERT( s7_is_c_object(item1) );
    /* (item1 :string?) => #t */
    s7_pointer err = s7_eval_c_string(s7, "(lambda (type info) #f)");
    s7_pointer body = s7_list(s7, 2,
                              s7_make_symbol(s7, "vector?"),
                              svector);
                              /* s7_make_integer(1)); */
                              /* s7_make_keyword(s7, ":string?")); */
    s7_pointer is_s = s7_call_with_catch(s7, s7_t(s7), body, err);
    /* log_debug("is_s: %s", s7_object_to_c_string(s7, is_s)); */
    /* TEST_ASSERT(s7_is_boolean(is_s)); */

    /* TEST_ASSERT( is_s == s7_f(s7) ); */
    /* (item1 :print) => "a2" (Scheme string) */
    s7_pointer item1_str = s7_apply_function(s7, item1,
                                        s7_cons(s7,
                                             s7_make_keyword(s7, "print"),
                                             s7_nil(s7)));
    log_debug("item1 str: %s", s7_object_to_c_string(s7, item1_str));

    /* metadata: line, col */

}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_int_vector);
    /* RUN_TEST(test_string_vector); */
    return UNITY_END();
}
