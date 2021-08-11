#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

#include "sealark.h"
#include "sunlark.h"

#include "test_vector_strings.h"

char *build_file = "test/unit/sunlark/BUILD.vectors";

int main(void) {
    UNITY_BEGIN();
    /* RUN_TEST(test_int_vector); */
    /* RUN_TEST(test_label_vector); */
    RUN_TEST(test_string_vector);
    return UNITY_END();
}

/* **************************************************************** */
void test_int_vector(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 0 :bindings string_vec :value)");
    s7_pointer bvalue = s7_apply_function(s7, pkg, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_List_Expr );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_List_Expr );
}

/* labels are syntactically same strings, but just in case we add
   label-aware ops */
void test_label_vector(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets 0 :bindings string_vec :value)");
    s7_pointer bvalue = s7_apply_function(s7, pkg, path);

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
        "'(:target \"string-vectors\" :binding string_veca :value)");
    s7_pointer svector = s7_apply_function(s7, pkg, path);

    log_debug("svector:\n%s", s7_object_to_c_string(s7, svector));

    TEST_ASSERT( s7_is_c_object(svector) );
    TEST_ASSERT( !s7_is_vector(svector) );

    /* vector-ref - returns sunlark :string, not Scheme string */
    /* (svector 1) => sunlark :string */
    s7_pointer item1 = s7_apply_function(s7, svector,
                                     s7_cons(s7,
                                             s7_make_keyword(s7, "1"),
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
