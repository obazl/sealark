#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_binding_get.h"

UT_string *buf;
UT_string *test_s;
/* UT_array  *result; */

char *build_file = "test/unit/sunlark/BUILD.binding_get";

s7_scheme *s7;

/* struct parse_state_s *parse_state; */

static s7_pointer ast;
struct node_s *root;

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_forall_targets_forall_bindings);
    /* RUN_TEST(test_binding_lists); */
    /* RUN_TEST(test_bindings_foreach); */
    /* RUN_TEST(test_binding_srcs); */
    /* RUN_TEST(test_binding_key); */
    /* RUN_TEST(test_binding_value_selectors); */
    RUN_TEST(test_binding_value_selector_dollar);
    /* RUN_TEST(test_binding_value_vector); */
    /* RUN_TEST(test_binding_predicate); */
    return UNITY_END();
}

/* **************************************************************** */
void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
    ast = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    root = s7_c_object_value(ast);
}

void tearDown(void) {
    s7_quit(s7);
}

void test_forall_targets_forall_bindings(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:targets :bindings)");
    s7_pointer targets = s7_apply_function(s7, ast, path);

    /* check type, tid */
    TEST_ASSERT( !s7_is_c_object(targets) );
    TEST_ASSERT( s7_is_list(s7, targets) );
    log_debug("len 1: %d", s7_list_length(s7, targets));
    TEST_ASSERT( s7_list_length(s7, targets) == 18 );

    /* same with concise op */
    s7_pointer path2 = s7_eval_c_string(s7,
                       "'(:>> :@@)");
    s7_pointer targets2 = s7_apply_function(s7, ast, path2);

    /* check type, tid */
    TEST_ASSERT( !s7_is_c_object(targets2) );
    TEST_ASSERT( s7_is_list(s7, targets2) );
    log_debug("len 2: %d", s7_list_length(s7, targets2));
    TEST_ASSERT( s7_list_length(s7, targets2) == 18 );

    /* we can access a single target using a path expression, as in
       test_target below. but here, since targets is a Scheme list,
       we must use Scheme list operations: */
    s7_pointer target;
    while ( !s7_is_null(s7, targets) ) {
        target = s7_car(targets);
        TEST_ASSERT( s7_is_c_object(target) );
        TEST_ASSERT( s7_c_object_type(target) == AST_NODE_T );
        /* (sunlark-node? target) => #t */
        s7_pointer is_node = s7_apply_function(s7, is_sunlark_node_proc,
                                               s7_cons(s7, target,
                                                       s7_nil(s7)));
        TEST_ASSERT( is_node == s7_t(s7) );
        /* (list? target) => #f */
        s7_pointer is_list = s7_apply_function(s7, is_list_s7,
                                               s7_cons(s7, target,
                                                       s7_nil(s7)));
        TEST_ASSERT( is_list == s7_f(s7) );

        targets = s7_cdr(targets);
    }
}

void test_map_over_bindings(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:>> :@@)");
    s7_pointer targets = s7_apply_function(s7, ast, path);

    /* check type, tid */
    TEST_ASSERT( !s7_is_c_object(targets) );
    TEST_ASSERT( s7_is_list(s7, targets) );
    TEST_ASSERT( s7_list_length(s7, targets) == 6 );

    /* we can access a single target using a path expression, as in
       test_target below. but here, since targets is a Scheme list,
       we must use Scheme list operations: */
    s7_pointer target;
    while ( !s7_is_null(s7, targets) ) {
        target = s7_car(targets);
        TEST_ASSERT( s7_is_c_object(target) );
        TEST_ASSERT( s7_c_object_type(target) == AST_NODE_T );
        /* (sunlark-node? target) => #t */
        s7_pointer is_node = s7_apply_function(s7, is_sunlark_node_proc,
                                               s7_cons(s7, target,
                                                       s7_nil(s7)));
        TEST_ASSERT( is_node == s7_t(s7) );
        /* (list? target) => #f */
        s7_pointer is_list = s7_apply_function(s7, is_list_s7,
                                               s7_cons(s7, target,
                                                       s7_nil(s7)));
        TEST_ASSERT( is_list == s7_f(s7) );

        targets = s7_cdr(targets);
    }
}

void test_binding_lists(void) {
    s7_pointer path1 = s7_eval_c_string(s7, "'(:target 1 :bindings)");
    s7_pointer bindings1 = s7_apply_function(s7, ast, path1);

    s7_pointer ct = s7_apply_function(s7,
                                      s7_eval_c_string(s7, "length"),
                                      s7_list(s7, 1, bindings1));
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(ct) );

    /* sealark_debug_log_ast_outline(s7_c_object_value(bindings1), 0); */
    /* (bindings1 :arg-list?) =? #t */
    s7_pointer pred = s7_apply_function(s7, bindings1,
                             s7_eval_c_string(s7, "'(:arg-list?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    /* (bindings1 :bindings?) =? #t */
    pred = s7_f(s7);
    pred = s7_apply_function(s7, bindings1,
                             s7_eval_c_string(s7, "'(:bindings?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bindings1) );
    TEST_ASSERT( !s7_is_list(s7, bindings1) );

    /* same again, for equality testing */
    s7_pointer path2 = s7_eval_c_string(s7, "'(:target 1 :bindings)");
    s7_pointer bindings2 = s7_apply_function(s7, ast, path2);
    TEST_ASSERT( s7_is_c_object(bindings2) );
    TEST_ASSERT( !s7_is_list(s7, bindings2) );

    /* from same path we get equal? but not eq? */
    s7_pointer equality = s7_apply_function(s7, s7_eval_c_string(s7, "eq?"),
                                            s7_list(s7, 2, bindings1, bindings2));
    TEST_ASSERT( equality == s7_f(s7) );
    equality = s7_f(s7);
    equality = s7_apply_function(s7, s7_eval_c_string(s7, "equal?"),
                                 s7_list(s7, 2, bindings1, bindings2));
    TEST_ASSERT( equality == s7_t(s7) );
}

void test_bindings_foreach(void) {
    /* binding (arg) lists are not Scheme lists nor vectors, but they
       are s7-iterable; (for-each (lambda (binding) ...) bindings1) */
    /* s7_pointer binding1, binding2; */

    s7_pointer path = s7_eval_c_string(s7, "'(:target 1 :bindings)");
    s7_pointer bindings = s7_apply_function(s7, ast, path);

    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, bindings);
    s7_pointer binding = s7_iterate(s7, iter);
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        TEST_ASSERT( s7_is_c_object(binding) );
        TEST_ASSERT( !s7_is_list(s7, binding) );
        pred = s7_apply_function(s7,
                                 binding,
                                 s7_eval_c_string(s7, "'(:binding?)"));
        TEST_ASSERT( pred == s7_t(s7) );

        binding = s7_iterate(s7, iter);
    }
}

void test_binding_srcs(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:> 1 :@ srcs)");
    s7_pointer binding = s7_apply_function(s7, ast, path);

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
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT( sunlark_node_tid(s7, key) == TK_ID );
    /* key is an identifier, hence :$ returns a Scheme symbol */
    s7_pointer str = s7_apply_function(s7, key, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_is_symbol(str) );
    TEST_ASSERT_EQUAL_STRING( "srcs", s7_symbol_name(str) );

    /* check underlying c object */
    struct node_s *id_node = utarray_eltptr(binding_node->subnodes, 0);
    TEST_ASSERT( id_node->tid == TK_ID );
    TEST_ASSERT( strncmp(id_node->s, "srcs", 4) == 0);
    TEST_ASSERT( strlen(id_node->s) == 4);

    /* check val == ["hello-world.cc"] */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_cons(s7, s7_make_keyword(s7,"value"),
                                    s7_nil(s7)));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_List_Expr );

    /* in this case val is a vector; index into it */
    s7_pointer item = s7_apply_function(s7, val,
                                        s7_cons(s7, s7_make_keyword(s7, "0"),
                                                s7_nil(s7)));
    TEST_ASSERT( s7_is_c_object(item) );
    TEST_ASSERT( sunlark_node_tid(s7, item) == TK_STRING );

    s7_pointer sval = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(sval) );
    TEST_ASSERT( s7_is_string(sval) );
    TEST_ASSERT_EQUAL_STRING( "\"hello-world.cc\"", s7_string(sval) );

    item = s7_apply_function(s7, val,
                             s7_list(s7,1,s7_make_keyword(s7, "1")));
    TEST_ASSERT( s7_is_c_object(item) );
    TEST_ASSERT( sunlark_node_tid(s7, item) == TK_STRING );

    /* use :print to get a string value */
    sval = NULL;
    sval = s7_apply_function(s7, item, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(sval) );
    TEST_ASSERT( s7_is_string(sval) );
    /* single-quotes */
    TEST_ASSERT_EQUAL_STRING( "'hello-singlequotes.cc'", s7_string(sval) );
}

void test_binding_key(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:> 1 :@ srcs :key)");
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

void test_binding_value_vector(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> 1 :@ srcs :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);

    log_debug("bvalue:\n%s", s7_object_to_c_string(s7, bvalue));

    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_List_Expr );

    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_List_Expr );
}

void test_binding_value_selectors(void) {
    /* srcs = ["hello-world.cc", 'hello-singlequotes.cc'], */
    s7_pointer path = s7_eval_c_string(s7, "'(:> 1 :@ srcs :value)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);
    s7_pointer blen = s7_apply_function(s7, s7_name_to_value(s7, "length"),
                                        s7_list(s7,1,bvalue));
    TEST_ASSERT_EQUAL(2, s7_integer(blen));

    path = bvalue = blen = NULL;
    path = s7_eval_c_string(s7, "'(:> 1 :@ srcs :value :1)");
    bvalue = s7_apply_function(s7, ast, path);
    s7_pointer pred = s7_apply_function(s7, bvalue,
                                        s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( s7_t(s7) == pred );
    s7_pointer str = s7_apply_function(s7, bvalue,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "'hello-singlequotes.cc'", s7_string(str) );
}

void test_binding_value_selector_dollar(void) {
    /* test :$ for :value */
    /* srcs = ["hello-world.cc", 'hello-singlequotes.cc'], */
    s7_pointer path = s7_eval_c_string(s7, "'(:> 1 :@ srcs :$)");
    s7_pointer bvalue = s7_apply_function(s7, ast, path);
    TEST_ASSERT( s7_is_c_object(bvalue) );
    TEST_ASSERT( sunlark_node_tid(s7, bvalue) == TK_List_Expr );
    struct node_s *bvalue_node = s7_c_object_value(bvalue);
    TEST_ASSERT( bvalue_node->tid == TK_List_Expr );
    s7_pointer blen = s7_apply_function(s7, s7_name_to_value(s7, "length"),
                                        s7_list(s7,1,bvalue));
    TEST_ASSERT_EQUAL(2, s7_integer(blen));

    path = bvalue = blen = NULL;
    path = s7_eval_c_string(s7, "'(:> 1 :@ srcs :$ :1)");
    bvalue = s7_apply_function(s7, ast, path);
    s7_pointer pred = s7_apply_function(s7, bvalue,
                                        s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( s7_t(s7) == pred );
    s7_pointer str = s7_apply_function(s7, bvalue,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "'hello-singlequotes.cc'", s7_string(str) );
}

/* (v (ast :target 1 :@ 'srcs :value 0)) */
void test_binding_predicate(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:> \"hello-lib\" :@ srcs :value 0)");
    s7_pointer item = s7_apply_function(s7, ast, path);
    /* log_debug("item: %s", s7_object_to_c_string(s7, item)); */
    /* check type, tid */
    TEST_ASSERT( s7_is_c_object(item) );
    /* log_debug("item tid: %s", s7_object_to_c_string(s7, item)); */
    TEST_ASSERT( sunlark_node_tid(s7, item) == TK_STRING );

    struct node_s *item_node = s7_c_object_value(item);
    TEST_ASSERT( item_node->tid == TK_STRING );
}
