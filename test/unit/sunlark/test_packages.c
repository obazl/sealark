#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_packages.h"

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_pkg_targets); /* (pkg :targets) */
    RUN_TEST(test_pkg_targets_attrs); /* (pkg :targets :attrs) */
    RUN_TEST(test_pkg_targets_bindings); /* (pkg :targets :bindings) */
    RUN_TEST(test_pkg_targets_atat); /* (pkg :targets :atat) */

    RUN_TEST(test_pkg_tgts);          /* (pkg :>>) */
    RUN_TEST(test_pkg_target_int);    /* (pkg :target 0) */
    RUN_TEST(test_pkg_tgt_int);       /* (pkg :> 0) */
    RUN_TEST(test_pkg_target_string); /* (pkg :target "hello-lib") */
    RUN_TEST(test_pkg_tgt_string);    /* (pkg :> "hello-lib")  */

    RUN_TEST(test_pkg_target_string_rule); /* (pkg :> "hello-lib" :rule)*/
    RUN_TEST(test_pkg_target_int_rule);    /* (pkg :> 0 :rule) */

    RUN_TEST(test_pkg_target_string_name); /*(pkg :> "hello-lib" :name)*/
    RUN_TEST(test_pkg_target_int_name);    /* (pkg :> 0 name)  */

    RUN_TEST(test_pkg_target_string_attrs);/*(pkg :> "hello-lib" :attrs)*/
    /* (pkg :> "hello-lib" :bindings) */
    RUN_TEST(test_pkg_target_string_bindings);
    RUN_TEST(test_pkg_target_string_atat); /* (pkg :> "hello-lib" :@@) */

    RUN_TEST(test_pkg_target_int_attrs);    /* (pkg :> 0 :attrs)  */
    RUN_TEST(test_pkg_target_int_bindings); /* (pkg :> 0 :bindings)  */
    RUN_TEST(test_pkg_target_int_atat);     /* (pkg :> 0 :@@)  */

    /* (pkg :target "hello-lib" :attr 'srcs) */
    RUN_TEST(test_pkg_target_string_attr_sym);
    /* (pkg :> "hello-lib" :attr 'srcs) */
    RUN_TEST(test_pkg_tgt_string_attr_sym);

    /* (pkg :target "hello-lib" :binding 'srcs) */
    RUN_TEST(test_pkg_target_string_binding_sym);
    /* (pkg :> "hello-lib" :binding 'srcs) */
    RUN_TEST(test_pkg_tgt_string_binding_sym);

    /* (pkg :target "hello-lib" :@ 'srcs) */
    RUN_TEST(test_pkg_target_string_at_sym);
    /* (pkg :> "hello-lib" :@ 'srcs) */
    RUN_TEST(test_pkg_tgt_string_at_sym);

    /* (pkg :target 0 :attr 'srcs) */
    RUN_TEST(test_pkg_target_int_attr_sym);
    /* (pkg :> 0 :attr 'srcs) */
    RUN_TEST(test_pkg_tgt_int_attr_sym);

    /* (pkg :target 0 :binding 'srcs) */
    RUN_TEST(test_pkg_target_int_binding_sym);
    /* (pkg :> 0 :binding 'srcs) */
    RUN_TEST(test_pkg_tgt_int_binding_sym);

    /* (pkg :target 0 :@ 'srcs) */
    RUN_TEST(test_pkg_target_int_at_sym);
    /* (pkg :> 0 :@ 'srcs) */
    RUN_TEST(test_pkg_tgt_int_at_sym);

    RUN_TEST(test_target_string_parse);
    return UNITY_END();
}

/* **************************************************************** */
void _validate_pkg_targets(s7_pointer targets) {
    TEST_ASSERT( !s7_is_c_object(targets) );
    TEST_ASSERT( s7_is_list(s7, targets) );
    TEST_ASSERT_EQUAL_INT( 6, s7_list_length(s7, targets) );

    /* same with concise op */
    s7_pointer path2 = s7_eval_c_string(s7,
                       "'(:>>)");
    s7_pointer targets2 = s7_apply_function(s7, pkg, path2);

    /* check type, tid */
    TEST_ASSERT( !s7_is_c_object(targets2) );
    TEST_ASSERT( s7_is_list(s7, targets2) );
    TEST_ASSERT( s7_list_length(s7, targets2) == 6 );

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

void test_pkg_targets(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:targets)");
    s7_pointer targets = s7_apply_function(s7, pkg, path);
    _validate_pkg_targets(targets);
}

void test_pkg_tgts(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:>>)");
    s7_pointer targets = s7_apply_function(s7, pkg, path);
    _validate_pkg_targets(targets);
}

void _validate_pkg_targets_attrs(s7_pointer attrs) {
    TEST_ASSERT( !s7_is_c_object(attrs) );
    TEST_ASSERT( s7_is_list(s7, attrs) );
    TEST_ASSERT_EQUAL_INT( 23, s7_list_length(s7, attrs) );

    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, attrs);
    s7_pointer attr = s7_iterate(s7, iter);
    /* each item in attrs is an attr */
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        TEST_ASSERT( s7_is_c_object(attr) );
        pred = s7_apply_function(s7, attr,
                                 s7_eval_c_string(s7, "'(:binding?)"));
        TEST_ASSERT( pred == s7_t(s7) );
        attr = s7_iterate(s7, iter);
    }
}

void test_pkg_targets_attrs(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:targets :attrs)");
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    _validate_pkg_targets_attrs(attrs);
}

void test_pkg_targets_bindings(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:targets :bindings)");
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    _validate_pkg_targets_attrs(attrs);
}

void test_pkg_targets_atat(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:targets :@@)");
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    _validate_pkg_targets_attrs(attrs);
}

/* **************************************************************** */
void test_pkg_target_int(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:target 0)");
    s7_pointer target = s7_apply_function(s7, pkg, path);
    validate_hello_world_target(target);
}

void test_pkg_tgt_int(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:> 0)");
    s7_pointer target = s7_apply_function(s7, pkg, path);
    validate_hello_world_target(target);
}

void test_pkg_target_string(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:target \"hello-lib\")");
    s7_pointer target = s7_apply_function(s7, pkg, path);
    validate_hello_world_target(target);
}

void test_pkg_tgt_string(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:> \"hello-lib\")");
    s7_pointer target = s7_apply_function(s7, pkg, path);
    validate_hello_world_target(target);
}

void _validate_rule(s7_pointer rule) {
    s7_pointer pred = s7_apply_function(s7, rule,
                                         s7_eval_c_string(s7, "'(:id?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    TEST_ASSERT( sunlark_node_tid(s7, rule) == TK_ID );
    TEST_ASSERT( s7_is_c_object(rule) );
    TEST_ASSERT( !s7_is_string(rule) );
    s7_pointer rule_sym = s7_apply_function(s7, rule,
                                            s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(rule_sym) );
    TEST_ASSERT( s7_is_symbol(rule_sym) );
    pred = NULL;
    pred = s7_apply_function(s7, s7_name_to_value(s7,"symbol?"),
                             s7_list(s7, 1, rule_sym));
    TEST_ASSERT( pred == s7_t(s7) );

    TEST_ASSERT_EQUAL_STRING( "cc_library", s7_symbol_name(rule_sym) );

}

void test_pkg_target_string_rule(void) {
    char *s = "'(:target \"hello-lib\" :rule)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer rule = s7_apply_function(s7, pkg, path);
    _validate_rule(rule);
}

void test_pkg_target_int_rule(void) {
    char *s = "'(:target 0 :rule)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer rule = s7_apply_function(s7, pkg, path);
    _validate_rule(rule);
}

void _validate_name(s7_pointer nm) {
    s7_pointer is_string
        = s7_apply_function(s7, nm, s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( s7_t(s7) == is_string );
    TEST_ASSERT( s7_is_c_object(nm) );
    TEST_ASSERT( !s7_is_string(nm) );
    TEST_ASSERT( sunlark_node_tid(s7, nm) == TK_STRING );

    s7_pointer sym = s7_apply_function(s7, nm,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(sym) );
    TEST_ASSERT( s7_is_string(sym) );
    TEST_ASSERT_EQUAL_STRING( "\"hello-lib\"", s7_string(sym) );
}

void test_pkg_target_string_name(void) {
    char *s = "'(:target \"hello-lib\" :name)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer nm = s7_apply_function(s7, pkg, path);
    _validate_name(nm);
}

void test_pkg_target_int_name(void) {
    char *s = "'(:target 0 :name)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer nm = s7_apply_function(s7, pkg, path);
    _validate_name(nm);
}

void _validate_attrs(s7_pointer attrs) {
    TEST_ASSERT( s7_is_c_object(attrs) );
    /* attrs not a Scheme list, but it has a length */
    TEST_ASSERT( !s7_is_list(s7, attrs) );
    s7_pointer attrs_ct = s7_apply_function(s7,
                                            s7_name_to_value(s7,"length"),
                                            s7_list(s7, 1, attrs));
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(attrs_ct) );

    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, attrs);
    s7_pointer attr = s7_iterate(s7, iter);
    /* each item in attrs is an attr */
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        TEST_ASSERT( s7_is_c_object(attr) );
        pred = s7_apply_function(s7, attr,
                                 s7_eval_c_string(s7, "'(:binding?)"));
        TEST_ASSERT( pred == s7_t(s7) );
        attr = s7_iterate(s7, iter);
    }
}

void test_pkg_target_string_attrs(void) {
    char *s = "'(:target \"hello-lib\" :attrs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    _validate_attrs(attrs);
}

void test_pkg_target_string_bindings(void) {
    char *s = "'(:target \"hello-lib\" :bindings)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    _validate_attrs(attrs);
}

void test_pkg_target_string_atat(void) {
    char *s = "'(:target \"hello-lib\" :@@)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    _validate_attrs(attrs);
}

void test_pkg_target_int_attrs(void) {
    char *s = "'(:target 0 :attrs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    _validate_attrs(attrs);
}

void test_pkg_target_int_bindings(void) {
    char *s = "'(:target 0 :bindings)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    _validate_attrs(attrs);
}

void test_pkg_target_int_atat(void) {
    char *s = "'(:target 0 :@@)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    _validate_attrs(attrs);
}

void test_pkg_target_string_attr_sym(void) {
    char *s = "'(:target \"hello-lib\" :attr srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}
void test_pkg_tgt_string_attr_sym(void) {
    char *s = "'(:> \"hello-lib\" :attr srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}

void test_pkg_target_string_binding_sym(void) {
    char *s = "'(:target \"hello-lib\" :binding srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}
void test_pkg_tgt_string_binding_sym(void) {
    char *s = "'(:> \"hello-lib\" :binding srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}

void test_pkg_target_string_at_sym(void) {
    char *s = "'(:target \"hello-lib\" :@ srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}
void test_pkg_tgt_string_at_sym(void) {
    char *s = "'(:> \"hello-lib\" :@ srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}

void test_pkg_target_int_attr_sym(void) {
    char *s = "'(:target 0 :attr srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}
void test_pkg_tgt_int_attr_sym(void) {
    char *s = "'(:> 0 :attr srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}

void test_pkg_target_int_binding_sym(void) {
    char *s = "'(:target 0 :binding srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}
void test_pkg_tgt_int_binding_sym(void) {
    char *s = "'(:> 0 :binding srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}


void test_pkg_target_int_at_sym(void) {
    char *s = "'(:target 0 :@ srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}
void test_pkg_tgt_int_at_sym(void) {
    char *s = "'(:> 0 :@ srcs)";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer attrs = s7_apply_function(s7, pkg, path);
    validate_attr_srcs(attrs);
}

/* **************************************************************** */
void test_target_string_parse(void) {
    char *s = "foo_library(\n\
    name = 'foo-lib',\n\
    srcs = ['foo-lib.cc', 'howdy.cc', 'howdy.h'],\n\
    hdrs = ['foo-lib.h'],\n\
    defines = DEFINES\n\
)\n";
    s7_pointer target = sunlark_parse_string(s7, s7_make_string(s7, s));
    /* sealark_debug_log_ast_outline(s7_c_object_value(target), 0); */

    /* to see the structure of target_node: */
    struct node_s *target_node = s7_c_object_value(target);

    /* check type, tid. We use :target, but in the ast TK_Call_Expr */
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Call_Expr );
    s7_pointer is_target
        = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:target?)"));
    TEST_ASSERT( is_target == s7_t(s7) );

    /* (target :rule) => cc_library */
    s7_pointer rule = s7_apply_function(s7, target, s7_eval_c_string(s7, "'(:rule)"));
    s7_pointer is_id = s7_apply_function(s7, rule, s7_eval_c_string(s7, "'(:id?)"));
    TEST_ASSERT( s7_t(s7) == is_id );
    TEST_ASSERT( s7_is_c_object(rule) );
    TEST_ASSERT( !s7_is_string(rule) );
    TEST_ASSERT( sunlark_node_tid(s7, rule) == TK_ID );

    s7_pointer rule_sym = s7_apply_function(s7, rule, s7_eval_c_string(s7, "'(:$)"));
    log_debug("rule_sym: %s", s7_object_to_c_string(s7, rule_sym));
    TEST_ASSERT( !s7_is_c_object(rule_sym) );
    TEST_ASSERT( s7_is_symbol(rule_sym) );
    TEST_ASSERT_EQUAL_STRING( "foo_library", s7_symbol_name(rule_sym) );

    /* check underlying c structure */
    TEST_ASSERT( target_node->tid == TK_Call_Expr );
    struct node_s *id_node = utarray_eltptr(target_node->subnodes, 0);
    TEST_ASSERT( id_node->tid == TK_ID );
    TEST_ASSERT( strncmp(id_node->s, "foo_library", 11) == 0);
    TEST_ASSERT( strlen(id_node->s) == 11);

    /* check target name: 'foo-lib'" */
    s7_pointer name = s7_apply_function(s7, target, s7_eval_c_string(s7, "'(:name)"));
    log_debug("name: %s", s7_object_to_c_string(s7, name));
    TEST_ASSERT( s7_is_c_object(name) );
    TEST_ASSERT( sunlark_node_tid(s7, name) == TK_STRING );

    s7_pointer name_str = s7_apply_function(s7, name, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(name_str) );
    TEST_ASSERT( s7_is_string(name_str) );
    s7_pointer is_string = s7_apply_function(s7, s7_name_to_value(s7, "string?"),
                                             s7_list(s7, 1, name_str));
    TEST_ASSERT( s7_t(s7) == is_string );
    TEST_ASSERT_EQUAL_STRING( "'foo-lib'", s7_string(name_str) );

    /* bindings */
    s7_pointer bindings = s7_apply_function(s7, target, s7_eval_c_string(s7, "'(:bindings)"));
    TEST_ASSERT( s7_is_c_object(bindings) );
    TEST_ASSERT( !s7_is_list(s7, bindings) );
    s7_pointer bindings_ct
        = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                            s7_list(s7, 1, bindings));
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(bindings_ct) );
}

/* **************************************************************** */
UT_string *buf;
UT_string *test_s;

char *build_file = "test/unit/sunlark/BUILD.package";

s7_scheme *s7;

struct parse_state_s *parse_state;

void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
    pkg = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);
    s7_quit(s7);
}

