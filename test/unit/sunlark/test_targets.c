#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_targets.h"

UT_string *buf;
UT_string *test_s;

char *build_file = "test/unit/sunlark/BUILD.targets";

s7_scheme *s7;

struct parse_state_s *parse_state;
struct node_s *root;

static s7_pointer pkg;
static s7_pointer tgt;

void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
    pkg = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    root = s7_c_object_value(pkg);
    s7_pointer path = s7_eval_c_string(s7, "'(:target \"hello-world\")");
    tgt = s7_apply_function(s7, pkg, path);
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);
    s7_quit(s7);
}

/* **************************************************************** */
void _validate_hello_world_target(s7_pointer target) {
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Call_Expr );
    s7_pointer is_target
        = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:target?)"));
    TEST_ASSERT( is_target == s7_t(s7) );

    /* (target :rule) => cc_binary */
    s7_pointer rule = s7_apply_function(s7, target,
                                        s7_eval_c_string(s7, "'(:rule)"));
    TEST_ASSERT( s7_is_c_object(rule) );
    TEST_ASSERT( !s7_is_string(rule) );
    TEST_ASSERT( sunlark_node_tid(s7, rule) == TK_ID );
    s7_pointer is_id = s7_apply_function(s7, rule,
                                         s7_eval_c_string(s7, "'(:id?)"));
    TEST_ASSERT( s7_t(s7) == is_id );

    s7_pointer rule_sym = s7_apply_function(s7, rule,
                                            s7_eval_c_string(s7, "'(:$)"));
    log_debug("rule_sym: %s", s7_object_to_c_string(s7, rule_sym));
    TEST_ASSERT( !s7_is_c_object(rule_sym) );
    TEST_ASSERT( s7_is_symbol(rule_sym) );
    TEST_ASSERT_EQUAL_STRING( "cc_binary", s7_symbol_name(rule_sym) );

    /* check underlying c structure */
    struct node_s *target_node = s7_c_object_value(target);
    TEST_ASSERT( target_node->tid == TK_Call_Expr );
    struct node_s *id_node = utarray_eltptr(target_node->subnodes, 0);
    TEST_ASSERT( id_node->tid == TK_ID );
    TEST_ASSERT( strncmp(id_node->s, "cc_binary", 9) == 0);
    TEST_ASSERT( strlen(id_node->s) == 9);

    /* check target name == "hello-world" */
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
    TEST_ASSERT_EQUAL_STRING( "\"hello-world\"", s7_string(name_str) );

    /* bindings */
    s7_pointer bindings = s7_apply_function(s7, target, s7_eval_c_string(s7, "'(:bindings)"));
    TEST_ASSERT( s7_is_c_object(bindings) );
    TEST_ASSERT( !s7_is_list(s7, bindings) );
    s7_pointer bindings_ct = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                                               s7_list(s7, 1, bindings));
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(bindings_ct) );
}

void test_target(void) {
    _validate_hello_world_target(tgt);
}

void test_tgt_rule(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:rule)");
    s7_pointer rule = s7_apply_function(s7, pkg, path);
    /* _validate_hello_world_target(target); */
}

void test_tgt_name(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:name)");
    s7_pointer name = s7_apply_function(s7, pkg, path);
    /* _validate_hello_world_target(target); */
}

void test_tgt_attr_sym(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:attr srcs)");
    s7_pointer target = s7_apply_function(s7, pkg, path);
    /* _validate_hello_world_target(target); */
}

void test_tgt_binding_sym(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:binding srcs)");
    s7_pointer target = s7_apply_function(s7, pkg, path);
    /* _validate_hello_world_target(target); */
}

void test_tgt_at_sym(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:@ srcs)");
    s7_pointer target = s7_apply_function(s7, pkg, path);
    /* _validate_hello_world_target(target); */
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_target);
    RUN_TEST(test_tgt_rule);    /* (:rule) */
    RUN_TEST(test_tgt_name);  /* (:name) */
    RUN_TEST(test_tgt_attr_sym);  /* (:attr srcs) */
    RUN_TEST(test_tgt_binding_sym);  /* (:binding srcs) */
    RUN_TEST(test_tgt_at_sym);  /* (:binding srcs) */

    return UNITY_END();
}
