#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "validators.h"

/* **************************************************************** */
/* validates this, used in BUILD.packages, BUILD.targets:

cc_binary(
    name = "hello-world",
    srcs = ["hello-world.cc"],
    deps = [":hello-lib"],
    defines = DEFINES
)
*/

void validate_hello_world_target(s7_pointer target) {
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

/* **************************************************************** */
/* **************************************************************** */
void validate_attr_srcs(s7_pointer attr) {
    TEST_ASSERT( s7_is_c_object(attr) );
    TEST_ASSERT( !s7_is_list(s7, attr) );
    s7_pointer pred
        = s7_apply_function(s7, attr, s7_eval_c_string(s7, "'(:binding?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    /* an attr is a (key . value) pair, so it has length 2 */
    s7_pointer component_ct
        = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                            s7_list(s7, 1, attr));
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(component_ct) );

    /* verify key */
    s7_pointer key_node = s7_apply_function(s7, attr,
                                            s7_eval_c_string(s7, "'(:key)"));
    TEST_ASSERT( s7_is_c_object(key_node) );
    pred = NULL;
    pred = s7_apply_function(s7, attr,
                                        s7_eval_c_string(s7, "'(:node?)"));
    TEST_ASSERT( s7_t(s7) == pred );

    /* Convert to Scheme datum */
    s7_pointer key_str = s7_apply_function(s7, key_node,
                                           s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( !s7_is_c_object(key_str) );
    TEST_ASSERT( s7_is_symbol(key_str) );
    TEST_ASSERT_EQUAL_STRING( "srcs", s7_symbol_name(key_str) );

    /* verify value */
    s7_pointer val_node
        = s7_apply_function(s7, attr, s7_eval_c_string(s7, "'(:value)"));
    TEST_ASSERT( s7_is_c_object(val_node) );
    pred = NULL; pred = s7_apply_function(s7, attr,
                                          s7_eval_c_string(s7, "'(:node?)"));
    TEST_ASSERT( s7_t(s7) == pred );

    /* in this case the value is a string list */
    pred = NULL;
    pred = s7_apply_function(s7, val_node,
                             s7_eval_c_string(s7, "'(:list-expr?)"));
    TEST_ASSERT( s7_t(s7) == pred );
    /* of length 1 */
    s7_pointer len = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                                       s7_list(s7, 1, val_node));
    TEST_ASSERT_EQUAL_INT( 1, s7_integer(len) );

    /* whose 0 item is "hello-world.cc" */
    s7_pointer item = s7_apply_function(s7, val_node,
                             s7_eval_c_string(s7, "'(0)"));
    /* which is a string node */
    pred = s7_apply_function(s7, item,
                             s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    /* whose Scheme value is string "hello-world.cc" */
    s7_pointer sval = s7_apply_function(s7, item,
                                        s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "\"hello-world.cc\"", s7_string(sval) );
}
