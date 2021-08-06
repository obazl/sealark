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

cc_library(
    name = "hello-lib",
    srcs = ["hello-lib.cc", "howdy.cc", "howdy.h"],
    hdrs = ["hello-lib.h"],
    defines = DEFINES
)

 0: TK_Call_Expr[97] @12:0
  1: TK_ID[37] @12:0    cc_library
  1: TK_Call_Sfx[98] @12:10
    2: TK_LPAREN[54] @12:10
    2: TK_Arg_List[87] @13:4
      3: TK_Binding[88] @13:4
        4: TK_ID[37] @13:4    name
        4: TK_EQ[26] @13:9
        4: TK_STRING[79] @13:11    "hello-lib"
      3: TK_COMMA[15] @13:22
      3: TK_Binding[88] @14:4
        4: TK_ID[37] @14:4    srcs
        4: TK_EQ[26] @14:9
        4: TK_List_Expr[116] @14:11
          5: TK_LBRACK[49] @14:11
          5: TK_Expr_List[109] @14:12
            6: TK_STRING[79] @14:12    "hello-lib.cc"
            6: TK_COMMA[15] @14:26
            6: TK_STRING[79] @14:28    "howdy.cc"
            6: TK_COMMA[15] @14:38
            6: TK_STRING[79] @14:40    "howdy.h"
          5: TK_RBRACK[69] @14:49
      3: TK_COMMA[15] @14:50
      3: TK_Binding[88] @15:4
        4: TK_ID[37] @15:4    hdrs
        4: TK_EQ[26] @15:9
        4: TK_List_Expr[116] @15:11
          5: TK_LBRACK[49] @15:11
          5: TK_Expr_List[109] @15:12
            6: TK_STRING[79] @15:12    "hello-lib.h"
          5: TK_RBRACK[69] @15:25
      3: TK_COMMA[15] @15:26
      3: TK_Binding[88] @16:4
        4: TK_ID[37] @16:4    defines
        4: TK_EQ[26] @16:12
        4: TK_ID[37] @16:14    DEFINES
    2: TK_RPAREN[71] @17:0

*/

void validate_hello_world_target(s7_pointer target) {
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Call_Expr );
    s7_pointer is_target
        = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:target?)"));
    TEST_ASSERT( is_target == s7_t(s7) );

    /* (target :rule) => cc_library */
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
    TEST_ASSERT_EQUAL_STRING( "cc_library", s7_symbol_name(rule_sym) );

    /* check underlying c structure */
    struct node_s *target_node = s7_c_object_value(target);
    TEST_ASSERT( target_node->tid == TK_Call_Expr );
    struct node_s *id_node = utarray_eltptr(target_node->subnodes, 0);
    TEST_ASSERT( id_node->tid == TK_ID );
    TEST_ASSERT( strncmp(id_node->s, "cc_library", 10) == 0);
    TEST_ASSERT( strlen(id_node->s) == 10);

    /* check target name == "hello-lib" */
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
    TEST_ASSERT_EQUAL_STRING( "\"hello-lib\"", s7_string(name_str) );

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
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(len) );

    /* whose 0 item is "hello-lib.cc" */
    s7_pointer item = s7_apply_function(s7, val_node,
                             s7_eval_c_string(s7, "'(:0)"));
    /* which is a string node */
    pred = s7_apply_function(s7, item,
                             s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    /* whose Scheme value is string "hello-lib.cc" */
    s7_pointer sval = s7_apply_function(s7, item,
                                        s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "\"hello-lib.cc\"", s7_string(sval) );
}
