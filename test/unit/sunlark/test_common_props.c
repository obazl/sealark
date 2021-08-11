#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_common_props.h"

int main(int argc, char *argv[])
{
    int opt;
    /* char *build_file = NULL; */

    while ((opt = getopt(argc, argv, "f:")) != -1) {
        switch (opt) {
        case 'f':
            /* BUILD.bazel or BUILD file */
            /* log_info("build file: %s", optarg); */
            build_file = optarg;
            break;
        default:
            log_error("Usage: bazel test test/unit/sunlark:common_props -- -f buildfile");
            exit(EXIT_FAILURE);
        }
    }

    if (build_file == NULL) {
        log_error("-f <buildfile> must be provided.");
        exit(EXIT_FAILURE);
    }

    UNITY_BEGIN();
    /* RUN_TEST(test_loadstmt_props); */
    RUN_TEST(test_loadstmt_args_props);

    /* RUN_TEST(test_target_props); */
    /* RUN_TEST(test_target_arg_list_props); */
    /* RUN_TEST(test_binding_props); */
    /* RUN_TEST(test_binding_val_props); */
    return UNITY_END();
}

/* **************************************************************** */
/* common properties: :tid, :tid->kw, :tid->string, :node-type, :line, :col, :trailing_newline, :qtype, :s, node?
:<nodetype>? (e.g. :binding?, :list-expr?, etc
:subnode-count, :subnode-count-recursive, :printable-subnode-count-recursive
:length
:subnodes
 */

/* **************************************************************** */
void test_loadstmt_props(void) {
    char *path = "'(:load \"@rules_cc//cc:defs.bzl\")";
    s7_pointer path_node = s7_eval_c_string(s7, path);
    s7_pointer loadstmt = s7_apply_function(s7, pkg, path_node);
    /* sealark_debug_log_ast_outline(s7_c_object_value(loadstmt), 0); */

    s7_pointer pred = s7_apply_function(s7, loadstmt,
                                        s7_eval_c_string(s7, "'(:node?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    pred = s7_apply_function(s7, loadstmt,
                             s7_eval_c_string(s7, "'(:load-stmt?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    s7_pointer nd = s7_apply_function(s7, loadstmt,
                                       s7_eval_c_string(s7, "'(:tid)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( TK_Load_Stmt, s7_integer(nd) );

    nd = s7_apply_function(s7, loadstmt,
                            s7_eval_c_string(s7, "'(:tid->kw)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":load-stmt", s7_symbol_name(nd) );

    nd = s7_apply_function(s7, loadstmt,
                            s7_eval_c_string(s7, "'(:tid->string)"));
    TEST_ASSERT( s7_is_string(nd) );
    TEST_ASSERT_EQUAL_STRING( "load-stmt", s7_string(nd) );

    nd = s7_apply_function(s7, loadstmt,
                            s7_eval_c_string(s7, "'(:node-type)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":load-stmt", s7_symbol_name(nd) );

    nd = s7_apply_function(s7, loadstmt,
                            s7_eval_c_string(s7, "'(:line)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(nd) );

    nd = s7_apply_function(s7, loadstmt,
                            s7_eval_c_string(s7, "'(:col)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 0, s7_integer(nd) );

    /* counts: :subnode-count, :subnode-count-recursive,
       :printable-subnode-count-recursive, :length */

    /* 0: TK_Load_Stmt[117] @2:0 */
    /*  1: TK_LOAD[53] @2:0 */
    /*  1: TK_LPAREN[54] @2:4 */
    /*  1: TK_STRING[79] @2:5    "@rules_cc//cc:defs.bzl" */
    /*  1: TK_COMMA[15] @2:29 */
    /*  1: TK_STRING[79] @2:31    "cc_binary" */
    /*  1: TK_COMMA[15] @2:42 */
    /*  1: TK_STRING[79] @2:44    "cc_library" */
    /*  1: TK_COMMA[15] @2:56 */
    /*  1: TK_STRING[79] @2:58    "cc_test" */
    /*  1: TK_RPAREN[71] @2:67 */

    nd = s7_apply_function(s7, loadstmt,
                            s7_eval_c_string(s7, "'(:length)"));
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(nd) ); /* exlude 'load' */

    nd = s7_apply_function(s7, loadstmt,
                            s7_eval_c_string(s7, "'(:subnode-count)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 10, s7_integer(nd) );

    nd = s7_apply_function(s7, loadstmt,
                            s7_eval_c_string(s7, "'(:subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 10, s7_integer(nd) );

    nd = s7_apply_function(s7, loadstmt,
                            s7_eval_c_string(s7,
                                             "'(:printable-subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 10, s7_integer(nd) );
}

/* **************************************************************** */
void test_loadstmt_args_props(void) {
    char *path = "'(:load \"@rules_cc//cc:defs.bzl\")";
    s7_pointer path_node = s7_eval_c_string(s7, path);
    s7_pointer args = s7_apply_function(s7, pkg, path_node);
    /* sealark_debug_log_ast_outline(s7_c_object_value(args), 0); */

    /* s7_pointer pred = s7_apply_function(s7, args, */
    /*                                     s7_eval_c_string(s7, "'(:node?)")); */
    /* TEST_ASSERT( pred == s7_t(s7) ); */
    /* pred = s7_apply_function(s7, args, */
    /*                          s7_eval_c_string(s7, "'(:load-stmt?)")); */
    /* TEST_ASSERT( pred == s7_t(s7) ); */

    /* s7_pointer nd = s7_apply_function(s7, args, */
    /*                                    s7_eval_c_string(s7, "'(:tid)")); */
    /* TEST_ASSERT( s7_is_integer(nd) ); */
    /* TEST_ASSERT_EQUAL_INT( TK_Load_Stmt, s7_integer(nd) ); */

    /* nd = s7_apply_function(s7, args, */
    /*                         s7_eval_c_string(s7, "'(:tid->kw)")); */
    /* TEST_ASSERT( s7_is_keyword(nd) ); */
    /* TEST_ASSERT_EQUAL_STRING( ":load-stmt", s7_symbol_name(nd) ); */

    /* nd = s7_apply_function(s7, args, */
    /*                         s7_eval_c_string(s7, "'(:tid->string)")); */
    /* TEST_ASSERT( s7_is_string(nd) ); */
    /* TEST_ASSERT_EQUAL_STRING( "load-stmt", s7_string(nd) ); */

    /* nd = s7_apply_function(s7, args, */
    /*                         s7_eval_c_string(s7, "'(:node-type)")); */
    /* TEST_ASSERT( s7_is_keyword(nd) ); */
    /* TEST_ASSERT_EQUAL_STRING( ":load-stmt", s7_symbol_name(nd) ); */

    /* nd = s7_apply_function(s7, args, */
    /*                         s7_eval_c_string(s7, "'(:line)")); */
    /* TEST_ASSERT( s7_is_integer(nd) ); */
    /* TEST_ASSERT_EQUAL_INT( 2, s7_integer(nd) ); */

    /* nd = s7_apply_function(s7, args, */
    /*                         s7_eval_c_string(s7, "'(:col)")); */
    /* TEST_ASSERT( s7_is_integer(nd) ); */
    /* TEST_ASSERT_EQUAL_INT( 0, s7_integer(nd) ); */

    /* /\* counts: :subnode-count, :subnode-count-recursive, */
    /*    :printable-subnode-count-recursive, :length *\/ */

    /* /\* 0: TK_Load_Stmt[117] @2:0 *\/ */
    /* /\*  1: TK_LOAD[53] @2:0 *\/ */
    /* /\*  1: TK_LPAREN[54] @2:4 *\/ */
    /* /\*  1: TK_STRING[79] @2:5    "@rules_cc//cc:defs.bzl" *\/ */
    /* /\*  1: TK_COMMA[15] @2:29 *\/ */
    /* /\*  1: TK_STRING[79] @2:31    "cc_binary" *\/ */
    /* /\*  1: TK_COMMA[15] @2:42 *\/ */
    /* /\*  1: TK_STRING[79] @2:44    "cc_library" *\/ */
    /* /\*  1: TK_COMMA[15] @2:56 *\/ */
    /* /\*  1: TK_STRING[79] @2:58    "cc_test" *\/ */
    /* /\*  1: TK_RPAREN[71] @2:67 *\/ */

    /* nd = s7_apply_function(s7, args, */
    /*                         s7_eval_c_string(s7, "'(:length)")); */
    /* TEST_ASSERT_EQUAL_INT( 4, s7_integer(nd) ); /\* exlude 'load' *\/ */

    /* nd = s7_apply_function(s7, args, */
    /*                         s7_eval_c_string(s7, "'(:subnode-count)")); */
    /* TEST_ASSERT( s7_is_integer(nd) ); */
    /* TEST_ASSERT_EQUAL_INT( 10, s7_integer(nd) ); */

    /* nd = s7_apply_function(s7, args, */
    /*                         s7_eval_c_string(s7, "'(:subnode-count-recursive)")); */
    /* TEST_ASSERT( s7_is_integer(nd) ); */
    /* TEST_ASSERT_EQUAL_INT( 10, s7_integer(nd) ); */

    /* nd = s7_apply_function(s7, args, */
    /*                         s7_eval_c_string(s7, */
    /*                                          "'(:printable-subnode-count-recursive)")); */
    /* TEST_ASSERT( s7_is_integer(nd) ); */
    /* TEST_ASSERT_EQUAL_INT( 10, s7_integer(nd) ); */
}

/* **************************************************************** */
void validate_target_props(s7_pointer node)
{
    s7_pointer pred = s7_apply_function(s7, node,
                                        s7_eval_c_string(s7, "'(:node?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    pred = s7_apply_function(s7, node,
                                        s7_eval_c_string(s7, "'(:target?)"));
    TEST_ASSERT( pred == s7_t(s7) );
}

void test_target_props(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:> \"hello-world\")");
    s7_pointer target = s7_apply_function(s7, pkg, path);

    validate_target_props(target);

    s7_pointer nd = s7_apply_function(s7, target,
                                       s7_eval_c_string(s7, "'(:tid)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( TK_Call_Expr, s7_integer(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:tid->kw)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":call-expr", s7_symbol_name(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:tid->string)"));
    TEST_ASSERT( s7_is_string(nd) );
    TEST_ASSERT_EQUAL_STRING( "call-expr", s7_string(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:node-type)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":call-expr", s7_symbol_name(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:line)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:col)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 0, s7_integer(nd) );

/* :subnode-count, :subnode-count-recursive, :printable-subnode-count-recursive */
/* :length */
/* :subnodes */
    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:length)"));
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(nd) ); /* Call_Expr */

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:subnode-count)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 38, s7_integer(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7,
                                             "'(:printable-subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 28, s7_integer(nd) );
}

/* **************************************************************** */
void validate_arg_list_props(s7_pointer node)
{
    s7_pointer pred = s7_apply_function(s7, node,
                                        s7_eval_c_string(s7, "'(:node?)"));
    TEST_ASSERT( pred == s7_t(s7) );
}

void test_target_arg_list_props(void) {
    char *path = "'(:> \"hello-world\" :@@)";
    s7_pointer path_node = s7_eval_c_string(s7, path);
    s7_pointer bindings = s7_apply_function(s7, pkg, path_node);

    validate_arg_list_props(bindings);

    s7_pointer pred = s7_apply_function(s7, bindings,
                               s7_eval_c_string(s7, "'(:arg-list?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    s7_pointer nd = s7_apply_function(s7, bindings,
                                       s7_eval_c_string(s7, "'(:tid)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( TK_Arg_List, s7_integer(nd) );

    nd = s7_apply_function(s7, bindings,
                            s7_eval_c_string(s7, "'(:tid->kw)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":arg-list", s7_symbol_name(nd) );

    nd = s7_apply_function(s7, bindings,
                           s7_eval_c_string(s7, "'(:tid->string)"));
    TEST_ASSERT( s7_is_string(nd) );
    TEST_ASSERT_EQUAL_STRING( "arg-list", s7_string(nd) );

    nd = s7_apply_function(s7, bindings,
                            s7_eval_c_string(s7, "'(:node-type)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":arg-list", s7_symbol_name(nd) );

    /* 0: TK_Arg_List[87] @20:4 */
    /*   1: TK_Binding[88] @20:4 */
    /*     2: TK_ID[37] @20:4    name */
    /*     2: TK_EQ[26] @20:9 */
    /*     2: TK_STRING[79] @20:11    "hello-world" */
    /*   1: TK_COMMA[15] @20:24 */
    /*   1: TK_Binding[88] @21:4 */
    /*     ... */

    nd = s7_apply_function(s7, bindings,
                            s7_eval_c_string(s7, "'(:line)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 5, s7_integer(nd) );

    nd = s7_apply_function(s7, bindings,
                            s7_eval_c_string(s7, "'(:col)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(nd) );

    nd = s7_apply_function(s7, bindings,
                            s7_eval_c_string(s7, "'(:length)"));
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(nd) ); /* omitting commas */

    nd = s7_apply_function(s7, bindings,
                           s7_eval_c_string(s7, "'(:subnode-count)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 7, s7_integer(nd) ); /* including commas */

    nd = s7_apply_function(s7, bindings,
                            s7_eval_c_string(s7, "'(:subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 33, s7_integer(nd) );

    nd = s7_apply_function(s7, bindings,
                            s7_eval_c_string(s7,
                            "'(:printable-subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 25, s7_integer(nd) );

    s7_pointer iter = s7_make_iterator(s7, bindings);
    s7_pointer binding = s7_iterate(s7, iter);
    /* each item in target arg_list is a binding */
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        validate_binding_props(binding);
        binding = s7_iterate(s7, iter);
    }
}

/* **************************************************************** */
void validate_binding_props(s7_pointer node)
{
    TEST_ASSERT( s7_is_c_object(node) );
    s7_pointer pred = s7_apply_function(s7, node,
                                        s7_eval_c_string(s7, "'(:node?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    pred = s7_apply_function(s7, node,
                                        s7_eval_c_string(s7, "'(:binding?)"));
    TEST_ASSERT( pred == s7_t(s7) );
}

void test_binding_props(void) {
    char *path = "'(:> \"hello-world\" :@ srcs)";
    s7_pointer path_node = s7_eval_c_string(s7, path);
    s7_pointer binding = s7_apply_function(s7, pkg, path_node);

    validate_binding_props(binding);

    s7_pointer nd = s7_apply_function(s7, binding,
                                       s7_eval_c_string(s7, "'(:tid)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( TK_Binding, s7_integer(nd) );

    nd = s7_apply_function(s7, binding,
                            s7_eval_c_string(s7, "'(:tid->kw)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":binding", s7_symbol_name(nd) );

    nd = s7_apply_function(s7, binding,
                            s7_eval_c_string(s7, "'(:tid->string)"));
    TEST_ASSERT( s7_is_string(nd) );
    TEST_ASSERT_EQUAL_STRING( "binding", s7_string(nd) );

    nd = s7_apply_function(s7, binding,
                            s7_eval_c_string(s7, "'(:node-type)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":binding", s7_symbol_name(nd) );

    /* 0: TK_Binding[88] @6:4 */
    /*   1: TK_ID[37] @6:4    srcs */
    /*   1: TK_EQ[26] @6:9 */
    /*   1: TK_List_Expr[116] @6:11 */
    /*     2: TK_LBRACK[49] @6:11 */
    /*     2: TK_Expr_List[109] @7:8 */
    /*       3: TK_STRING[79] @7:8    "hello-world.cc" */
    /*       3: TK_COMMA[15] @7:24 */
    /*       3: TK_STRING[79] @7:26    "hello-world.h" */
    /*       3: TK_COMMA[15] @7:41 */
    /*       3: TK_STRING[79] @8:8    "bonjour-monde.cc" */
    /*       3: TK_COMMA[15] @8:26 */
    /*       3: TK_STRING[79] @8:28    "bonjour-monde.h" */
    /*     2: TK_RBRACK[69] @9:4 */

    nd = s7_apply_function(s7, binding,
                            s7_eval_c_string(s7, "'(:line)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 6, s7_integer(nd) );

    nd = s7_apply_function(s7, binding,
                            s7_eval_c_string(s7, "'(:col)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(nd) );

    /* counts: :subnode-count, :subnode-count-recursive,
       :printable-subnode-count-recursive
       :length
       :subnodes */

    nd = s7_apply_function(s7, binding,
                            s7_eval_c_string(s7, "'(:length)"));
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(nd) );

    nd = s7_apply_function(s7, binding,
                            s7_eval_c_string(s7, "'(:subnode-count)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(nd) );

    nd = s7_apply_function(s7, binding,
                            s7_eval_c_string(s7, "'(:subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 13, s7_integer(nd) );

    nd = s7_apply_function(s7, binding,
                            s7_eval_c_string(s7,
                            "'(:printable-subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 11, s7_integer(nd) );
}

/* **************************************************************** */
void test_binding_val_props(void) {
    char *path = "'(:> \"hello-world\" :@ srcs :value)";
    s7_pointer path_node = s7_eval_c_string(s7, path);
    s7_pointer val = s7_apply_function(s7, pkg, path_node);

    /* val is string list */
    TEST_ASSERT( s7_is_c_object(val) );
    s7_pointer pred = s7_apply_function(s7, val,
                                        s7_eval_c_string(s7, "'(:node?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    pred = s7_apply_function(s7, val,
                             s7_eval_c_string(s7, "'(:list-expr?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    s7_pointer nd = s7_apply_function(s7, val,
                                       s7_eval_c_string(s7, "'(:tid)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( TK_List_Expr, s7_integer(nd) );

    nd = s7_apply_function(s7, val,
                            s7_eval_c_string(s7, "'(:tid->kw)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":list-expr", s7_symbol_name(nd) );

    nd = s7_apply_function(s7, val,
                            s7_eval_c_string(s7, "'(:tid->string)"));
    TEST_ASSERT( s7_is_string(nd) );
    TEST_ASSERT_EQUAL_STRING( "list-expr", s7_string(nd) );

    nd = s7_apply_function(s7, val,
                            s7_eval_c_string(s7, "'(:node-type)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":list-expr", s7_symbol_name(nd) );

    /* 0: TK_List_Expr[116] @6:11 */
    /*   1: TK_LBRACK[49] @6:11 */
    /*   1: TK_Expr_List[109] @7:8 */
    /*     2: TK_STRING[79] @7:8    "hello-world.cc" */
    /*     2: TK_COMMA[15] @7:24 */
    /*     2: TK_STRING[79] @7:26    "hello-world.h" */
    /*     2: TK_COMMA[15] @7:41 */
    /*     2: TK_STRING[79] @8:8    "bonjour-monde.cc" */
    /*     2: TK_COMMA[15] @8:26 */
    /*     2: TK_STRING[79] @8:28    "bonjour-monde.h" */
    /*   1: TK_RBRACK[69] @9:4 */

    nd = s7_apply_function(s7, val,
                            s7_eval_c_string(s7, "'(:line)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 6, s7_integer(nd) );

    nd = s7_apply_function(s7, val,
                           s7_eval_c_string(s7, "'(:col)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 11, s7_integer(nd) );

    nd = s7_apply_function(s7, val,
                            s7_eval_c_string(s7, "'(:length)"));
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(nd) );

    nd = s7_apply_function(s7, val,
                            s7_eval_c_string(s7, "'(:subnode-count)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(nd) );

    nd = s7_apply_function(s7, val,
                  s7_eval_c_string(s7, "'(:subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 10, s7_integer(nd) );

    nd = s7_apply_function(s7, val,
                            s7_eval_c_string(s7,
                            "'(:printable-subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 9, s7_integer(nd) );
}

/* **************************************************************** */
UT_string *buf;
UT_string *test_s;

char *build_file;

s7_scheme *s7;

struct parse_state_s *parse_state;
struct node_s *root;

void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
    pkg = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    root = s7_c_object_value(pkg);
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);
    s7_quit(s7);
}

