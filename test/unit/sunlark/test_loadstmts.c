#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_loadstmts.h"

UT_string *buf;
UT_string *test_s;
/* UT_array  *result; */

char *build_file = "test/unit/sunlark/BUILD.loadstmts";

s7_scheme *s7;
/* static s7_pointer old_port, result; */
/* static int gc_loc = -1; */

LOCAL s7_pointer _error_handler(s7_scheme *sc, s7_pointer args)
{
  fprintf(stdout, "error: %s\n", s7_string(s7_car(args)));
  return(s7_f(sc));
}

struct parse_state_s *parse_state;

static s7_pointer ast;
struct node_s *root;


void setUp(void) {
    s7 = sunlark_init();
    /* trap error messages */

    /* old_port = s7_set_current_error_port(s7, s7_open_output_string(s7)); */
    /* if (old_port != s7_nil(s7)) */
    /*     gc_loc = s7_gc_protect(s7, old_port); */

    /* s7_define_function(s7, "error-handler", */
    /*                    _error_handler, 1, 0, false, */
    /*                    "our error handler"); */

    ast = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    root = s7_c_object_value(ast);
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);

    /* s7_close_output_port(s7, s7_current_error_port(s7)); */
    /* s7_set_current_error_port(s7, old_port); */
    /* if (gc_loc != -1) */
    /*     s7_gc_unprotect_at(s7, gc_loc); */

    s7_quit(s7);
}

void test_all_loadstmts(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:loads)");
    s7_pointer loadstmts = s7_apply_function(s7, ast, path);

    TEST_ASSERT( ! s7_is_c_object(loadstmts) );
    TEST_ASSERT(   s7_is_list(s7, loadstmts) );

    s7_pointer loadstmt_ct
        = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                            s7_list(s7, 1, loadstmts));
    TEST_ASSERT_EQUAL_INT( 4, s7_integer(loadstmt_ct) );

    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, loadstmts);
    s7_pointer loadstmt = s7_iterate(s7, iter);
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        TEST_ASSERT( s7_is_c_object(loadstmt) );
        TEST_ASSERT( !s7_is_list(s7, loadstmt) );
        pred = s7_apply_function(s7,
                                 loadstmt,
                                 s7_eval_c_string(s7, "'(:load-stmt?)"));
        TEST_ASSERT( pred == s7_t(s7) );

        loadstmt = s7_iterate(s7, iter);
    }
}

void test_pkg_load_src(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:load \"@repoc//pkgc:targetc.bzl\")");
    s7_pointer loadstmt = s7_apply_function(s7, ast, path);

    TEST_ASSERT( ! s7_is_list(s7, loadstmt) );
    TEST_ASSERT( s7_is_c_object(loadstmt) );
    s7_pointer pred
        = s7_apply_function(s7, loadstmt,
                            s7_eval_c_string(s7, "'(:load-stmt?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    s7_pointer loadstmt_ct
        = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                            s7_list(s7, 1, loadstmt));
    TEST_ASSERT_EQUAL_INT( 0, s7_integer(loadstmt_ct) );

}

void test_pkg_load_src_args(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:load \"@repoc//pkgc:targetc.bzl\" :args)");
    s7_pointer args = s7_apply_function(s7, ast, path);

    TEST_ASSERT( !s7_is_c_object(args) );
    TEST_ASSERT( s7_is_list(s7, args) );

    s7_pointer args_ct
        = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                            s7_list(s7, 1, args));
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(args_ct) );

    /* s7_pointer args = s7_apply_function(s7, */
    /*                                     loadstmt, */
    /*                                     s7_eval_c_string(s7, "'(:args)")); */
    /* TEST_ASSERT( ! s7_is_c_object(args) ); */
    /* TEST_ASSERT( s7_is_list(s7, args) ); */

    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, args);
    s7_pointer arg = s7_iterate(s7, iter);
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        /* sealark_debug_log_ast_outline(s7_c_object_value(arg), 0); */
        TEST_ASSERT( !s7_is_list(s7, arg) );
        TEST_ASSERT( s7_is_c_object(arg) );
        pred = s7_apply_function(s7, arg,
                                 s7_eval_c_string(s7, "'(:string?)"));
        TEST_ASSERT( pred == s7_t(s7) );
        arg = s7_iterate(s7, iter);
    }
}

void test_pkg_load_int(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:load 2)");
    s7_pointer loadstmt = s7_apply_function(s7, ast, path);

    TEST_ASSERT( ! s7_is_list(s7, loadstmt) );
    TEST_ASSERT( s7_is_c_object(loadstmt) );
    s7_pointer pred
        = s7_apply_function(s7, loadstmt,
                            s7_eval_c_string(s7, "'(:load-stmt?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    s7_pointer loadstmt_ct
        = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                            s7_list(s7, 1, loadstmt));
    TEST_ASSERT_EQUAL_INT( 0, s7_integer(loadstmt_ct) );

}

void test_pkg_load_int_args(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:load 2 :args)");
    s7_pointer args = s7_apply_function(s7, ast, path);

    TEST_ASSERT( !s7_is_c_object(args) );
    TEST_ASSERT( s7_is_list(s7, args) );

    s7_pointer args_ct
        = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                            s7_list(s7, 1, args));
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(args_ct) );

    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, args);
    s7_pointer arg = s7_iterate(s7, iter);
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        /* sealark_debug_log_ast_outline(s7_c_object_value(arg), 0); */
        TEST_ASSERT( !s7_is_list(s7, arg) );
        TEST_ASSERT( s7_is_c_object(arg) );
        pred = s7_apply_function(s7, arg,
                                 s7_eval_c_string(s7, "'(:string?)"));
        TEST_ASSERT( pred == s7_t(s7) );
        arg = s7_iterate(s7, iter);
    }
}

void test_pkg_load_int_bindings(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:load 2 :bindings)");
    s7_pointer args = s7_apply_function(s7, ast, path);

    TEST_ASSERT( !s7_is_c_object(args) );
    TEST_ASSERT( s7_is_list(s7, args) );

    s7_pointer args_ct
        = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                            s7_list(s7, 1, args));
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(args_ct) );

    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, args);
    s7_pointer arg = s7_iterate(s7, iter);
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        /* sealark_debug_log_ast_outline(s7_c_object_value(arg), 0); */
        TEST_ASSERT( !s7_is_list(s7, arg) );
        TEST_ASSERT( s7_is_c_object(arg) );
        pred = s7_apply_function(s7, arg,
                                 s7_eval_c_string(s7, "'(:binding?)"));
        TEST_ASSERT( pred == s7_t(s7) );
        arg = s7_iterate(s7, iter);
    }
}

void test_loadstmt_args(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:load \"@repoc//pkgc:targetc.bzl\")");
    s7_pointer loadstmt = s7_apply_function(s7, ast, path);
    s7_pointer args = s7_apply_function(s7, loadstmt,
                                        s7_eval_c_string(s7, "'(:args)"));
    TEST_ASSERT( !s7_is_c_object(args) );
    TEST_ASSERT( s7_is_list(s7, args) );
    s7_pointer args_ct
        = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                            s7_list(s7, 1, args));
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(args_ct) );

    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, args);
    s7_pointer arg = s7_iterate(s7, iter);
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        /* sealark_debug_log_ast_outline(s7_c_object_value(arg), 0); */
        TEST_ASSERT( !s7_is_list(s7, arg) );
        TEST_ASSERT( s7_is_c_object(arg) );
        pred = s7_apply_function(s7, arg,
                                 s7_eval_c_string(s7, "'(:string?)"));
        TEST_ASSERT( pred == s7_t(s7) );
        arg = s7_iterate(s7, iter);
    }
}

void test_pkg_load_src_arg_int(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:load \"@repoc//pkgc:targetc.bzl\" :arg 0)");
    s7_pointer arg = s7_apply_function(s7, ast, path);
    TEST_ASSERT( s7_is_c_object(arg) );
    TEST_ASSERT( !s7_is_list(s7, arg) );
    s7_pointer pred = s7_apply_function(s7, arg,
                                        s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    s7_pointer str = s7_apply_function(s7, arg,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "\"arg0c\"", s7_string(str) );
}

void test_pkg_load_src_arg_string(void) {
    char *path_str = "'(:load \"@repoc//pkgc:targetc.bzl\" :arg \"arg2c\")";
    s7_pointer path = s7_eval_c_string(s7, path_str);
    s7_pointer arg = s7_apply_function(s7, ast, path);
    TEST_ASSERT( s7_is_c_object(arg) );
    TEST_ASSERT( !s7_is_list(s7, arg) );

    s7_pointer pred = s7_apply_function(s7, arg,
                                        s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    s7_pointer str = s7_apply_function(s7, arg,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT_EQUAL_STRING( "\"arg2c\"", s7_string(str) );
}

void test_pkg_load_src_arg_string_prop(void) {
    /* a common prop, :tid-kw */
    char *path_str = "'(:load \"@repoc//pkgc:targetc.bzl\" :arg \"arg2c\" :tid->kw)";
    s7_pointer path = s7_eval_c_string(s7, path_str);
    s7_pointer arg = s7_apply_function(s7, ast, path);
    TEST_ASSERT( !s7_is_c_object(arg) );
    TEST_ASSERT( s7_is_keyword(arg) );
    TEST_ASSERT( s7_make_keyword(s7, "string") == arg );

    s7_pointer pred = s7_apply_function(s7, s7_name_to_value(s7,"keyword?"),
                                        s7_list(s7, 1, arg));
    TEST_ASSERT( s7_t(s7) == pred );
}

void test_pkg_load_src_bindings(void) {
    char *path_str = "'(:load \"@repoc//pkgc:targetc.bzl\" :bindings)";
    s7_pointer path = s7_eval_c_string(s7, path_str);
    s7_pointer bindings = s7_apply_function(s7, ast, path);
    TEST_ASSERT( !s7_is_c_object(bindings) );
    TEST_ASSERT( s7_is_list(s7, bindings) );
    s7_pointer bindings_ct
        = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                            s7_list(s7, 1, bindings));
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(bindings_ct) );

    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, bindings);
    s7_pointer binding = s7_iterate(s7, iter);
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        /* sealark_debug_log_ast_outline(s7_c_object_value(binding), 0); */
        TEST_ASSERT( !s7_is_list(s7, binding) );
        TEST_ASSERT( s7_is_c_object(binding) );
        pred = s7_apply_function(s7, binding,
                                 s7_eval_c_string(s7, "'(:binding?)"));
        TEST_ASSERT( pred == s7_t(s7) );
        binding = s7_iterate(s7, iter);
    }
}

void test_pkg_load_src_binding_int(void) {
    char *path_str
        = "'(:load \"@repoc//pkgc:targetc.bzl\" :binding 0)";
    s7_pointer path = s7_eval_c_string(s7, path_str);
    s7_pointer arg = s7_apply_function(s7, ast, path);

    TEST_ASSERT( s7_is_c_object(arg) );
    TEST_ASSERT( !s7_is_list(s7, arg) );

    s7_pointer pred = s7_apply_function(s7, arg,
                                      s7_eval_c_string(s7, "'(:binding?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    /* s7_pointer str = s7_apply_function(s7, arg, */
    /*                                    s7_eval_c_string(s7, "'(:$)")); */
    /* TEST_ASSERT_EQUAL_STRING( "\"arg2c\"", s7_string(str) ); */
}

void test_pkg_load_src_binding_int_key(void) {
    char *path_str
        = "'(:load \"@repoc//pkgc:targetc.bzl\" :binding 0 :key)";
    s7_pointer path = s7_eval_c_string(s7, path_str);
    s7_pointer arg = s7_apply_function(s7, ast, path);

    TEST_ASSERT( s7_is_c_object(arg) );
    TEST_ASSERT( !s7_is_list(s7, arg) );

    /* binding key has type :id */
    s7_pointer pred = s7_apply_function(s7, arg,
                                      s7_eval_c_string(s7, "'(:id?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    /* s7_pointer str = s7_apply_function(s7, arg, */
    /*                                    s7_eval_c_string(s7, "'(:$)")); */
    /* TEST_ASSERT_EQUAL_STRING( "\"arg2c\"", s7_string(str) ); */
}

void test_pkg_load_src_binding_value(void) {
    char *path_str
        = "'(:load \"@repoc//pkgc:targetc.bzl\" :binding 0 :value)";
    s7_pointer path = s7_eval_c_string(s7, path_str);
    s7_pointer arg = s7_apply_function(s7, ast, path);

    TEST_ASSERT( s7_is_c_object(arg) );
    TEST_ASSERT( !s7_is_list(s7, arg) );

    s7_pointer pred = s7_apply_function(s7, arg,
                                      s7_eval_c_string(s7, "'(:binding?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    /* s7_pointer str = s7_apply_function(s7, arg, */
    /*                                    s7_eval_c_string(s7, "'(:$)")); */
    /* TEST_ASSERT_EQUAL_STRING( "\"arg2c\"", s7_string(str) ); */
}

void test_pkg_load_int_binding_int(void) {
    char *path_str
        = "'(:load 2 :binding 0)";
    s7_pointer path = s7_eval_c_string(s7, path_str);
    s7_pointer arg = s7_apply_function(s7, ast, path);

    TEST_ASSERT( s7_is_c_object(arg) );
    TEST_ASSERT( !s7_is_list(s7, arg) );

    s7_pointer pred = s7_apply_function(s7, arg,
                                      s7_eval_c_string(s7, "'(:binding?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    /* s7_pointer str = s7_apply_function(s7, arg, */
    /*                                    s7_eval_c_string(s7, "'(:$)")); */
    /* TEST_ASSERT_EQUAL_STRING( "\"arg2c\"", s7_string(str) ); */
}

void test_pkg_load_src_binding_sym(void) {
    char *path_str
        = "'(:load \"@repoc//pkgc:targetc.bzl\" :binding key1c)";
    s7_pointer path = s7_eval_c_string(s7, path_str);
    s7_pointer arg = s7_apply_function(s7, ast, path);
    TEST_ASSERT( s7_is_c_object(arg) );
    TEST_ASSERT( !s7_is_list(s7, arg) );

    s7_pointer pred = s7_apply_function(s7, arg,
                                     s7_eval_c_string(s7, "'(:binding?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    /* s7_pointer str = s7_apply_function(s7, arg, */
    /*                                    s7_eval_c_string(s7, "'(:$)")); */
    /* TEST_ASSERT_EQUAL_STRING( "\"arg2c\"", s7_string(str) ); */
}

void test_pkg_load_src_binding_sym_prop(void) {
    char *path_str
        = "'(:load \"@repoc//pkgc:targetc.bzl\" :binding key1c :tid->kw)";
    s7_pointer path = s7_eval_c_string(s7, path_str);
    s7_pointer arg = s7_apply_function(s7, ast, path);
    TEST_ASSERT( !s7_is_c_object(arg) );
    TEST_ASSERT( s7_is_keyword(arg) );
    TEST_ASSERT( s7_make_keyword(s7, "binding") == arg );
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_all_loadstmts);

    /* (pkg :load "@repoc//pkgc:targetc.bzl") */
    RUN_TEST(test_pkg_load_src);

    /* (pkg :load "@repoc//pkgc:targetc.bzl" :args) */
    RUN_TEST(test_pkg_load_src_args);

    /* (let ((loadstmt (pkg :load "@repoc//pkgc:targetc.bzl"))) */
    /*     (loadstmt :args)) */
    RUN_TEST(test_loadstmt_args);

    /* (:load "@repoc//pkgc:targetc.bzl" :arg 0) */
    RUN_TEST(test_pkg_load_src_arg_int); /* singular :arg */

    /* (:load "@repoc//pkgc:targetc.bzl" :arg "arg2c") */
    RUN_TEST(test_pkg_load_src_arg_string); /* singular :arg */

    /* (:load "@repoc//pkgc:targetc.bzl" :arg "arg2c :tid->kw") */
    RUN_TEST(test_pkg_load_src_arg_string_prop);

    /* **************** */
    /* (pkg :load "@repoc//pkgc:targetc.bzl" :bindings) */
    RUN_TEST(test_pkg_load_src_bindings);

    /* (pkg :load "@repoc//pkgc:targetc.bzl" :binding 0) */
    RUN_TEST(test_pkg_load_src_binding_int);

    /* (pkg :load "@repoc//pkgc:targetc.bzl" :binding key1c) */
    RUN_TEST(test_pkg_load_src_binding_sym);

    /* (pkg :load "@repoc//pkgc:targetc.bzl" :binding 0 :key) */
    RUN_TEST(test_pkg_load_src_binding_int_key);

    /* (:load "@repoc//pkgc:targetc.bzl" :binding key1c  :tid->kw") */
    RUN_TEST(test_pkg_load_src_binding_sym_prop);

    /* ******************************** */
    /* (pkg :load 1) */
    RUN_TEST(test_pkg_load_int);

    /* (pkg :load 1 :args) */
    RUN_TEST(test_pkg_load_int_args);

    /* (pkg :load 1 :bindings) */
    RUN_TEST(test_pkg_load_int_bindings);

    /* (pkg :load 2 :binding 0) */
    RUN_TEST(test_pkg_load_int_binding_int);

    return UNITY_END();
}
