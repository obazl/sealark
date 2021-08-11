#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_dictionary_set.h"

char *build_file = "test/unit/sunlark/BUILD.dictionaries";

/* static s7_pointer pkg;          /\* set by setUp routine below *\/ */

int main(void) {
    UNITY_BEGIN();
    /* sld: string-list dict */
    /* RUN_TEST(test_pkg_path_5_rm_binding); */
    /* RUN_TEST(test_pkg_path_2_4_2_rm_list_item); */
    RUN_TEST(test_pkg_path_2_4_rm_dentry);
    return UNITY_END();
}

void test_pkg_path_5_rm_binding(void)
{
    form = "'(:> \"string-list-dict\")";
    path = s7_eval_c_string(s7, form);
    target = s7_apply_function(s7, pkg, path);
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Call_Expr );

    dentry = s7_apply_function(s7, target,
                               s7_eval_c_string(s7, "'(:@ adict)"));
    TEST_ASSERT( s7_is_c_object(dentry) );
    TEST_ASSERT( sunlark_node_tid(s7, dentry) == TK_Binding );

    getter = s7_list(s7, 3, dentry,
                     s7_make_keyword(s7, "value"),
                     s7_make_keyword(s7, "0"));

    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2, getter,
                                          s7_make_keyword(s7, "null")));

    /* form = "'(:> \"string-list-dict\" :@ bdict)"; */
    /* path = s7_eval_c_string(s7, form); */
    /* result = s7_apply_function(s7, pkg, path); */
    /* sealark_debug_log_ast_outline(s7_c_object_value(target), 0); */
}

void test_pkg_path_2_4_rm_dentry(void)
{
    char *form2_ = "'(:> \"string-list-dict\")";
    s7_pointer path2_ = s7_eval_c_string(s7, form2_);
    target = s7_apply_function(s7, pkg, path2_);

    char *form_dexpr = "'(:@ adict :value)"; /* for verifying */
    s7_pointer path_dexpr = s7_eval_c_string(s7, form_dexpr);
    dexpr = s7_apply_function(s7, target, path_dexpr);
    s7_pointer len = s7_apply_function(s7, dexpr, length_op);
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
    if ((errmsg) && (*errmsg))
            log_error("ERROR [%s]", errmsg);
    TEST_ASSERT( s7_is_integer(len) );
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(len) );

    //NB: both list and syms must be quoted
    /* char *form_4 = "'(:@ 'adict :value \"akey2\"))"; */
    /* s7_pointer path_4 = s7_eval_c_string(s7, form_4); */
    /* getter = s7_cons(s7, target, path_4); */

    /* s7_pointer set = s7_apply_function(s7, set_bang, */
    /*                                    s7_list(s7, 2, getter, null_kw)); */
    /* len = NULL; */
    /* len = s7_apply_function(s7, dexpr, length_op); */
    /* TEST_ASSERT( s7_is_integer(len) ); */
    /* TEST_ASSERT_EQUAL_INT( 2, s7_integer(len) ); */

    /* char *verify = "(pkg :> \"string-list-dict\" :@ 'adict :$ \"akey2\")"; */
    /* s7_pointer get = s7_eval_c_string(s7, verify); */
    /* TEST_ASSERT_EQUAL_STRING( "not_found", s7_symbol_name(get) ); */

    /* TEST_ASSERT( sunlark_node_tid(s7, get) == TK_STRING ); */
    /* s7_pointer istr = s7_apply_function(s7, get, dollar_op); */
    /* TEST_ASSERT_EQUAL_STRING( "\"astr12\"", s7_string(istr)); */
}

void test_pkg_path_2_4_2_rm_list_item(void)
{
    char *form2 = "'(:> \"string-list-dict\")";
    char *form3 = "'(:@ adict :value)"; /* for verifying */
    char *form4 = "'(:@ adict :$ \"akey1\")";
    char *form_2 = "'(:value :0)";

    s7_pointer path2 = s7_eval_c_string(s7, form2);
    s7_pointer path3 = s7_eval_c_string(s7, form3);
    s7_pointer path4 = s7_eval_c_string(s7, form4);
    s7_pointer path_2 = s7_eval_c_string(s7, form_2);

    target = s7_apply_function(s7, pkg, path2);
    dexpr = s7_apply_function(s7, target, path3);
    dentry = s7_apply_function(s7, target, path4);

    s7_pointer len_before = s7_apply_function(s7, dexpr, length_op);
    TEST_ASSERT( s7_is_integer(len_before) );
    TEST_ASSERT_EQUAL_INT( 3, s7_integer(len_before) );

    getter = s7_cons(s7, dentry, path_2);
    s7_pointer set = s7_apply_function(s7, set_bang,
                                       s7_list(s7, 2, getter, null_kw));

    /* now item :0 is "astr12" */
    s7_pointer get = s7_apply_function(s7, dentry, path_2);
    TEST_ASSERT( sunlark_node_tid(s7, get) == TK_STRING );
    s7_pointer istr = s7_apply_function(s7, get, dollar_op);
    TEST_ASSERT_EQUAL_STRING( "\"astr12\"", s7_string(istr));
}

/* **************************************************************** */
UT_string *buf;
UT_string *test_s;
/* UT_array  *result; */

s7_scheme *s7;

struct parse_state_s *parse_state;

void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
    s7_define_function(s7, "error-handler",
                       error_handler, 1, 0, false, "our error handler");

    const char *errmsg = NULL;

    /* trap error messages */
    old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
    if (old_port != s7_nil(s7))
        gc_loc = s7_gc_protect(s7, old_port);

    pkg = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    s7_define_variable(s7, "pkg", pkg);
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);
    s7_quit(s7);
}
