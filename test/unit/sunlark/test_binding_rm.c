#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

#include "sealark.h"
#include "sunlark.h"

#include "test_binding_rm.h"

char *build_file = "test/unit/sunlark/BUILD.binding_rm";

s7_scheme *s7;

/**************/
int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_remove_binding_by_int);
    RUN_TEST(test_remove_binding_by_sym);

    RUN_TEST(test_local_remove_binding_by_int);

    RUN_TEST(test_rm_binding_val_item_by_int);
    return UNITY_END();
}

/* **************************************************************** */
void test_rm_binding_val_item_by_int(void)
{
    form = "'(:> \"binding-rm-1\")";
    path = s7_eval_c_string(s7, form);
    target = s7_apply_function(s7, pkg, path);
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Call_Expr );

    dentry = s7_apply_function(s7, target,
                               s7_eval_c_string(s7, "'(:@ binding3)"));
    TEST_ASSERT( s7_is_c_object(dentry) );
    TEST_ASSERT( sunlark_node_tid(s7, dentry) == TK_Binding );

    /* (let ((target (pkg :> "binding-rm-1"))
             (dentry (target :@ 'binding3))
           (set! (dentry :value :0) :null)) */

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

void test_remove_binding_by_int(void) {
    /* remove bool_attr attrib */
    char *s = "'(:> \"binding-rm-1\")";
    s7_pointer path = s7_eval_c_string(s7, s);
    s7_pointer target = s7_apply_function(s7, pkg, path);
    s7_pointer pred= s7_apply_function(s7, target,
                                       s7_eval_c_string(s7, "'(:target?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    s7_pointer attrs = s7_apply_function(s7, target,
                                         s7_eval_c_string(s7, "'(:@@)"));
    s7_pointer len = s7_apply_function(s7, attrs,
                            s7_eval_c_string(s7, "'(:length)"));
    TEST_ASSERT_EQUAL_INT( 6, s7_integer(len) );

    s7_pointer binding = s7_apply_function(s7, target,
                                      s7_eval_c_string(s7, "'(:@ 1)"));
    pred= s7_apply_function(s7, binding,
                            s7_eval_c_string(s7, "'(:binding?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    /* (let ((target (pkg :> * "binding-rm-1"))
         (set! (target :@ 1) :null))) */
    s7_pointer getter = s7_list(s7, 3, target,
                                s7_make_keyword(s7,"@"),
                                s7_make_integer(s7,1));

    s7_pointer res = s7_apply_function(s7, set_bang,
                                       s7_list(s7, 2,
                                               getter,
                                               s7_make_keyword(s7,"null")));
    attrs = NULL;
    attrs = s7_apply_function(s7, target,
                              s7_eval_c_string(s7, "'(:@@)"));
    len = NULL;
    len = s7_apply_function(s7, attrs,
                            s7_eval_c_string(s7, "'(:length)"));
    TEST_ASSERT_EQUAL_INT( 5, s7_integer(len) );

    /* sealark_debug_log_ast_outline(s7_c_object_value(target), 0); */


    /* s7_pointer target = s7_apply_function(s7, pkg, path); */

    /* pred = NULL; */
    /* pred= s7_apply_function(s7, binding, */
    /*                         s7_eval_c_string(s7, "'(:binding?)")); */
    /* TEST_ASSERT( pred == s7_t(s7) ); */

    /* result = s7_apply_function(s7, set_bang, */
    /*                            s7_list(s7, 2, getter, */
    /*                                    s7_make_keyword(s7, "null"))); */

    /* val = s7_apply_function(s7, result, s7_eval_c_string(s7, "'(:$)")); */
    /* val = s7_apply_function(s7, val, s7_eval_c_string(s7, "'(:$)")); */
    /* TEST_ASSERT_EQUAL_INT( 99, s7_integer(val)); */
}

void test_remove_binding_by_sym(void) {
    /* remove binding1 attrib */
    char *bstr = "(pkg :> \"binding-rm-1\" :@ 'binding1)";
    s7_pointer binding = s7_eval_c_string(s7, bstr);
    TEST_ASSERT( s7_is_c_object(binding));

    bstr = "(pkg :> \"binding-rm-1\" :@@)"; // NB: no quote!
    s7_pointer bindings = s7_eval_c_string(s7, bstr);
    s7_pointer len = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                                       s7_list(s7, 1, bindings));
    TEST_ASSERT_EQUAL_INT( 6, s7_integer(len) );

    /* NB: no quote on (set! ...) */
    char *rm = "(set! (pkg :> \"binding-rm-1\" :@ 'binding1) :null)";
    s7_pointer set = s7_eval_c_string(s7, rm);

    len = NULL;
    len = s7_apply_function(s7, s7_name_to_value(s7,"length"),
                                       s7_list(s7, 1, bindings));
    TEST_ASSERT_EQUAL_INT( 5, s7_integer(len) );

    bstr = "(pkg :> \"binding-rm-1\" :@ 'binding1)";
    binding = s7_eval_c_string(s7, bstr);
    TEST_ASSERT( ! s7_is_c_object(binding));
    TEST_ASSERT( binding == s7_make_symbol(s7,"binding_not_found") );

    /* look for error messages */
    /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
    /* if ((errmsg) && (*errmsg)) */
    /*     log_error("CAUGHT: [%s]", errmsg); */

}

/* **************************************************************** */
void test_local_remove_binding_by_int(void)
{
    char *s = "(define bb (make-binding 'akey '(1 2 3)))";
    s7_pointer binding = s7_eval_c_string(s7, s);
    s7_pointer pred= s7_apply_function(s7, binding,
                                       s7_eval_c_string(s7, "'(:binding?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    /* s7_pointer getter = s7_list(s7, 3, */
    /*                             binding, */
    /*                             s7_make_keyword(s7, "value"), */
    /*                             s7_make_keyword(s7, "0")); */

    /* s7_pointer result = s7_apply_function(s7, set_bang, */
    /*                                       s7_list(s7, 2, getter, */
    /*                                       s7_make_keyword(s7, "null"))); */

    char *rm = "(set! (bb :$ :0) :null)";
    s7_pointer set = s7_eval_c_string(s7, rm);

    /* s7_pointer vec = s7_apply_function(s7, binding, */
    /*                                    s7_eval_c_string(s7, "'(:value)")); */
    /* s7_pointer len = s7_apply_function(s7, vec, */
    /*                         s7_eval_c_string(s7, "'(:length)")); */
    /* TEST_ASSERT_EQUAL_INT( 2, s7_integer(len) ); */
}

/* **************************************************************** */
void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);

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
    s7_quit(s7);
}

