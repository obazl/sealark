#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

#include "sealark.h"
#include "sunlark.h"

#include "test_binding_set.h"

/* UT_string *buf; */
/* UT_string *test_s; */

char *build_file = "test/unit/sunlark/BUILD.binding_set";

s7_scheme *s7;

/**************/
int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_binding_replace);

    return UNITY_END();
}

/* **************************************************************** */
void test_binding_replace(void)
{
    form = "'(:> \"bindings-set-1\")";
    path = s7_eval_c_string(s7, form);
    target = s7_apply_function(s7, pkg, path);
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Call_Expr );

    /* log_debug("Before set!"); */
    /* sealark_debug_log_ast_outline(s7_c_object_value(target), 0); */

    dentry = s7_apply_function(s7, target,
                               s7_eval_c_string(s7, "'(:@ binding2)"));
    TEST_ASSERT( s7_is_c_object(dentry) );
    TEST_ASSERT( sunlark_node_tid(s7, dentry) == TK_Binding );

    /* (set! (pkg :> "bindings-set-1" :@ 'int_list)
              #@(newb "new str binding")) */
    getter = s7_list(s7, 1, dentry);
                     /* s7_make_keyword(s7, "value"), */
                     /* s7_make_keyword(s7, "0")); */

    s7_pointer newb = s7_eval_c_string(s7,
                             "(make-binding 'newb \"new str binding\")");

    sealark_debug_log_ast_outline(s7_c_object_value(newb), 0);

    s7_pointer result = s7_apply_function(s7, set_bang,
                                          s7_list(s7, 2,
                                                  getter,
                                                  newb));
    log_debug("xxxxxxxxxxxxxxxx %s", s7_object_to_c_string(s7, result));
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
    if ((errmsg) && (*errmsg))
            log_error("ERROR [%s]", errmsg);

    log_debug("After set!");
    sealark_debug_log_ast_outline(s7_c_object_value(target), 0);
    /* form = "'(:> \"string-list-dict\" :@ bdict)"; */
    /* path = s7_eval_c_string(s7, form); */
    /* result = s7_apply_function(s7, pkg, path); */
    /* sealark_debug_log_ast_outline(s7_c_object_value(target), 0); */
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

