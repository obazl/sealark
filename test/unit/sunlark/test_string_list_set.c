#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

#include "sealark.h"
#include "sunlark.h"

#include "test_bool_set.h"

UT_string *buf;
UT_string *test_s;

char *build_file = "test/unit/sunlark/BUILD.binding_set";

s7_scheme *s7;

/**************/
int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_set_string_list_to_bool);
    /* RUN_TEST(test_set_string_list_to_int); */
    /* RUN_TEST(test_set_string_list_to_int_list); */
    /* RUN_TEST(test_set_string_list_to_string); */
    return UNITY_END();
}

/* struct parse_state_s *parse_state; */

static s7_pointer ast;
struct node_s *root;

static s7_pointer result;
static s7_pointer item, val, count;

void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);

    ast = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    s7_define_variable(s7, "ast", ast);
    root = s7_c_object_value(ast);
}

void tearDown(void) {
    s7_quit(s7);
}

/* **************************************************************** */
void test_set_string_list_to_bool(void) {
    /* int_list = [1, 2, 3], */
    char *cmd = "(set! (ast :> 0 :@ 'int_list :value) #t)";
    s7_pointer r = s7_eval_c_string(s7, cmd);
    log_debug("r: %s", s7_object_to_c_string(s7, r));
    s7_pointer path = s7_eval_c_string(s7, "'(:> 0 :@ int_list)");
    s7_pointer binding = s7_apply_function(s7, ast, path);
    log_debug("binding: %s", s7_object_to_c_string(s7, binding));
    s7_pointer valnode= s7_apply_function(s7, binding,
                                          s7_eval_c_string(s7, "'(:value)"));
    s7_pointer val= s7_apply_function(s7, valnode,
                                      s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_t(s7) == val );
    TEST_ASSERT_EQUAL_INT( 1, s7_boolean(s7, val));
}
