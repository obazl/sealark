#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_loads.h"

UT_string *buf;
UT_string *test_s;
/* UT_array  *result; */

char *build_file = "test/unit/sunlark/BUILD.loads";

s7_scheme *s7;

struct parse_state_s *parse_state;

static s7_pointer ast;
struct node_s *root;

void setUp(void) {
    s7 = sunlark_init();
    /* parse_state = sealark_parse_file(build_file); */
    /* ast = parse_state->root; */
    ast = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    root = s7_c_object_value(ast);
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);
    s7_quit(s7);
}

void test_all_loads(void) {
    s7_pointer path = s7_eval_c_string(s7,
                       "'(:loads)");
    s7_pointer loads = s7_apply_function(s7, ast, path);

    TEST_ASSERT( ! s7_is_c_object(loads) );
    TEST_ASSERT(   s7_is_list(s7, loads) );

    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, loads);
    s7_pointer binding = s7_iterate(s7, iter);
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        TEST_ASSERT( s7_is_c_object(binding) );
        TEST_ASSERT( !s7_is_list(s7, binding) );
        pred = s7_apply_function(s7,
                                 binding,
                                 s7_eval_c_string(s7, "'(:load-stmt?)"));
        TEST_ASSERT( pred == s7_t(s7) );

        binding = s7_iterate(s7, iter);
    }
}


int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_all_loads);
    return UNITY_END();
}
