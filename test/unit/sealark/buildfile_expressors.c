#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "sealark.h"

#include "buildfile_expressors.h"

UT_string *buf;
UT_string *test_s;
UT_array  *result;

struct parse_state_s *parse_state;


void setUp(void) {
    parse_state = sealark_parse_file(build_file);
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);
}

void test_toplevel(void) {
        result = starlark_lex_string(utstring_body(test_s));
        utstring_renew(buf);
        starlark_nodelist2string(result, buf);
        /* printf(utstring_body(buf)); */
        TEST_ASSERT_EQUAL_STRING(utstring_body(test_s), utstring_body(buf));
        utarray_free(result);
    }
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_toplevel);
    return UNITY_END();
}
