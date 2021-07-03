#include <errno.h>
#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "starlark.h"
#include "syntaxis.h"

#include "roundtrip.h"

UT_string *build_file;

UT_string *buffer;

void setUp(void) {
    utstring_new(build_file);
    utstring_new(buffer);
}

void tearDown(void) {
    utstring_free(build_file);
    utstring_free(buffer);
}

void test_1(void) {
    TEST_ASSERT(roundtrip("data/bazel/examples/BUILD.test") == EXIT_SUCCESS);
}

void test_cpp(void) {
    TEST_ASSERT(roundtrip("data/bazel/examples/cpp/BUILD.test") == EXIT_SUCCESS);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_1);
    RUN_TEST(test_cpp);
    return UNITY_END();
}
