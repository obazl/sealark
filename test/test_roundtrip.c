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

void test_cpp(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.cpp") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.cpp"
    );
}

void test_gen(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.gen") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.gen"
    );
}

void test_filegroup(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.filegroup") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.filegroup"
    );
}

void test_load_aliases(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.load_aliases_1") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.load_aliases_1"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.load_aliases_2") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.load_aliases_2"
    );
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_cpp);
    RUN_TEST(test_filegroup);
    RUN_TEST(test_gen);
    RUN_TEST(test_load_aliases);
    return UNITY_END();
}
