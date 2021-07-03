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

void test_android(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.android") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.android"
    );
}

void test_cpp(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.cpp") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.cpp"
    );
}

void test_general_rules(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.filegroup") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.filegroup"
    );

    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.gen") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.gen"
    );
}

void test_load(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.load_1") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.load_1"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.load_2") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.load_2"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.load_3") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.load_3"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.load_aliases_1") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.load_aliases_1"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.load_aliases_2") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.load_aliases_2"
    );
}

void test_python(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.py") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.py"
    );

    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.py_native") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.py_native"
    );
}

void test_shell(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.shell") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.shell"
    );
}

void test_windows(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/BUILD.windows_dll") == EXIT_SUCCESS,
        "Mismatch on test/data/BUILD.windows_dll"
    );
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_android);
    RUN_TEST(test_cpp);
    RUN_TEST(test_general_rules);
    RUN_TEST(test_load);
    RUN_TEST(test_python);
    RUN_TEST(test_shell);
    RUN_TEST(test_windows);
    return UNITY_END();
}
