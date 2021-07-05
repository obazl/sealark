#include <errno.h>
#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"

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
        roundtrip("test/data/android.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/android.BUILD"
    );
}

void test_cpp(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/cpp.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/cpp.BUILD"
    );
}

void test_general_rules(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/filegroup.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/filegroup.BUILD"
    );

    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/gen.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/gen.BUILD"
    );
}

void test_load(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/load_1.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/load_1.BUILD"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/load_2.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/load_2.BUILD"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/load_3.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/load_3.BUILD"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/load_aliases_1.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/load_aliases_1.BUILD"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/load_aliases_2.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/load_aliases_2.BUILD"
    );
}

void test_python(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/py.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/py.BUILD"
    );

    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/py_native.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/py_native.BUILD"
    );
}

void test_shell(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/shell.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/shell.BUILD"
    );
}

void test_windows(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/windows_dll.BUILD") == EXIT_SUCCESS,
        "Mismatch on test/data/windows_dll.BUILD"
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
