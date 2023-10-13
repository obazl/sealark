#include <errno.h>
#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"

#include "test_starlark.h"

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
        roundtrip("test/data/android/BUILD.test") == EXIT_SUCCESS,
        "Mismatch on test/data/android/BUILD.test"
    );
}

void test_cc(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/cc/BUILD.test") == EXIT_SUCCESS,
        "Mismatch on test/data/cc/BUILD.test"
    );
}

void test_general_rules(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/filegroup/BUILD.test") == EXIT_SUCCESS,
        "Mismatch on test/data/filegroup/BUILD.test"
    );

    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/genrules/BUILD.test1") == EXIT_SUCCESS,
        "Mismatch on test/data/genrules/BUILD.test1"
    );
}

void test_load(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/load/BUILD.test1") == EXIT_SUCCESS,
        "Mismatch on test/data/load/BUILD.test1"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/load/BUILD.test2") == EXIT_SUCCESS,
        "Mismatch on test/data/load/BUILD.test2"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/load/BUILD.testsq") == EXIT_SUCCESS,
        "Mismatch on test/data/load/BUILD.testsq"
    );
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/load/BUILD.args") == EXIT_SUCCESS,
        "Mismatch on test/data/load/BUILD.args"
    );
    /* TEST_ASSERT_MESSAGE( */
    /*     roundtrip("test/data/load/_aliases_2.BUILD") == EXIT_SUCCESS, */
    /*     "Mismatch on test/data/load/_aliases_2.BUILD" */
    /* ); */
}

void test_python(void) {
    /* TEST_ASSERT_MESSAGE( */
    /*     roundtrip("test/data/py.BUILD") == EXIT_SUCCESS, */
    /*     "Mismatch on test/data/py.BUILD" */
    /* ); */

    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/python/native/BUILD.test") == EXIT_SUCCESS,
        "Mismatch on test/data/python/native/BUILD.test"
    );
}

void test_shell(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/shell/BUILD.test") == EXIT_SUCCESS,
        "Mismatch on test/data/shell/BUILD.test"
    );
}

void test_windows(void) {
    TEST_ASSERT_MESSAGE(
        roundtrip("test/data/windows/dll/BUILD.test") == EXIT_SUCCESS,
        "Mismatch on test/data/windows/dll/BUILD.test"
    );
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_android);
    RUN_TEST(test_cc);
    RUN_TEST(test_general_rules);
    RUN_TEST(test_load);
    RUN_TEST(test_python);
    RUN_TEST(test_shell);
    RUN_TEST(test_windows);
    return UNITY_END();
}
