#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_common_props.h"

UT_string *buf;
UT_string *test_s;

char *build_file = "test/unit/sunlark/BUILD.targets";

s7_scheme *s7;

struct parse_state_s *parse_state;
struct node_s *root;

static s7_pointer pkg;

void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
    pkg = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    root = s7_c_object_value(pkg);
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);
    s7_quit(s7);
}

/* **************************************************************** */
/* common properties: :tid, :tid->kw, :tid->string, :node-type, :line, :col, :trailing_newline, :qtype, :s, node?
:<nodetype>? (e.g. :binding?, :list-expr?, etc
:subnode-count, :subnode-count-recursive, :printable-subnode-count-recursive
:length
:subnodes
 */
void validate_common_props(s7_pointer node)
{
    s7_pointer pred = s7_apply_function(s7, node,
                                        s7_eval_c_string(s7, "'(:node?)"));
    TEST_ASSERT( pred == s7_t(s7) );
}

/* **************************************************************** */
void test_target_props(void) {
    s7_pointer path = s7_eval_c_string(s7, "'(:> \"hello-world\")");
    s7_pointer target = s7_apply_function(s7, pkg, path);
    validate_common_props(target);
    s7_pointer nd = s7_apply_function(s7, target,
                                       s7_eval_c_string(s7, "'(:tid)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( TK_Call_Expr, s7_integer(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:tid->kw)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":call-expr", s7_symbol_name(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:tid->string)"));
    TEST_ASSERT( s7_is_string(nd) );
    TEST_ASSERT_EQUAL_STRING( "call-expr", s7_string(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:node-type)"));
    TEST_ASSERT( s7_is_keyword(nd) );
    TEST_ASSERT_EQUAL_STRING( ":call-expr", s7_symbol_name(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:line)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 19, s7_integer(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:col)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 0, s7_integer(nd) );

/* :subnode-count, :subnode-count-recursive, :printable-subnode-count-recursive */
/* :length */
/* :subnodes */
    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:length)"));
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(nd) ); /* Call_Expr */

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:subnode-count)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 2, s7_integer(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7, "'(:subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 32, s7_integer(nd) );

    nd = s7_apply_function(s7, target,
                            s7_eval_c_string(s7,
                                             "'(:printable-subnode-count-recursive)"));
    TEST_ASSERT( s7_is_integer(nd) );
    TEST_ASSERT_EQUAL_INT( 22, s7_integer(nd) );
}

/* **************************************************************** */
int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_target_props);

    /* RUN_TEST(test_tgt_at_sym);      /\* (:@ srcs) *\/ */

    /* RUN_TEST(test_tgt_at_sym_key);      /\* (:@ srcs :key) *\/ */
    /* RUN_TEST(test_tgt_at_sym_value);    /\* (:@ srcs :value) *\/ */

    /* RUN_TEST(test_tgt_at_sym_value_i);    /\* (:@ srcs :value 0) *\/ */

    return UNITY_END();
}
