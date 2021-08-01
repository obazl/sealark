#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_dictionaries.h"

UT_string *buf;
UT_string *test_s;
/* UT_array  *result; */

char *build_file = "test/unit/sunlark/BUILD.dictionaries";

s7_scheme *s7;

struct parse_state_s *parse_state;

static s7_pointer ast;
struct node_s *root;

void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
    ast = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
    root = s7_c_object_value(ast);
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);
    s7_quit(s7);
}

void test_dict(void) {
    char *adict = "{'akey1': 'aval1', 'akey2': 'aval2' }\n";
    s7_pointer target = sunlark_parse_string(s7, s7_make_string(s7, adict));
    /* to see the structure of target_node: */
    struct node_s *dict_node = s7_c_object_value(target);
    sealark_debug_print_ast_outline(dict_node, 0);

    /* check type, tid. We use :target, but in the ast TK_Call_Expr */
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Dict_Expr );
    s7_pointer is_target = s7_apply_function(s7, target,
                                 s7_eval_c_string(s7, "'(:dict-expr?)"));
    TEST_ASSERT( is_target == s7_t(s7) );

    /* (target :rule) => cc_binary */
    /* s7_pointer rule = s7_apply_function(s7, target, */
    /*                                     s7_eval_c_string(s7, "'(:rule)")); */
    /* TEST_ASSERT( s7_t(s7) == is_id ); */
    /* TEST_ASSERT( s7_is_c_object(rule) ); */
    /* TEST_ASSERT( !s7_is_string(rule) ); */
    /* TEST_ASSERT( sunlark_node_tid(s7, rule) == TK_ID ); */

    /* /\* check underlying c structure *\/ */
    /* TEST_ASSERT( target_node->tid == TK_Call_Expr ); */
    /* struct node_s *id_node = utarray_eltptr(target_node->subnodes, 0); */
    /* TEST_ASSERT( id_node->tid == TK_ID ); */
    /* TEST_ASSERT( strncmp(id_node->s, "foo_library", 11) == 0); */
    /* TEST_ASSERT( strlen(id_node->s) == 11); */

}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_dict);
    return UNITY_END();
}
