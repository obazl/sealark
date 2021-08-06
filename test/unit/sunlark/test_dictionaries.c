#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_dictionaries.h"

char *build_file = "test/unit/sunlark/BUILD.dictionaries";

static s7_pointer pkg;          /* set by setUp routine below */

int main(void) {
    UNITY_BEGIN();
    /* RUN_TEST(test_pkg__lksd_attr); /\* lksd: label-keyed string dict *\/ */
    RUN_TEST(test_pkg__sld_attr); /* sld: string-list dict */
    return UNITY_END();
}

void test_pkg__sld_attr(void) {
    form = "'(:> \"string-list-dict\")";
    path = s7_eval_c_string(s7, form);
    target = s7_apply_function(s7, pkg, path);
    TEST_ASSERT( s7_is_c_object(target) );
    TEST_ASSERT( sunlark_node_tid(s7, target) == TK_Call_Expr );
    pred = s7_apply_function(s7, target, target_p);
    TEST_ASSERT( pred == s7_t(s7) );

    binding = s7_apply_function(s7, target,
                                s7_eval_c_string(s7, "'(:@ bdict)"));
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );

    s7_pointer key = s7_apply_function(s7, binding, key_op);
    pred = s7_apply_function(s7, key,
                             s7_eval_c_string(s7, "'(:id?)"));
    TEST_ASSERT( pred == s7_t(s7) );
    /* starlark attr key node => scheme symbol   */
    s7_pointer ksym = s7_apply_function(s7, key, dollar_op);
    TEST_ASSERT(s7_is_symbol(ksym));
    TEST_ASSERT_EQUAL_STRING( "bdict", s7_symbol_name(ksym));

    /* (define val (binding :value) */
    val = s7_apply_function(s7, binding, value_op);
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_Dict_Expr );
    pred = NULL; pred = s7_apply_function(s7, val,
                             s7_eval_c_string(s7, "'(:dict-expr?)"));
    TEST_ASSERT( pred = s7_t(s7) );

    /* (define dentry (val "@brepo//bkey1")) => entire entry for key */
    s7_pointer dentry = s7_apply_function(s7, val,
                                          s7_list(s7, 1,
                                          s7_make_string(s7,
                                          "bkey2")));
    TEST_ASSERT( sunlark_node_tid(s7, dentry) == TK_Dict_Entry );
    pred = NULL; pred = s7_apply_function(s7, dentry,
                             s7_eval_c_string(s7, "'(:dict-entry?)"));
    TEST_ASSERT( pred = s7_t(s7) );

    /* (dentry :key) => "bkey2" */
    s7_pointer k = s7_apply_function(s7, dentry, key_op);
    TEST_ASSERT( sunlark_node_tid(s7, k) == TK_STRING );

    /* (dentry :value) => ["bstr21", "bstr22", "bstr23"] */
    s7_pointer lval = s7_apply_function(s7, dentry, value_op);
    TEST_ASSERT( sunlark_node_tid(s7, lval) == TK_List_Expr );

    /* (v :1) => "bstr22" */
    s7_pointer item = s7_apply_function(s7, lval,
                                        s7_eval_c_string(s7, "'(:1)"));
    TEST_ASSERT( sunlark_node_tid(s7, item) == TK_STRING );
    s7_pointer istr = s7_apply_function(s7, item, dollar_op);
    TEST_ASSERT_EQUAL_STRING( "\"bstr22\"", s7_string(istr));
}

void test_pkg__lksd_attr(void) {
    form = "'(:> \"label-keyed-string-dict\" :@ bdict)";
    path = s7_eval_c_string(s7, form);
    binding = s7_apply_function(s7, pkg, path);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );
    pred = s7_apply_function(s7, binding, binding_p);
    TEST_ASSERT( pred == s7_t(s7) );

    key = s7_apply_function(s7, binding, key_op);
    TEST_ASSERT( sunlark_node_tid(s7, key) == TK_ID );
    /* starlark attr key node => scheme symbol   */
    s7_pointer sym = s7_apply_function(s7, key, dollar_op);
    TEST_ASSERT(s7_is_symbol(sym));
    TEST_ASSERT_EQUAL_STRING( "bdict", s7_symbol_name(sym));

    /* (define val (binding :value) */
    val = s7_apply_function(s7, binding, value_op);
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_Dict_Expr );
    pred = NULL; pred = s7_apply_function(s7, val,
                             s7_eval_c_string(s7, "'(:dict-expr?)"));
    TEST_ASSERT( pred = s7_t(s7) );

    /* /\* (define dentry (val "@brepo//bkey1")) => entire entry for key *\/ */
    s7_pointer dentry = s7_apply_function(s7, val,
                                          s7_list(s7, 1,
                                          s7_make_string(s7,
                                          "@brepo//bkey1")));
    TEST_ASSERT( sunlark_node_tid(s7, dentry) == TK_Dict_Entry );
    pred = NULL; pred = s7_apply_function(s7, dentry,
                             s7_eval_c_string(s7, "'(:dict-entry?)"));
    TEST_ASSERT( pred = s7_t(s7) );

    /* (dentry :key) => "@brepo//bkey1" */
    s7_pointer k = s7_apply_function(s7, dentry, key_op);
    TEST_ASSERT( sunlark_node_tid(s7, k) == TK_STRING );

    /* (dentry :value) => "@brepo//bkey1" */
    s7_pointer v = s7_apply_function(s7, dentry, key_op);
    TEST_ASSERT( sunlark_node_tid(s7, v) == TK_STRING );
}

/* **************************************************************** */
void test_parse_str_dict(void) {
    char *adict = "{'akey1': 'aval1', 'akey2': 'aval2' }\n";
    s7_pointer target = sunlark_parse_string(s7, s7_make_string(s7, adict));
    /* to see the structure of target_node: */
    struct node_s *dict_node = s7_c_object_value(target);
    sealark_debug_log_ast_outline(dict_node, 0);

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

/* **************************************************************** */
UT_string *buf;
UT_string *test_s;
/* UT_array  *result; */

s7_scheme *s7;

struct parse_state_s *parse_state;

void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
    pkg = sunlark_parse_build_file(s7,
                                   s7_list(s7, 1,
                                           s7_make_string(s7, build_file)));
}

void tearDown(void) {
    sealark_parse_state_free(parse_state);
    s7_quit(s7);
}

