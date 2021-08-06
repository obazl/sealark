#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "test_make_binding.h"

s7_scheme *s7;

static s7_pointer tgt;

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_with_true);
    RUN_TEST(test_with_false);

    RUN_TEST(test_with_int);
    RUN_TEST(test_with_int_list);
    RUN_TEST(test_with_int_vector);

    RUN_TEST(test_with_string);
    RUN_TEST(test_with_string_list);
    RUN_TEST(test_with_string_vector);

    RUN_TEST(test_with_symbol);
    RUN_TEST(test_with_symbol_list);
    RUN_TEST(test_with_symbol_vector);

    RUN_TEST(test_with_mixed_list);
    RUN_TEST(test_with_mixed_vector);

    return UNITY_END();
}
/* **************************************************************** */
/* **************************************************************** */
void _validate_key(s7_pointer binding)
{
    struct node_s *binding_node = s7_c_object_value(binding);
    TEST_ASSERT( binding_node->tid == TK_Binding );

    s7_pointer key
        = s7_apply_function(s7, binding,
                            s7_list(s7, 1, s7_make_keyword(s7,"key")));

    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT( sunlark_node_tid(s7, key) == TK_ID );
    s7_pointer str = s7_apply_function(s7, key,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_is_symbol(str) );
    TEST_ASSERT_EQUAL_STRING( "akey", s7_symbol_name(str) );

    /* check underlying c object */
    struct node_s *id_node = utarray_eltptr(binding_node->subnodes, 0);
    TEST_ASSERT( id_node->tid == TK_ID );
    TEST_ASSERT( strncmp(id_node->s, "akey", 4) == 0);
    TEST_ASSERT( strlen(id_node->s) == 4);
}

/* **************************************************************** */
void test_with_true(void) {
    char *s = "(make-binding 'akey #t)";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );

    _validate_key(binding);

    /* verify val == True / #t */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_list(s7,1,s7_make_keyword(s7,"value")));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( !s7_is_boolean(val) ); /* not a Scheme int */

    /* we do not have a TK_Boolean node type atm */
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_ID );

    s7_pointer pred = s7_apply_function(s7, val,
                                        s7_eval_c_string(s7, "'(:id?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    /* Starlark booleans stored in AST as ID nodes True and False, then
       converted to and from Scheme #t and #f */
    /* underlying c */
    struct node_s *binding_node = s7_c_object_value(binding);
    struct node_s *vnode = utarray_eltptr(binding_node->subnodes, 2);
    TEST_ASSERT_EQUAL_INT( TK_ID, vnode->tid);
    TEST_ASSERT_EQUAL_STRING( "True", vnode->s);
    /* convert to Scheme */
    s7_pointer v = s7_apply_function(s7, val,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_boolean(s7, v) );
    TEST_ASSERT( s7_t(s7) == v );
}

void test_with_false(void) {
    char *s = "(make-binding 'akey #f)";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );

    _validate_key(binding);

    /* verify val == False (#f) */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_list(s7,1,s7_make_keyword(s7,"value")));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( !s7_is_boolean(val) ); /* not a Scheme int */

    /* we do not have a TK_Boolean node type atm */
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_ID );

    s7_pointer pred = s7_apply_function(s7, val,
                                        s7_eval_c_string(s7, "'(:id?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    /* Starlark booleans stored in AST as ID nodes True and False, then
       converted to and from Scheme #t and #f */
    /* underlying c */
    struct node_s *binding_node = s7_c_object_value(binding);
    struct node_s *vnode = utarray_eltptr(binding_node->subnodes, 2);
    TEST_ASSERT_EQUAL_INT( TK_ID, vnode->tid);
    TEST_ASSERT_EQUAL_STRING( "False", vnode->s);
    /* convert to Scheme */
    s7_pointer v = s7_apply_function(s7, val,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_is_symbol(v) );
    TEST_ASSERT_EQUAL_STRING( "#f", s7_symbol_name(v) );
}

/* **************************************************************** */
void test_with_int(void) {
    char *s = "(make-binding 'akey 7)";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );
    struct node_s *binding_node = s7_c_object_value(binding);
    TEST_ASSERT( binding_node->tid == TK_Binding );

    s7_pointer key
        = s7_apply_function(s7, binding,
                            s7_list(s7, 1, s7_make_keyword(s7,"key")));
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT( sunlark_node_tid(s7, key) == TK_ID );
    /* key is an identifier, hence :$ returns a Scheme symbol */
    s7_pointer str = s7_apply_function(s7, key,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_is_symbol(str) );
    TEST_ASSERT_EQUAL_STRING( "akey", s7_symbol_name(str) );
    /* check underlying c object */
    struct node_s *id_node = utarray_eltptr(binding_node->subnodes, 0);
    TEST_ASSERT( id_node->tid == TK_ID );
    TEST_ASSERT( strncmp(id_node->s, "akey", 4) == 0);
    TEST_ASSERT( strlen(id_node->s) == 4);

    /* verify val == 7 */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_list(s7,1,s7_make_keyword(s7,"value")));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( !s7_is_integer(val) ); /* not a Scheme int */
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_INT );

    s7_pointer pred = s7_apply_function(s7, val,
                                        s7_eval_c_string(s7, "'(:int?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    s7_pointer v = s7_apply_function(s7, val,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_is_integer(v) );
    TEST_ASSERT_EQUAL_INT( 7, s7_integer(v) );
}

void test_with_int_list(void) {
    char *s = "(make-binding 'akey '(1 2 3))";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );
    struct node_s *binding_node = s7_c_object_value(binding);
    TEST_ASSERT( binding_node->tid == TK_Binding );

    s7_pointer key
        = s7_apply_function(s7, binding,
                            s7_cons(s7, s7_make_keyword(s7,"key"),
                                    s7_nil(s7)));
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT( sunlark_node_tid(s7, key) == TK_ID );
    /* key is an identifier, hence :$ returns a Scheme symbol */
    s7_pointer str = s7_apply_function(s7, key, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_is_symbol(str) );
    TEST_ASSERT_EQUAL_STRING( "akey", s7_symbol_name(str) );

    /* check underlying c object */
    struct node_s *id_node = utarray_eltptr(binding_node->subnodes, 0);
    TEST_ASSERT( id_node->tid == TK_ID );
    TEST_ASSERT( strncmp(id_node->s, "akey", 4) == 0);
    TEST_ASSERT( strlen(id_node->s) == 4);

    /* verify val == [1,2,3] */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_cons(s7, s7_make_keyword(s7,"value"),
                                    s7_nil(s7)));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_List_Expr );
    /* sealark_debug_log_ast_outline(s7_c_object_value(val), 0); */
    s7_pointer vlen
        = s7_apply_function(s7, s7_name_to_value(s7, "length"),
                            s7_list(s7, 1, val));
    TEST_ASSERT_EQUAL(3, s7_integer(vlen));

    /* /\* verify type of each item *\/ */
    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, val);
    s7_pointer item = s7_iterate(s7, iter);
    /* int i = 0; */
    /* while ( ! s7_iterator_is_at_end(s7, iter) ) { */
    /*     TEST_ASSERT( s7_is_c_object(item) ); */
    /*     TEST_ASSERT( !s7_is_list(s7, item) ); */
    /*     TEST_ASSERT( !s7_is_integer(item) ); */
    /*     if ( i % 2 == 0) { */
    /*         pred = s7_apply_function(s7, item, */
    /*                                  s7_eval_c_string(s7, "'(:int?)")); */
    /*         TEST_ASSERT( pred == s7_t(s7) ); */
    /*     } */
    /*     item = s7_iterate(s7, iter); */
    /*     i++; */
    /* } */

    /* /\* index into list *\/ */
    /* for (int i = 0; i < 3; i++) { */
    /*     item = NULL; */
    /*     item = s7_apply_function(s7, val, /\* (list i) == list[i] *\/ */
    /*                              s7_list(s7, 1, s7_make_integer(s7, i))); */
    /*     TEST_ASSERT( s7_is_c_object(item) ); */
    /*     TEST_ASSERT( !s7_is_integer(item) ); /\* not a Scheme int *\/ */
    /*     if ( i % 2 == 0) { /\* skip commas *\/ */
    /*         TEST_ASSERT( sunlark_node_tid(s7, item) == TK_INT ); */
    /*         pred = s7_apply_function(s7, item, /\* but it is an int node *\/ */
    /*                                  s7_eval_c_string(s7, "'(:int?)")); */
    /*         TEST_ASSERT( pred == s7_t(s7) ); */
    /*         /\* whose value is 1 *\/ */
    /*         s7_pointer sval = s7_apply_function(s7, item, */
    /*                                             s7_eval_c_string(s7, "'(:$)")); */
    /*         TEST_ASSERT( !s7_is_c_object(sval) ); */
    /*         TEST_ASSERT( s7_is_integer(sval) ); */
    /*         TEST_ASSERT_EQUAL_INT( i/2+1, s7_integer(sval) ); */
    /*     } */
    /* } */
}

void test_with_int_vector(void) {
    char *s = "(make-binding 'akey #(1 2 3))";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );
    struct node_s *binding_node = s7_c_object_value(binding);
    TEST_ASSERT( binding_node->tid == TK_Binding );

    s7_pointer key
        = s7_apply_function(s7, binding,
                            s7_cons(s7, s7_make_keyword(s7,"key"),
                                    s7_nil(s7)));
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT( sunlark_node_tid(s7, key) == TK_ID );
    /* key is an identifier, hence :$ returns a Scheme symbol */
    s7_pointer str = s7_apply_function(s7, key, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_is_symbol(str) );
    TEST_ASSERT_EQUAL_STRING( "akey", s7_symbol_name(str) );
    /* check underlying c object */
    struct node_s *id_node = utarray_eltptr(binding_node->subnodes, 0);
    TEST_ASSERT( id_node->tid == TK_ID );
    TEST_ASSERT( strncmp(id_node->s, "akey", 4) == 0);
    TEST_ASSERT( strlen(id_node->s) == 4);

    /* verify val == [1,2,3] */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_cons(s7, s7_make_keyword(s7,"value"),
                                    s7_nil(s7)));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_List_Expr );

    s7_pointer vlen
        = s7_apply_function(s7, s7_name_to_value(s7, "length"),
                            s7_list(s7, 1, val));
    TEST_ASSERT_EQUAL(3, s7_integer(vlen));

    /* verify type of each item */
    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, val);
    s7_pointer item = s7_iterate(s7, iter);
    int i = 0;
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        TEST_ASSERT( s7_is_c_object(item) );
        TEST_ASSERT( !s7_is_list(s7, item) );
        TEST_ASSERT( !s7_is_integer(item) );
        if ( i % 2 == 0) {
            pred = s7_apply_function(s7, item,
                                     s7_eval_c_string(s7, "'(:int?)"));
            TEST_ASSERT( pred == s7_t(s7) );
        }
        item = s7_iterate(s7, iter);
        i++;
    }

    /* index into list */
    for (int i = 0; i < 3; i++) {
        item = NULL;
        item = s7_apply_function(s7, val, /* list[i] */
                                 s7_list(s7, 1, s7_make_integer(s7, i)));
        TEST_ASSERT( s7_is_c_object(item) );
        TEST_ASSERT( !s7_is_integer(item) ); /* not a Scheme int */
        TEST_ASSERT( sunlark_node_tid(s7, item) == TK_INT );
        pred = s7_apply_function(s7, item, /* but it is an int node */
                                 s7_eval_c_string(s7, "'(:int?)"));
        TEST_ASSERT( pred == s7_t(s7) );
        /* whose value is 1 */
        s7_pointer sval = s7_apply_function(s7, item,
                                            s7_eval_c_string(s7, "'(:$)"));
        TEST_ASSERT( !s7_is_c_object(sval) );
        TEST_ASSERT( s7_is_integer(sval) );
        TEST_ASSERT_EQUAL_INT( i+1, s7_integer(sval) );
    }
}

/* **************************************************************** */
void test_with_string(void) {
    char *s = "(make-binding 'akey \"astring\")";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );
    struct node_s *binding_node = s7_c_object_value(binding);
    TEST_ASSERT( binding_node->tid == TK_Binding );

    s7_pointer key
        = s7_apply_function(s7, binding,
                            s7_list(s7, 1, s7_make_keyword(s7,"key")));
    TEST_ASSERT( s7_is_c_object(key) );
    TEST_ASSERT( sunlark_node_tid(s7, key) == TK_ID );
    s7_pointer str = s7_apply_function(s7, key, s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_is_symbol(str) );
    TEST_ASSERT_EQUAL_STRING( "akey", s7_symbol_name(str) );
    /* check underlying c object */
    struct node_s *id_node = utarray_eltptr(binding_node->subnodes, 0);
    TEST_ASSERT( id_node->tid == TK_ID );
    TEST_ASSERT( strncmp(id_node->s, "akey", 4) == 0);
    TEST_ASSERT( strlen(id_node->s) == 4);

    /* verify val == "astring" */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_list(s7,1,s7_make_keyword(s7,"value")));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( !s7_is_string(val) ); /* not a Scheme string */
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_STRING );

    s7_pointer pred = s7_apply_function(s7, val,
                                        s7_eval_c_string(s7, "'(:string?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    s7_pointer v = s7_apply_function(s7, val,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_is_string(v) );
    TEST_ASSERT_EQUAL_STRING( "\"astring\"", s7_string(v) );
}

void _validate_stringlist_abc(s7_pointer binding)
{
    /* verify val == ["a","b","c"] */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_cons(s7, s7_make_keyword(s7,"value"),
                                    s7_nil(s7)));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_List_Expr );

    s7_pointer vlen
        = s7_apply_function(s7, s7_name_to_value(s7, "length"),
                            s7_list(s7, 1, val));
    TEST_ASSERT_EQUAL(3, s7_integer(vlen));

    /* verify type of each item */
    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, val);
    s7_pointer item = s7_iterate(s7, iter);
    int i = 0;
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        TEST_ASSERT( s7_is_c_object(item) );
        TEST_ASSERT( !s7_is_list(s7, item) );
        TEST_ASSERT( !s7_is_string(item) ); /* not a Scheme string */
        if ( i % 2 == 0) {
            pred = s7_apply_function(s7, item,  /* but a string node */
                                     s7_eval_c_string(s7, "'(:string?)"));
            TEST_ASSERT( pred == s7_t(s7) );
        }
        i++;
        item = s7_iterate(s7, iter);
    }

    /* index into list */
    for (int i = 0; i < 3; i++) {
        item = NULL;
        item = s7_apply_function(s7, val, /* list[i] */
                                 s7_list(s7, 1, s7_make_integer(s7, i)));
        TEST_ASSERT( s7_is_c_object(item) );
        TEST_ASSERT( !s7_is_string(item) ); /* not a Scheme string */
        /* if ( i % 2 == 0) { */
            TEST_ASSERT( sunlark_node_tid(s7, item) == TK_STRING );
            pred = s7_apply_function(s7, item, /* but it is an string node */
                                     s7_eval_c_string(s7, "'(:string?)"));
            TEST_ASSERT( pred == s7_t(s7) );
            /* Scheme value */
            s7_pointer sval = s7_apply_function(s7, item,
                                                s7_eval_c_string(s7, "'(:$)"));
            TEST_ASSERT( !s7_is_c_object(sval) );
            TEST_ASSERT( s7_is_string(sval) );
            TEST_ASSERT_EQUAL_STRING((i==0)? "\"astring\""
                                     : (i==1)? "\"bstring\""
                                     : (i==2)? "\"cstring\""
                                     : "foo",
                                     s7_string(sval) );
        /* } */
    }
}

void test_with_string_list(void) {
    char *s = "(make-binding 'akey '(\"astring\" \"bstring\" \"cstring\"))";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );
    struct node_s *binding_node = s7_c_object_value(binding);
    TEST_ASSERT( binding_node->tid == TK_Binding );

    _validate_key(binding);

    _validate_stringlist_abc(binding);
}

void test_with_string_vector(void) {
    char *s = "(make-binding 'akey #(\"astring\" \"bstring\" \"cstring\"))";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );

    _validate_key(binding);

    _validate_stringlist_abc(binding);
}

/* **************************************************************** */
void test_with_symbol(void) {
    char *s = "(make-binding 'akey 'asymbol)";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );

    _validate_key(binding);

    /* verify val == 'asymbol */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_list(s7,1,s7_make_keyword(s7,"value")));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( !s7_is_symbol(val) ); /* not a Scheme symbol */
    /* in the AST, Scheme symbols are identifiers */
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_ID );

    s7_pointer pred = s7_apply_function(s7, val,
                                        s7_eval_c_string(s7, "'(:symbol?)"));
    TEST_ASSERT( pred == s7_t(s7) );

    s7_pointer v = s7_apply_function(s7, val,
                                       s7_eval_c_string(s7, "'(:$)"));
    TEST_ASSERT( s7_is_symbol(v) );
    TEST_ASSERT_EQUAL_STRING( "asymbol", s7_symbol_name(v) );
}

void _validate_symlist_abc(s7_pointer binding)
{
    /* verify val == [a,b,c] */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_cons(s7, s7_make_keyword(s7,"value"),
                                    s7_nil(s7)));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_List_Expr );

    s7_pointer vlen
        = s7_apply_function(s7, s7_name_to_value(s7, "length"),
                            s7_list(s7, 1, val));
    TEST_ASSERT_EQUAL(3, s7_integer(vlen));

    /* verify type of each item */
    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, val);
    s7_pointer item = s7_iterate(s7, iter);
    int i = 0;
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        TEST_ASSERT( s7_is_c_object(item) );
        TEST_ASSERT( !s7_is_list(s7, item) );
        TEST_ASSERT( !s7_is_symbol(item) ); /* not a Scheme symbol */
        if ( i % 2 == 0) {
            pred = s7_apply_function(s7, item,  /* but a symbol node */
                                     s7_eval_c_string(s7, "'(:symbol?)"));
            TEST_ASSERT( pred == s7_t(s7) );
            /* also an ID node */
            pred = s7_apply_function(s7, item,
                                     s7_eval_c_string(s7, "'(:id?)"));
            TEST_ASSERT( pred == s7_t(s7) );
        }
        item = s7_iterate(s7, iter);
        i++;
    }

    /* index into list */
    for (int i = 0; i < 3; i++) {
        item = NULL;
        item = s7_apply_function(s7, val, /* list[i] */
                                 s7_list(s7, 1, s7_make_integer(s7, i)));
        TEST_ASSERT( s7_is_c_object(item) );
        TEST_ASSERT( !s7_is_symbol(item) ); /* not a Scheme symbol */
            TEST_ASSERT( sunlark_node_tid(s7, item) == TK_ID );
            pred = s7_apply_function(s7, item, /* but it is a symbol node */
                                     s7_eval_c_string(s7, "'(:symbol?)"));
            TEST_ASSERT( pred == s7_t(s7) );
            /* Scheme value */
            s7_pointer sval = s7_apply_function(s7, item,
                                                s7_eval_c_string(s7, "'(:$)"));
            TEST_ASSERT( !s7_is_c_object(sval) );
            TEST_ASSERT( s7_is_symbol(sval) );
            TEST_ASSERT_EQUAL_STRING((i==0)? "asym"
                                     : (i==1)? "bsym"
                                     : (i==2)? "csym"
                                     : "ERROR!!!",
                                     s7_symbol_name(sval) );
    }
}

void test_with_symbol_list(void) {
    char *s = "(make-binding 'akey '(asym bsym csym))";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );

    _validate_key(binding);

    _validate_symlist_abc(binding);
}

void test_with_symbol_vector(void) {
    char *s = "(make-binding 'akey #(asym bsym csym))";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );

    _validate_key(binding);

    _validate_symlist_abc(binding);
}

/* **************************************************************** */
void _validate_mixed_list(s7_pointer binding)
{
    /* verify val == [1, "bstring", csym] */
    s7_pointer val
        = s7_apply_function(s7, binding,
                            s7_cons(s7, s7_make_keyword(s7,"value"),
                                    s7_nil(s7)));
    TEST_ASSERT( s7_is_c_object(val) );
    TEST_ASSERT( sunlark_node_tid(s7, val) == TK_List_Expr );

    s7_pointer vlen
        = s7_apply_function(s7, s7_name_to_value(s7, "length"),
                            s7_list(s7, 1, val));
    TEST_ASSERT_EQUAL(3, s7_integer(vlen));

    /* verify type of each item */
    s7_pointer pred;
    s7_pointer iter = s7_make_iterator(s7, val);
    s7_pointer item = s7_iterate(s7, iter);
    log_debug("item xxxx: %s", s7_object_to_c_string(s7, item));
    int i = 0;
    while ( ! s7_iterator_is_at_end(s7, iter) ) {
        TEST_ASSERT( s7_is_c_object(item) );
        TEST_ASSERT( !s7_is_list(s7, item) );
        TEST_ASSERT( !s7_is_integer(item) );
        TEST_ASSERT( !s7_is_string(item) );
        TEST_ASSERT( !s7_is_symbol(item) );

        if (i == 0) {
            pred = s7_apply_function(s7, item,
                                     s7_eval_c_string(s7, "'(:int?)"));
            TEST_ASSERT( pred == s7_t(s7) );
        } else {
            if (i == 1) { // counting commas
                pred = s7_apply_function(s7, item,
                                    s7_eval_c_string(s7, "'(:string?)"));
                TEST_ASSERT( pred == s7_t(s7) );
            } else {
                if (i == 2) { // counting commas
                    pred = s7_apply_function(s7, item,
                                             s7_eval_c_string(s7, "'(:symbol?)"));
                    TEST_ASSERT( pred == s7_t(s7) );
                    pred = s7_apply_function(s7, item,
                                             s7_eval_c_string(s7, "'(:id?)"));
                    TEST_ASSERT( pred == s7_t(s7) );
                }
            }
        }
        item = s7_iterate(s7, iter);
        i++;
    }

    for (int i = 0; i < 3; i++) {
        item = NULL;
        item = s7_apply_function(s7, val, /* list[i] */
                                 s7_list(s7, 1, s7_make_integer(s7, i)));
        TEST_ASSERT( s7_is_c_object(item) );
        TEST_ASSERT( !s7_is_integer(item) );
        TEST_ASSERT( !s7_is_string(item) );
        TEST_ASSERT( !s7_is_symbol(item) );

        if (i==0) {
            pred = s7_apply_function(s7, item, /* but it is a symbol node */
                                     s7_eval_c_string(s7, "'(:int?)"));
            TEST_ASSERT( pred == s7_t(s7) );
        } else {
            if (i==1) {
                pred = s7_apply_function(s7, item,
                               s7_eval_c_string(s7, "'(:string?)"));
                TEST_ASSERT( pred == s7_t(s7) );
            } else {
                /* symbol */
                if (i==2) {
                    pred = s7_apply_function(s7, item,
                                  s7_eval_c_string(s7, "'(:symbol?)"));
                    TEST_ASSERT( pred == s7_t(s7) );
                    TEST_ASSERT( sunlark_node_tid(s7, item) == TK_ID );
                }
            }
        }
        /* Scheme value */
        s7_pointer sval = s7_apply_function(s7, item,
                                            s7_eval_c_string(s7, "'(:$)"));
        TEST_ASSERT( !s7_is_c_object(sval) );
        if (i==0) {
            TEST_ASSERT( s7_is_integer(sval) );
            TEST_ASSERT_EQUAL_INT( 1, s7_integer(sval) );
        } else {
            if (i==1) {
                TEST_ASSERT( s7_is_string(sval) );
                TEST_ASSERT_EQUAL_STRING( "\"bstring\"", s7_string(sval) );
            } else {
                TEST_ASSERT( s7_is_symbol(sval) );
                TEST_ASSERT_EQUAL_STRING( "csym", s7_symbol_name(sval) );
            }
        }
    }
}

void test_with_mixed_list(void) {
    char *s = "(make-binding 'akey '(1 \"bstring\" csym))";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );

    _validate_key(binding);

    _validate_mixed_list(binding);
}

void test_with_mixed_vector(void) {
    char *s = "(make-binding 'akey #(1 \"bstring\" csym))";
    s7_pointer binding = s7_eval_c_string(s7, s);
    TEST_ASSERT( s7_is_c_object(binding) );
    TEST_ASSERT( sunlark_node_tid(s7, binding) == TK_Binding );

    _validate_key(binding);

    _validate_mixed_list(binding);
}

/* **************************************************************** */
void setUp(void) {
    s7 = sunlark_init();
    init_s7_syms(s7);
}

void tearDown(void) {
    s7_quit(s7);
}

