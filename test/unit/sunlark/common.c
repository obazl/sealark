#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "common.h"

s7_scheme *s7;

char *form;
s7_pointer pkg;
s7_pointer path;
s7_pointer target;
s7_pointer bindings, binding;
s7_pointer key, keystr, keysym;
s7_pointer val;
s7_pointer pred;
s7_pointer list;
s7_pointer item;
s7_pointer itemval;
s7_pointer dexpr, dentry;
s7_pointer result;
s7_pointer count;

s7_pointer getter;

/* Scheme procs */
s7_pointer set_bang;            /* set! */
s7_pointer length_proc;

/* predicates */
s7_pointer is_eq_s7;
s7_pointer is_equal_s7;
s7_pointer is_list_s7;
s7_pointer is_sunlark_node_proc;   /* sunlark-node? */

/* sunlark ops (properties) initialized by setUp below */
s7_pointer binding_op;              /* '(:binding) */
s7_pointer at_op;              /* '(:@) */
s7_pointer key_op;              /* '(:key) */
s7_pointer value_op;            /* '(:value) */
s7_pointer dollar_op;           /* '(:$) */
s7_pointer length_op;           /* '(:length) */
s7_pointer item_0_op;             /* '(:0) */
s7_pointer item_1_op;             /* '(:1) */
s7_pointer item_2_op;             /* '(:2) */
s7_pointer length_op;              /* '(:length) */
s7_pointer null_kw;             /* '() */

/* predicate props */
s7_pointer target_p;            /* '(:call-expr?) */
s7_pointer binding_p;           /* '(:binding?) */
s7_pointer list_expr_p;         /* '(:list-expr?) */
s7_pointer dict_expr_p;         /* '(:dict-expr?) */

void init_s7_syms(s7_scheme *s7)
{
    is_eq_s7 = s7_name_to_value(s7, "eq?");
    is_equal_s7 = s7_name_to_value(s7, "equal?");
    is_list_s7 = s7_name_to_value(s7, "list?");
    is_sunlark_node_proc = s7_name_to_value(s7, "sunlark-node?");

    set_bang = s7_name_to_value(s7, "set!");
    length_proc = s7_name_to_value(s7, "length");

    binding_op = s7_eval_c_string(s7, "'(:binding)");
    at_op = s7_eval_c_string(s7, "'(:@)");
    key_op = s7_eval_c_string(s7, "'(:key)");
    value_op = s7_eval_c_string(s7, "'(:value)");
    dollar_op = s7_eval_c_string(s7, "'(:$)");
    length_op = s7_eval_c_string(s7, "'(:length)");

    item_0_op = s7_eval_c_string(s7, "'(:0)");
    item_1_op = s7_eval_c_string(s7, "'(:1)");
    item_2_op = s7_eval_c_string(s7, "'(:2)");
    null_kw = s7_make_keyword(s7, "null");

    target_p = s7_eval_c_string(s7, "'(:call-expr?)");
    binding_p = s7_eval_c_string(s7, "'(:binding?)");
    list_expr_p = s7_eval_c_string(s7, "'(:list-expr?)");
    dict_expr_p = s7_eval_c_string(s7, "'(:dict-expr?)");
}

s7_pointer old_port;
s7_pointer res;
int gc_loc;
const char *errmsg = NULL;

s7_pointer error_handler(s7_scheme *sc, s7_pointer args)
{
  fprintf(stdout, "error: %s\n", s7_string(s7_car(args)));
  return(s7_f(sc));
}

    /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
    /* if ((errmsg) && (*errmsg)) */
    /*         log_error("ERROR [%s]", errmsg); */

