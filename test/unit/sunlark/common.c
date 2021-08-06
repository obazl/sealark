#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "common.h"

char *form;
s7_pointer path, target, binding, key, val, pred;

s7_pointer is_eq_s7;
s7_pointer is_equal_s7;
s7_pointer is_list_s7;
s7_pointer is_sunlark_node_s7;

/* Scheme procs */
s7_pointer length_s7;
s7_pointer set_bang;            /* set! */

/* sunlark ops (properties) initialized by setUp below */
s7_pointer key_op;              /* '(:key) */
s7_pointer value_op;            /* '(:value) */
s7_pointer dollar_op;           /* '(:$) */
s7_pointer target_p;            /* '(:call-expr?) */
s7_pointer binding_p;           /* '(:binding?) */
s7_pointer null_op;             /* '() */

void init_s7_syms(s7_scheme *s7)
{
    is_eq_s7 = s7_name_to_value(s7, "eq?");
    is_equal_s7 = s7_name_to_value(s7, "equal?");
    is_list_s7 = s7_name_to_value(s7, "list?");
    is_sunlark_node_s7 = s7_name_to_value(s7, "sunlark-node?");
    length_s7 = s7_name_to_value(s7, "length");
    set_bang = s7_name_to_value(s7, "set!");

    key_op = s7_eval_c_string(s7, "'(:key)");
    value_op = s7_eval_c_string(s7, "'(:value)");
    target_p = s7_eval_c_string(s7, "'(:call-expr?)");
    binding_p = s7_eval_c_string(s7, "'(:binding?)");
    dollar_op = s7_eval_c_string(s7, "'(:$)");
    null_op = s7_eval_c_string(s7, "'()");
}
