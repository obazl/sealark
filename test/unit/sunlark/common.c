#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "s7.h"

/* we need both APIs for test validation */
#include "sealark.h"
#include "sunlark.h"

#include "common.h"

s7_pointer is_eq_s7;
s7_pointer is_equal_s7;
s7_pointer is_list_s7;
s7_pointer is_sunlark_node_s7;
s7_pointer length_s7;
s7_pointer set_bang;

void init_s7_syms(s7_scheme *s7)
{
    is_eq_s7 = s7_name_to_value(s7, "eq?");
    is_equal_s7 = s7_name_to_value(s7, "equal?");
    is_list_s7 = s7_name_to_value(s7, "list?");
    is_sunlark_node_s7 = s7_name_to_value(s7, "sunlark-node?");
    length_s7 = s7_name_to_value(s7, "length");
    set_bang = s7_name_to_value(s7, "set!");
}
