#include <ctype.h>
#include <errno.h>
#include <stdlib.h>

#include "log.h"
#include "s7.h"

#include "sunlark_macros.h"

char *binding_macro = ""
"(set! *#readers* "
      "(cons (cons #\\@ (lambda (str) "
                        "(let ((args (read))) "
                          "(list quote args))) "
                  ") "
    "*#readers*)) ";

void register_binding_ctor_macro(s7_scheme *s7)
{
    /* s7_pointer mac = s7_eval_c_string(s7, binding_macro); */
}
