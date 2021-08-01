#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"

#include "sunlark_debug.h"

EXPORT void sunlark_debug_print_node(s7_scheme *s7,
                                     s7_pointer node)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_debug_print_node");
#endif
    UT_string *buf;
    utstring_new(buf);

    if ( !s7_is_c_object(node)) {
        log_error("sealark_dump_node: expected node c-object, got: %s",
                  s7_object_to_c_string(s7, s7_type_of(s7, node)));
        exit(EXIT_FAILURE);
    }
    struct node_s *nd = s7_c_object_value(node);
    sealark_display_node(nd, buf, 0);

    if (utstring_body(buf)[utstring_len(buf)-1] == '\n')
        utstring_body(buf)[utstring_len(buf)-1] = '\0';
    log_debug("%s", utstring_body(buf));
    utstring_free(buf);
}

