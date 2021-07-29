#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"

#include "sunlark_debug.h"

void sunlark_debug_print_node(s7_scheme *s7, void *node)
{
#ifdef DEBUG_TRACE
    /* log_debug("sunlark_debug_print_node"); */
#endif
    UT_string *buf;
    utstring_new(buf);

    sunlark_node_display(s7, node, buf);

    if (utstring_body(buf)[utstring_len(buf)-1] == '\n')
        utstring_body(buf)[utstring_len(buf)-1] = '\0';
    log_debug("%s", utstring_body(buf));
    utstring_free(buf);
}

