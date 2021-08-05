#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "log.h"
/* #include "utarray.h" */
/* #include "utstring.h" */

#include "s7.h"

#include "sunlark_utils.h"

int sunlark_is_nbr_kw(s7_scheme *s7, s7_pointer arg)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_is_nbr_kw: %s", s7_object_to_c_string(s7, arg));
#endif

    if (s7_is_keyword(arg)) {
        s7_pointer sym = s7_keyword_to_symbol(s7, arg);
        const char *kw = s7_symbol_name(sym);
        int len = strlen(kw); // very unlikely to be more than 2
        int i = (kw[0] == '-')? 1 : 0;
        for ( ; i < len; i++) {
            if ( isdigit( (int)kw[i] == 0 ) ) {
                errno = EKW_NOT_NBR;
                return -1;
            }
        }
        errno = 0;
        return atoi(kw);
    } else {
        errno = EKW_NOT_NBR;
        return -1;
    }
}
