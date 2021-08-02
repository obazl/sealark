#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_loadstmts.h"

/* **************************************************************** */
/* (car path_args): string (= src); int (index)  */
EXPORT struct node_s *sunlark_pkg_loadstmt_select(s7_scheme *s7,
                                           struct node_s *pkg,
                                           s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sunlark_pkg_loadstmt_select %s",
              s7_object_to_c_string(s7, path_args));
#endif

    assert(pkg->tid == TK_Package);

    s7_pointer op = s7_car(path_args);

    s7_pointer result_list;

    /* s7_pointer op2; */
    /* op2 = s7_cadr(path_args); */

    struct node_s *loadstmt;

    /* **************** */
    if (s7_is_string(op)) {
        if (s7_is_null(s7, s7_cdr(path_args))) {
            loadstmt = sealark_pkg_loadstmt_for_src(pkg, s7_string(op));
        } else {
            loadstmt = _loadstmt_select(s7, loadstmt, s7_cdr(path_args));
        }
        return loadstmt;
    }

    /* **************** */
    if (s7_is_integer(op)) {
        /* loadstmt = sealark_pkg_loadstmt_for_int(bf_node, s7_string(op2)); */
        /* return loadstmt; */
        log_error("not yet load int");
    }
    log_error("Invalid arg: %s", s7_object_to_c_string(s7, path_args));
    errno = EINVALID_ARG_LOAD;
    return NULL;
}

/* **************************************************************** */
/* (car path_args): :args, :bindings */
LOCAL struct node_s *_loadstmt_select(s7_scheme *s7,
                                      struct node_s *pkg,
                                      s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_LOADS)
    log_debug("sunlark_loadstmt_select %s",
              s7_object_to_c_string(s7, path_args));
#endif

    assert(pkg->tid == TK_Load_Stmt);

    log_error("NOT YET: sunlark_loadstmt_select");
    exit(-1);
}
