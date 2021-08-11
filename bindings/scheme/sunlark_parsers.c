#include <errno.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_parsers.h"

#if INTERFACE
#define SUNLARK_PARSE_PACKAGE_HELP "(parse-package label) parses BUILD..."

#define SUNLARK_PARSE_TARGET_HELP "(parse-target label) parses BUILD..."

#define SUNLARK_PARSE_BUILD_FILE_HELP "(parse-build-file fname) parses BUILD file fname."

#define SUNLARK_PARSE_BZL_FILE_HELP "(parse-bzl-file fname) parses extension (.bzl) file fname."

#define SUNLARK_PARSE_STRING_HELP "(parse-string s) parses string s and returs its AST."
#endif

EXPORT s7_pointer sunlark_parse_build_file(s7_scheme *s7,
                                           s7_pointer args)
{
    char *fname = (char*)s7_string(s7_car(args));

#if defined(DEBUG_PROPERTIES)
    log_debug("sunlark_parse_build_file: %s", fname);
#endif

    /* int r = access(fname, F_OK); */
    errno = 0;
    /* struct parse_state_s *parse_state */
    struct node_s *pkg_node = sealark_parse_file(fname);
    if (pkg_node == NULL) {
        return handle_errno(s7, errno, s7_make_string(s7, fname));
    }
#if defined(DEBUG_PARSERS)
    log_debug("parsed file %s", fname);
    log_debug("converting ast");
#endif

    /* s7_pointer ast = sunlark_ast2scm(s7, parse_state); */
    s7_pointer ast = sunlark_ast2scm(s7, pkg_node);
    return ast;
}

s7_pointer sunlark_parse_bzl_file(s7_scheme *s7,
                                    s7_pointer args)
{
    const char *fname = s7_string(s7_car(args));

#if defined(DEBUG_PARSERS)
    log_debug("sunlark_parse_bzl_file: %s", fname);
#endif

    struct node_s *pkg = sealark_parse_file(fname);
    log_debug("parsed file %s", fname);
    /* dump_node(parse_state->root); */

    log_debug("converting ast");
    s7_pointer ast = sunlark_ast2scm(s7, pkg);
    return ast;
}

EXPORT s7_pointer sunlark_parse_string(s7_scheme *s7,
                                       s7_pointer args)
{
#if defined(DEBUG_PARSERS)
    log_debug("sunlark_parse_string: %s", fname);
#endif
    const char *str;
    if (s7_is_list(s7, args)) {
        if(s7_list_length(s7, args) > 1) {
            log_warn("Expected 1 arg, got %d: %s",
                     s7_list_length(s7, args),
                     s7_object_to_c_string(s7, args));
            return s7_unspecified(s7);
        } else {
            str = s7_string(s7_car(args));
        }
    } else {
        if (s7_is_string(args)) {
            log_debug("is_string? %d", s7_is_string(args));
            str = (char*)s7_string(args);
        }
    }

    struct node_s *node = sealark_parse_string(str);
/* "cc_library(a,b)\n"); */
    return sunlark_node2scm(s7, node);
}

s7_pointer sunlark_node2scm(s7_scheme *s7, struct node_s *node)
{
#if defined(DEBUG_PARSERS)
    log_debug("sunlark_node2scm");
#endif

    s7_pointer new_ast_node_s7 = s7_make_c_object(s7,
                                                  ast_node_t,
                                                  (void *)node);
    return new_ast_node_s7;
}

EXPORT s7_pointer sunlark_ast2scm(s7_scheme *s7,
                                  struct node_s *pkg)
                                  /* struct parse_state_s *parse) */
{
#if defined(DEBUG_PARSERS)
    log_debug("sunlark_ast2scm");
#endif

    s7_pointer new_ast_node_s7 = s7_make_c_object(s7,
                                                  ast_node_t,
                                                  (void *)pkg);
    return new_ast_node_s7;
}


