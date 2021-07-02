#include <errno.h>
#include <fcntl.h>
#include <libgen.h>

#if INTERFACE
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#endif

#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#if INTERFACE
#include "utstring.h"
#endif

#include "test_lex.h"

int main(int argc, char *argv[])
{
    int opt;

    utstring_new(build_file);

    while ((opt = getopt(argc, argv, "f:hv")) != -1) {
        switch (opt) {
        case 'f':
            /* BUILD.bazel or BUILD file */
            utstring_printf(build_file, "%s", optarg);
            break;
        case 'h':
            log_info("Help: ");
            exit(EXIT_SUCCESS);
        default:
            log_error("Usage: %s [-f] [buildfile]", argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    if (utstring_len(build_file) == 0) {
        log_error("-f <buildfile> must be provided.");
        exit(EXIT_FAILURE);
    }

    UT_array *result = obazl_starlark_lex_file(utstring_body(build_file));

    /* UT_array *result = obazl_starlark_lex_string("'hello'\n#cmt1\n"); */

    log_debug("main RESULT dump:");
    dump_nodes(result);
}
