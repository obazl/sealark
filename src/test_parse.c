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

#include "test_parse.h"

UT_string *build_file;

/* struct obazl_buildfile_s *ast; */

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

    struct node_s *root = obazl_starlark_parse_file(utstring_body(build_file));
    log_debug("parsed file %s", utstring_body(build_file));
    dump_node(root);

    /* for(p=(intpair_t*)utarray_front((*ast)->nodelist); */
    /*     p!=NULL; */
    /*     p=(intpair_t*)utarray_next((*ast)->nodelist,p)) { */
    /*     printf("%d %d\n", p->a, p->b); */
    /* } */
    /* utarray_free((*ast)->nodelist); */
    /* free(ast); */
}
