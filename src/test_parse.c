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

UT_string *buffer;

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

    struct node_s *root = starlark_parse_file(utstring_body(build_file));
    log_debug("parsed file %s", utstring_body(build_file));
    dump_node(root);

    /* serialization routines expect a UT_string, not a char buffer */
    utstring_new(buffer);
    starlark_node2string(root, buffer);
    /* printf("%s", utstring_body(buffer)); */

    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    /* log_info("BUILD_WORKING_DIRECTORY: %s", wd); */
    chdir(wd);

    FILE *fp;
    fp = fopen("./test.BUILD.bazel", "w+");
    if (fp == NULL) {
        printf("fopen error: %d\n", errno);
    /* } else { */
    /*     printf("opened test.BUILD.bazel\n"); */
    }
    int r = fputs(utstring_body(buffer), fp);
    printf("fputs r: %d\n", r);
    fclose(fp);

    utstring_free(buffer);
    node_dtor(root);
}
