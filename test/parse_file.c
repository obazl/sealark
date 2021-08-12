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
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "parse_file.h"

UT_string *build_file;

UT_string *buffer;

int compareFiles(FILE *file1, FILE *file2)
{
    /* printf("comparing files\n"); */
    char ch1 = getc(file1);
    char ch2 = getc(file2);
    int error = 0, pos = 0, line = 1;
    while (ch1 != EOF && ch2 != EOF){
        pos++;
        if (ch1 == '\n' && ch2 == '\n'){
            line++;
            pos = 0;
        }
        if (ch1 != ch2){
            error++;
            printf("File mismatch at (%d:%d)\n", line, pos);
            return -1;
        }
        ch1 = getc(file1);
        ch2 = getc(file2);
    }
    return 0;
}

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

    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    if (wd) {
        /* we launched from bazel workspace, cd to launch dir */
        chdir(wd);
    }

    struct node_s *pkg_node = sealark_parse_file(utstring_body(build_file));
    UT_string *buf = sealark_debug_display_ast_outline(pkg_node, 0);
    printf("%s", utstring_body(buf));
    utstring_free(buf);

    /* serialization routines expect a UT_string, not a char buffer */
    utstring_new(buffer);
    sealark_node_to_starlark(pkg_node, buffer);
    printf("%s", utstring_body(buffer));

    char *outfile = tmpnam(NULL);
    /* printf("serializing to: %s\n", outfile); */
    FILE *fp;
    fp = fopen(outfile, "w+");
    if (fp == NULL) {
        printf("fopen error: %d\n", errno);
    /* } else { */
    /*     printf("created test output file: %s\n", outfile); */
    }
    int r = fputs(utstring_body(buffer), fp);
    if (r < 0) {
        printf("error on fputs: %d", errno);
        goto cleanup;
    }
    fclose(fp);

    /* now compare input and output files */
    FILE *file1 = fopen(utstring_body(build_file), "r");
    FILE *file2 = fopen(outfile, "r");
    if (file1 == NULL || file2 == NULL){
        printf("Error : Files not open");
        goto cleanup;
    }
    r = compareFiles(file1, file2);
    if (r < 0) {
        printf("mismatch %d, outfile: %s\n", r, outfile);
    } else {
        printf("match!\n");
    }
    fclose(file1);
    fclose(file2);

 cleanup:
    if (r == 0) {
        r = remove(outfile);
        if (r < 0) {
            printf("remove(%s) failed, rc: %d", outfile, errno);
        /* } else { */
        /*     printf("removed tmp file %s\n", outfile); */
        }
    } else {
        printf("tmp outfile: %s\n", outfile);
    }
    utstring_free(buffer);
    /* free(parse_state->lexer->fname); */
    sealark_node_free(pkg_node);
    return r;
}
