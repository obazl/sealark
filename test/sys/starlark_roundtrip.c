#include <errno.h>
#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "sealark.h"
/* #include "syntaxis.h" */

#include "starlark_roundtrip.h"

UT_string *build_file;

UT_string *buffer;

int compareFiles(FILE *file1, FILE *file2)
{
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

int roundtrip(char *_file)
{
    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    if (wd) {
        log_info("BUILD_WORKING_DIRECTORY: %s", wd);
        chdir(wd);
    } else {
        log_info("BUILD_WORKING_DIRECTORY not found");
    }

    char cwd[PATH_MAX];
    if (getcwd(cwd, sizeof(cwd)) != NULL) {
        log_info("Current working dir: %s\n", cwd);
    } else {
        perror("getcwd() error");
        return 1;
    }

    utstring_renew(build_file);

    utstring_printf(build_file, "%s", _file);

    struct node_s *parse = sealark_parse_file(utstring_body(build_file));
    log_debug("parsed file %s", utstring_body(build_file));
    /* dump_node(root); */

    /* serialization routines expect a UT_string, not a char buffer */
    utstring_renew(buffer);
    sealark_node_to_starlark(parse, buffer);
    /* printf("%s", utstring_body(buffer)); */

    // FIXME: use mkstemp instead of tmpnam
    char *outfile = tmpnam(NULL);
    /* printf("outfile name: %s\n", outfile); */
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
    static int diff;
    diff = compareFiles(file1, file2);
    if (diff < 0) {
        printf("MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM\n");
        log_fatal("mismatch at %d for\n\t%s and\n\t%s\n",
                  r,
                  utstring_body(build_file),
                  outfile);
    } else {
        printf("MATCH!\n");
    }

    fclose(file1);
    fclose(file2);

 cleanup:
    if (diff == 0) {
        r = remove(outfile);
        if (r < 0) {
            printf("remove(%s) failed, rc: %d", outfile, errno);
        /* } else { */
        /*     printf("removed tmp file %s\n", outfile); */
        }
    }
    sealark_node_free(parse);
    /* node_dtor(parse->root); */
    return diff; // r;
}
