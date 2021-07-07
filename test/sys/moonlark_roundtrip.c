#include <errno.h>
#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "unity.h"
#include "starlark.h"
/* #include "syntaxis.h" */

#include "moonlark_roundtrip.h"

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

int moonlark_roundtrip(char *_build_file, char *_lua_file)
{

    /* utstring_renew(build_file); */

    /* utstring_printf(build_file, "%s", _build_file); */

    moonlark_setup(_build_file, _lua_file);

    // FIXME: use mkstemp instead of tmpnam
 /*    char *outfile = tmpnam(NULL); */
 /*    /\* printf("outfile name: %s\n", outfile); *\/ */
 /*    FILE *fp; */
 /*    fp = fopen(outfile, "w+"); */
 /*    if (fp == NULL) { */
 /*        printf("fopen error: %d\n", errno); */
 /*    /\* } else { *\/ */
 /*    /\*     printf("created test output file: %s\n", outfile); *\/ */
 /*    } */
 /*    int r = fputs(utstring_body(buffer), fp); */
 /*    if (r < 0) { */
 /*        printf("error on fputs: %d", errno); */
 /*        goto cleanup; */
 /*    } */
 /*    fclose(fp); */

 /*    /\* now compare input and output files *\/ */
 /*    FILE *file1 = fopen(utstring_body(build_file), "r"); */
 /*    FILE *file2 = fopen(outfile, "r"); */
 /*    if (file1 == NULL || file2 == NULL){ */
 /*        printf("Error : Files not open"); */
 /*        goto cleanup; */
 /*    } */
 /*    static int diff; */
 /*    diff = compareFiles(file1, file2); */
 /*    if (diff < 0) { */
 /*        printf("MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM\n"); */
 /*        log_fatal("mismatch at %d for\n\t%s and\n\t%s\n", */
 /*                  r, */
 /*                  utstring_body(build_file), */
 /*                  outfile); */
 /*    } else { */
 /*        printf("MATCH!\n"); */
 /*    } */

 /*    fclose(file1); */
 /*    fclose(file2); */

 /* cleanup: */
 /*    if (diff == 0) { */
 /*        r = remove(outfile); */
 /*        if (r < 0) { */
 /*            printf("remove(%s) failed, rc: %d", outfile, errno); */
 /*        /\* } else { *\/ */
 /*        /\*     printf("removed tmp file %s\n", outfile); *\/ */
 /*        } */
 /*    } */
 /*    parser_free(parse); */
 /*    /\* node_dtor(parse->root); *\/ */
 /*    return diff; // r; */
    return EXIT_SUCCESS;
}
