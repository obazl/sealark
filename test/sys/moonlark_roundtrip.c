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

    moonlark_process_buildfile(_build_file, _lua_file);

    return EXIT_SUCCESS;
}
