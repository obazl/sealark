#include <libgen.h>             /* dirname */
#include<stdio.h>               /* fopen, getline, perror */
#include<stdlib.h>               /* getenv */
#include<string.h>              /* strcmp, strrchr, strsep */
#include <unistd.h>             /* getcwd */

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "bazel_config.h"

EXPORT char *get_bazel_script_dir(char *scriptfile)
{
    /* log_debug("bazel_get_script_dir %s", scriptfile); */
    char *bazel_script_dir = NULL;
    char *s = getcwd(NULL, 0);
    /* log_info("CURRENT WORKING DIRECTORY: %s", s); */
    char *mdir = dirname(s);
    /* log_info("MANIFEST DIR: %s", mdir); */
    UT_string *manifest;
    utstring_new(manifest);
    utstring_printf(manifest, "%s%s", mdir, "/MANIFEST");
    /* log_info("MANIFEST: %s", utstring_body(manifest)); */

    FILE * fp;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;

    fp = fopen(utstring_body(manifest), "r");
    if (fp == NULL) {
        log_error("fopen failure %s", utstring_body(manifest));
        exit(EXIT_FAILURE);
    }

    char *token;
    while ((read = getline(&line, &len, fp)) != -1) {
        /* log_debug("Retrieved line of length %zu:", read); */
        /* log_debug("\n%s", line); */
        bool hit = false;
        /* two tokens per line */
        while ((token = strsep(&line, " ")) != NULL) {
            if (hit) {
                goto exit;
            } else {
                char *dot = strrchr(token, '/');
                if (dot && !strcmp(dot+1, scriptfile))
                    hit = true;
            }
        }
    }
    log_error("default script dir for %s not found", scriptfile);
    exit(EXIT_FAILURE);
 exit:
    if (token != NULL) {
        /* printf("  tok: %s\n", token); */
        bazel_script_dir = dirname(token);
        /* printf("  bazel_script_dir: %s\n", bazel_script_dir); */
    }

    fclose(fp);
    /* log_debug("bazel_script_dir: %s", bazel_script_dir); */
    return bazel_script_dir;
}
