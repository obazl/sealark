#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "linenoise.h"
#include "s7.h"

#include "utarray.h"
#include "sunlark.h"

char *history = ".history.txt";

void completion(const char *buf, linenoiseCompletions *lc) {
    if (buf[0] == 'h') {
        linenoiseAddCompletion(lc,"hello");
        linenoiseAddCompletion(lc,"hello there");
    }
}

char *hints(const char *buf, int *color, int *bold) {
    if (!strcasecmp(buf,"hello")) {
        *color = 35;
        *bold = 0;
        return " World";
    }
    return NULL;
}

int main(int argc, char **argv) {

    s7_scheme *s7;
    char response[1024];        /* result of evaluating input */

    char *line;
    char *prgname = argv[0];

    /* Parse options, with --multiline we enable multi line editing. */
    while(argc > 1) {
        argc--;
        argv++;
        if (!strcmp(*argv,"--multiline")) {
            linenoiseSetMultiLine(1);
            printf("Multi-line mode enabled.\n");
        } else if (!strcmp(*argv,"--keycodes")) {
            linenoisePrintKeyCodes();
            exit(0);
        } else {
            fprintf(stderr, "Usage: %s [--multiline] [--keycodes]\n", prgname);
            exit(1);
        }
    }
    linenoiseSetMultiLine(1);   /* always support multiline */

    char *callback_script_file = "edit.scm";

    char *wd = getenv("BUILD_WORKING_DIRECTORY");
    if (wd) {
        char *bazel_script_dir = get_bazel_script_dir(callback_script_file);
        chdir(wd);
    }

    /* Set the completion callback. This will be called every time the
     * user uses the <tab> key. */
    linenoiseSetCompletionCallback(completion);
    linenoiseSetHintsCallback(hints);

    /* Load history from file. The history file is just a plain text file
     * where entries are separated by newlines. */
    linenoiseHistoryLoad(history); /* Load the history at startup */

    /* Now this is the main loop of the typical linenoise-based application.
     * The call to linenoise() will block as long as the user types something
     * and presses enter.
     *
     * The typed string is returned as a malloc() allocated string by
     * linenoise, so the user needs to free() it. */

    //s7 = s7_init();                 /* initialize the interpreter */
    s7 = sunlark_init();

    while((line = linenoise("s7> ")) != NULL) {

        if (line[0] != '\0' && line[0] != '/') {

            snprintf(response, 1024, "(write %s)", line);
            s7_eval_c_string(s7, response);
            printf("%s", "\n");

            linenoiseHistoryAdd(line); /* Add to the history. */
            linenoiseHistorySave(history); /* Save the history on disk. */
        } else if (!strncmp(line,"/historylen",11)) {
            /* The "/historylen" command will change the history len. */
            int len = atoi(line+11);
            linenoiseHistorySetMaxLen(len);
        } else if (!strncmp(line, "/mask", 5)) {
            linenoiseMaskModeEnable();
        } else if (!strncmp(line, "/unmask", 7)) {
            linenoiseMaskModeDisable();
        } else if (line[0] == '/') {
            printf("Unreconized command: %s\n", line);
        }
        free(line);
    }
    return 0;
}
