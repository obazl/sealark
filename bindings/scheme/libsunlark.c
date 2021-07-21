/*
  * convert C nodes to scheme
 */

#include <ctype.h>
#include <errno.h>
/* #include <pthread.h> */
#include <stdarg.h>
#include <sys/stat.h>
#include <unistd.h>

#include "log.h"

#include "s7.h"
#include "libsunlark.h"

UT_string *proj_root;
UT_string *runfiles_root;

UT_string *default_handlers_dir;

/* location of user-defined scheme files: $cwd/.bazel.d/scm/ */
UT_string *user_handlers_dir;
/* UT_string *runtime_data_dir; */

char *default_handler_file_name = "handler.scm";
UT_string *user_s7_file;

int x;

/* commonly used keywords */
s7_pointer kw_targets;
s7_pointer kw_target;
s7_pointer kw_attrs;
s7_pointer kw_attr;

s7_pointer kw_tid;
s7_pointer kw_line;
s7_pointer kw_col;
s7_pointer kw_subnodes;
s7_pointer kw_comments;

s7_pointer kw_node_p;
s7_pointer kw_nodelist_p;

s7_pointer kw_add;
s7_pointer kw_replace;
s7_pointer kw_remove;

s7_pointer kw_crush;
s7_pointer kw_squeeze;

static s7_pointer error_handler(s7_scheme *sc, s7_pointer args)
{
  fprintf(stdout, "error: %s\n", s7_string(s7_car(args)));
  return(s7_f(sc));
}

void export_token_tables(s7_scheme *s7)
{
    /* log_debug("export_token_tables"); */

    /* WARNING: token_ct must match the #define ct in syntaxis.h */
    int token_ct = 137;
    s7_pointer toks = s7_make_vector(s7, (s7_int)token_ct);
    s7_pointer tids = s7_make_hash_table(s7, (s7_int)token_ct);
    int i, j, len;
    s7_pointer result;
    char tknm[token_ct];
    for (i = 1; i < token_ct; i++) {
        if (token_name[i][0] != NULL) {
            len = strlen(token_name[i][0]);
            /* log_debug("tok: %s len %d", token_name[i][0], len); */
            snprintf(tknm, len+1, "%s", (token_name[i][0]));
            /* log_debug("tknm: %s", tknm); */
            for (j = 0; j < len; j++) {
                tknm[j] = tolower(tknm[j]);
                if (tknm[j] == '_') tknm[j] = '-';
            }
            /* log_debug("tknm: %d %s", i, tknm); */

            result = s7_hash_table_set(s7, tids,
                                       s7_make_keyword(s7, tknm + 3),
                                       s7_make_integer(s7, i));

            result = s7_vector_set(s7, toks, (s7_int)i,
                                   s7_make_keyword(s7, tknm + 3));

            /* char *msg = s7_object_to_c_string(s7, result); */
            /* log_debug("vecset result: %s", msg); */
            /* free(msg); */
        } else {
            log_debug("token name %d:  NULL", i);
            break;
        }
    }
    s7_define_variable(s7, "tokens", toks);
    s7_define_variable(s7, "sunlark-tids", tids);
}

EXPORT s7_scheme *sunlark_init(void)
{
    s7_scheme *s7 = s7_init();
    s7_pointer old_port, result;
    int gc_loc = -1;
    const char *errmsg = NULL;

    /* trap error messages */
    old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
    if (old_port != s7_nil(s7))
        gc_loc = s7_gc_protect(s7, old_port);

    s7_int an_t = configure_s7_ast_node_type(s7);
    s7_int anl_t = configure_s7_ast_nodelist_type(s7);

    s7_define_function(s7, "error-handler",
                       error_handler, 1, 0, false,
                       "our error handler");

    export_token_tables(s7);

    /* create commonly used keywords */
    kw_targets = s7_make_keyword(s7, "targets");
    kw_target = s7_make_keyword(s7, "target");
    kw_attrs = s7_make_keyword(s7, "attrs");
    kw_attr = s7_make_keyword(s7, "attr");

    kw_tid = s7_make_keyword(s7, "tid");
    kw_line = s7_make_keyword(s7, "line");
    kw_col = s7_make_keyword(s7, "col");
    kw_subnodes = s7_make_keyword(s7, "subnodes");
    kw_comments = s7_make_keyword(s7, "comments");


    kw_node_p = s7_make_keyword(s7, "node?");
    kw_nodelist_p = s7_make_keyword(s7, "nodelist?");

    kw_add = s7_make_keyword(s7, "add");
    kw_replace = s7_make_keyword(s7, "replace");
    kw_remove = s7_make_keyword(s7, "remove");
    /* formatting */
    kw_crush = s7_make_keyword(s7, "crush");
    kw_squeeze = s7_make_keyword(s7, "squeeze");

    /* look for error messages */
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));

    /* if we got something, wrap it in "[]" */
    if ((errmsg) && (*errmsg)) {
        log_error("[%s\n]", errmsg);
        s7_quit(s7);
        exit(EXIT_FAILURE);
    }
    s7_close_output_port(s7, s7_current_error_port(s7));
    s7_set_current_error_port(s7, old_port);
    if (gc_loc != -1)
        s7_gc_unprotect_at(s7, gc_loc);

    return s7;
}

/*
  Top of Stack: parsed lAST
 */
EXPORT void sunlark_call_user_handler(s7_scheme *s7, char *handler)
{
    log_debug("sunlark_call_user_handler");

    /* log_debug("lua user-provided callback '%s' returned", handler); */
}

/**
   configure lua search paths for bazel env

   assumption: launched via `$ bazel run ...`
   uses: obazl_d, runfiles_root

   lua search path always contains:
       <proj_root>/.sunlark.d/  -- contains user-defined handler
       if run by `bazel run`:
           <exec_root>/<runfiles_dir>/sunlark/lua -- default handler
           (linked from @sunlark//sunlark/lua)
       else:
           ???
    for runtime files: see https://github.com/bazelbuild/bazel/issues/10022
        https://github.com/laszlocsomor/bazel/commit/21989926c1a002709ec3eba9ee7a992506f2d50a
 */

EXPORT void sunlark_augment_load_path(s7_scheme *s7, char *path)
{
    if (path == NULL) return;

    log_debug("sunlark_augment_load_path %s", path);

    s7_pointer newlp =  s7_add_to_load_path(s7, path);
    /* lp = s7_load_path(s7); */
    /* log_debug("new load path: %s", s7_object_to_c_string(s7, newlp)); */
}

/*
  called by //sunlark:edit, but not //sunlark:repl
 */
EXPORT void sunlark_scm_load_file(s7_scheme *s7, char *scm_file)
{
    // log_debug("starlark_scm_load_handlers");

    /* log_debug("loading scm file: %s", default_handler_file_name); */
}

EXPORT void sunlark_create_token_enums(s7_scheme *s7)
{
    /* log_debug("starlark_scm_create_tokens_enum"); */
}

/*
  called by sunlark:edit, not lsunlark pkg
 */
EXPORT void sunlark_config_sunlark_table(s7_scheme *s7)
{
    // log_debug("sunlark_config_sunlark_table");

    /* if sunlark exists, we're sunlark:repl; otherwise we're sunlark:edit */
}
