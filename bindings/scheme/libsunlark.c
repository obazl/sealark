/*
  * convert C nodes to scheme
 */

#include <errno.h>
/* #include <pthread.h> */
#include <stdarg.h>
#include <sys/stat.h>
#include <unistd.h>

#include "log.h"
#include "utstring.h"

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

EXPORT s7_scheme *sunlark_init(void)
{
    s7_scheme *s7 = s7_init();
    s7_int an_t = configure_s7_ast_node_type(s7);
    s7_int anl_t = configure_s7_ast_nodelist_type(s7);
    return s7;
}

void s7lark_nodelist2scm(s7_scheme *s7, UT_array *_nodelist, int level)
{
    /* log_debug("s7lark_nodelist2scm level %d ct: %d", */
    /*           level, utarray_len(_nodelist)); */

    struct node_s *node=NULL;
    int i = 1;
    while( (node=(struct node_s*)utarray_next(_nodelist, node))) {
        /* log_debug("subnode %d", i); */
        s7lark_node2scm(s7, node, level); /* puts node tbl on ToS */
        i++;
    }
    /* log_debug("/s7lark_nodelist2scm level %d", utarray_len(_nodelist)); */
}

EXPORT void s7lark_node2scm(s7_scheme *s7, struct node_s *node, int level)
{
    log_debug("s7lark_node2scm %d", level);

    log_debug("pushing type %d %s",
              node->type, token_name[node->type][0]);
    log_debug("/s7lark_node2scm %d", level);
}

EXPORT s7_pointer s7lark_ast2scm(s7_scheme *s7, struct parse_state_s *parse)
{
    log_debug("s7lark_ast2scm");

    s7_pointer new_ast_node_s7 = s7_make_c_object(s7,
                                                  ast_node_t,
                                                  (void *)parse->root);
    return new_ast_node_s7;
}

/*
  Top of Stack: parsed lAST
 */
EXPORT void s7lark_call_user_handler(s7_scheme *s7, char *handler)
{
    log_debug("s7lark_call_user_handler");

    /* log_debug("lua user-provided callback '%s' returned", handler); */
}

/**
   configure lua search paths for bazel env

   assumption: launched via `$ bazel run ...`
   uses: obazl_d, runfiles_root

   lua search path always contains:
       <proj_root>/.s7lark.d/  -- contains user-defined handler
       if run by `bazel run`:
           <exec_root>/<runfiles_dir>/s7lark/lua -- default handler
           (linked from @s7lark//s7lark/lua)
       else:
           ???
    for runtime files: see https://github.com/bazelbuild/bazel/issues/10022
        https://github.com/laszlocsomor/bazel/commit/21989926c1a002709ec3eba9ee7a992506f2d50a
 */

EXPORT void s7lark_augment_load_path(s7_scheme *s7, char *path)
{
    if (path == NULL) return;

    log_debug("s7lark_augment_load_path %s", path);

    s7_pointer newlp =  s7_add_to_load_path(s7, path);
    /* lp = s7_load_path(s7); */
    /* log_debug("new load path: %s", s7_object_to_c_string(s7, newlp)); */
}

/*
  called by //s7lark:edit, but not //s7lark:repl
 */
EXPORT void s7lark_scm_load_file(s7_scheme *s7, char *scm_file)
{
    // log_debug("starlark_scm_load_handlers");

    /* log_debug("loading scm file: %s", default_handler_file_name); */
}

EXPORT void s7lark_create_token_enums(s7_scheme *s7)
{
    /* log_debug("starlark_scm_create_tokens_enum"); */
}

/*
  called by s7lark:edit, not ls7lark pkg
 */
EXPORT void s7lark_config_s7lark_table(s7_scheme *s7)
{
    // log_debug("s7lark_config_s7lark_table");

    /* if s7lark exists, we're s7lark:repl; otherwise we're s7lark:edit */
}
