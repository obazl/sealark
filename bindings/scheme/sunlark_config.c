#include <ctype.h>
#include <errno.h>
#include <stdlib.h>

#include "log.h"
#include "s7.h"

#include "sunlark_config.h"

char *def = "(define define "
   "(let ((o-define define)) "
     "(let ((sym (gensym))) "
       "(macro (k v . rest) "
        "`(begin "
            "(,o-define ,k ,v ,@rest) "
            "(if (list? ',k) "
                "(car ',k) "
                "',k))))))";

LOCAL void _redefine_define(s7_scheme *s7)
{
    s7_eval_c_string(s7, def);
}

LOCAL s7_pointer _error_handler(s7_scheme *sc, s7_pointer args)
{
  fprintf(stdout, "error: %s\n", s7_string(s7_car(args)));
  return(s7_f(sc));
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
    /* s7_int anl_t = configure_s7_ast_nodelist_type(s7); */

    s7_define_function(s7, "error-handler",
                       _error_handler, 1, 0, false,
                       "our error handler");

    export_token_tables(s7);

    /* _redefine_define(s7); */

    register_binding_ctor_macro(s7);

    //FIXME: error handling
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
