#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"

#include "s7.h"

#include "sunlark_package.h"

s7_pointer sunlark_package_dispatcher(s7_scheme *s7,
                                         s7_pointer data,
                                         s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_PATHS)
    log_debug("sunlark_package_dispatcher: %s",
              s7_object_to_c_string(s7, path_args));
#endif
    struct node_s *pkg = s7_c_object_value(data);
    if (pkg->tid != TK_Package) {
        log_error("Expected node tid %d, got %d %s", TK_Package,
                  pkg->tid, TIDNAME(pkg));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    int op_count = s7_list_length(s7, path_args);
    /* log_debug("op count: %d", op_count); */

    s7_pointer op = s7_car(path_args);
    //s7_pointer op2, op3, op4, op5;
    if ( !s7_is_keyword(op) ) {
        log_error("Path op %s not supported for :package nodes",
                  s7_object_to_c_string(s7, op));
        exit(EXIT_FAILURE);     /* FIXME */
    }

    s7_pointer result_list;

    if (op == KW(>>) || op == KW(targets)) {
        s7_pointer r
            = sunlark_forall_targets(s7, pkg, s7_cdr(path_args));
        return r;
    }

    if (op == KW(>) || op == KW(target)) {
        log_debug("dispatching on pkg for target");
        result_list //= sunlark_targets_for_buildfile(s7, pkg);
            = _pkg_target_dispatcher(s7, pkg, s7_cdr(path_args));
            return result_list;
    }

    if (op == KW(load)) {
        // :package > :stmt-list :smallstmt-list > load-expr,...
        errno = 0;
        s7_pointer loadstmt
            = sunlark_pkg_loadstmt_dispatch(s7, pkg, s7_cdr(path_args));
        /* /\* UT_array *loads = sealark_loadstmts(pkg); *\/ */
        if (loadstmt)
            return loadstmt;
        else
            return handle_errno(s7, errno, path_args);
    }

    if (op == KW(loads)) {
        // :package > :stmt-list :smallstmt-list > load-expr,...
        /* result_list = sunlark_fetch_load_stmts(s7, pkg); */

        UT_array *loads = sealark_procs_for_id(pkg,
                                               "load");
        /* UT_array *loads = sealark_loadstmts(pkg); */
        if (loads)
            return nodelist_to_s7_list(s7, loads);
        else
            log_debug("ERROR: ...fixme...");
    }

    if (op == KW(package)) {
        UT_array *procs = sealark_procs_for_id(pkg,
                                               "package");
        if (utarray_len(procs) == 1)
            return sunlark_node_new(s7,
                                    utarray_eltptr(procs, 0));
        else
            return s7_nil(s7);
    }
    if (op == KW(directives)) {
        /* all procs and definitions */
        UT_array *directives = sealark_directives(pkg);
        return nodelist_to_s7_list(s7, directives);
        return NULL;
    }
    if (op == KW(definitions)) {
        /* sealark_debug_print_ast_outline(pkg, 0); */
        UT_array *defns = sealark_definitions(pkg);
        return nodelist_to_s7_list(s7, defns);
    }
    if (op == KW(vardefs)) {
        /* result_list = */
        UT_array *vardefs = sealark_vardefs(pkg);
        return nodelist_to_s7_list(s7, vardefs);
    }
    if (op == KW(procedures)) {
        UT_array *procs = sealark_procs(pkg);
        return nodelist_to_s7_list(s7, procs);
    }
    /* common properties */
    s7_pointer result = sunlark_common_property_lookup(s7, pkg, op);
    if (result) return result;

        log_error("Bad arg on buildfile: %s", s7_object_to_c_string(s7, op));
    return(s7_error(s7,
                    s7_make_symbol(s7, "invalid_argument"),
                    s7_list(s7, 2, s7_make_string(s7,
                 "Arg \"~A\" inadmissable here"),
                            op)));

    /* switch(op_count) { */
    /* case 1: */
    /*     /\* if (op == KW(>>) || op == KW(>) || op == KW(targets)) { *\/ */
    /*     /\*     result_list = sunlark_targets_for_buildfile(s7, pkg); *\/ */
    /*     /\*     //FIXME: switch to: *\/ */
    /*     /\*     /\\* UT_array *loads = sealark_procs_for_id(pkg, *\\/ *\/ */
    /*     /\*     /\\*                                       "target"); *\\/ *\/ */
    /*     /\*     return result_list; *\/ */
    /*     /\* } *\/ */
    /*    if (op == KW(loads)) { */
    /*         // :package > :stmt-list :smallstmt-list > load-expr,... */
    /*        /\* result_list = sunlark_fetch_load_stmts(s7, pkg); *\/ */

    /*        UT_array *loads = sealark_procs_for_id(pkg, */
    /*                                               "load"); */
    /*        /\* UT_array *loads = sealark_loadstmts(pkg); *\/ */
    /*        if (loads) */
    /*            return nodelist_to_s7_list(s7, loads); */
    /*        else */
    /*            log_debug("ERROR: ...fixme..."); */
    /*     } */
    /*    if (op == KW(package)) { */
    /*        UT_array *procs = sealark_procs_for_id(pkg, */
    /*                                               "package"); */
    /*        if (utarray_len(procs) == 1) */
    /*            return sunlark_node_new(s7, */
    /*                                    utarray_eltptr(procs, 0)); */
    /*        else */
    /*            return s7_nil(s7); */
    /*     } */
    /*     if (op == KW(directives)) { */
    /*         /\* all procs and definitions *\/ */
    /*         UT_array *directives = sealark_directives(pkg); */
    /*         return nodelist_to_s7_list(s7, directives); */
    /*         return NULL; */
    /*     } */
    /*     if (op == KW(definitions)) { */
    /*         /\* sealark_debug_print_ast_outline(pkg, 0); *\/ */
    /*         UT_array *defns = sealark_definitions(pkg); */
    /*         return nodelist_to_s7_list(s7, defns); */
    /*     } */
    /*     if (op == KW(vardefs)) { */
    /*         /\* result_list = *\/ */
    /*         UT_array *vardefs = sealark_vardefs(pkg); */
    /*         return nodelist_to_s7_list(s7, vardefs); */
    /*     } */
    /*     if (op == KW(procedures)) { */
    /*         UT_array *procs = sealark_procs(pkg); */
    /*         return nodelist_to_s7_list(s7, procs); */
    /*     } */
    /*     /\* common properties *\/ */
    /*     s7_pointer result = sunlark_common_property_lookup(s7, pkg, op); */
    /*     if (result) return result; */
    /*     break; */
    /* case 2: */
    /*     return buildfile_handle_dyadic_path(s7, pkg, path_args); */
    /*     break; */
    /* case 3: */
    /*     return buildfile_handle_triadic_path(s7, pkg, path_args); */
    /*     break; */
    /* case 4: */
    /*     return buildfile_handle_tetradic_path(s7, pkg, path_args); */
    /*     break; */
    /* case 5: */
    /*     return buildfile_handle_pentadic_path(s7, pkg, path_args); */
    /*     break; */
    /* default: */
    /*     return buildfile_handle_pentadic_path(s7, pkg, path_args); */
    /* } */

    /* /\* predicates *\/ */
    /* s7_pointer sym = s7_keyword_to_symbol(s7, op); */
    /* char *key = (char*)s7_symbol_name(sym); */
    /* if (strrchr(key, '?') - key == strlen(key)-1 ) { */
    /*     result_list = sunlark_is_kw(s7, key, pkg); */
    /* } */

    /* /\* common props *\/ */
    /* result_list = sunlark_common_property_lookup(s7, pkg, op); */
    /* if (result_list == NULL) { */
    /*     /\* result_list = s7_unspecified(s7); *\/ */
    /*     result_list =(s7_error(s7, s7_make_symbol(s7, */
    /*                                               "invalid_argument"), */
    /*                            s7_list(s7, 2, s7_make_string(s7, */
    /*                                                          "ast-node-ref arg must be one of :package, :loads, :targets; got ~A"), */
    /*                                    op))); */

    /* } */

    /* s7_pointer next_step = s7_cadr(path_args); */
    /* if (s7_is_null(s7, next_step)) { */
    /*     return result_list; */
    /* } else { */
    /*     return sunlark_dispatch(s7, result_list, s7_cdr(path_args)); */
    /* } */
}

/* ************************************************ */
/* path resolved so far: :> or :target
   next step options:
       string - target_for_name
       int    - target_for_index
   (options after :>> or :targets :
       sym    - filter by rule (e.g. 'cc_library)
       list   - filter e.g. '(cc_library cc_test "hello-lib")
 */

LOCAL s7_pointer _pkg_target_dispatcher(s7_scheme *s7,
                                        struct node_s *pkg,
                                        s7_pointer path_args)
{
#if defined (DEBUG_TRACE) || defined(DEBUG_TARGETS)
    log_debug("sunlark_pkg_target_dispatcher: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    int op_count = s7_list_length(s7, path_args);
    /* log_debug("op count: %d", op_count); */

    s7_pointer op = s7_car(path_args);
    /* s7_pointer op2 = s7_cadr(path_args); */

    s7_pointer result;

    /* resolved so far: :> */
    if (s7_is_string(op)) {     /* :> "hello-world") */
        errno = 0;
        struct node_s *tgt_node
            = sealark_target_for_name(pkg, s7_string(op));

        if (tgt_node == NULL) {
            errno = ENOT_FOUND;
            return NULL;
        }

        if ( s7_is_null(s7, s7_cdr(path_args)) ) { /* e.g. (:> "mylib") */
            return sunlark_node_new(s7, tgt_node);
        } else {
            /* e.g. (:> "mylib" :@ ...), (:> "mylib" :rule), etc. */
            /* return sunlark_target_1(s7, tgt_node, s7_cdr(path_args)); */
            return sunlark_target_dispatcher(s7, tgt_node,
                                             s7_cdr(path_args));
        }

    }
    if (s7_is_integer(op)) {
        struct node_s *tgt_node = sealark_target_for_index(pkg,
                                                           s7_integer(op));
        if ( s7_is_null(s7, s7_cdr(path_args)) ) { /* e.g. (:> 0) */
            return sunlark_node_new(s7, tgt_node);
        } else {
            /* e.g. (:> 1 :@ ...), (:> 1 :rule) */
            return sunlark_target_dispatcher(s7, tgt_node,
                                             s7_cdr(path_args));
            /* return sunlark_target_1(s7, tgt_node, */
            /*                           s7_cdr(path_args)); */
        }

    }
    log_error("Bad arg: %s after :target", s7_object_to_c_string(s7, op));
    return(s7_error(s7,
                    s7_make_symbol(s7, "invalid_argument"),
                    s7_list(s7, 2, s7_make_string(s7,
                     "Bad arg ~S after target (only string or int allowed)"),
                            op)));
}

