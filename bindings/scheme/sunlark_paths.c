#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"

#include "s7.h"

#include "sunlark_paths.h"

s7_pointer sunlark_resolve_path(s7_scheme *s7,
                                s7_pointer self_s7,
                                s7_pointer path_args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_resolve_path: %s",
              s7_object_to_c_string(s7, path_args));
#endif

    /* struct node_s *self =  (struct node_s *)s7_c_object_value(self_s7); */
    s7_pointer self = self_s7;
    s7_pointer tmp;

    /* problem: three kinds of value: node, nodelist, scalar */
    int self_tid = sunlark_node_tid(s7, self);

    s7_pointer prev_path_arg = s7_car(path_args);

    s7_pointer path_arg;
    const char *prop;
    int loop_idx = 0;
    while ( !s7_is_null(s7, path_args) ) {
        log_debug("LOOP (resolve) %d: %s", loop_idx++,
                  s7_object_to_c_string(s7, path_args));
        path_arg = s7_car(path_args);
        /* log_debug("path_arg: %s", s7_object_to_c_string(s7, path_arg)); */

        log_debug("\tdata tid: %d %s",
                  self_tid,
                  token_name[self_tid][0]);

        if (s7_is_keyword(path_arg) || s7_is_integer(path_arg)) {
            if (s7_is_keyword(path_arg)) {
                s7_pointer sym = s7_keyword_to_symbol(s7, path_arg);
                prop = s7_symbol_name(sym);
                log_debug("kw prop: %s", prop);
            }

            /* scalar-valued props (e.g. :tid) must come last */
            switch( self_tid ) {

            case TK_Build_File: /* build_target */
                log_debug("dispatching on TK_Build_File %d", loop_idx);
                // valid keys: :target[s], :load,
                // returns: nodelist (UT_hash)
                /* e.g.(set! (bfnode :targets ::tgt :deps :name) "foo") */
                /* :targets, :loads, :package */

                tmp = sunlark_dispatch_on_buildfile(s7,
                                                 s7_c_object_value(self),
                                                 path_args);
                if (s7_is_c_object(tmp)) {
                    self = tmp;
                    self_tid = sunlark_node_tid(s7, tmp);
                    log_debug("is node?: %d",
                              c_is_sunlark_node(s7,tmp));
                    log_debug("is nodelist?: %d",
                              c_is_sunlark_nodelist(s7,tmp));
                    log_debug("lookup data len: %d", utarray_len(
                            (UT_array*)s7_c_object_value(self)));
                } else {
                    return tmp;
                }
                break;

            case TK_Node_List:
                log_debug("dispatching on TK_Node_list %d", loop_idx);
                /* key indices: :attrs */
                /* e.g. (ast :target 'cc_test :attrs) */
                /* e.g. (ast :targets :attrs) */

                /* int indexing */
                if (s7_is_integer(path_arg)) {
                    tmp = sunlark_nodelist_lookup(s7,
                                                  s7_c_object_value(self),
                                                  path_arg);
                    if (s7_is_c_object(tmp)) {
                        self = tmp;
                        self_tid = sunlark_node_tid(s7, tmp);
                    } else {
                        return tmp;
                    }
                }

                break;
            /* case TK_Load_Stmt: */
            /*     /\* :build-file > :stmt-list > smallstmt-list > :load-stmt *\/ */
            /*     break; */
            /* case Package - same as :call-expr */
            case TK_Call_Expr: /* build_target */
                log_debug("dispatching on TK_Call_Expr %d", loop_idx);
                /* e.g. (set! (target :attrs :deps :name) "foo") */
                /* :rule, :attrs */

                /* return _path_op_target(s7, self, s7_cdr(path_args)); */
                return sunlark_dispatch_on_target(s7, self, path_args);

                /* return sunlark_path_for_target(s7, */
                /*                               s7_c_object_value(self), */
                /*                               path_args); */
                break;
            case TK_Arg_List: /* rename :attr-list? */
                log_debug("dispatching on TK_Arg_List %d", loop_idx);
                /* e.g. (set! (args :deps :name) "foo") */
                /* :<attrname>, :n (nth arg) */
                tmp = sunlark_attr_list_kw_lookup(s7,
                                                  s7_c_object_value(self),
                                                  path_arg);
                if (s7_is_c_object(tmp)) {
                    self = tmp;
                    self_tid = sunlark_node_tid(s7, tmp);
                } else {
                    return tmp;
                }
                break;
            case TK_Binding: /* rule attribute */
                log_debug("dispatching on TK_Binding %d", loop_idx);
                /* e.g. (set! (attr :name) "foo") */
                /* :name, :value */
                tmp = sunlark_attribute_property_lookup(s7,
                                                        s7_c_object_value(self),
                                                        prop);
                if (s7_is_c_object(tmp)) {
                    self = tmp;
                    self_tid = sunlark_node_tid(s7, tmp);
                } else {
                    return tmp;
                }
                break;
            default:
                log_debug("dispatching on default %d", loop_idx);
                /* usually :subnodes */
                if (s7_is_integer(path_arg)) {
                    log_debug("dispatching on default (int) %d", loop_idx);

                    if (c_is_sunlark_node(s7, self)) {
                        struct node_s *n = s7_c_object_value(self);
                        if (n->subnodes) {
                            self = sunlark_nodelist_lookup(s7, n->subnodes,
                                                           path_arg);
                            self_tid = sunlark_node_tid(s7, self);
                        } else {
                            log_debug("0 no subnodes for int indexing");
                            // error? unspecified?
                            return s7_unspecified(s7);
                        }
                    } else {
                        if (c_is_sunlark_nodelist(s7, self)) {
                            UT_array *nl = s7_c_object_value(self);
                            self = sunlark_nodelist_lookup(s7, nl,
                                                           path_arg);
                            self_tid = sunlark_node_tid(s7, self);
                        } else {
                            log_debug("1 no subnodes for int indexing");
                            // error? unspecified?
                            return s7_unspecified(s7);
                        }
                    }
                } else {
                    tmp = sunlark_common_property_lookup(s7,
                                         s7_c_object_value(self),
                                                         path_arg);
                    if (s7_is_c_object(tmp)) {
                        self = tmp;
                        self_tid = sunlark_node_tid(s7, tmp);
                    } else {
                        return tmp;
                    }
                }
            }
        } else {
            if (s7_is_symbol(path_arg)) {
                log_debug("dispatching on symbol %d", loop_idx);
                /* log_debug("SYMBOL path step: %s", */
                /*           s7_object_to_c_string(s7, path_arg)); */
                /* log_debug("prev path step: %s", */
                /*           s7_object_to_c_string(s7, prev_path_arg)); */

                /* ok: symbol after :target, :attrs */
                /* not ok: symbol after :load */
                if (prev_path_arg == s7_make_keyword(s7, "load")) {
                    /* log_error("ERROR: path step %s may not be followed by symbol; found: '%s", */
                    /*           s7_object_to_c_string(s7, prev_path_arg), */
                    /*           s7_object_to_c_string(s7, path_arg)); */

                    return s7_error(s7, s7_make_symbol(s7,
                                                       "invalid_argument"),
                            s7_list(s7, 3, s7_make_string(s7,
                "ERROR: path step ~A may not be followed by symbol; found: '~A"),
                                    prev_path_arg, path_arg));

                }
                if (prev_path_arg == kw_target) {
                    /* symbols match starlark Id productions */
                    /* e.g. rule and attr names */
                    self = sunlark_get_targets_by_rule_name(s7,
                                                           path_arg,
                                                           self);
                    self_tid = sunlark_node_tid(s7, self);
                }
                if (prev_path_arg == kw_attrs) {
                    /* symbols match starlark Id productions */
                    /* e.g. rule and attr names */
                    self = sunlark_get_attr_by_name(s7,
                                                    path_arg,
                                                    self);
                    self_tid = sunlark_node_tid(s7, self);
                }

            } else {
                if (s7_is_string(path_arg)) {
                    log_debug("dispatching on string %d", loop_idx);
                    /* log_debug("STRING path step: %s", */
                    /*           s7_object_to_c_string(s7, path_arg)); */
                    /* :loads "foo" - get load("foo"...) node */
                    /* :targets "bar" - selects target :bar */
                    /* :attrs "baz" - selects "baz" attr */
                    /* better: special syntax? ::foo, ::bar? */
                    self = sunlark_handle_string_query_arg(s7,
                                                           s7_string(path_arg),
                                                           self);
                    self_tid = sunlark_node_tid(s7, self);
                } else {
                    if (s7_is_procedure(path_arg)) {
                        log_debug("dispatching on procedure %d", loop_idx);

                        if (sunlark_is_nodelist(s7, self))
                            log_debug("PASSING NODELIST");
                        else
                            if (sunlark_is_node(s7, self))
                                log_debug("PASSING NODE");

                        log_debug("  ARG: %d %s",
                                  sunlark_node_tid(s7, self),
                                  token_name[sunlark_node_tid(s7, self)][0]);
                        s7_pointer args = s7_cons(s7,
                                                  self,
                                                  s7_nil(s7));
                        /* here path_arg is the function */
                        return s7_call(s7, path_arg, args);
                    } else {
                        return(s7_wrong_type_arg_error(s7, "AST-node-ref",
                                                       2,
                                                       path_arg,
                                                       "a keyword"));
                    }
                }
            }
        }
        path_args  = s7_cdr(path_args);
        prev_path_arg = path_arg;
    }
    /* log_debug("RESOLVED! xxxxxxxxxxxxxxxx"); */
    return self;
}

