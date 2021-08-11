#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"

#include "sunlark_serializers.h"

#if INTERFACE
#define SZDISPLAY_BUF (4096 * 4)
#endif
//FIXME: use UT_string
char *display_buf;
int   display_bufsz;
char *display_ptr;

int indent = 2;

/* **************************************************************** */
#if INTERFACE
#define SUNLARK_TO_STRING_HELP "(sunlark->string ast_node <syntax>) where <sytax> is one of :starlark, :scheme, :repl, :debug"
#endif

/* args is always an s7 list.

   arg1: object to print
   arg2: optional style kw, :squeeze, :crush

 */
EXPORT s7_pointer sunlark_to_string(s7_scheme *s7, s7_pointer args)
{
#if defined(DEBUG_SERIALIZERS)
    log_debug(">>>>>>>>>>>>>>>> sunlark_to_string <<<<<<<<<<<<<<<<");
    /* log_debug("args: %s", s7_object_to_c_string(s7, args)); */
#endif

    s7_pointer obj = s7_car(args);
    if (!s7_is_c_object(obj)) {
        log_error("sunlark->string can only serialize sunlark nodes; got arg of type %s",
                  s7_object_to_c_string(s7, s7_type_of(s7, obj)));
        return handle_errno(s7, EINVALID_SERIALIZE_ARG,
                            s7_type_of(s7, obj));
    }

    s7_pointer syntax;
    if (s7_cadr(args) == KW(ast)) {
        UT_string *buf = sealark_debug_display_ast_outline(s7_c_object_value(obj), 0);
        s7_pointer disp = s7_make_string(s7, utstring_body(buf));
        utstring_free(buf);
        return disp;
    }
    if (s7_cadr(args) == KW(starlark)) {
        s7_pointer _args;
        if (s7_list_length(s7, args) == 3) {
            _args = s7_list(s7, 2, s7_car(args), s7_caddr(args));
        } else {
            _args = s7_list(s7, 1, s7_car(args));
        }
        s7_pointer p = sunlark_to_starlark(s7, _args);
        return p;
    }
    if (s7_cadr(args) == KW(scheme)) {
        return s7_make_string(s7, "SCHEME");
    }
    log_error("Syntax specifier not recognizable: %s",
              s7_object_to_c_string(s7, s7_cadr(args)));
    return NULL;
}

/* **************************************************************** */
void sunlark_register_to_string_entry_pt(s7_scheme *s7)
{
    s7_c_type_set_to_string(s7, ast_node_t, sunlark_display);
}

/* Scheme entry point for object->string */
/* WARNING: called by s7_object_to_c_string - do not call directly  */
LOCAL s7_pointer sunlark_display(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_SERIALIZERS
    log_debug(">>>>>>>>>>>>>>>> sunlark_display <<<<<<<<<<<<<<<<");
#endif

    /* if (sunlark_node_tid(s7,s7_car(args)) == TK_Package) { */
    /*     return s7_nil(s7); */
    /* } */

    UT_string *buffer;
    utstring_new(buffer);

    display_buf = calloc(1, SZDISPLAY_BUF);
    if (display_buf == NULL) {
        log_error("ERROR on calloc");
        //FIXME cleanup
        exit(EXIT_FAILURE);
    } else {
        display_bufsz = SZDISPLAY_BUF;
    }
    display_ptr = display_buf;

    s7_pointer obj, choice;
    char *descr;
    obj = s7_car(args);
    /* sealark_display_node(s7_c_object_value(obj), buffer, 0); */
    /* sealark_debug_log_ast_outline(s7_c_object_value(obj), 0); */

    if (s7_is_pair(s7_cdr(args)))
        choice = s7_cadr(args);
    else choice = s7_t(s7);

    if (choice == s7_make_keyword(s7, "readable")) {
        memset(display_buf, '\0', SZDISPLAY_BUF);
        display_ptr = (char*)display_buf;
        descr = sunlark_node_display_readably(s7, s7_c_object_value(obj));
    }
    else {
        /* descr = sunlark_node_display(s7, s7_c_object_value(obj)); */
        sunlark_display_node(s7_c_object_value(obj), buffer, 0);
    }

    obj = s7_make_string(s7, utstring_body(buffer));
    /* obj = s7_make_string(s7, descr); */

    /* free(descr); // frees display_buf? */
    /* display_bufsz = 0; */
    utstring_free(buffer);
    return(obj);
}

/** sunlark_node_display_readably

    produces a "roundtrippable" string, one that when read by the reader
    results in an object equal to the original.
 */
char *sunlark_node_display_readably(s7_scheme *s7, void *value)
{
#ifdef DEBUG_SERIALIZERSs
    log_debug("sunlark_node_display_readably");
#endif

    struct node_s *nd = (struct node_s *)value;

    // check display_buf size, expand if needed

    char buf[128];
    int len;

    sprintf(display_ptr++, "(");

   sprintf(buf, "(tid %d) ;; %s\n", nd->tid, token_name[nd->tid][0]);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " (line %d)\n", nd->line);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " (col  %d)\n", nd->col);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    sprintf(buf, " (trailing_newline %d)\n", nd->trailing_newline);
    len = strlen(buf);
    snprintf(display_ptr, len+1, "%s", buf);
    display_ptr += len;

    if (nd->tid == TK_STRING) {
        sprintf(buf, " (qtype #x%02X)\n", nd->qtype);
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    if (nd->s) {
        sprintf(buf, " (s %s)\n", nd->s);
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    if (nd->comments) {
        sprintf(buf, " (comments ");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;

        /* updates global display_buf */
        //FIXME sunlark_nodelist_display_readably(s7, (UT_array*)nd->comments);

        /* sprintf(buf, ") ;; end comments\n\n"); */
        sprintf(buf, ")\n");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    if (nd->subnodes) {
        sprintf(buf, " (subnodes\n\t");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;

        /* updates global display_buf */
        //FIXME sunlark_nodelist_display_readably(s7, (UT_array*)nd->subnodes);

        sprintf(buf, ")\n");
        len = strlen(buf);
        snprintf(display_ptr, len+1, "%s", buf);
        display_ptr += len;
    }

    sprintf(display_ptr - 1, ")\n");  /* backup to rem last newline */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    display_ptr++;

    return display_buf; // workbuffer;
}

/* **************************************************************** */
#if INTERFACE
#define SUNLARK_TO_STARLARK_HELP "(ast-node->starlark ast_node)"
#endif

/* args is always an s7 list.

   arg1: object to print
   arg2: optional style kw, :squeeze, :crush

 */
EXPORT s7_pointer sunlark_to_starlark(s7_scheme *s7, s7_pointer args)
{
#if defined(DEBUG_SERIALIZERS)
    log_debug(">>>>>>>>>>>>>>>> sunlark_to_starlark <<<<<<<<<<<<<<<<");
    log_debug("args: %s", s7_object_to_c_string(s7, args));
    log_debug("args type: %s", s7_object_to_c_string(s7, s7_type_of(s7, args)));
#endif

    UT_string *buf;
    utstring_new(buf);

    s7_pointer form = s7_car(args); // what to print

    s7_pointer style = s7_cadr(args);
    /* log_debug("style: %s", s7_object_to_c_string(s7, style)); */

    if ( s7_is_c_object(form) ) {
        struct node_s *n1 = s7_c_object_value(form);
        sealark_node_to_starlark(n1, buf);
        goto resume;
    }
    if ( s7_is_list(s7, form) ) {
            log_debug("printing s7 list");

            if (! s7_is_null(s7, s7_car(form))) return NULL; //FIXME

            s7_pointer _list = form;
            if ( !s7_is_c_object(s7_car(form)) ) {
                log_error("sunlark->starlark only applies to sunlark nodes");
                return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                                s7_list(s7, 2, s7_make_string(s7,
                "Cannot serialize a non-starlark node to starlark: ~A"),
                                        form)));
            }

            while (! s7_is_null(s7, _list)) {
                log_debug("form type: %s",
                          s7_object_to_c_string(s7, s7_type_of(s7, s7_car(_list))));
                /* if (s7_is_c_object(_list)) { */
                struct node_s *t = s7_c_object_value(s7_car(_list));
                /* log_debug("\titem tid: %d %s", t->tid, TIDNAME(t)); */
                sealark_node_to_starlark(t, buf);
                /* } */
                _list = s7_cdr(_list);
            }
        goto resume;
    }
    if ( s7_is_vector(form) ) {
        log_debug("printing s7 vector");
        utstring_printf(buf, "%s", s7_object_to_c_string(s7, (form)));
        goto resume;
    }
    if (s7_is_string(form)) {
        utstring_printf(buf, "%s", s7_string(form));
        goto resume;
    }
    if (s7_is_character(form)) {
        utstring_printf(buf, "%c", s7_character(form));
        goto resume;
    }
    if (s7_is_boolean(form)) {
        if (form == s7_t(s7))
            utstring_printf(buf, "#t");
        else
            utstring_printf(buf, "#f");
        goto resume;
    }
    if (s7_is_number(form)) {
        utstring_printf(buf, "%s", s7_number_to_string(s7,form,10));
        goto resume;
    }
    if (s7_is_keyword(form)) {
        s7_pointer sym = s7_keyword_to_symbol(s7,form);
        utstring_printf(buf, "%s",
                        s7_symbol_name(form));
        goto resume;
    }
    if (s7_is_unspecified(s7, form)) {
        /* FIXME: return null? */
        utstring_printf(buf, "%s",s7_object_to_c_string(s7, form));
        goto resume;
    }

    log_error("Unexpected form for printing, should be c-object or list.");

 resume: ;

    char *output;
    if (style == KW(squeeze)) {
        output = sealark_squeeze_string(buf);
    } else {
        if (style == KW(crush)) {
            output = sealark_crush_string(buf);
        } else {
            output = utstring_body(buf);
        }
    }

    s7_pointer out = s7_make_string(s7, output);
    free(output);
    return out;
}

/* **************** */
LOCAL void _display_arg_list(struct node_s *nd,
                             UT_string *buffer,
                             int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_arg_list");
#endif

    assert(nd->tid == TK_Arg_List);

    utstring_printf(buffer, "%*s:attrs (",
                    level*indent, " ", (level+1)*indent, " ");

    struct node_s *sub = NULL;
    int len = utarray_len(nd->subnodes);
    int i = 0, nameidx = -1;
    /* NB: 'name' attr can occur anywhere, we need to skip it */
    while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
        if (sealark_is_name_attr(sub)) {
            nameidx = i;
            i+=2;
            continue;
        }
        if (sub->tid == TK_COMMA) {
            continue;
        }
        sunlark_display_node(sub, buffer,
                             (i==nameidx+2)? 0 : level+2);
        /* utstring_printf(buffer, " == LEN %d, i %d ==", len, i); */
        if (len - i > 1) {
            utstring_printf(buffer, "\n");
        }
        i+=2;
    }
    utstring_printf(buffer, ")");
}

/* **************************************************************** */
LOCAL void _display_assign_stmt(struct node_s *nd,
                                UT_string *buffer,
                                int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_assign_stmt");
#endif

    assert(nd->tid == TK_Assign_Stmt);

    struct node_s *lhs = utarray_eltptr(nd->subnodes, 0);
    assert(lhs->tid == TK_Expr_List);

    int len = utarray_len(lhs->subnodes);
    if (len > 2) { /* first item plus comma  */
        /* multi-assign */
        utstring_printf(buffer, "%*s(def-vars (",
                        level*indent, " ", (level+1)*indent, " ");
        struct node_s *sub = NULL;
        int i;
        while( (sub=(struct node_s*)utarray_next(lhs->subnodes, sub)) ) {
            if (sub->tid == TK_COMMA) {
                utstring_printf(buffer, " ");
                i++; continue;
            }
            utstring_printf(buffer, "%s", sub->s);
            if (len - i > 2) utstring_printf(buffer, " ");
            i++;
        }
        utstring_printf(buffer, ") ");
    } else {
        /* single assign */
        utstring_printf(buffer, "%*s(def-var ",
                        level*indent, " ", (level+1)*indent, " ");
        struct node_s *var = utarray_eltptr(lhs->subnodes, 0);
        utstring_printf(buffer, "%s", var->s);
    }

    struct node_s *rhs = utarray_eltptr(nd->subnodes, 2);
    assert(rhs->tid == TK_Expr_List);
    if (len > 2) {
        utstring_printf(buffer, "(");
    }
    /* utstring_printf(buffer, " ... NOT YET imlemented ..."); */
    _display_expr_list(rhs, buffer, 0);
    if (len > 2) {
        utstring_printf(buffer, ")");
    }

    utstring_printf(buffer, ")");
}

/* **************************************************************** */
LOCAL void _display_package(struct node_s *nd,
                               UT_string *buffer,
                               int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_package, level %d", level);
#endif

    utstring_printf(buffer, "(package\n");
    /* struct node_s *subnodes = utarray_eltptr(nd->subnodes, 0); */
    struct node_s *sub = NULL;
    while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
        sunlark_display_node(sub, buffer, level+1);
    }
}

/* **************** */
LOCAL void _display_binding_node(struct node_s *nd,
                                  UT_string *buffer,
                                  int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("sunlark_display_binding_node");
#endif

    utstring_printf(buffer, "%*s(",
                    (level==0)? 0 : level*indent+4,
                    (level==0)? "" : " ");
                    /* (level==0)? 0 : (level+1)*indent+level+1, */
                    /* (level==0)? "" : " "); */
    struct node_s *key = utarray_eltptr(nd->subnodes, 0);
    assert(key->tid == TK_ID);
    utstring_printf(buffer, "%s ", key->s);
    /* skip TK_EQ */
    struct node_s *val = utarray_eltptr(nd->subnodes, 2);
    _display_binding_value(val, buffer, level);

    utstring_printf(buffer, ")");
                    /* (level==0)? 0 : (level+1)*indent+level+1, "."); */
}

/* **************************************************************** */
LOCAL void _display_binding_value(struct node_s *nd,
                                  UT_string *buffer,
                                  int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("sunlark_display_binding_value");
#endif

    switch(nd->tid) {
    case TK_INT:
        utstring_printf(buffer, "%s", nd->s);
        break;
    case TK_STRING: {
        char *br = SEALARK_STRTYPE(nd->qtype);
        char *q = sealark_quote_type(nd);
        utstring_printf(buffer, "%s%s%s%s",
                        br, q, nd->s, q);
        /* utstring_printf(buffer, " %s", nd->s); */
    }
        break;
    case TK_List_Expr:
        _display_vector(nd, buffer, level);
        break;
    case TK_ID:
        if (strncmp(nd->s, "True", 4) == 0)
            utstring_printf(buffer, " #t");
        else
            if (strncmp(nd->s, "False", 5) == 0)
                utstring_printf(buffer, " #f");
            else
                utstring_printf(buffer, "'%s", nd->s);
        break;
    default:
        sunlark_display_node(nd, buffer, level);
        ;
    }
}

/* **************** */
LOCAL void _display_call_expr(struct node_s *nd,
                              UT_string *buffer,
                              int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_call_expr");
#endif

    assert(nd->tid == TK_Call_Expr);
    /* sealark_debug_log_ast_outline(nd, 0); */

    struct node_s *id = utarray_eltptr(nd->subnodes, 0);
    if ( (strncmp(id->s, "package", 7) == 0)
         && (strlen(id->s) == 7) ) {
        utstring_printf(buffer, "%*s(def-package",
                        level*indent, " ", (level+1)*indent, " ");
        struct node_s *sub = NULL;
        while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
            if (sub->tid == TK_ID) continue;
            if (sub->tid == TK_COMMA) continue;
            sunlark_display_node(sub, buffer, level+1);
            /* utstring_printf(buffer, "\n"); */
        }
        utstring_printf(buffer, ")");

    } else {
        bool is_target = sealark_call_expr_is_target(nd);
        if (is_target) {
            utstring_printf(buffer, "\n%*s(def-target",
                            level*indent,
                            (level==0)? "" : " ", (level+1)*indent, " ");
            struct node_s *rule = utarray_eltptr(nd->subnodes, 0);
            struct node_s *name = sealark_target_name(nd);
            if (name) {
                utstring_printf(buffer, " :name \"%s\"", name->s);
            }
            utstring_printf(buffer, " :rule %s", rule->s);
            struct node_s *sub = NULL;
            while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
                if (sub->tid == TK_ID) continue;
                if (sub->tid == TK_COMMA) continue;
                sunlark_display_node(sub, buffer, level+1);
                /* utstring_printf(buffer, "\n"); */
            }
            utstring_printf(buffer, ")");
        } else {
            _display_funcall_attr(nd, buffer, level);
        }
    }
}

/* **************** */
/*
 0: TK_Dict_Entry[103] @7:13
  1: TK_STRING[79] @7:13    "akey1"
  1: TK_COLON[14] @7:20
  1: TK_STRING[79] @7:22    "aval1"

 0: TK_Dict_Entry[103] @26:8
  1: TK_STRING[79] @26:8    "a"
  1: TK_COLON[14] @26:11
  1: TK_INT[40] @26:13        100
*/
LOCAL void _display_dict_entry(struct node_s *nd,
                               UT_string *buffer,
                               int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("sunlark_display_dict_entry");
#endif
    assert(nd->tid == TK_Dict_Entry);

    struct node_s *key = utarray_eltptr(nd->subnodes, 0);
    sunlark_display_node(key, buffer, level);
    utstring_printf(buffer, ": ");

    struct node_s *val = utarray_eltptr(nd->subnodes, 2);
    sunlark_display_node(val, buffer, level);
}

/* **************** */
/*
 0: TK_Dict_Entry_List[104] @7:13
  1: TK_Dict_Entry[103] @7:13
    2: TK_STRING[79] @7:13    "akey1"
    2: TK_COLON[14] @7:20
    2: TK_STRING[79] @7:22    "aval1"
  1: TK_COMMA[15] @7:29
  ...
*/
LOCAL void _display_dict_entry_list(struct node_s *nd,
                                    UT_string *buffer,
                                    int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("sunlark_display_dict_entry");
#endif
    assert(nd->tid == TK_Dict_Entry_List);

    int len = utarray_len(nd->subnodes);
    int i = 0;

    struct node_s *sub = NULL;
    while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
        if (sub->tid == TK_COMMA) {i++; continue;}
        sunlark_display_node(sub, buffer, level);
        if (len - i > 1) {
            utstring_printf(buffer, ", ");
        }
        i++;
    }
}

/* ****************************************************************

 4: TK_Dict_Expr[105] @11:12
   5: TK_LBRACE[48] @11:12
   5: TK_Dict_Entry_List[104] @12:8
     6: TK_Dict_Entry[103] @12:8
       7: TK_STRING[79] @12:8    "ckey1"
       7: TK_COLON[14] @12:15
       7: TK_STRING[79] @12:17    "cval1"
     6: TK_COMMA[15] @12:24
     6: TK_Dict_Entry[103] @13:8
       7: TK_STRING[79] @13:8    "ckey2"
       7: TK_COLON[14] @13:15
       7: TK_STRING[79] @13:17    "cval2"
     6: TK_COMMA[15] @13:24
     6: TK_Dict_Entry[103] @14:8
       7: TK_STRING[79] @14:8    "ckey3"
       7: TK_COLON[14] @14:15
       7: TK_STRING[79] @14:17    "cval3"
   5: TK_RBRACE[68] @15:4
 */

LOCAL void _display_dict_expr(struct node_s *nd,
                                    UT_string *buffer,
                                    int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_dict_expr");
#endif
    assert(nd->tid == TK_Dict_Expr);

    /* sealark_debug_log_ast_outline(nd, 0); */

    utstring_printf(buffer, "{");
    /* utstring_printf(buffer, "%*s(dict ", */
    /*                 (level==0)? 0 : level*indent+4, */
    /*                 (level==0)? "" : " "); */
    /*                 /\* (level==0)? 0 : (level+1)*indent+level+1, *\/ */
    /*                 /\* (level==0)? "" : " "); *\/ */

    struct node_s *key = utarray_eltptr(nd->subnodes, 1);
    sunlark_display_node(key, buffer, level);

    utstring_printf(buffer, "}");
}

/* **************** */
/* funcall that is value of an attr binding */
LOCAL void _display_funcall_attr(struct node_s *nd,
                                 UT_string *buffer,
                                 int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_funcall_attr");
#endif

    assert(nd->tid == TK_Call_Expr);

    utstring_printf(buffer, "%*s(funcall :fn ", 0, " ");
    /* (level==0)? 0 : level*indent, " "); */
    struct node_s *fn = utarray_eltptr(nd->subnodes, 0);
    utstring_printf(buffer, "%s :args ", fn->s);

    struct node_s *call_sfx = utarray_eltptr(nd->subnodes, 1);
    struct node_s *arg_list = utarray_eltptr(call_sfx->subnodes, 1);

    struct node_s *sub = NULL;
    int i;
    while( (sub=(struct node_s*)utarray_next(arg_list->subnodes, sub)) ) {
        if (i == 0) continue; // ID already printed
        if (sub->tid == TK_COMMA) continue;
        /* _display_funcall_attr_args(sub, buffer, level+1); */
        _display_binding_value(sub, buffer, level+1);
        i++;
    }
    utstring_printf(buffer, "))");
}

/* **************** */
LOCAL void _display_funcall_attr_args(struct node_s *nd,
                                      UT_string *buffer,
                                      int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_funcall_attr_args");
#endif

    struct node_s *sub = NULL;
    utstring_printf(buffer, "\n");
    while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
        if (sub->tid == TK_COMMA) continue;
        sunlark_display_node(sub, buffer, level);
        /* utstring_printf(buffer, "\n"); */
    }
    /* utstring_printf(buffer, ")"); */
}

/* **************** */
LOCAL void _display_call_sfx(struct node_s *nd,
                             UT_string *buffer,
                             int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_call_sfx");
#endif

    /* utstring_printf(buffer, "\n%*s#^(:call-sfx ", level*indent, " "); */

    struct node_s *sub = NULL;
    utstring_printf(buffer, "\n");
    while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
        if (sub->tid == TK_COMMA) continue;
        sunlark_display_node(sub, buffer, level);
        /* utstring_printf(buffer, "\n"); */
    }
    /* utstring_printf(buffer, ")"); */
}

/* **************** */
LOCAL void _display_comments(struct node_s *nd,
                             UT_string *buffer,
                             int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_comments");
#endif
    utstring_printf(buffer, "(comments ");

    utstring_printf(buffer, ")");
}

/* **************************************************************** */
LOCAL void _display_expr_list(struct node_s *nd,
                              UT_string *buffer,
                              int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_expr_list");
#endif

    int len = utarray_len(nd->subnodes);
    int i = 0;

    struct node_s *sub = NULL;
    while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
        if (sub->tid == TK_COMMA) {i++; continue;}
        sunlark_display_node(sub, buffer, level);
        if (len - i > 1) {
            utstring_printf(buffer, " ");
        }
        i++;
    }
}

/* **************************************************************** */
/*
   3: TK_Load_Stmt 117
         4: TK_LOAD 53
         4: TK_LPAREN 54
         4: TK_STRING 79: @rules_cc//cc:defs.bzl
         4: TK_COMMA 15
         4: TK_STRING 79: cc_binary
         4: TK_COMMA 15
         4: TK_STRING 79: cc_library
         4: TK_COMMA 15
         4: TK_STRING 79: cc_test
         4: TK_RPAREN 71
*/
LOCAL void _display_load_args(struct node_s *nd,
                              UT_string *buffer,
                              int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_load_args");
#endif

    struct node_s *sub = NULL;

    /* we have to iterate twice, once to determine if there are any
       singleton args (so we can decide to print ':args ( )', then to
       print them */

    int i = 0;
    int ct = 0;
    while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
        if (i == 2) { i++; continue; }
        if (sub->tid == TK_STRING) ct++;
    }

    int len = utarray_len(nd->subnodes);
    i = 0;
    if (ct > 0) {
        utstring_printf(buffer, "%*s:args (", level*indent+8, " ");
        sub = NULL;
        while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
            /* utstring_printf(buffer, "%d %s: len %d, i %d: len-i: %d\n", */
            /*                 sub->tid, TIDNAME(sub), */
            /*                 len, i, len-i); */
            if (i == 2) { goto loop; }
            if (sub->tid == TK_STRING) {
                sunlark_display_node(sub, buffer, 0); // level+2);
                ct--;
                if ( ct > 1 )
                    utstring_printf(buffer, " ");
                /* else */
                /*     utstring_printf(buffer, "AAAA"); */
            }
        loop:
            i++;
        }
        utstring_printf(buffer, ")");
    }
}

/* **************************************************************** */
LOCAL void _display_load_bindings(struct node_s *nd,
                              UT_string *buffer,
                              int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_load_bindings");
#endif

    struct node_s *sub = NULL;

    /* we have to iterate twice, once to determine if there are any
       singleton args (so we can decide to print ':args ( )', then to
       print them */

    int ct = 0;
    while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
        if (sub->tid == TK_Binding) ct++;
    }

    /* int len = utarray_len(nd->subnodes); */
    if (ct > 0) {
        utstring_printf(buffer, "%*s:bindings (", level*indent+8, " ");
        sub = NULL;
        while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
            if (sub->tid == TK_Binding) {
                sunlark_display_node(sub, buffer, 0); // level+2);
                ct--;
                if ( ct > 0 )
                    utstring_printf(buffer, " ");
            }
        }
        utstring_printf(buffer, ")");
    }
}

/* **************************************************************** */
LOCAL void _display_load_stmt(struct node_s *nd,
                              UT_string *buffer,
                              int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_load_stmt");
#endif

    assert(nd->tid == TK_Load_Stmt);

    char *src = sealark_loadstmt_src_string(nd);

    utstring_printf(buffer, "%*s(def-load :src \"%s\"\n",
                    level*indent, " ", src);

    _display_load_args(nd, buffer, level);
    /* fixme: only add newline if bindings and args exist */
    utstring_printf(buffer, "\n");

    _display_load_bindings(nd, buffer, level);

    utstring_printf(buffer, ")");
}

/* **************************************************************** */
LOCAL void _display_small_stmt_list(struct node_s *nd,
                                    UT_string *buffer,
                                    int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_small_stmt_list");
#endif

    /* utstring_printf(buffer, "#^(:small-smt-list"); */
    /* utstring_printf(buffer, "%*s#^(:small-stmt-list\n", */
    /*                 level*indent, " "); */
                    /* (level==0)? 0 : level*indent, " "); */
    /* struct node_s *subnodes = utarray_eltptr(nd->subnodes, 1); */

    int len = utarray_len(nd->subnodes);
    int i = 0;
    struct node_s *sub = NULL;
    /* utstring_printf(buffer, "small item count %d\n", len); */
    while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
        /* utstring_printf(buffer, "small item %d\n", i); */
        sunlark_display_node(sub, buffer, level);
        utstring_printf(buffer, "\n");
        if (len - i > 1) {      /* add blank line between items */
            utstring_printf(buffer, "\n");
        }
        i++;
    }
    /* utstring_printf(buffer, "Z)"); */
}

/* **************************************************************** */
LOCAL void _display_stmt_list(struct node_s *nd,
                              UT_string *buffer,
                              int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_stmt_list");
#endif

    /* utstring_printf(buffer, "%*s#^(:stmt_list\n", level*indent, " "); */
    /*                 /\* (level==0)? 0 : level*indent, " "); *\/ */

    struct node_s *sub = NULL;
    while( (sub=(struct node_s*)utarray_next(nd->subnodes, sub)) ) {
        sunlark_display_node(sub, buffer, level);
    }
    /* utstring_printf(buffer, ")"); */
}

/* **************************************************************** */
LOCAL void _display_vector_item(struct node_s *nd,
                                UT_string *buffer,
                                int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("_display_vector_item");
#endif

    switch(nd->tid) {
    case TK_INT:
        utstring_printf(buffer, "%s", nd->s);
        break;
    case TK_STRING: {
        char *br = SEALARK_STRTYPE(nd->qtype);
        char *q = sealark_quote_type(nd);
        utstring_printf(buffer, "%s%s%s%s", br, q, nd->s, q);
    }
        break;
    case TK_ID:
        if (strncmp(nd->s, "True", 4) == 0)
            utstring_printf(buffer, " #t");
        else
            if (strncmp(nd->s, "False", 5) == 0)
                utstring_printf(buffer, " #f");
            else
                utstring_printf(buffer, "'%s", nd->s);
        break;
    /* case TK_List_Expr: */
    /*     _display_vector(nd, buffer, level); */
    /*     break; */
    default:
        log_error("Support list item for nodes of type %d %s not yet implemented...", nd->tid, TIDNAME(nd));
        utstring_printf(buffer, "Support list item for nodes of type %d %s not yet implemented...", nd->tid, TIDNAME(nd));
    }
}

/* **************************************************************** */
LOCAL void _display_vector(struct node_s *nd,
                           UT_string *buffer,
                           int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("sunlark_display_vector");
#endif

    assert(nd->tid == TK_List_Expr);

    struct node_s *expr_list = utarray_eltptr(nd->subnodes, 1);
    int len = utarray_len(expr_list->subnodes);
    bool split = (len/2 > 4)? true : false;

    utstring_printf(buffer, "[%s", split? "\n" : "");

    struct node_s *sub = NULL;
    int i = 0;
    while( (sub=(struct node_s*)utarray_next(expr_list->subnodes, sub)) ) {
        if (sub->tid == TK_COMMA) {
            utstring_printf(buffer, "%s%*s",
                            split? "\n" : " ",
                            split? (level+3)*2 : 0,
                            split? " " : "");
        } else {
            if (i==0)
                utstring_printf(buffer, "%*s",
                                split? (level+3)*2 : 0,
                                split? " " : "");
            _display_vector_item(sub, buffer, level);
        }
        i++;
    }
    utstring_printf(buffer, "]");
}

/* **************************************************************** */
// sunlark-aware version of sealark_display_node
void sunlark_display_node(// s7_scheme *s7,
                          struct node_s *nd,
                          UT_string *buffer,
                          int level)
{
#ifdef DEBUG_SERIALIZERS
    log_debug("sunlark_display_node: %d %s", nd->tid, TIDNAME(nd));
#endif

    int len;

    /* if (level == 0) utstring_printf(buffer, "\n"); */

    switch(nd->tid) {
    case TK_Assign_Stmt:
        _display_assign_stmt(nd, buffer, level);
        break;
    case TK_Package:
        _display_package(nd, buffer, level);
        break;
    case TK_Arg_List:
        _display_arg_list(nd, buffer, level);
        break;
    case TK_Binding:
        _display_binding_node(nd, buffer, level);
        break;
    case TK_Call_Expr:
        _display_call_expr(nd, buffer, level);
        break;
    case TK_Call_Sfx:
        _display_call_sfx(nd, buffer, level);
        break;
    case TK_Dict_Entry:
        _display_dict_entry(nd, buffer, level);
        break;
    case TK_Dict_Entry_List:
        _display_dict_entry_list(nd, buffer, level);
        break;
    case TK_Dict_Expr:
        _display_dict_expr(nd, buffer, level);
        break;
    case TK_Expr_List:
        _display_expr_list(nd, buffer, level);
        break;
    case TK_ID: {
        if (strncmp(nd->s, "True", 4) == 0)
            utstring_printf(buffer, "#t");
        else
            if (strncmp(nd->s, "False", 5) == 0)
                utstring_printf(buffer, "#f");
            else
                utstring_printf(buffer, "%s", nd->s);
    }
        break;
    case TK_INT:
        utstring_printf(buffer, "%s", nd->s);
        break;
    case TK_List_Expr:
        _display_vector(nd, buffer, level);
        break;
    case TK_LOAD:
        utstring_printf(buffer, "%*s#^(:load", level*indent, " ");
        break;
    case TK_Load_Stmt:
        _display_load_stmt(nd, buffer, level);
        break;
    case TK_Small_Stmt_List:
        _display_small_stmt_list(nd, buffer, level);
        break;
    case TK_Stmt_List:
        _display_stmt_list(nd, buffer, level);
        break;
    case TK_STRING: {
        char *br = SEALARK_STRTYPE(nd->qtype);
        char *q = sealark_quote_type(nd);
        utstring_printf(buffer, "%s%s%s%s",
                        br, q, nd->s, q);
        /* utstring_printf(buffer, " s=\"%s\"", nd->s); */
    }
        break;
    case TK_COMMA:
    case TK_COLON:
    case TK_LBRACE:
    case TK_RBRACE:
    case TK_LBRACK:
    case TK_RBRACK:
    case TK_LPAREN:
    case TK_RPAREN:
        break;
    default:
        utstring_printf(buffer, "\nUNCAUGHT:%s\n", sealark_tid_to_string(nd->tid));

        if (nd->comments) {
            _display_comments(nd, buffer, level);
        }

        if (nd->subnodes) {
            /* utstring_printf(buffer, "\n%*.s(", */
            /*                 (level==0)? 2 : (level+2)*indent+level+1, "."); */

            //FIXME sunlark_nodelist_display(s7, (UT_array*)nd->subnodes);
            struct node_s *subn = NULL;
            int lvl = ++level;
            while((subn=(struct node_s*)utarray_next(nd->subnodes,
                                                     subn))) {
                sunlark_display_node(subn, buffer, lvl);
            }

            /* utstring_printf(buffer, "%*.s)", (level+1)*indent+1, " "); */
        }
    }
    /* utstring_printf(buffer, ")\n"); */
}
