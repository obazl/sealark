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

//FIXME: handle large files. use dynamic alloc
void sunlark_node_display(s7_scheme *s7, void *value, UT_string *buffer)
{
#ifdef DEBUG_TRACE
    /* log_debug("sunlark_node_display"); */
#endif

    struct node_s *nd = (struct node_s *)value;

    // check display_buf size, expand if needed

    /* char buf[128]; */
    /* UT_string *buf; */
    /* utstring_new(buf); */
    int len;

    utstring_printf(buffer, "#ast_node<");
    /* sprintf(buf, "#ast_node<\n"); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    utstring_printf(buffer, "tid=%d", nd->tid);
    /* sprintf(buf, " tid  = %d,\n", nd->tid); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    utstring_printf(buffer, " tnm=%s", token_name[nd->tid][0]);
    /* sprintf(buf, " tnm  = %s,\n", token_name[nd->tid][0]); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    utstring_printf(buffer, " line=%d", nd->line);
    /* sprintf(buf, " line  = %d,\n", nd->line); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    utstring_printf(buffer, " col=%d", nd->col);
    /* sprintf(buf, " col   = %d,\n", nd->col); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    utstring_printf(buffer, " trailing_newline=%d", nd->trailing_newline);
    /* sprintf(buf, " trailing_newline = %d,\n", nd->trailing_newline); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr += len; */

    if (nd->tid == TK_STRING) {
        utstring_printf(buffer, " qtype=#x%#X", nd->qtype);
        /* sprintf(buf, " qtype = #x%#X,\n", nd->qtype); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */
    }

    if (nd->s) {
        char *br = SEALARK_STRTYPE(nd->qtype);
        char *q = sealark_quote_type(nd);

        utstring_printf(buffer, " s=%s%s%s%s",
                        br, q, nd->s, q);

        /* sprintf(buf, " s     = %s,\n", nd->s); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */
    }

    if (nd->comments) {
        utstring_printf(buffer, " comments= ");
        /* sprintf(buf, " comments = "); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */

        /* updates global display_buf */
        //FIXME sunlark_nodelist_display(s7, (UT_array*)nd->comments);

        utstring_printf(buffer, ",");
        /* sprintf(buf, ",\n"); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */
    }

    if (nd->subnodes) {
        utstring_printf(buffer, "\n  subnodes=[");
        /* sprintf(buf, " subnodes =\n\t"); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */

        //FIXME sunlark_nodelist_display(s7, (UT_array*)nd->subnodes);
        struct node_s *subn = NULL;
        while((subn=(struct node_s*)utarray_next(nd->subnodes,
                                                 subn))) {
            sunlark_node_display(s7, subn, buffer);
        }

        utstring_printf(buffer, "]");
        /* sprintf(buf, ",\n"); */
        /* len = strlen(buf); */
        /* snprintf(display_ptr, len+1, "%s", buf); */
        /* display_ptr += len; */
    }

    utstring_printf(buffer, ">\n");
    /* sprintf(display_ptr - 2, ">,\n"); */
    /* len = strlen(buf); */
    /* snprintf(display_ptr, len+1, "%s", buf); */
    /* display_ptr++; // -= 1; */

    /* return display_buf; */
}

/** sunlark_node_display_readably

    produces a "roundtrippable" string, one that when read by the reader
    results in an object equal to the original.
 */
char *sunlark_node_display_readably(s7_scheme *s7, void *value)
{
/* #ifdef DEBUG_TRACE */
/*     log_debug("sunlark_node_display_readably"); */
/* #endif */

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

#if INTERFACE
#define SUNLARK_TO_STARLARK_HELP "(ast-node->starlark ast_node)"
#endif

/* args is always an s7 list.

   arg1: object to print
   arg2: optional style kw, :squeeze, :crush

 */
s7_pointer sunlark_to_starlark(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_to_starlark");
#endif

    UT_string *buf;
    utstring_new(buf);

    s7_pointer form = s7_car(args); // what to print
    s7_pointer style = s7_cadr(args);
            /* if (s7_list_length(s7, args) == 2) { */
            /*     style = s7_cadr(args); */
            /*     if (style != kw_crush) { */
            /*         if (style != kw_squeeze) { */
            /*             log_error("SQUEEZE/CRUSH missing"); */
            /*         } */
            /*     } */
            /* } */

    if ( s7_is_c_object(form) ) {
        log_debug("printing c-object");
        struct node_s *n1 = s7_c_object_value(form);
        sealark_node_to_starlark(n1, buf);
    } else {
        if ( s7_is_list(s7, form) ) {
            log_debug("printing s7 list");
            s7_pointer _list = form;
            while (! s7_is_null(s7, _list)) {
                struct node_s *t = s7_c_object_value(s7_car(_list));
                log_debug("\titem tid: %d %s", t->tid, TIDNAME(t));
                sealark_node_to_starlark(t, buf);
                _list = s7_cdr(_list);
            }
        } else {
            if (s7_is_string(form)) {
                utstring_printf(buf, "%s", s7_string(form));
            } else {
                if (s7_is_character(form)) {
                    utstring_printf(buf, "%c", s7_character(form));
                } else {
                    if (s7_is_number(form)) {
                        utstring_printf(buf, "%s", s7_number_to_string(s7,form,10));
                    } else {
                        if (s7_is_keyword(form)) {
                            s7_pointer sym = s7_keyword_to_symbol(s7,form);
                            utstring_printf(buf, "%s",
                                            s7_symbol_name(form));
                        } else {
                            log_error("Unexpected form for printing, should be c-object or list.");
                        }
                    }
                }
            }
        }
    }

    char *output;
    if (style == kw_squeeze) {
        output = sealark_squeeze_string(buf);
    } else {
        if (style == kw_crush) {
            output = sealark_crush_string(buf);
        } else {
            output = utstring_body(buf);
        }
    }

    s7_pointer out = s7_make_string(s7, output);
    free(output);
    return out;


/*     s7_pointer arg = s7_car(args); */
/*     s7_pointer format; */

/*     if (s7_list_length(s7, args) == 2) { */
/*         format = s7_cadr(args); */
/*         if (format != kw_squeeze) */
/*             if (format != kw_crush) */
/*                 return s7_error(s7, s7_make_symbol(s7, */
/*                                                    "invalid_argument"), */
/*                                 s7_list(s7, 2, s7_make_string(s7, */
/* "optional arg to sunlark->starlark must be :squeeze or :crush; got ~A"), */
/*                                         format)); */
/*     } */

    /* UT_string *buf; */
    /* utstring_new(buf); */

    /* if (c_is_sunlark_node(s7, arg)) { */
    /*     log_debug("single node %d %s", */
    /*               sunlark_node_tid(s7, arg), */
    /*               token_name[sunlark_node_tid(s7, arg)][0]); */

    /*     /\*this should not happen for sunlark nodes: *\/ */
    /*     if (sunlark_node_tid(s7, arg) == TK_Node_List) { */
    /*         UT_array *nodelist = s7_c_object_value(arg); */
    /*         log_debug("node ct %d", utarray_len(nodelist)); */

    /*         struct node_s *n1=NULL; */
    /*         while(n1=(struct node_s*)utarray_next(nodelist, n1)) { */
    /*             sealark_node_to_starlark(n1, buf); */
    /*         } */

    /*     } else { */
    /*         /\* a single target node *\/ */
    /*         if (sunlark_node_tid(s7, arg) == TK_Call_Expr) { */
    /*             struct node_s *n1 = s7_c_object_value(arg); */
    /*             sealark_node_to_starlark(n1, buf); */
    /*         } else { */
    /*             if (sunlark_node_tid(s7, arg) == TK_Build_File) { */
    /*                 struct node_s *n1 = s7_c_object_value(arg); */
    /*                 sealark_node_to_starlark(n1, buf); */
    /*             } else { */
    /*                 log_warn("Unexpected arg type: %d, %s", */
    /*                      sunlark_node_tid(s7, arg), */
    /*                      token_name[sunlark_node_tid(s7, arg)][0]); */
    /*             } */
    /*         } */
    /*     } */
    /* } else { */
    /*     if (sunlark_is_nodelist(s7, args)) { */
    /*         log_debug("nodelist"); */
    /*         UT_array *nodelist = s7_c_object_value(arg); */
    /*         log_debug("node ct %d", utarray_len(nodelist)); */

    /*         struct node_s *n1=NULL; */
    /*         while(n1=(struct node_s*)utarray_next(nodelist, n1)) { */
    /*             sealark_node_to_starlark(n1, buf); */
    /*         } */

    /*     } else { */
    /*         log_error("unexpected args, neither node nor nodelist"); */
    /*     } */
    /* } */
    /* char *output; */
    /* if (format == kw_squeeze) { */
    /*     output = sealark_squeeze_string(buf); */
    /* } else { */
    /*     if (format == kw_crush) { */
    /*     output = sealark_crush_string(buf); */
    /*     } else { */
    /*         output = utstring_body(buf); */
    /*     } */
    /* } */

    /* s7_pointer out = s7_make_string(s7, output); */
    /* free(output); */
    /* return out; */

    /* s7_pointer out = s7_make_string(s7, utstring_body(output)); */
    /* utstring_free(buf); */
    /* utstring_free(output); */
    /* return out; */
}

s7_pointer sunlark_node_to_string(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("sunlark_node_to_string");
    debug_print_s7(s7, "to_string cdr: ", s7_cdr(args));
#endif

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
        sunlark_node_display(s7, s7_c_object_value(obj), buffer);
    }

    /* log_debug("TO_STRING LEN: %d", strlen(descr)); */
    obj = s7_make_string(s7, utstring_body(buffer));
    /* obj = s7_make_string(s7, descr); */

    /* free(descr); // frees display_buf? */
    /* display_bufsz = 0; */
    utstring_free(buffer);
    return(obj);
}

/* **************************************************************** */
/* s7_pointer sunlark_nodelist_to_string(s7_scheme *s7, s7_pointer args) */
/* { */
/* #ifdef DEBUG_TRACE */
/*     log_debug("sunlark_nodelist_to_string"); */
/*     /\* debug_print_s7(s7, "to_string cdr: ", s7_cdr(args)); *\/ */
/* #endif */
/*     if (display_bufsz == 0) { */
/*         display_buf = calloc(1, SZDISPLAY_BUF); */
/*         if (display_buf == NULL) { */
/*             log_error("ERROR on calloc"); */
/*             //FIXME cleanup */
/*             exit(EXIT_FAILURE); */
/*         } else { */
/*             display_bufsz = SZDISPLAY_BUF; */
/*         } */
/*     } */

/*     display_ptr = display_buf; */

/*     s7_pointer obj, choice; */
/*     char *descr; */
/*     obj = s7_car(args); */

/*     if (s7_is_pair(s7_cdr(args))) */
/*         choice = s7_cadr(args); */
/*     else choice = s7_t(s7); */

/*     if (choice == s7_make_keyword(s7, "readable")) */
/*         descr = sunlark_nodelist_display_readably(s7, s7_c_object_value(obj)); */
/*     else descr = sunlark_nodelist_display(s7, s7_c_object_value(obj)); */

/*     /\* printf("sunlark_nodelist_display => %s", descr); *\/ */
/*     obj = s7_make_string(s7, descr); */

/*     /\* free(descr); //BUG? FIXME free substruct strings *\/ */
/*     return(obj); */
/* } */
/* /section: serialization */

