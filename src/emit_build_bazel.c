#if EXPORT_INTERFACE
#include <stdio.h>
#endif

#include "log.h"
#include "utarray.h"

#include "emit_build_bazel.h"

/* **************************************************************** */
static int level = 0;
static int spfactor = 4;
static char *sp = " ";

static int indent = 2;
static int delta = 2;


void emit_bazel_hdr(FILE* ostream, int level, char *repo, obazl_meta_package *_pkg)
{
    char *pkg_name = obazl_meta_package_name(_pkg);
    char *pname = "requires";
    obazl_meta_entries *entries = obazl_meta_package_entries(_pkg);

    struct obazl_meta_property *deps_prop = obazl_meta_package_property(_pkg, pname);
    if ( deps_prop == NULL ) {
        log_error("Prop '%s' not found.", pname);
        return;
    /* } else { */
    /*     /\* log_debug("Got prop: %s", obazl_meta_property_name(deps_prop)); *\/ */
    }

    obazl_meta_settings *settings = obazl_meta_property_settings(deps_prop);
    obazl_meta_setting *setting = NULL;

    obazl_meta_values *vals;
    obazl_meta_value *v = NULL;

    fprintf(ostream, "%*sdeps = [\n", level*spfactor, sp);

    for (int i = 0; i < obazl_meta_settings_count(settings); i++) {
        setting = obazl_meta_settings_nth(settings, i);
        /* log_debug("setting %d", i+1); */

        vals = obazl_meta_setting_values(setting);
        /* log_debug("vals ct: %d", obazl_meta_values_count(vals)); */
        /* dump_values(0, vals); */

        for (int i = 0; i < obazl_meta_values_count(vals); i++) {
            v = obazl_meta_values_nth(vals, i);
            /* log_debug("val %d: %s", i, *v); */
            fprintf(ostream, "%*s\"%s//%s:%s\"\n", (1+level)*spfactor, sp, repo, pkg_name, *v);
        }
    }
    fprintf(ostream, "%*s],\n", level*spfactor, sp);
}

// deps: construct bazel pkg path, in @opam
// the pkgs in the deps list are opam/ocamlfind pkg names;
// as paths, they are relative to the opam lib dir.
// dotted dep strings must be converted to filesys paths

// what about non-opam META files? we always need a repo name ('@foo') and a pkg path.

void emit_bazel_deps(FILE* ostream, int level, char *repo, char *pkg, obazl_meta_package *_pkg)
{
    char *pname = "requires";
    obazl_meta_entries *entries = obazl_meta_package_entries(_pkg);

    struct obazl_meta_property *deps_prop = obazl_meta_package_property(_pkg, pname);
    if ( deps_prop == NULL ) {
        log_error("Prop '%s' not found.", pname);
        return;
    }

    obazl_meta_settings *settings = obazl_meta_property_settings(deps_prop);
    obazl_meta_setting *setting = NULL;

    if (obazl_meta_settings_count(settings) == 0) {
        log_info("No deps for %s", obazl_meta_property_name(_pkg));
        return;
    }

    obazl_meta_values *vals;
    obazl_meta_value *v = NULL;

    int settings_ct = obazl_meta_settings_count(settings);
    /* log_info("settings count: %d", settings_ct); */

    if (settings_ct > 1) {
        fprintf(ostream, "%*sdeps = select({\n", level*spfactor, sp);
    } else {
        fprintf(ostream, "%*sdeps = [\n", level*spfactor, sp);
    }

    for (int i = 0; i < settings_ct; i++) {
        setting = obazl_meta_settings_nth(settings, i);
        /* log_debug("setting %d", i+1); */

        obazl_meta_flags *flags = obazl_meta_setting_flags(setting);

        char *condition;
        if (flags == NULL)
            condition =  "//conditions:default";
        else
            condition =  "condition";

        char *condition_comment = obazl_meta_flags_to_string(flags);
        /* log_debug("condition_comment: %s", condition_comment); */

        /* FIXME: multiple settings means multiple flags; decide how to handle for deps */
        // construct a select expression on the flags
        if (settings_ct > 1) {
            fprintf(ostream, "%*s\"%s\": [ ## %s\n",
                    (1+level)*spfactor, sp,
                    condition, /* FIXME: name the condition */
                    condition_comment);
        }
        vals = obazl_meta_setting_values(setting);
        /* log_debug("vals ct: %d", obazl_meta_values_count(vals)); */
        /* dump_values(0, vals); */

        for (int j = 0; j < obazl_meta_values_count(vals); j++) {
            v = obazl_meta_values_nth(vals, j);
            /* log_info("property val: '%s'", *v); */

            char *s = (char*)*v;
            while (*s) {
                /* printf("s: %s\n", s); */
                if(s[0] == '.') {
                    /* log_info("Hit"); */
                    s[0] = '/';
                }
                s++;
            }
            if (settings_ct > 1) {
                fprintf(ostream, "%*s\"%s//%s/%s\",\n",
                        (2+level)*spfactor, sp,
                        repo, pkg, *v);
            } else {
                fprintf(ostream, "%*s\"%s//%s/%s\"\n", (1+level)*spfactor, sp, repo, pkg, *v);
            }
        }
        if (settings_ct > 1) {
            fprintf(ostream, "%*s],\n", (1+level)*spfactor, sp);
        }
        free(condition_comment);
    }
    if (settings_ct > 1)
        fprintf(ostream, "%*s}),\n", level*spfactor, sp);
    else
        fprintf(ostream, "%*s],\n", level*spfactor, sp);
}

EXPORT void emit_build_bazel(struct obazl_meta_package *_pkg, char *_repo, char *_pkg_prefix)
{
#ifdef DEBUG_TRACE
    log_info("emit_build_bazel");
    log_info("\t@%s//%s/%s", _repo, _pkg_prefix, obazl_meta_package_name(_pkg));
    /* log_set_quiet(false); */
    log_debug("%*sparsed name: %s", indent, sp, obazl_meta_package_name(_pkg));
    log_debug("%*sparsed dir:  %s", indent, sp, obazl_meta_package_dir(_pkg));
    log_debug("%*sparsed src:  %s", indent, sp, obazl_meta_package_src(_pkg));
#endif

    emit_bazel_deps(stdout, 1, "@opam", "lib", _pkg);
}
