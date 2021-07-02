void emit_rule_ocaml_module(FILE *fp, struct fileset_s *the_fileset)
{
    fputs("ocaml_import(\n", fp);
    fprintf(fp, "    name = \"%s\",\n", the_fileset->name);
    fputs("    srcs = [\n", fp);
    static char **p;
    p = NULL;
    while ( (p=(char**)utarray_next(the_fileset->files, p))) {
        fprintf(fp, "        \"%s\",\n", *p);
    }
    fputs("    ]\n", fp);
    fputs(")\n\n", fp);
}

void emit_rule_ocaml_archive(FILE *fp, struct fileset_s *the_fileset)
{
    fputs("ocaml_import(\n", fp);
    fprintf(fp, "    name = \"%s\",\n", the_fileset->name);
    fputs("    archive = [\n", fp);
    static char **p;
    p = NULL;
    while ( (p=(char**)utarray_next(the_fileset->files, p))) {
        fprintf(fp, "        \"%s\",\n", *p);
    }
    fputs("    ]\n", fp);
    fputs(")\n\n", fp);
}

void emit_rule_coq_module(FILE *fp, struct fileset_s *the_fileset)
{
    fputs("coq_import(\n", fp);
    /* depname[0] = '\0'; */
    /* mystrcat(depname, the_fileset->name); */
    /* depname[0] = depname[0]; // - 32; /\* uppercase first char *\/ */
    /* depname[0] = toupper(depname[0]); */
    fprintf(fp, "    name = \"%s\",\n", the_fileset->name);
    fputs("    srcs = [\n", fp);
    static char **p;
    p = NULL;
    while ( (p=(char**)utarray_next(the_fileset->files, p))) {
        fprintf(fp, "        \"%s\",\n", *p);
    }
    fputs("    ]\n", fp);
    fputs(")\n\n", fp);
}

void emit_rule_c_headers(FILE *fp, struct fileset_s *the_fileset)
{
    fputs("cc_library(\n", fp);
    /* depname[0] = '\0'; */
    /* mystrcat(depname, the_fileset->name); */
    /* depname[0] = depname[0]; // - 32; /\* uppercase first char *\/ */
    /* depname[0] = toupper(depname[0]); */
    fprintf(fp, "    name = \"headers\",\n");
    fputs("    hdrs = [\n", fp);
    static char **p;
    p = NULL;
    while ( (p=(char**)utarray_next(the_fileset->files, p))) {
        fprintf(fp, "        \"%s\",\n", *p);
    }
    fputs("    ]\n", fp);
    fputs(")\n\n", fp);
}

void emit_build_file(struct package_s *the_pkg)
{
    /* printf("emit_build_file %s\n", the_pkg->name); */
    FILE *fp;
    char fname[128] = {0};
    mystrcat(fname, tgtroot_lib);
    mystrcat(fname, "/");
    mystrcat(fname, the_pkg->name);
    mystrcat(fname, "/BUILD.bazel");
    /* printf("Writing %s\n", fname); */

    fp = fopen(fname, "w");
    if (fp != NULL) {
        fputs("load(\n", fp);
        fputs("    \"@obazl_rules_ocaml//ocaml:rules.bzl\",\n", fp);
        fputs("    \"ocaml_import\",\n", fp);

        /* if ( CHECK_BIT(the_pkg->rule_types, OCAML_MODULE) */
        /*     || CHECK_BIT(the_pkg->rule_types, OCAML_ARCHIVE) ) { */
        /*     fputs("    \"ocaml_import\",\n", fp); */
        /* } */
        /* if (CHECK_BIT(the_pkg->rule_types, OCAML_ARCHIVE)) { */
        /*     fputs("    \"ocaml_import,\"\n", fp); */
        /* } */
        /* fputs("    \"ocaml_import,\"\n", fp); */
        fputs(")\n\n", fp);

        if (CHECK_BIT(the_pkg->rule_types, COQ_MODULE)) {
            fputs("load(\n", fp);
            fputs("    \"@obazl_rules_coq//coq:rules.bzl\",\n", fp);
            fputs("    \"coq_import\",\n", fp);
            fputs(")\n\n", fp);
        }

        if (CHECK_BIT(the_pkg->rule_types, C_HEADER)) {
            fputs("load(\n", fp);
            fputs("    \"@rules_cc//cc:defs.bzl\",\n", fp);
            fputs("    \"cc_library\",\n", fp);
            fputs(")\n\n", fp);
        }

        fputs("package(default_visibility = [\"//visibility:public\"])\n\n", fp);

        struct fileset_s *the_fileset, *tmp;
        HASH_ITER(hh, the_pkg->filesets, the_fileset, tmp) {

            if (CHECK_BIT(the_fileset->type, OCAML_MODULE)) {
                emit_rule_ocaml_module(fp, the_fileset);
                continue;
            } /* else { */
            if (CHECK_BIT(the_fileset->type, OCAML_ARCHIVE)) {
                emit_rule_ocaml_archive(fp, the_fileset);
                continue;
            } /* else { */
            if (CHECK_BIT(the_fileset->type, COQ_MODULE)) {
                emit_rule_coq_module(fp, the_fileset);
                continue;
            } /* else { */
            if (CHECK_BIT(the_fileset->type, C_HEADER)) {
                emit_rule_c_headers(fp, the_fileset);
                continue;
            } /* else { */

            /* fprintf(stderr, "WARNING: emit_build_file, pkg %s, fileset %s, unhandled type: %x\n", */
            /*         the_pkg->name, the_fileset->name, the_fileset->type); */

            /* fprintf(stderr, "CHECKBIT: %x\n", CHECK_BIT(the_fileset->type, OCAML_ARCHIVE)); */
            /* fprintf(stderr, "OCAML_ARCHIVE: %x\n", OCAML_ARCHIVE); */

        }
        fclose(fp);
    } else {
        errnum = errno;
        fprintf(stderr, "emit_build_file fopen fail for %s\n", fname);
        fprintf(stderr, "Value of errno: %d\n", errno);
        fprintf(stderr, "Error opening file %s: %s\n", fname, strerror( errnum ));
        exit(1);
    }
}

void emit_ocaml_lib_build_file(struct package_s *the_pkg)
{
    /* printf("emit_ocaml_lib_build_file '%s'\n", the_pkg->name); */
    FILE *fp;
    static char fname[PATH_MAX];
    fname[0] = '\0';

    mystrcat(fname, "lib"); // tgtroot_lib);
    if ( strlen(the_pkg->name) > 0 ) {
        mystrcat(fname, "/");
        mystrcat(fname, the_pkg->name);
    }
    /* char *newdir = */
    mkdir_r("./", fname);

    if ( strlen(the_pkg->name) > 0 )
        mystrcat(fname, "/");
    mystrcat(fname, "BUILD.bazel");
    /* printf("Writing %s\n", fname); */

    fp = fopen(fname, "w");
    if (fp != NULL) {
        fputs("load(\n", fp);
        fputs("    \"@obazl_rules_ocaml//ocaml:rules.bzl\",\n", fp);
        fputs("    \"ocaml_import\",\n", fp);

        /* if ( CHECK_BIT(the_pkg->rule_types, OCAML_MODULE) */
        /*     || CHECK_BIT(the_pkg->rule_types, OCAML_ARCHIVE) ) { */
        /*     fputs("    \"ocaml_import\",\n", fp); */
        /* } */
        /* if (CHECK_BIT(the_pkg->rule_types, OCAML_ARCHIVE)) { */
        /*     fputs("    \"ocaml_import,\"\n", fp); */
        /* } */
        /* fputs("    \"ocaml_import,\"\n", fp); */
        fputs(")\n\n", fp);

        if (CHECK_BIT(the_pkg->rule_types, COQ_MODULE)) {
            fputs("load(\n", fp);
            fputs("    \"@obazl_rules_coq//coq:rules.bzl\",\n", fp);
            fputs("    \"coq_import\",\n", fp);
            fputs(")\n\n", fp);
        }

        if (CHECK_BIT(the_pkg->rule_types, C_HEADER)) {
            fputs("load(\n", fp);
            fputs("    \"@rules_cc//cc:defs.bzl\",\n", fp);
            fputs("    \"cc_library\",\n", fp);
            fputs(")\n\n", fp);
        }

        fputs("package(default_visibility = [\"//visibility:public\"])\n\n", fp);

        struct fileset_s *the_fileset, *tmp;
        HASH_ITER(hh, the_pkg->filesets, the_fileset, tmp) {

            if (CHECK_BIT(the_fileset->type, OCAML_MODULE)) {
                emit_rule_ocaml_module(fp, the_fileset);
                continue;
            } /* else { */
            if (CHECK_BIT(the_fileset->type, OCAML_ARCHIVE)) {
                emit_rule_ocaml_archive(fp, the_fileset);
                continue;
            } /* else { */
            if (CHECK_BIT(the_fileset->type, COQ_MODULE)) {
                emit_rule_coq_module(fp, the_fileset);
                continue;
            } /* else { */
            if (CHECK_BIT(the_fileset->type, C_HEADER)) {
                emit_rule_c_headers(fp, the_fileset);
                continue;
            } /* else { */
            /* fprintf(stderr, "WARNING: emit_build_file, pkg %s, fileset %s, unhandled type: %x\n", */
            /*         the_pkg->name, the_fileset->name, the_fileset->type); */
            ;
            /* fprintf(stderr, "CHECKBIT: %x\n", CHECK_BIT(the_fileset->type, OCAML_ARCHIVE)); */
            /* fprintf(stderr, "OCAML_ARCHIVE: %x\n", OCAML_ARCHIVE); */

        }
        fclose(fp);
    } else {
        errnum = errno;
        fprintf(stderr, "emit_ocaml_lib_build_file fopen fail\n");
        perror(fname);
        fprintf(stderr, "Value of errno: %d\n", errno);
        fprintf(stderr, "Error opening file %s: %s\n", fname, strerror( errnum ));
        exit(1);
    }
}

