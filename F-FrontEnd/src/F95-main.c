/**
 * \file F95-main.c
 */

/* Fortran lanuage front-end */

#include "F-front.h"
#include "F-front-context.h"
#include "F-output-xcodeml.h"
#include "F-second-pass.h"
#include <math.h>
#include "omni_errors.h"
#include "cli_options.h"

extern int yyparse(ffront_context* local_ctx);
static void check_nerrors();

static int getVarSize(const char *str)
{
    int ret = 0;
    char *ePtr = NULL;

    if (str == NULL || *str == '\0') {
        return 0;
    }
    ret = strtol(str, &ePtr, 10);
    if (ePtr != str) {
        return ret;
    } else {
        return 0;
    }
}

static void cmd_error_exit EXC_VARARGS_DEF(const char *, fmt)
{
    va_list args;

    fprintf(stderr, "error: ");
    EXC_VARARGS_START(const char *, fmt, args);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
    check_nerrors();
    FATAL_ERROR();
}

static void cmd_warning EXC_VARARGS_DEF(const char *, fmt)
{
    va_list args;

    fprintf(stderr, "warning: ");
    EXC_VARARGS_START(const char *, fmt, args);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
    check_nerrors();
}

static void usage(const cli_options* opts)
{
    const char *usages[] = {
        "", "OPTIONS", "",
        /* "-d", */
        /* "-yd", */
        "-o [outputfile]           specify output file path.",
        "-I [dirpath]              specify include directory path.",
        "-M [dirpath]              specify module include directory path.",
        "-fopenmp                  enable openmp translation.",
        "-facc                     enable OpenACC translation.",
        "-fxmp                     enable XcalableMP translation.",
    "-pgi                      keep PGI directives",
        "-fno-xmp-coarray          disable translation coarray statements to "
        "XcalableMP subroutin calls.",
        "-fintrinsic-xmodules-path specify a xmod path for the intrinsic "
        "modules.",
        "-Kscope-omp               enable conditional compilation.",
        "-force-fixed-format       read file as fixed format.",
        "-force-free-format        read file as free format.",
        "-max-line-length[=n]      set max columns in a line.",
        "                          (default n=72 in fixed format,",
        "                                   n=132 in free format.)",
        "-max-cont-line[=n]        set max number of continuation lines.",
        "                          (default n=255)",
        "-force-c-comment          enable 'c' comment in free format.",
        "-f77                      use F77 spec intrinsic.",
        "-f90                      use F90 spec intrinsic.",
        "-f95                      use F95 spec intrinsic.",
        "-u                        use no implicit type.",
        "-r[N]                     set double precision size (default N=8).",
        "--save[=n]                add save attribute than n kbytes except",
        "                          in a recursive function and common "
        "variables.",
        "                          (default n=1)",
        "-max_name_len=n           set maximum identifier name length.",
        "-fdollar-ok               enable using \'$\' in identifier.",
        "-fleave-comment           leave comment in xcodeml file.",
        "-endlineno                output the endlineno attribute.", "",
        "internal options:", "-d                        enable debug mode.",
        "-no-module-cache          always load module from file.",

        NULL};
    const char *const *p = usages;

    fprintf(stderr, "usage: %s <OPTIONS> <INPUT_FILE>\n", get_bin_name(opts));

    while (*p != NULL) {
        fprintf(stderr, "%s\n", *(p++));
    }
}

void parse_cli_args(cli_options* opts, int argc, char *argv[])
{
        set_bin_name(opts, argv[0]);
    --argc;
    ++argv;

    /* parse command line */
    while (argc > 0 && argv[0] != NULL) {
        if (argv[0][0] != '-' && argv[0][0] != '\0') {
            if (sdslen(get_src_file_path(opts)) != 0)
                cmd_error_exit("too many arguments");
            set_src_file_path(opts, argv[0]);
        } else if (strcmp(argv[0], "-d") == 0) {
            set_debug_enabled(opts, true);
        } else if (strcmp(argv[0], "-yd") == 0) {
#if YYDEBUG != 0
            set_yacc_debug_enabled(opts, true);
#endif
        } else if (strcmp(argv[0], "-o") == 0) {
            argc--;
            argv++;
            if ((argc == 0) || (argv[0] == NULL) || (*argv[0] == '\0')) {
                cmd_error_exit("output file name not specified.");
            }
            set_out_file_path(opts, argv[0]);
        } else if (strcmp(argv[0], "-fopenmp") == 0) {
            set_omp_enabled(opts, true); /* enable openmp */
        } else if (strcmp(argv[0], "-fxmp") == 0) {
            set_xmp_enabled(opts, true); /* enable XcalableMP */
        } else if (strcmp(argv[0], "-fno-xmp-coarray") == 0) {
            set_xmp_coarray_enabled(opts, false);/* disable XcalableMP coarray subroutine*/
        } else if (strcmp(argv[0], "-facc") == 0) {
            set_acc_enabled(opts, true);/* enable OpenACC */
        } else if (strcmp(argv[0], "-Kscope-omp") == 0) {
            set_cond_compile_enabled(opts, true);
        } else if (strcmp(argv[0], "-fleave-comment") == 0) {
            set_leave_comment_enabled(opts, true);
        } else if (strncmp(argv[0], "-max-line-length=", 17) == 0) {
            const int val = atoi(argv[0] + 17);
            if(val < 0)
            {
                cmd_error_exit("invalid value after -max-line-length=");
            }
            set_max_line_len(opts, val);
        } else if (strncmp(argv[0], "-max-cont-line=", 15) == 0) {
            set_max_cont_line(opts, atoi(argv[0] + 15));
        } else if (strcmp(argv[0], "-u") == 0) {
            set_do_implicit_undef(opts, true);
        } else if (strcmp(argv[0], "-C") == 0) {
            cmd_warning(
                "Array range check is not supported, just ignore this option.");
        } else if (strncmp(argv[0], "-r", 2) == 0) {
            int sz = getVarSize(argv[0] + 2);
            switch (sz) {
                case SIZEOF_FLOAT:
                    set_default_single_real_type(opts, TYPE_REAL);
                    break;
                case SIZEOF_DOUBLE:
                    set_default_single_real_type(opts, TYPE_DREAL);
                    break;
                default: {
                    cmd_error_exit(
                        "invalid single-real size %d, must be %d or %d.", sz,
                        SIZEOF_FLOAT, SIZEOF_DOUBLE);
                }
            }
        } else if (strncmp(argv[0], "-d", 2) == 0) {
            int sz = getVarSize(argv[0] + 2);
            switch (sz) {
                case SIZEOF_FLOAT:
                    set_default_double_real_type(opts, TYPE_REAL);
                    break;
                case SIZEOF_DOUBLE:
                    set_default_double_real_type(opts, TYPE_DREAL);
                    break;
                default: {
                    cmd_error_exit(
                        "invalid double-real size %d, must be %d or %d.", sz,
                        SIZEOF_FLOAT, SIZEOF_DOUBLE);
                }
            }
        } else if (strcmp(argv[0], "-force-fixed-format") == 0) {
            if (get_force_fixed_format_enabled(opts))
                cmd_warning("it seems to be set both of -force-fixed-format "
                            "and -force-free-format.");
            /* do not file name checking for fixed-format.  */
            set_force_fixed_format_enabled(opts, true);
        } else if (strcmp(argv[0], "-force-free-format") == 0) {
            if (get_force_fixed_format_enabled(opts))
                cmd_warning("it seems to be set both of -force-fixed-format "
                            "and -force-free-format.");
            set_force_fixed_format_enabled(opts, false);
        } else if (strcmp(argv[0], "-module-compile") == 0) {
            set_module_compile_enabled(opts, true);
        } else if (strncmp(argv[0], "-I", 2) == 0) {
            /* -I <anotherDir> or -I<anotherDir> */
            const char *path;
            if (strlen(argv[0]) == 2) {
                /* -I <anotherDir> */
                if (--argc <= 0)
                    cmd_error_exit("no arg for -I.");
                argv++;
                path = argv[0];
            } else {
                /* -I<anotherDir> */
                path = argv[0] + 2;
            }
            add_inc_dir_path(opts, path);
        } else if (strncmp(argv[0], "-M", 2) == 0) {
            /* -M <anotherDir> or -M<anotherDir> */
            const char *path;
            if (strlen(argv[0]) == 2) {
                /* -M <anotherDir> */
                if (--argc <= 0)
                    cmd_error_exit("no arg for -M.");
                argv++;
                path = argv[0];
            } else {
                /* -M<anotherDir> */
                path = argv[0] + 2;
            }
            add_xmod_inc_dir_path(opts, path);
        } else if (strcmp(argv[0], "-fintrinsic-xmodules-path") == 0) {
            const char *path;
            if (strlen(argv[0]) == 25) {
                /* -fintrinsic-xmodules-path <intrinsic xmodule dir> */
                if (--argc <= 0)
                    cmd_error_exit("no arg for -fintrinsic-xmodules-path.");
                argv++;
                path = argv[0];
            } else {
                /* -fintrinsic-xmodules-path<intrinsic xmodule dir> */
                path = argv[0] + 25;
            }
            set_intrinsic_xmod_dir_path(opts, path);
        } else if (strcmp(argv[0], "-f77") == 0) {
            set_lang_spec_set(opts, LANGSPEC_F77_SET);
        } else if (strcmp(argv[0], "-f90") == 0) {
            set_lang_spec_set(opts, LANGSPEC_F90_SET);
        } else if (strcmp(argv[0], "-f95") == 0) {
            set_lang_spec_set(opts, LANGSPEC_F95_SET);
        } else if (strcmp(argv[0], "-force-c-comment") == 0) {
            /* enable c comment in free format.  */
            set_force_c_comments_enabled(opts, true);
            if (get_force_fixed_format_enabled(opts)) {
                cmd_warning(
                    "no need option for enable c comment(-force-c-comment) in "
                    "fixed format mode(.f or .F).");
            }
        } else if (strcmp(argv[0], "--save") == 0) {
            set_auto_save_attr_kb(opts, 1);
        } else if (strncmp(argv[0], "--save=", 7) == 0) {
            set_auto_save_attr_kb(opts, atoi(argv[0] + 7));
            if (get_auto_save_attr_kb(opts) < 0)
                cmd_error_exit("invalid value after -save.");
        } else if (strncmp(argv[0], "-max-name-len=", 14) == 0) {
            const int val = atoi(argv[0] + 14);
            if(val < 0)
            {
                cmd_error_exit("invalid value after -max-name-len.");
            }
            set_max_name_len(opts, val);
        } else if (strcmp(argv[0], "-fdollar-ok") == 0) {
            set_dollar_in_id_enabled(opts, true);
        } else if (strcmp(argv[0], "-endlineno") == 0) {
            set_end_line_no_enabled(opts, true);
        } else if (strcmp(argv[0], "-ocl") == 0) {
            set_ocl_enabled(opts, true);
        } else if (strcmp(argv[0], "-cdir") == 0) {
            set_cdir_enabled(opts, true);
        } else if (strcmp(argv[0], "-pgi") == 0) {
            set_pgi_enabled(opts, true);
        } else if (strcmp(argv[0], "--help") == 0) {
            set_print_help(opts, true);
            break;
        } else if (strcmp(argv[0], "-print-opts") == 0) {
            set_print_opts(opts, true);
            break;
        } else if (strcmp(argv[0], "-no-module-cache") == 0) {
            set_module_cache_enabled(opts, false);
        } else {
            cmd_error_exit("unknown option : %s", argv[0]);
        }
        --argc;
        ++argv;
    }
}

void init_context_from_cli_opts(ffront_context* local_ctx, const cli_options* opts)
{
    ffront_params* params = (ffront_params*)local_ctx->params;
    init_ffront_params(params);
    init_ffront_context(local_ctx);
    set_sds_string(&local_ctx->src_file_path, get_src_file_path(opts));
    params->debug_enabled = get_debug_enabled(opts);
#if YYDEBUG != 0
    // This variable cannot be made non-global because it is generated by Bison/Yacc
    extern int yydebug;
    yydebug = get_yacc_debug_enabled(opts) ? 1 : 0;
#endif
    set_sds_string(&params->out_file_path, get_out_file_path(opts));
    if(sdslen(params->out_file_path) > 0)
    {
        if (!open_out_file_str_stream(&local_ctx->src_output, params->out_file_path))
            cmd_error_exit("cannot open file : %s", params->out_file_path);
    }
    else
    {
        set_str_stream(&local_ctx->src_output, STD_IO_STREAM, stdout);
    }
    params->omp_enabled = get_omp_enabled(opts);
    params->xmp_enabled = get_xmp_enabled(opts);
    params->xmp_coarray_enabled = get_xmp_coarray_enabled(opts);
    params->acc_enabled = get_acc_enabled(opts);
    params->cond_compile_enabled = get_cond_compile_enabled(opts);
    params->leave_comment_enabled = get_leave_comment_enabled(opts);
    params->max_cont_line = get_max_cont_line(opts);
    params->do_implicit_undef_enabled= get_do_implicit_undef(opts);
    params->default_single_real_type = get_default_single_real_type(opts);
    params->default_double_real_type = get_default_double_real_type(opts);
    params->module_compile_enabled = get_module_compile_enabled(opts);
    copy_sds_string_vector(&params->inc_dir_paths, get_inc_dir_paths(opts));
    copy_sds_string_vector(&params->xmod_inc_dir_paths, get_xmod_inc_dir_paths(opts));
    set_sds_string(&params->intrinsic_xmod_dir_path, get_intrinsic_xmod_dir_path(opts));
    params->lang_spec_set= get_lang_spec_set(opts);
    params->force_c_comments_enabled= get_force_c_comments_enabled(opts);
    params->auto_save_attr_kb = get_auto_save_attr_kb(opts);
    params->dollar_in_id_enabled = get_dollar_in_id_enabled(opts);/* enable using '$' in identifier */
    params->end_line_no_enabled = get_end_line_no_enabled(opts);
    params->ocl_enabled = get_ocl_enabled(opts);
    params->cdir_enabled = get_cdir_enabled(opts);
    params->pgi_enabled = get_pgi_enabled(opts);
    params->module_cache_enabled = get_module_cache_enabled(opts);
    const bool force_fixed_format = get_force_fixed_format_enabled(opts);

    local_ctx->fixed_format_enabled = force_fixed_format;
    if (sdslen(local_ctx->src_file_path) == 0) {
        set_str_stream(&local_ctx->src_input, STD_IO_STREAM, stdin);
    } else {
        if(!open_in_file_str_stream(&local_ctx->src_input, local_ctx->src_file_path))
            cmd_error_exit("cannot open file : %s", local_ctx->src_file_path);
        /* file name checking for fixed-format.  */
        if (!force_fixed_format) { /* unset?  */
            const char *dotPos = strrchr(local_ctx->src_file_path, '.');
            if (dotPos != NULL && (strcasecmp(dotPos, ".f") == 0 ||
                                   strcasecmp(dotPos, ".f77") == 0)) {
                local_ctx->fixed_format_enabled = true;
            }
        }
    }

    params->max_line_len = get_max_line_len(opts);
    if (params->max_line_len < 0) { /* unset */
        params->max_line_len = local_ctx->fixed_format_enabled ? DEFAULT_MAX_LINE_LEN_FIXED
                                         : DEFAULT_MAX_LINE_LEN_FREE;
    }

    const int input_max_id_name_len = get_max_name_len(opts);
    if(input_max_id_name_len != -1)
    {
        params->max_id_name_len = input_max_id_name_len;
        if (params->max_id_name_len < MAX_NAME_LEN) {
            params->max_id_name_len = MAX_NAME_LEN;
            cmd_warning("attempt to set too small value for max_name_len. use %d.", MAX_NAME_LEN);
        }
        if (params->max_id_name_len > MAX_NAME_LEN_UPPER_LIMIT) {
            params->max_id_name_len = MAX_NAME_LEN_UPPER_LIMIT;
            cmd_warning("attempt to set too large value for max_name_len. use %d.", MAX_NAME_LEN_UPPER_LIMIT);
        }
        if (get_debug_enabled(opts)) {
            cmd_warning("max_name_len = %d", params->max_id_name_len);
        }
    }
    else
    {
        params->max_id_name_len = MAX_NAME_LEN;
    }

    /* DEBUG */
    set_str_stream(&local_ctx->debug_output, STD_IO_STREAM, stderr);
    set_str_stream(&local_ctx->diag_output, STD_IO_STREAM, stderr);
}

int execute()
{
    initialize_lex();
    initialize_compile();

    /* start processing */
    /* FEAST add start */
    second_pass_init();
    /* FEAST add  end  */
    int parseError = yyparse(ctx);
    if (num_errors() != 0 || parseError != 0) {
        goto Done;
    }

    /* FEAST add start */
    /* second pass */
    parseError = second_pass();
    /* printf("parseError = %d, nerrors = %d\n", parseError, nerrors); */
    if (num_errors() != 0 || parseError != 0) {
        goto Done;
    }
    /* FEAST add  end  */

    /* end compile */
    if (unit_ctl_level != 0) {
        error("contains stack is not closed properly");
    }
    finalize_compile();
    if (num_errors() != 0) {
        goto Done;
    }

    final_fixup();
    if (num_errors() != 0) {
        goto Done;
    }

    /* output XcodeML/Fortran code */
    output_XcodeML_file();

Done:
    if (num_errors() != 0) {
        if (src_output()) {
            release_str_stream(&ctx->src_output);
            if(sdslen(out_file_path()) != 0 && file_exists(out_file_path()))
            {
                remove(out_file_path());
            }
        }
    }
    else
    {
        free_ffront_params((ffront_params*)ctx->params);
        free_ffront_context(ctx);
    }
    return num_errors() != 0 ? EXITCODE_ERR : EXITCODE_OK;
}

int main(int argc, char *argv[])
{
#ifdef HAVE_SETLOCALE
    (void)setlocale(LC_ALL, "C");
#endif /* HAVE_SETLOCALE */
    cli_options opts;
    init_cli_options(&opts);
    parse_cli_args(&opts, argc, argv);
    if(get_print_help(&opts))
    {
        usage(&opts);
        return EXITCODE_OK;
    }
    else if(get_print_opts(&opts))
    {
        print_options(&opts, stdout);
        return EXITCODE_OK;
    }
    ffront_params params;
    ffront_context local_ctx = {&params};
    init_context_from_cli_opts(&local_ctx, &opts);
    set_ffront_context(&local_ctx);
    const int ret_code = execute();
    free_cli_options(&opts);
    return ret_code;
}

bool search_include_path(const char *filename, sds_string* path)
{
    int i;
    int length;

    if (file_exists(filename)) {
        set_sds_string(path, filename);
        return true;
    }

    const size_t includeDirvI = vector_size(inc_dir_paths());
    char** const includeDirv = (char** const)inc_dir_paths();
    const size_t modincludeDirvI = vector_size(xmod_inc_dir_paths());
    char** const modincludeDirv = (char** const)xmod_inc_dir_paths();

    length = strlen(filename);
    if ((includeDirvI == 0 && modincludeDirvI == 0) ||
        (length >= 1 && strncmp("/", filename, 1) == 0) ||
        (length >= 2 && strncmp("./", filename, 2) == 0) ||
        (length >= 3 && strncmp("../", filename, 3) == 0)) {
        set_sds_string(path, filename);
        return true;
    }

    if (modincludeDirvI > 0) {
        // Iterate over available module search path
        for (i = 0; i < modincludeDirvI; i++) {
            set_sds_string(path, modincludeDirv[i]);
            *path = sdscat(*path, "/");
            *path = sdscat(*path, filename);
            if (file_exists(*path)) {
                return true;
            }
        }
    }

    for (i = 0; i < includeDirvI; i++) {
        set_sds_string(path, includeDirv[i]);
        *path = sdscat(*path, "/");
        *path = sdscat(*path, filename);
        if (file_exists(*path)) {
                return true;
        }
    }
    set_sds_string(path, "");
    return false;
}

void where(lineno_info *ln)
{
    /* print location of error  */
    if (ln != NULL) {
        if (current_module_name() == NULL)
            fprintf(stderr, "\"%s\", line %d: ", FILE_NAME(ln->file_id),
                    ln->ln_no);
        else
            fprintf(stderr, "\"%s:%s\", line %d: ", FILE_NAME(ln->file_id),
                    SYM_NAME(current_module_name()), ln->ln_no);
    } else {
        fprintf(stderr, "\"??\", line ??: ");
    }
}

/* nonfatal error message */
/* VARARGS0 */
void error EXC_VARARGS_DEF(const char *, fmt)
{
    va_list args;

    inc_num_errors();
    where(current_line());
    EXC_VARARGS_START(const char *, fmt, args);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
    check_nerrors();
}

/* VARARGS0 */
void error_at_node EXC_VARARGS_DEF(expr, x)
{
    va_list args;
    char *fmt;

    inc_num_errors();
    EXC_VARARGS_START(expr, x, args);
    where(EXPR_LINE(x));
    fmt = va_arg(args, char *);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
    check_nerrors();
}

/* VARARGS0 */
void error_at_id EXC_VARARGS_DEF(ID, x)
{
    va_list args;
    char *fmt;

    inc_num_errors();
    EXC_VARARGS_START(ID, x, args);
    where(ID_LINE(x));
    fmt = va_arg(args, char *);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
    check_nerrors();
}

/* VARARGS0 */
void warning_at_node EXC_VARARGS_DEF(expr, x)
{
    va_list args;
    char *fmt;

    where(EXPR_LINE(x)); /*, "WarnAtNode"); */
    fprintf(stderr, "warning: ");
    EXC_VARARGS_START(expr, x, args);
    fmt = va_arg(args, char *);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
}

void warning_at_id EXC_VARARGS_DEF(ID, x)
{
    va_list args;
    char *fmt;

    where(ID_LINE(x));
    fprintf(stderr, "warning: ");
    EXC_VARARGS_START(ID, x, args);
    fmt = va_arg(args, char *);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
}

/* debug message */
/* VARARGS0 */
void debug EXC_VARARGS_DEF(const char *, fmt)
{
    va_list args;

    if (!debug_enabled())
        return;

    inc_num_errors();
    where(current_line());
    EXC_VARARGS_START(const char *, fmt, args);
    vfprintf(debug_output(), fmt, args);
    va_end(args);
    fprintf(debug_output(), "\n");
    fflush(debug_output());
    check_nerrors();
}

static void check_nerrors()
{
    if (num_errors() > 30) {
        /* give the compiler the benefit of the doubt */
        fprintf(
            stderr,
            "too many error, cannot recover from earlier errors: goodbye!\n");
        FATAL_ERROR();
    }
}

/* compiler error: die */
/* VARARGS1 */
void fatal EXC_VARARGS_DEF(const char *, fmt)
{
    va_list args;

    where(current_line()); /*, "Fatal");*/
    fprintf(stderr, "compiler error: ");
    EXC_VARARGS_START(const char *, fmt, args);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
    FATAL_ERROR();
}

/* warning with lineno_info */
void warning_lineno(lineno_info *info, const char *fmt, ...)
{
    va_list args;
    where(info); /*, "Warn");*/
    EXC_VARARGS_START(const char *, fmt, args);
    fprintf(stderr, "warning: ");
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
}

/* warning */
void warning EXC_VARARGS_DEF(const char *, fmt)
{
    va_list args;
    where(current_line()); /*, "Warn");*/
    EXC_VARARGS_START(const char *, fmt, args);
    fprintf(stderr, "warning: ");
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
}
