/**
 * \file F95-main.c
 */

/* Fortran lanuage front-end */

#include "F-front.h"
#include "F-output-xcodeml.h"
#include "F-second-pass.h"
#include <math.h>
#include "omni_errors.h"
#include "cli_options.h"

/* for debug */
bool debug_flag = 0;
FILE *debug_fp;
FILE *diag_file;

/* default variable type */
BASIC_DATA_TYPE defaultIntType = TYPE_INT;
BASIC_DATA_TYPE defaultSingleRealType = TYPE_REAL;
BASIC_DATA_TYPE defaultDoubleRealType = TYPE_DREAL;

/* Treat implicit typed variable as undefined. */
bool doImplicitUndef = false;

/* the number of errors */
int nerrors = 0;

const char *original_source_file_name = NULL;
const char *source_file_name = NULL;
const char *output_file_name = NULL;
FILE *source_file, *output_file;

/* -save=? */
int auto_save_attr_kb = -1;

bool endlineno_flag = false;
bool ocl_flag = false;
bool cdir_flag = false;
bool pgi_flag = false;
int max_name_len = -1;
bool dollar_ok = false; // accept '$' in identifier or not.

extern int yyparse _ANSI_ARGS_((void));
static void check_nerrors _ANSI_ARGS_((void));

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

/* module compile(-MC=) arg.  */
int mcLn_no = -1;
long mcStart, mcEnd;

/* has termination element.  */
const char ** includeDirv = NULL;
size_t includeDirvI = 0;

/* user module search path */
const char ** modincludeDirv = NULL;
size_t modincludeDirvI = 0;

/* -MC?  */
bool flag_module_compile = false;

bool flag_do_module_cache = true;

const char *intrinsicXmodIncDir = NULL;

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
            if (source_file_name != NULL)
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

int execute(const cli_options* opts)
{
#ifdef HAVE_SETLOCALE
    (void)setlocale(LC_ALL, "C");
#endif /* HAVE_SETLOCALE */
    extern bool fixed_format_flag;
    extern int max_line_len;
    extern int max_cont_line;
    extern bool flag_force_c_comment;
#if YYDEBUG != 0
    extern int yydebug;
#endif
    int parseError = 0;
    bool flag_force_fixed_format = false;

    if(get_print_help(opts))
    {
        usage(opts);
        return EXITCODE_OK;
    }
    else if(get_print_opts(opts))
    {
    	print_options(opts, stdout);
        return EXITCODE_OK;
    }
    char message_str[128];
    source_file_name = NULL;
    output_file_name = NULL;
    source_file_name = get_src_file_path(opts);
    original_source_file_name = source_file_name;
    debug_flag = get_debug_enabled(opts);
    yydebug = get_yacc_debug_enabled(opts) ? 1 : 0;
    if(sdslen(get_out_file_path(opts)) > 0)
    {
        output_file_name = get_out_file_path(opts);
        if ((output_file = fopen(output_file_name, "w")) == NULL)
            cmd_error_exit("cannot open file : %s", output_file_name);
    }
    else
    {
        output_file = stdout;
    }
    OMP_flag = get_omp_enabled(opts);
    XMP_flag = get_xmp_enabled(opts);
	XMP_coarray_flag = get_xmp_coarray_enabled(opts);
    ACC_flag = get_acc_enabled(opts);
    cond_compile_enabled = get_cond_compile_enabled(opts);
    leave_comment_flag = get_leave_comment_enabled(opts);
    max_cont_line = get_max_cont_line(opts);
    doImplicitUndef = get_do_implicit_undef(opts);
    defaultSingleRealType = get_default_single_real_type(opts);
    defaultDoubleRealType = get_default_double_real_type(opts);
    flag_force_fixed_format = get_force_fixed_format_enabled(opts);
    flag_module_compile = get_module_compile_enabled(opts);
    includeDirv = (const char ** )get_inc_dir_paths(opts);
    includeDirvI = get_inc_dir_paths_size(opts);
    modincludeDirv = (const char ** )get_xmod_inc_dir_paths(opts);
    modincludeDirvI = get_xmod_inc_dir_paths_size(opts);
    intrinsicXmodIncDir = get_intrinsic_xmod_dir_path(opts);
    langSpecSet = get_lang_spec_set(opts);
    flag_force_c_comment = get_force_c_comments_enabled(opts);
    auto_save_attr_kb = get_auto_save_attr_kb(opts);
    dollar_ok = get_dollar_in_id_enabled(opts);/* enable using '$' in identifier */
    endlineno_flag = get_end_line_no_enabled(opts);
    ocl_flag = get_ocl_enabled(opts);
    cdir_flag = get_cdir_enabled(opts);
    pgi_flag = get_pgi_enabled(opts);
    flag_do_module_cache = get_module_cache_enabled(opts);

    if ((source_file = fopen(source_file_name, "r")) == NULL)
        cmd_error_exit("cannot open file : %s", source_file_name);

    if (source_file_name == NULL) {
        source_file = stdin;
        /* set this as option.  */
        if (flag_force_fixed_format) {
            fixed_format_flag = flag_force_fixed_format;
        }
    } else {
        /* file name checking for fixed-format.  */
        if (!flag_force_fixed_format) { /* unset?  */
            const char *dotPos = strrchr(source_file_name, '.');
            if (dotPos != NULL && (strcasecmp(dotPos, ".f") == 0 ||
                                   strcasecmp(dotPos, ".f77") == 0)) {
                fixed_format_flag = true;
            }
        } else {
            fixed_format_flag = flag_force_fixed_format;
        }
    }

    max_line_len = get_max_line_len(opts);
    if (max_line_len < 0) { /* unset */
        max_line_len = fixed_format_flag ? DEFAULT_MAX_LINE_LEN_FIXED
                                         : DEFAULT_MAX_LINE_LEN_FREE;
    }


    if(get_max_name_len(opts) != -1)
    {
        max_name_len = get_max_name_len(opts);
        if (max_name_len < MAX_NAME_LEN) {
            max_name_len = MAX_NAME_LEN;
            sprintf(
                message_str,
                "attempt to set too small value for max_name_len. use %d.",
                MAX_NAME_LEN);
            cmd_warning(message_str);
        }
        if (max_name_len > MAX_NAME_LEN_UPPER_LIMIT) {
            max_name_len = MAX_NAME_LEN_UPPER_LIMIT;
            sprintf(
                message_str,
                "attempt to set too large value for max_name_len. use %d.",
                MAX_NAME_LEN_UPPER_LIMIT);
            cmd_warning(message_str);
        }
        if (get_debug_enabled(opts)) {
            sprintf(message_str, "max_name_len = %d", max_name_len);
            cmd_warning(message_str);
        }
    }
    else
    {
        max_name_len = MAX_NAME_LEN;
    }

    /* DEBUG */
    debug_fp = stderr;
    diag_file = stderr;

    initialize_lex();
    initialize_compile();

    /* start processing */
    /* FEAST add start */
    second_pass_init();
    /* FEAST add  end  */
    parseError = yyparse();
    if (nerrors != 0 || parseError != 0) {
        goto Done;
    }
    nerrors = 0;

    /* FEAST add start */
    /* second pass */
    parseError = second_pass();
    /* printf("parseError = %d, nerrors = %d\n", parseError, nerrors); */
    if (nerrors != 0 || parseError != 0) {
        goto Done;
    }
    /* FEAST add  end  */

    /* end compile */
    if (unit_ctl_level != 0) {
        error("contains stack is not closed properly");
    }
    finalize_compile();
    if (nerrors != 0) {
        goto Done;
    }

    final_fixup();
    if (nerrors != 0) {
        goto Done;
    }

    /* output XcodeML/Fortran code */
    output_XcodeML_file();

Done:
    if (nerrors != 0) {
        if (output_file_name != NULL) {
            fclose(output_file);
            (void)unlink(output_file_name);
        }
    }

    return (nerrors ? EXITCODE_ERR : EXITCODE_OK);
}

int main(int argc, char *argv[])
{
    cli_options opts;
    init_cli_options(&opts);
#ifdef HAVE_SETLOCALE
    (void)setlocale(LC_ALL, "C");
#endif /* HAVE_SETLOCALE */
    parse_cli_args(&opts, argc, argv);
    const int ret_code = execute(&opts);
    free_cli_options(&opts);
    return ret_code;
}

const char *search_include_path(const char *filename)
{
    int i;
    int length;
    static char path[MAX_PATH_LEN];
    FILE *fp;

    if ((fp = fopen(filename, "r")) != NULL) {
        fclose(fp);
        return filename;
    }

    length = strlen(filename);

    if ((includeDirvI <= 0 && modincludeDirv == NULL) ||
        (length >= 1 && strncmp("/", filename, 1) == 0) ||
        (length >= 2 && strncmp("./", filename, 2) == 0) ||
        (length >= 3 && strncmp("../", filename, 3) == 0)) {
        return filename;
    }

    if (modincludeDirvI > 0) {
        // Iterate over available module search path
        for (i = 0; i < modincludeDirvI; i++) {
            strcpy(path, modincludeDirv[i]);
            strcat(path, "/");
            strcat(path, filename);
            if ((fp = fopen(path, "r")) != NULL) {
                fclose(fp);
                return path;
            }
        }
    }

    for (i = 0; i < includeDirvI; i++) {
        strcpy(path, includeDirv[i]);
        strcat(path, "/");
        strcat(path, filename);

        if ((fp = fopen(path, "r")) != NULL) {
            fclose(fp);
            return path;
        }
    }

    return NULL;
}

void where(lineno_info *ln)
{
    extern SYMBOL current_module_name;

    /* print location of error  */
    if (ln != NULL) {
        if (current_module_name == NULL)
            fprintf(stderr, "\"%s\", line %d: ", FILE_NAME(ln->file_id),
                    ln->ln_no);
        else
            fprintf(stderr, "\"%s:%s\", line %d: ", FILE_NAME(ln->file_id),
                    SYM_NAME(current_module_name), ln->ln_no);
    } else {
        fprintf(stderr, "\"??\", line ??: ");
    }
}

/* nonfatal error message */
/* VARARGS0 */
void error EXC_VARARGS_DEF(const char *, fmt)
{
    va_list args;

    ++nerrors;
    where(current_line);
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

    ++nerrors;
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

    ++nerrors;
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

    if (!debug_flag)
        return;

    ++nerrors;
    where(current_line);
    EXC_VARARGS_START(const char *, fmt, args);
    vfprintf(debug_fp, fmt, args);
    va_end(args);
    fprintf(debug_fp, "\n");
    fflush(debug_fp);
    check_nerrors();
}

static void check_nerrors()
{
    if (nerrors > 30) {
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

    where(current_line); /*, "Fatal");*/
    fprintf(stderr, "compiler error: ");
    EXC_VARARGS_START(const char *, fmt, args);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
    FATAL_ERROR();
}

int warning_flag = FALSE;

/* warning with lineno_info */
void warning_lineno(lineno_info *info, const char *fmt, ...)
{
    va_list args;

    if (warning_flag)
        return;
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

    if (warning_flag)
        return;
    where(current_line); /*, "Warn");*/
    EXC_VARARGS_START(const char *, fmt, args);
    fprintf(stderr, "warning: ");
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
}
