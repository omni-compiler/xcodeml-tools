#include "cli_options.h"
#include "F-front.h"

static const int DEFAULT_MAX_PATHS = 256;
static const int DEFAULT_MAX_CONT_LINE = 255;
static const int DEFAULT_MAX_LINE_LEN = -1;
static const int DEFAULT_AUTO_SAVE_ATTR_KB = -1;
static const int DEFAULT_MAX_NAME_LEN = -1;
static const bool DEFAULT_DEBUG_ENABLED = false;
static const bool DEFAULT_YACC_DEBUG_ENABLED = false;
static const bool DEFAULT_MODULE_COMPILE_ENABLED = false;
static const bool DEFAULT_OMP_ENABLED = false;
static const bool DEFAULT_XMP_ENABLED = false;
static const bool DEFAULT_XMP_COARRAY_ENABLED = true;
static const bool DEFAULT_ACC_ENABLED = false;
static const bool DEFAULT_COND_COMPILE_ENABLED = false;
static const bool DEFAULT_LEAVE_COMMENT_ENABLED = false;
static const bool DEFAULT_DO_IMPLICIT_UNDEF = false;
static const bool DEFAULT_FORCE_FIXED_FORMAT_ENABLED = false;
static const bool DEFAULT_FORCE_C_COMMENTS_ENABLED = false;
static const bool DEFAULT_DOLLAR_IN_ID_ENABLED = false;
static const bool DEFAULT_END_LINE_NO_ENABLED = false;
static const bool DEFAULT_OCL_ENABLED = false;
static const bool DEFAULT_PGI_ENABLED = false;
static const bool DEFAULT_CDIR_ENABLED = false;
static const bool DEFAULT_MODULE_CACHE_ENABLED = true;
static const bool DEFAULT_PRINT_HELP = false;
static const bool DEFAULT_PRINT_OPTS = false;

void init_cli_options(cli_options* opts)
{
    opts->bin_name = sdsempty();
    opts->src_file_path = sdsempty();
    opts->out_file_path = sdsempty();
    opts->intrinsic_xmod_dir_path = sdsempty();
    opts->inc_dir_paths = vector_init(sds_string, DEFAULT_MAX_PATHS);
    opts->xmod_inc_dir_paths = vector_init(sds_string, DEFAULT_MAX_PATHS);
    opts->max_line_len = DEFAULT_MAX_LINE_LEN;
    opts->max_cont_line = DEFAULT_MAX_CONT_LINE;
    opts->lang_spec_set = LANGSPEC_DEFAULT_SET;
    opts->auto_save_attr_kb = DEFAULT_AUTO_SAVE_ATTR_KB;
    opts->max_name_len = DEFAULT_MAX_NAME_LEN;
    opts->default_single_real_type = TYPE_REAL;
    opts->default_double_real_type = TYPE_DREAL;
    opts->debug_enabled = DEFAULT_DEBUG_ENABLED;
    opts->yacc_debug_enabled = DEFAULT_YACC_DEBUG_ENABLED;
    opts->module_compile_enabled = DEFAULT_MODULE_COMPILE_ENABLED;
    opts->omp_enabled = DEFAULT_OMP_ENABLED;
    opts->xmp_enabled = DEFAULT_XMP_ENABLED;
    opts->xmp_coarray_enabled = DEFAULT_XMP_COARRAY_ENABLED;
    opts->acc_enabled = DEFAULT_ACC_ENABLED;
    opts->cond_compile_enabled = DEFAULT_COND_COMPILE_ENABLED;
    opts->leave_comment_enabled = DEFAULT_LEAVE_COMMENT_ENABLED;
    opts->do_implicit_undef = DEFAULT_DO_IMPLICIT_UNDEF;
    opts->force_fixed_format_enabled = DEFAULT_FORCE_FIXED_FORMAT_ENABLED;
    opts->force_c_comments_enabled = DEFAULT_FORCE_C_COMMENTS_ENABLED;
    opts->dollar_in_id_enabled = DEFAULT_DOLLAR_IN_ID_ENABLED;
    opts->end_line_no_enabled = DEFAULT_END_LINE_NO_ENABLED;
    opts->ocl_enabled = DEFAULT_OCL_ENABLED;
    opts->cdir_enabled = DEFAULT_CDIR_ENABLED;
    opts->pgi_enabled = DEFAULT_PGI_ENABLED;
    opts->module_cache_enabled = DEFAULT_MODULE_CACHE_ENABLED;
    opts->print_help = DEFAULT_PRINT_HELP;
    opts->print_opts = DEFAULT_PRINT_OPTS;
}

void free_cli_options(cli_options* opts)
{
    sdsfree(opts->bin_name);
    sdsfree(opts->src_file_path);
    sdsfree(opts->out_file_path);
    sdsfree(opts->intrinsic_xmod_dir_path);
    free_sds_string_vector(&opts->inc_dir_paths);
    free_sds_string_vector(&opts->xmod_inc_dir_paths);
}

#define PRINT_STRING(name) fprintf(out, "%s: %s\n", #name, get_ ## name(opts))
#define PRINT_STRING_VECTOR(name) \
{ \
    size_t size = get_ ## name ## _size(opts); \
    size_t i = 0; \
    for(i = 0; i < size; ++i) \
    { \
        fprintf(out, "%s[%zu]: %s\n", #name, i, get_ ## name(opts)[i]); \
    } \
}
#define PRINT_INT(name) fprintf(out, "%s: %i\n", #name, get_ ## name(opts))
#define PRINT_ENUM(name) fprintf(out, "%s: %i\n", #name, (int)get_ ## name(opts))
#define PRINT_BOOL(name) fprintf(out, "%s: %s\n", #name, get_ ## name(opts) ? "true" : "false")

void print_options(const cli_options* opts, FILE* out)
{
    PRINT_STRING(bin_name);
    PRINT_STRING(src_file_path);
    PRINT_STRING(out_file_path);
    PRINT_STRING(intrinsic_xmod_dir_path);
    PRINT_STRING_VECTOR(inc_dir_paths);
    PRINT_STRING_VECTOR(xmod_inc_dir_paths);
    PRINT_INT(max_line_len);
    PRINT_INT(max_cont_line);
    PRINT_INT(lang_spec_set);
    PRINT_INT(auto_save_attr_kb);
    PRINT_INT(max_name_len);
    PRINT_ENUM(default_single_real_type);
    PRINT_ENUM(default_double_real_type);
    PRINT_BOOL(debug_enabled);
    PRINT_BOOL(yacc_debug_enabled);
    PRINT_BOOL(module_compile_enabled);
    PRINT_BOOL(omp_enabled);
    PRINT_BOOL(xmp_enabled);
    PRINT_BOOL(xmp_coarray_enabled);
    PRINT_BOOL(acc_enabled);
    PRINT_BOOL(cond_compile_enabled);
    PRINT_BOOL(leave_comment_enabled);
    PRINT_BOOL(do_implicit_undef);
    PRINT_BOOL(force_fixed_format_enabled);
    PRINT_BOOL(force_c_comments_enabled);
    PRINT_BOOL(dollar_in_id_enabled);
    PRINT_BOOL(end_line_no_enabled);
    PRINT_BOOL(ocl_enabled);
    PRINT_BOOL(cdir_enabled);
    PRINT_BOOL(pgi_enabled);
    PRINT_BOOL(module_cache_enabled);
    PRINT_BOOL(print_help);
    PRINT_BOOL(print_opts);
}
