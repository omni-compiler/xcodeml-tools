#ifndef CLI_OPTIONS_H
#define CLI_OPTIONS_H

#include "F-datatype.h"
#include "utils.h"
#include "bool.h"
#include "c_vector/vector.h"
#include <stdio.h>

typedef struct {
    sds_string bin_name;
    sds_string src_file_path;
    sds_string out_file_path;
    sds_string intrinsic_xmod_dir_path;
    sds_string_vector inc_dir_paths;
    sds_string_vector xmod_inc_dir_paths;
    int max_line_len;
    int max_cont_line;
    int lang_spec_set;
    int auto_save_attr_kb;
    int max_name_len;
    BASIC_DATA_TYPE default_single_real_type;
    BASIC_DATA_TYPE default_double_real_type;
    bool debug_enabled;
    bool yacc_debug_enabled;
    bool module_compile_enabled;
    bool omp_enabled;
    bool xmp_enabled;
    bool xmp_coarray_enabled;
    bool acc_enabled;
    bool cond_compile_enabled;
    bool leave_comment_enabled;
    bool do_implicit_undef;
    bool force_fixed_format_enabled;
    bool force_c_comments_enabled;
    bool dollar_in_id_enabled;
    bool end_line_no_enabled;
    bool ocl_enabled;
    bool cdir_enabled;
    bool pgi_enabled;
    bool module_cache_enabled;
    bool print_help;
    bool print_opts;
} cli_options;

void init_cli_options(cli_options *opts);
void free_cli_options(cli_options *opts);
void print_options(const cli_options* opts, FILE* out);

static inline void set_bin_name(cli_options *opts, const char *name) {
    set_sds_string(&opts->bin_name, name);
}
static inline const sds get_bin_name(const cli_options *opts) {
    return opts->bin_name;
}
static inline void set_src_file_path(cli_options *opts,
        const char *src_file_path) {
    set_sds_string(&opts->src_file_path, src_file_path);
}
static inline const sds get_src_file_path(const cli_options *opts) {
    return opts->src_file_path;
}
static inline void set_out_file_path(cli_options *opts,
        const char *out_file_path) {
    set_sds_string(&opts->out_file_path, out_file_path);
}
static inline const sds get_out_file_path(const cli_options *opts) {
    return opts->out_file_path;
}
static inline void set_intrinsic_xmod_dir_path(cli_options *opts,
        const char *intrinsic_xmod_dir_path) {
    set_sds_string(&opts->intrinsic_xmod_dir_path,
            intrinsic_xmod_dir_path);
}
static inline const sds get_intrinsic_xmod_dir_path(const cli_options *opts) {
    return opts->intrinsic_xmod_dir_path;
}
static inline void add_inc_dir_path(cli_options *opts, const char *path) {
    vector_push_back(opts->inc_dir_paths, sdsnew(path));
}
static inline size_t get_inc_dir_paths_size(const cli_options *opts) {
    return vector_size(opts->inc_dir_paths);
}
static inline const char* get_inc_dir_path(const cli_options *opts,
        size_t idx) {
    return opts->inc_dir_paths[idx];
}
static inline char** const get_inc_dir_paths(const cli_options *opts) {
    return opts->inc_dir_paths;
}
static inline size_t get_xmod_inc_dir_paths_size(const cli_options *opts) {
    return vector_size(opts->xmod_inc_dir_paths);
}
static inline void add_xmod_inc_dir_path(cli_options *opts, const char *path) {
    vector_push_back(opts->xmod_inc_dir_paths, sdsnew(path));
}
static inline const char* get_xmod_inc_dir_path(const cli_options *opts,
        size_t idx) {
    return opts->xmod_inc_dir_paths[idx];
}
static inline char** const get_xmod_inc_dir_paths(const cli_options *opts) {
    return opts->xmod_inc_dir_paths;
}
static inline void set_max_line_len(cli_options *opts, int val) {
    opts->max_line_len = val;
}
static inline int get_max_line_len(const cli_options *opts) {
    return opts->max_line_len;
}
static inline void set_max_cont_line(cli_options *opts, int val) {
    opts->max_cont_line = val;
}
static inline int get_max_cont_line(const cli_options *opts) {
    return opts->max_cont_line;
}
static inline void set_lang_spec_set(cli_options *opts, int val) {
    opts->lang_spec_set = val;
}
static inline int get_lang_spec_set(const cli_options *opts) {
    return opts->lang_spec_set;
}
static inline void set_auto_save_attr_kb(cli_options *opts, int val) {
    opts->auto_save_attr_kb = val;
}
static inline int get_auto_save_attr_kb(const cli_options *opts) {
    return opts->auto_save_attr_kb;
}
static inline void set_max_name_len(cli_options *opts, int val) {
    opts->max_name_len = val;
}
static inline int get_max_name_len(const cli_options *opts) {
    return opts->max_name_len;
}
static inline void set_default_single_real_type(cli_options *opts,
        BASIC_DATA_TYPE val) {
    opts->default_single_real_type = val;
}
static inline BASIC_DATA_TYPE get_default_single_real_type(
        const cli_options *opts) {
    return opts->default_single_real_type;
}
static inline void set_default_double_real_type(cli_options *opts,
        BASIC_DATA_TYPE val) {
    opts->default_double_real_type = val;
}
static inline BASIC_DATA_TYPE get_default_double_real_type(
        const cli_options *opts) {
    return opts->default_double_real_type;
}
static inline void set_debug_enabled(cli_options *opts, bool val) {
    opts->debug_enabled = val;
}
static inline bool get_debug_enabled(const cli_options *opts) {
    return opts->debug_enabled;
}
static inline void set_yacc_debug_enabled(cli_options *opts, bool val) {
    opts->yacc_debug_enabled = val;
}
static inline bool get_yacc_debug_enabled(const cli_options *opts) {
    return opts->yacc_debug_enabled;
}
static inline void set_module_compile_enabled(cli_options *opts, bool val) {
    opts->module_compile_enabled = val;
}
static inline bool get_module_compile_enabled(const cli_options *opts) {
    return opts->module_compile_enabled;
}
static inline void set_omp_enabled(cli_options *opts, bool val) {
    opts->omp_enabled = val;
}
static inline bool get_omp_enabled(const cli_options *opts) {
    return opts->omp_enabled;
}
static inline void set_xmp_enabled(cli_options *opts, bool val) {
    opts->xmp_enabled = val;
}
static inline bool get_xmp_enabled(const cli_options *opts) {
    return opts->xmp_enabled;
}
static inline void set_xmp_coarray_enabled(cli_options *opts, bool val) {
    opts->xmp_coarray_enabled = val;
}
static inline bool get_xmp_coarray_enabled(const cli_options *opts) {
    return opts->xmp_coarray_enabled;
}
static inline void set_acc_enabled(cli_options *opts, bool val) {
    opts->acc_enabled = val;
}
static inline bool get_acc_enabled(const cli_options *opts) {
    return opts->acc_enabled;
}
static inline void set_cond_compile_enabled(cli_options *opts, bool val) {
    opts->cond_compile_enabled = val;
}
static inline bool get_cond_compile_enabled(const cli_options *opts) {
    return opts->cond_compile_enabled;
}
static inline void set_leave_comment_enabled(cli_options *opts, bool val) {
    opts->leave_comment_enabled = val;
}
static inline bool get_leave_comment_enabled(const cli_options *opts) {
    return opts->leave_comment_enabled;
}
static inline void set_do_implicit_undef(cli_options *opts, bool val) {
    opts->do_implicit_undef = val;
}
static inline bool get_do_implicit_undef(const cli_options *opts) {
    return opts->do_implicit_undef;
}

static inline void set_force_fixed_format_enabled(cli_options *opts, bool val) {
    opts->force_fixed_format_enabled = val;
}
static inline bool get_force_fixed_format_enabled(const cli_options *opts) {
    return opts->force_fixed_format_enabled;
}
static inline void set_force_c_comments_enabled(cli_options *opts, bool val) {
    opts->force_c_comments_enabled = val;
}
static inline bool get_force_c_comments_enabled(const cli_options *opts) {
    return opts->force_c_comments_enabled;
}
static inline void set_dollar_in_id_enabled(cli_options *opts, bool val) {
    opts->dollar_in_id_enabled = val;
}
static inline bool get_dollar_in_id_enabled(const cli_options *opts) {
    return opts->dollar_in_id_enabled;
}
static inline void set_end_line_no_enabled(cli_options *opts, bool val) {
    opts->end_line_no_enabled = val;
}
static inline bool get_end_line_no_enabled(const cli_options *opts) {
    return opts->end_line_no_enabled;
}
static inline void set_ocl_enabled(cli_options *opts, bool val) {
    opts->ocl_enabled = val;
}
static inline bool get_ocl_enabled(const cli_options *opts) {
    return opts->ocl_enabled;
}
static inline void set_cdir_enabled(cli_options *opts, bool val) {
    opts->cdir_enabled = val;
}
static inline bool get_cdir_enabled(const cli_options *opts) {
    return opts->cdir_enabled;
}
static inline void set_pgi_enabled(cli_options *opts, bool val) {
    opts->pgi_enabled = val;
}
static inline bool get_pgi_enabled(const cli_options *opts) {
    return opts->pgi_enabled;
}
static inline void set_module_cache_enabled(cli_options *opts, bool val) {
    opts->module_cache_enabled = val;
}
static inline bool get_module_cache_enabled(const cli_options *opts) {
    return opts->module_cache_enabled;
}
static inline void set_print_help(cli_options *opts, bool val) {
    opts->print_help = val;
}
static inline bool get_print_help(const cli_options *opts) {
    return opts->print_help;
}
static inline void set_print_opts(cli_options *opts, bool val) {
    opts->print_opts = val;
}
static inline bool get_print_opts(const cli_options *opts) {
    return opts->print_opts;
}

#endif //CLI_OPTIONS_H
