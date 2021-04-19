#ifndef _F_FRONT_CONTEXT_H_
#define _F_FRONT_CONTEXT_H_

#include "F-datatype.h"
#include "bool.h"
#include "utils.h"
#include "c_vector/vector.h"
#include <stdio.h>

typedef struct {
    sds_string out_file_path;
    sds_string intrinsic_xmod_dir_path;
    sds_string_vector inc_dir_paths;
    sds_string_vector xmod_inc_dir_paths;
    int max_line_len;
    int max_cont_line;
    int lang_spec_set;
    int auto_save_attr_kb;
    int max_id_name_len;
    BASIC_DATA_TYPE default_single_real_type;
    BASIC_DATA_TYPE default_double_real_type;
    bool debug_enabled;
    bool module_compile_enabled;
    bool omp_enabled;
    bool xmp_enabled;
    bool xmp_coarray_enabled;
    bool acc_enabled;
    bool cond_compile_enabled;
    bool leave_comment_enabled;
    bool do_implicit_undef_enabled;
    bool force_c_comments_enabled;
    bool dollar_in_id_enabled;
    bool end_line_no_enabled;
    bool ocl_enabled;
    bool cdir_enabled;
    bool pgi_enabled;
    bool module_cache_enabled;
} ffront_params;

struct symbol;
typedef struct symbol* SYMBOL;

struct s_lineno_info;

typedef struct s_lineno_info lineno_info;

typedef struct {
    const ffront_params* const params;
    sds_string src_file_path;
    bool fixed_format_enabled;
    str_stream src_input;
    str_stream src_output;
    unsigned num_errors;
    str_stream diag_output;
    str_stream debug_output;
    SYMBOL current_module_name;
    lineno_info* current_line;
} ffront_context;

extern THREAD_LOCAL ffront_context* ctx;

void init_ffront_params(ffront_params* params);
void free_ffront_params(ffront_params* params);

void init_ffront_context(ffront_context* ctx);
void free_ffront_context(ffront_context* ctx);
void set_ffront_context(ffront_context*);

static inline FILE* src_input() {
    return ctx->src_input.handle;
}
static inline FILE* src_output() {
    return ctx->src_output.handle;
}
static inline FILE* diag_output() {
    return ctx->diag_output.handle;
}
static inline FILE* debug_output() {
    return ctx->debug_output.handle;
}
static inline unsigned num_errors() {
    return ctx->num_errors;
}
static inline void inc_num_errors() {
    ++ctx->num_errors;
}
static inline SYMBOL current_module_name() {
    return ctx->current_module_name;
}
static inline lineno_info* current_line() {
    return ctx->current_line;
}
static inline const sds out_file_path() {
    return ctx->params->out_file_path;
}
static inline const sds intrinsic_xmod_dir_path() {
    return ctx->params->intrinsic_xmod_dir_path;
}
static inline const sds_string_vector inc_dir_paths() {
    return ctx->params->inc_dir_paths;
}
static inline const sds_string_vector xmod_inc_dir_paths() {
    return ctx->params->xmod_inc_dir_paths;
}
static inline int max_line_len() { return ctx->params->max_line_len; }
static inline int max_cont_line() { return ctx->params->max_cont_line; }
static inline int lang_spec_set() { return ctx->params->lang_spec_set; }
static inline int auto_save_attr_kb() { return ctx->params->auto_save_attr_kb; }
static inline int max_id_name_len() { return ctx->params->max_id_name_len; }
static inline BASIC_DATA_TYPE default_single_real_type() { return ctx->params->default_single_real_type; }
static inline BASIC_DATA_TYPE default_double_real_type() { return ctx->params->default_double_real_type; }
static inline bool debug_enabled() { return ctx->params->debug_enabled; }
static inline bool module_compile_enabled() { return ctx->params->module_compile_enabled; }
static inline bool openmp_enabled() { return ctx->params->omp_enabled; }
static inline bool xmp_enabled() { return ctx->params->xmp_enabled; }
static inline bool xmp_coarray_enabled() { return ctx->params->xmp_coarray_enabled; }
static inline bool openacc_enabled() { return ctx->params->acc_enabled; }
static inline bool conditional_compile_enabled() { return ctx->params->cond_compile_enabled; }
static inline bool leave_comment_enabled() { return ctx->params->leave_comment_enabled; }
static inline bool do_implicit_undef_enabled() { return ctx->params->do_implicit_undef_enabled; }
static inline bool force_c_comments_enabled() { return ctx->params->force_c_comments_enabled; }
static inline bool dollar_in_id_enabled() { return ctx->params->dollar_in_id_enabled; }
static inline bool end_line_no_enabled() { return ctx->params->end_line_no_enabled; }
static inline bool ocl_enabled() { return ctx->params->ocl_enabled; }
static inline bool cdir_enabled() { return ctx->params->cdir_enabled; }
static inline bool pgi_enabled() { return ctx->params->pgi_enabled; }
static inline bool module_cache_enabled() { return ctx->params->module_cache_enabled; }

#endif //_F_FRONT_CONTEXT_H_
