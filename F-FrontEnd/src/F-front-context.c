#include "F-front-context.h"
#include "c_vector/vector.h"

void init_ffront_params(ffront_params* params)
{
    params->out_file_path = sdsempty();
    params->intrinsic_xmod_dir_path = sdsempty();
    params->inc_dir_paths = vector_init(sds_string, 0);
    params->xmod_inc_dir_paths = vector_init(sds_string, 0);
}

void free_ffront_params(ffront_params* params)
{
    sdsfree(params->out_file_path);
    sdsfree(params->intrinsic_xmod_dir_path);
    free_sds_string_vector(&params->inc_dir_paths);
    free_sds_string_vector(&params->xmod_inc_dir_paths);
}

void init_ffront_context(ffront_context* ctx)
{
    ctx->src_file_path = sdsempty();
    init_str_stream(&ctx->src_input);
    init_str_stream(&ctx->src_output);
    init_str_stream(&ctx->diag_output);
    init_str_stream(&ctx->debug_output);
    ctx->num_errors = 0;
    ctx->current_module_name = NULL;
    ctx->current_line = NULL;
}

void free_ffront_context(ffront_context* ctx)
{
    sdsfree(ctx->src_file_path);
    release_str_stream(&ctx->src_input);
    release_str_stream(&ctx->src_output);
    release_str_stream(&ctx->diag_output);
    release_str_stream(&ctx->debug_output);
}

THREAD_LOCAL ffront_context* ctx;

void set_ffront_context(ffront_context* in_ctx)
{
    ctx = in_ctx;
}
