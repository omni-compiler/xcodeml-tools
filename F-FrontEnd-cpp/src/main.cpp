#include "cli_options.hpp"
#include "cli_options.h"
#include "io_cache.h"
#include "app_constants.hpp"
#if __cplusplus > 201703L
#include <filesystem>
namespace std_fs = std::filesystem;
#else
#include <experimental/filesystem>
namespace std_fs = std::experimental::filesystem;
#endif

#include <iostream>

int execute_cli_opts(const cli_options *opts, io_cache files_cache);

void fwd_cli_opts(const CLIOptions &inOpts, cli_options *outOpts);

int main(int argc, char *argv[])
{
    const auto WORKING_DIR = static_cast<std::string>(std_fs::current_path());
    auto opts = CLIOptions::parseCmdlineArguments(argc, argv, WORKING_DIR);
    if (opts)
    {
        if (opts->get_print_version())
        {
            std::cout << AppConstants::PACKAGE_VERSION;
            std::cout.flush();
            return 0;
        }
        else if (opts->get_print_version_tag())
        {
            std::cout << AppConstants::PACKAGE_VERSION_TAG;
            std::cout.flush();
            return 0;
        }
        else
        {
            cli_options c_opts;
            init_cli_options(&c_opts);
            fwd_cli_opts(*opts, &c_opts);
            const int ret_code = execute_cli_opts(&c_opts, nullptr);
            free_cli_options(&c_opts);
            return ret_code;
        }
        /*checkCLIArgs(opts);
         System.loadLibrary("ffront-jni");
         if (opts.native_in_mem_mode_enabled)
         {
         try (IOCache filesCache = createCache(opts))
         {
         final int retCode = execute(opts, filesCache);
         updateCache(filesCache, opts);
         System.exit(retCode);
         }
         } else
         {
         System.exit(execute(opts, null));
         }*/
    }
    else
    {
        return 0;
    }
}

void fwd_cli_opts(const CLIOptions &inOpts, cli_options *outOpts)
{
#define set_path(field_name) \
    { \
        const auto& path = inOpts.get_##field_name(); \
        if(path) \
           set_##field_name(outOpts, path->c_str()); \
    }
#define set_path_vector(field_name) \
    { \
    const auto& paths = inOpts.get_##field_name(); \
    for(const auto& path: paths) \
       add_##field_name(outOpts, path.c_str()); \
    }
#define set_int(field_name) \
    { \
        const auto& val = inOpts.get_##field_name(); \
        if(val) \
           set_##field_name(outOpts, *val); \
    }
#define set_bool(field_name) \
    { \
        const auto& val = inOpts.get_##field_name(); \
        if(val) \
           set_##field_name(outOpts, *val); \
    }
    set_path(src_file_path);
    set_path(out_file_path);
    set_path(intrinsic_xmod_dir_path);
    set_path_vector(inc_dir_path);
    set_int(max_line_len);
    set_int(max_cont_line);
    set_int(auto_save_attr_kb);
    set_int(max_name_len);
    set_int(default_single_real_type_size);
    set_int(default_double_real_type_size);
    {
        const auto& f77 = inOpts.get_lang_f77();
        if (f77 && (*f77))
        {
            set_lang_spec_set(outOpts, F77_SPEC);
        }
        const auto& f90 = inOpts.get_lang_f90();
        if (f90 && (*f90))
        {
            set_lang_spec_set(outOpts, F90_SPEC);
        }
        const auto& f95 = inOpts.get_lang_f95();
        if (f95 && (*f95))
        {
            set_lang_spec_set(outOpts, F95_SPEC);
        }
    }
    set_bool(debug_enabled);
    set_bool(yacc_debug_enabled);
    set_bool(module_compile_enabled);
    set_bool(omp_enabled);
    set_bool(xmp_enabled);
    set_bool(xmp_coarray_enabled);
    set_bool(acc_enabled);
    set_bool(cond_compile_enabled);
    set_bool(leave_comment_enabled);
    set_bool(do_implicit_undef);
    set_bool(force_fixed_format_enabled);
    set_bool(force_c_comments_enabled);
    set_bool(dollar_in_id_enabled);
    set_bool(end_line_no_enabled);
    set_bool(ocl_enabled);
    set_bool(cdir_enabled);
    set_bool(pgi_enabled);
    set_bool(module_cache_enabled);
    set_bool(add_timestamp_enabled);
    set_bool(print_help);
    set_bool(print_opts);
}
