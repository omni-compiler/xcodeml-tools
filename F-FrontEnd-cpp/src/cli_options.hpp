/* Author: Mikhail Zhigun */
#ifndef CLIOPTIONS_HPP
#define CLIOPTIONS_HPP

#include <vector>
#include <string>
#include <memory>

class CLIOptions
{
public:

    static std::unique_ptr<CLIOptions> parseCmdlineArguments(int argc,
            char *argv[], const std::string &workingDir);

    const std::unique_ptr<bool>& get_acc_enabled() const
    {
        return acc_enabled;
    }

    const std::unique_ptr<bool>& get_add_timestamp_enabled() const
    {
        return add_timestamp_enabled;
    }

    const std::unique_ptr<int>& get_auto_save_attr_kb() const
    {
        return auto_save_attr_kb;
    }

    const std::unique_ptr<bool>& get_cdir_enabled() const
    {
        return cdir_enabled;
    }

    const std::unique_ptr<bool>& get_cond_compile_enabled() const
    {
        return cond_compile_enabled;
    }

    const std::unique_ptr<bool>& get_debug_enabled() const
    {
        return debug_enabled;
    }

    const std::unique_ptr<int>& get_default_double_real_type_size() const
    {
        return default_double_real_type_size;
    }

    const std::unique_ptr<int>& get_default_single_real_type_size() const
    {
        return default_single_real_type_size;
    }

    const std::unique_ptr<bool>& get_do_implicit_undef() const
    {
        return do_implicit_undef;
    }

    const std::unique_ptr<bool>& get_dollar_in_id_enabled() const
    {
        return dollar_in_id_enabled;
    }

    const std::unique_ptr<bool>& get_end_line_no_enabled() const
    {
        return end_line_no_enabled;
    }

    const std::unique_ptr<bool>& get_force_c_comments_enabled() const
    {
        return force_c_comments_enabled;
    }

    const std::unique_ptr<bool>& get_force_fixed_format_enabled() const
    {
        return force_fixed_format_enabled;
    }

    const std::unique_ptr<bool>& get_fsync_enabled() const
    {
        return fsync_enabled;
    }

    const std::vector<std::string>& get_inc_dir_path() const
    {
        return inc_dir_paths;
    }

    const std::unique_ptr<std::string>& get_intrinsic_xmod_dir_path() const
    {
        return intrinsic_xmod_dir_path;
    }

    const std::unique_ptr<bool>& get_lang_f77() const
    {
        return lang_f77;
    }

    const std::unique_ptr<bool>& get_lang_f90() const
    {
        return lang_f90;
    }

    const std::unique_ptr<bool>& get_lang_f95() const
    {
        return lang_f95;
    }

    const std::unique_ptr<bool>& get_leave_comment_enabled() const
    {
        return leave_comment_enabled;
    }

    const std::unique_ptr<int>& get_max_cont_line() const
    {
        return max_cont_line;
    }

    const std::unique_ptr<int>& get_max_line_len() const
    {
        return max_line_len;
    }

    const std::unique_ptr<int>& get_max_name_len() const
    {
        return max_name_len;
    }

    const std::unique_ptr<bool>& get_module_cache_enabled() const
    {
        return module_cache_enabled;
    }

    const std::unique_ptr<bool>& get_module_compile_enabled() const
    {
        return module_compile_enabled;
    }

    const std::unique_ptr<bool>& get_native_in_mem_mode_enabled() const
    {
        return native_in_mem_mode_enabled;
    }

    const std::unique_ptr<bool>& get_ocl_enabled() const
    {
        return ocl_enabled;
    }

    const std::unique_ptr<bool>& get_omp_enabled() const
    {
        return omp_enabled;
    }

    const std::unique_ptr<std::string>& get_out_file_path() const
    {
        return out_file_path;
    }

    const std::unique_ptr<bool>& get_pgi_enabled() const
    {
        return pgi_enabled;
    }

    const std::unique_ptr<bool>& get_print_help() const
    {
        return print_help;
    }

    const std::unique_ptr<bool>& get_print_opts() const
    {
        return print_opts;
    }

    const std::unique_ptr<bool>& get_print_version() const
    {
        return print_version;
    }

    const std::unique_ptr<bool>& get_print_version_tag() const
    {
        return print_version_tag;
    }

    const std::unique_ptr<std::string>& get_src_file_path() const
    {
        return src_file_path;
    }

    const std::unique_ptr<std::string>& get_stdout_file_path() const
    {
        return stdout_file_path;
    }

    const std::vector<std::string>& get_xmod_inc_dir_paths() const
    {
        return xmod_inc_dir_paths;
    }

    const std::vector<std::string>& get_xmod_inc_paths() const
    {
        return xmod_inc_paths;
    }

    const std::unique_ptr<bool>& get_xmp_coarray_enabled() const
    {
        return xmp_coarray_enabled;
    }

    const std::unique_ptr<bool>& get_xmp_enabled() const
    {
        return xmp_enabled;
    }

    const std::unique_ptr<bool>& get_yacc_debug_enabled() const
    {
        return yacc_debug_enabled;
    }

private:

    static std::vector<std::string> correctOptsStyle(int argc, char *argv[]);
    static std::vector<const char*> toCOpts(const std::vector<std::string>&);
    static std::string toAbsPath(const std::string &workingDir,
            const std::string &pathStr);
    static void normalizePath(const std::string &workingDir,
            std::unique_ptr<std::string> &pathStr);
    static void normalizePaths(const std::string &workingDir,
            std::vector<std::string> &paths);

    std::unique_ptr<std::string> src_file_path;
    std::unique_ptr<std::string> out_file_path;
    std::unique_ptr<std::string> intrinsic_xmod_dir_path;
    std::unique_ptr<std::string> stdout_file_path;
    std::vector<std::string> inc_dir_paths;
    std::vector<std::string> xmod_inc_dir_paths;
    std::vector<std::string> xmod_inc_paths;
    std::unique_ptr<int> max_line_len;
    std::unique_ptr<int> max_cont_line;
    std::unique_ptr<int> auto_save_attr_kb;
    std::unique_ptr<int> max_name_len;
    std::unique_ptr<int> default_single_real_type_size;
    std::unique_ptr<int> default_double_real_type_size;
    std::unique_ptr<bool> lang_f77;
    std::unique_ptr<bool> lang_f90;
    std::unique_ptr<bool> lang_f95;
    std::unique_ptr<bool> debug_enabled;
    std::unique_ptr<bool> yacc_debug_enabled;
    std::unique_ptr<bool> module_compile_enabled;
    std::unique_ptr<bool> omp_enabled;
    std::unique_ptr<bool> xmp_enabled;
    std::unique_ptr<bool> xmp_coarray_enabled;
    std::unique_ptr<bool> acc_enabled;
    std::unique_ptr<bool> cond_compile_enabled;
    std::unique_ptr<bool> leave_comment_enabled;
    std::unique_ptr<bool> do_implicit_undef;
    std::unique_ptr<bool> force_fixed_format_enabled;
    std::unique_ptr<bool> force_c_comments_enabled;
    std::unique_ptr<bool> dollar_in_id_enabled;
    std::unique_ptr<bool> end_line_no_enabled;
    std::unique_ptr<bool> ocl_enabled;
    std::unique_ptr<bool> cdir_enabled;
    std::unique_ptr<bool> pgi_enabled;
    std::unique_ptr<bool> module_cache_enabled;
    std::unique_ptr<bool> add_timestamp_enabled;
    std::unique_ptr<bool> print_help;
    std::unique_ptr<bool> print_opts;
    std::unique_ptr<bool> native_in_mem_mode_enabled;
    std::unique_ptr<bool> fsync_enabled;
    std::unique_ptr<bool> print_version;
    std::unique_ptr<bool> print_version_tag;
};

#endif //CLIOPTIONS_HPP

