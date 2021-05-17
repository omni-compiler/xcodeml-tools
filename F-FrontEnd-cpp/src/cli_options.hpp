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

    const std::unique_ptr<bool>& getAccEnabled() const
    {
        return acc_enabled;
    }

    const std::unique_ptr<bool>& getAddTimestampEnabled() const
    {
        return add_timestamp_enabled;
    }

    const std::unique_ptr<int>& getAutoSaveAttrKb() const
    {
        return auto_save_attr_kb;
    }

    const std::unique_ptr<bool>& getCdirEnabled() const
    {
        return cdir_enabled;
    }

    const std::unique_ptr<bool>& getCondCompileEnabled() const
    {
        return cond_compile_enabled;
    }

    const std::unique_ptr<bool>& getDebugEnabled() const
    {
        return debug_enabled;
    }

    const std::unique_ptr<int>& getDefaultDoubleRealTypeSize() const
    {
        return default_double_real_type_size;
    }

    const std::unique_ptr<int>& getDefaultSingleRealTypeSize() const
    {
        return default_single_real_type_size;
    }

    const std::unique_ptr<bool>& getDoImplicitUndef() const
    {
        return do_implicit_undef;
    }

    const std::unique_ptr<bool>& getDollarInIdEnabled() const
    {
        return dollar_in_id_enabled;
    }

    const std::unique_ptr<bool>& getEndLineNoEnabled() const
    {
        return end_line_no_enabled;
    }

    const std::unique_ptr<bool>& getForceCCommentsEnabled() const
    {
        return force_c_comments_enabled;
    }

    const std::unique_ptr<bool>& getForceFixedFormatEnabled() const
    {
        return force_fixed_format_enabled;
    }

    const std::unique_ptr<bool>& getFsyncEnabled() const
    {
        return fsync_enabled;
    }

    const std::vector<std::string>& getIncDirPaths() const
    {
        return inc_dir_paths;
    }

    const std::unique_ptr<std::string>& getIntrinsicXmodDirPath() const
    {
        return intrinsic_xmod_dir_path;
    }

    const std::unique_ptr<bool>& getLangF77() const
    {
        return lang_f77;
    }

    const std::unique_ptr<bool>& getLangF90() const
    {
        return lang_f90;
    }

    const std::unique_ptr<bool>& getLangF95() const
    {
        return lang_f95;
    }

    const std::unique_ptr<bool>& getLeaveCommentEnabled() const
    {
        return leave_comment_enabled;
    }

    const std::unique_ptr<int>& getMaxContLine() const
    {
        return max_cont_line;
    }

    const std::unique_ptr<int>& getMaxLineLen() const
    {
        return max_line_len;
    }

    const std::unique_ptr<int>& getMaxNameLen() const
    {
        return max_name_len;
    }

    const std::unique_ptr<bool>& getModuleCacheEnabled() const
    {
        return module_cache_enabled;
    }

    const std::unique_ptr<bool>& getModuleCompileEnabled() const
    {
        return module_compile_enabled;
    }

    const std::unique_ptr<bool>& getNativeInMemModeEnabled() const
    {
        return native_in_mem_mode_enabled;
    }

    const std::unique_ptr<bool>& getOclEnabled() const
    {
        return ocl_enabled;
    }

    const std::unique_ptr<bool>& getOmpEnabled() const
    {
        return omp_enabled;
    }

    const std::unique_ptr<std::string>& getOutFilePath() const
    {
        return out_file_path;
    }

    const std::unique_ptr<bool>& getPgiEnabled() const
    {
        return pgi_enabled;
    }

    const std::unique_ptr<bool>& getPrintHelp() const
    {
        return print_help;
    }

    const std::unique_ptr<bool>& getPrintOpts() const
    {
        return print_opts;
    }

    const std::unique_ptr<bool>& getPrintVersion() const
    {
        return print_version;
    }

    const std::unique_ptr<bool>& getPrintVersionTag() const
    {
        return print_version_tag;
    }

    const std::unique_ptr<std::string>& getSrcFilePath() const
    {
        return src_file_path;
    }

    const std::unique_ptr<std::string>& getStdoutFilePath() const
    {
        return stdout_file_path;
    }

    const std::vector<std::string>& getXmodIncDirPaths() const
    {
        return xmod_inc_dir_paths;
    }

    const std::vector<std::string>& getXmodIncPaths() const
    {
        return xmod_inc_paths;
    }

    const std::unique_ptr<bool>& getXmpCoarrayEnabled() const
    {
        return xmp_coarray_enabled;
    }

    const std::unique_ptr<bool>& getXmpEnabled() const
    {
        return xmp_enabled;
    }

    const std::unique_ptr<bool>& getYaccDebugEnabled() const
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

