/* Author: Mikhail Zhigun */
#include "cli_options.hpp"
#include <sstream>
#if __cplusplus > 201703L
#include <filesystem>
#else
#include <experimental/filesystem>
using namespace std::experimental;
#endif

using namespace std;

namespace CLI
{

template<typename T>
istringstream& operator>>(istringstream &in, unique_ptr<T> &val);

template<>
istringstream& operator>><bool>(istringstream &in, unique_ptr<bool> &val)
{
    string s;
    in >> s;
    if (s == "true")
    {
        val = make_unique<bool>(true);
    }
    else if (s == "false")
    {
        val = make_unique<bool>(false);
    }
    return in;
}

template<typename T>
istringstream& operator>>(istringstream &in, unique_ptr<T> &val)
{
    val = make_unique<T>();
    in >> *val;
    return in;
}

}

#include "cli11/CLI11.hpp"

vector<string> CLIOptions::correctOptsStyle(int argc, char *argv[])
{
    vector<string> opts(argc);
    for (int i = 0; i < argc; ++i)
    {
        string opt{ argv[i] };
        if (opt.size() > 2 && opt[0] == '-' && opt[1] != '-')
        {
            if(opt.compare(0, 2, "-I") == 0)
            {
                opt.insert(2, "=");
            }
            else
            {
                opt = "-" + opt;
            }
        }
        opts[i] = opt;
    }
    return opts;
}

vector<const char*> CLIOptions::toCOpts(const vector<string> &opts)
{
    vector<const char*> cOpts(opts.size());
    for (size_t i = 0, n = opts.size(); i < n; ++i)
    {
        cOpts[i] = opts[i].c_str();
    }
    return cOpts;
}

unique_ptr<CLIOptions> CLIOptions::parseCmdlineArguments(int argc, char *argv[],
        const std::string &workingDir)
{
    auto res = make_unique<CLIOptions>();
    CLI::App app
    {
            "FFront is a source-to-source compiler which translates Fortran source into XcodeML intermediate representation",
            "ffront-cpp" };
    app.add_option("fortran-file", res->src_file_path, "Input file");
    app.add_option("-o,--output-file", res->out_file_path, "Output file path");
    app.add_option("--fintrinsic-xmodules-path", res->intrinsic_xmod_dir_path,
            "Intrinsic xmod include dir");
    app.add_option("-I,--pp-include-dir", res->inc_dir_paths,
            "Add the directory to the search path for include files reference in preprocessor directives");
    app.add_option("-M,--MI,--mod-include-dir", res->xmod_inc_dir_paths,
            "Add directory to the search path for .xmod files.");
    app.add_flag("-d", res->debug_enabled, "Enable debug mode");
    app.add_flag("--yd", res->yacc_debug_enabled, "Enable YACC debug mode");
    app.add_flag("--no-module-cache{false}", res->module_cache_enabled,
            "Always load module from file");
    app.add_flag("--module-compile", res->module_compile_enabled,
            "Only generate Xmod file");
    app.add_flag("--print-help", res->print_help, "Print help");
    app.add_flag("--print-opts", res->print_opts, "Print CLI options");
    app.add_flag("--fopenmp", res->omp_enabled, "Enable OpenMP translation");
    app.add_flag("--facc", res->acc_enabled, "Enable OpenACC translation");
    app.add_flag("--fxmp", res->xmp_enabled, "Enable XcalableMP translation");
    app.add_flag("--pgi", res->pgi_enabled, "Keep PGI directives");
    app.add_flag("--fno-xmp-coarray{false}", res->xmp_coarray_enabled,
            "Disable translation coarray statements to XcalableMP subroutine calls");
    app.add_flag("--Kscope-omp", res->cond_compile_enabled,
            "Enable conditional compilation");
    app.add_flag("--force-fixed-format", res->force_fixed_format_enabled,
            "Read file as fixed format");
    app.add_option("--max-line-length", res->max_line_len,
            "Set max columns in a line (default is 72 in fixed format and 132 in free format)");
    app.add_option("--max-cont-line", res->max_cont_line,
            "Set max number of continuation lines (default n=255)");
    app.add_flag("--force-c-comment", res->force_c_comments_enabled,
            "Enable 'c' comment in free format");
    auto f77Flag = app.add_flag("--f77", res->lang_f77,
            "Use F77 spec intrinsic");
    auto f90Flag = app.add_flag("--f90", res->lang_f90,
            "Use F90 spec intrinsic");
    auto f95Flag = app.add_flag("--f95", res->lang_f95,
            "Use F95 spec intrinsic");
    f77Flag->excludes(f90Flag, f95Flag);
    f90Flag->excludes(f77Flag, f95Flag);
    app.add_option_group("Language  options")->add_options(f77Flag, f90Flag,
            f95Flag);
    app.add_flag("-u", res->do_implicit_undef, "Use no implicit type");
    app.add_option("-r,--single-prec-size", res->default_single_real_type_size,
            "Set single precision size (default N=4)")->check(CLI::IsMember(
    { 4, 8 }));
    app.add_option("--double-prec-size", res->default_double_real_type_size,
            "Set double precision size (default N=8)")->check(CLI::IsMember(
    { 4, 8 }));
    app.add_option("--save", res->auto_save_attr_kb,
            "Add save attribute n kbytes except in a recursive function and common variables (default n=1)");
    app.add_option("--max-name-len", res->max_name_len,
            "Set maximum identifier name length");
    app.add_flag("--fdollar-ok", res->dollar_in_id_enabled,
            "Enable using '$' in identifier");
    app.add_flag("--fleave-comment", res->leave_comment_enabled,
            "Leave comment in xcodeml file");
    app.add_flag("--endlineno", res->end_line_no_enabled,
            "Output the endlineno attribute");
    app.add_flag("--ocl", res->ocl_enabled, "Enable ocl");
    app.add_flag("--cdir", res->cdir_enabled, "Enable cdir");
    app.add_flag("--no-time{false}", res->add_timestamp_enabled,
            "Do not add timestamp to xcodeml");
    app.add_option("-m,--input-xmod", res->xmod_inc_paths,
            "Add xmod file to import cache");
    app.add_flag("--in-memory-mode", res->native_in_mem_mode_enabled,
            "In this mode ffront library is not allowed to access files or IO streams. All external IO will be performed by ffront-cpp wrapper");
    app.add_option("--so,--stdout-file", res->stdout_file_path,
            "Path to file where stdout should be written");
    app.add_flag("--no-fsync{false}", res->fsync_enabled,
            "Do not explicitly flush output files to disk");
    app.add_flag("-v,--version", res->print_version,
            "Display frontend version information");
    app.add_flag("--vt,--version-tag", res->print_version_tag,
            "Display frontend version tag");

    auto opts = correctOptsStyle(argc, argv);
    try
    {
        app.parse(argc, toCOpts(opts).data());
    } catch (const CLI::CallForHelp &e)
    {
        app.exit(e);
        return {};
    } catch (const CLI::ParseError &e)
    {
        app.exit(e);
        throw;
    }
    normalizePath(workingDir, res->src_file_path);
    normalizePath(workingDir, res->out_file_path);
    normalizePath(workingDir, res->intrinsic_xmod_dir_path);
    normalizePath(workingDir, res->stdout_file_path);
    normalizePaths(workingDir, res->inc_dir_paths);
    normalizePaths(workingDir, res->xmod_inc_dir_paths);
    normalizePaths(workingDir, res->xmod_inc_paths);
    return res;
}

void CLIOptions::normalizePath(const string &workingDir,
        std::unique_ptr<std::string> &pathStr)
{
    if (pathStr)
    {
        *pathStr = toAbsPath(workingDir, *pathStr);
    }
}

void CLIOptions::normalizePaths(const string &workingDir,
        std::vector<std::string> &paths)
{
    for (auto &pathStr : paths)
    {
        pathStr = toAbsPath(workingDir, pathStr);
    }
}

string CLIOptions::toAbsPath(const string &workingDir, const string &pathStr)
{
    filesystem::path inPath
    { pathStr }, workingDirPath
    { workingDir };
    if (!inPath.is_absolute())
    {
        inPath = workingDirPath / inPath;
    }
    //inPath = filesystem::canonical(inPath); //Only works on existing paths
    return inPath;
}
