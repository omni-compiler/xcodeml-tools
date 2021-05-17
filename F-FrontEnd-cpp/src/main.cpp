#include "cli_options.hpp"
#include "app_constants.hpp"
#if __cplusplus > 201703L
#include <filesystem>
namespace std_fs = std::filesystem;
#else
#include <experimental/filesystem>
namespace std_fs = std::experimental::filesystem;
#endif

#include <iostream>

extern "C"
{
int execute_args(int argc, char *argv[]);
}


int main(int argc, char *argv[])
{
    const auto WORKING_DIR = static_cast<std::string>(std_fs::current_path());
    auto opts = CLIOptions::parseCmdlineArguments(argc, argv, WORKING_DIR);
    if (opts)
            {
                if(opts->getPrintVersion())
                {
                    std::cout<<AppConstants::PACKAGE_VERSION;
                    std::cout.flush();
                    return 0;
                }
                else if(opts->getPrintVersionTag())
                {
                    std::cout<<AppConstants::PACKAGE_VERSION_TAG;
                    std::cout.flush();
                    return 0;
                }
                return execute_args(argc, argv);
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
            } else
            {
                return 0;
            }
}
