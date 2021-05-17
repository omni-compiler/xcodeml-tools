/* Author: Mikhail Zhigun */
#ifndef IO_CACHE_HPP
#define IO_CACHE_HPP

#include "cli_options.hpp"
#include "io_cache.h"
#include <unordered_map>

class IOCache
{
public:
    IOCache(const CLIOptions &opts, const std::string& workingDir);
    ~IOCache();

    void addInputFile(const std::string &filename);
    void addInputFile(const std::string &filename, const std::string &fileName);
    void addOutputFile(const std::string &filename);
    void addOutputFile(const std::string &filename, const std::string &fileName);
    void update();

    io_cache get_data() const
    {
        return data;
    }

private:
    struct IOCacheInputEntry
    {
        IOCacheInputEntry(const std::string &filename, int fd,
                const void *mapStartAddr, size_t size);
        ~IOCacheInputEntry();

        const std::string filename;
        const int fd;
        const void *mapStartAddr;
        const size_t size;
    };
    class IOCacheOutputEntry
    {
    public:
        IOCacheOutputEntry(const std::string &filename);
        ~IOCacheOutputEntry();

        const void* get_map_Start_Addr() const
        {
            return mapStartAddr;
        }

        const size_t get_size() const
        {
            return size;
        }

        void update(io_cache data);

        const std::string filename;
    private:
        void *mapStartAddr;
        size_t size;
    };
    const bool flushFilesToDisk;
    std::unordered_map<std::string, IOCacheInputEntry> inputFiles;
    std::unordered_map<std::string, IOCacheOutputEntry> outputFiles;
    io_cache data;
};

#endif //IO_CACHE_HPP

