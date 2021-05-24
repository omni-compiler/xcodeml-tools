/* Author: Mikhail Zhigun */
#include "io_cache.hpp"
#include "omni_errors.h"
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <cstring>
#include <iostream>
#if __cplusplus > 201703L
#include <filesystem>
namespace std_fs = std::filesystem;
#else
#include <experimental/filesystem>
namespace std_fs = std::experimental::filesystem;
#endif

IOCache::IOCache(const CLIOptions &opts, const std::string &workingDir) :
        data
        { create_io_cache() }, flushFilesToDisk
        { opts.get_fsync_enabled() ? *opts.get_fsync_enabled() : true }
{
    if (opts.get_src_file_path())
    {
        addInputFile(*opts.get_src_file_path());
    }
    if (opts.get_stdout_file_path())
    {
        addOutputFile(*opts.get_stdout_file_path(), "stdout");
    }
    else
    {
        addOutputFile("stdout", "stdout");
    }
    if (opts.get_out_file_path())
    {
        addOutputFile(*opts.get_out_file_path());
    }
    for (const auto &xmodPath : opts.get_xmod_inc_paths())
    {
        addInputFile(xmodPath, std_fs::path
        { xmodPath }.filename());
    }
}

IOCache::~IOCache()
{
    free_io_cache(data);
}

void IOCache::addInputFile(const std::string &filePath)
{
    addInputFile(filePath, filePath);
}

void IOCache::addInputFile(const std::string &filePath,
        const std::string &filename)
{
    if (inputFiles.find(filePath) == inputFiles.end())
    {
        const int fd = open(filePath.c_str(), O_RDONLY);
        if (fd < 0)
        {
            FATAL_ERROR_WITH_MSG(("Failed to open file " + filePath).c_str());
        }
        const size_t size = std_fs::file_size(filePath);
        void *mapStartAddr = mmap(NULL, size, PROT_READ, MAP_SHARED, fd, 0);
        if (mapStartAddr == MAP_FAILED)
        {
            FATAL_ERROR_WITH_MSG(("Failed to map file " + filePath).c_str());
        }
        inputFiles.emplace(std::piecewise_construct,
                std::forward_as_tuple(filePath),
                std::forward_as_tuple(filename, fd, mapStartAddr, size));

        io_cache_add_input_file(data, filename.c_str(), mapStartAddr, size);
    }
}

void IOCache::addOutputFile(const std::string &filePath)
{
    addOutputFile(filePath, filePath);
}

void IOCache::addOutputFile(const std::string &filePath,
        const std::string &filename)
{
    if (outputFiles.find(filePath) == outputFiles.end())
    {
        outputFiles.emplace(std::piecewise_construct,
                std::forward_as_tuple(filePath),
                std::forward_as_tuple(filename));
        io_cache_add_output_file(data, filename.c_str());
    }
}

IOCache::IOCacheInputEntry::IOCacheInputEntry(const std::string &filename,
        int fd, const void *mapStartAddr, size_t size) :
        filename
        { filename }, fd
        { fd }, mapStartAddr
        { mapStartAddr }, size
        { size }
{
}

IOCache::IOCacheInputEntry::~IOCacheInputEntry()
{
    munmap((void*) mapStartAddr, size);
    close(fd);
}

IOCache::IOCacheOutputEntry::IOCacheOutputEntry(const std::string &filename) :
        filename
        { filename }, mapStartAddr
        { nullptr }, size
        { 0 }
{
}

IOCache::IOCacheOutputEntry::~IOCacheOutputEntry()
{
}

void IOCache::update()
{
    for (auto &it : outputFiles)
    {
        const auto &filePath = it.first;
        auto &e = it.second;
        e.update(data);
        if (filePath == "stdout")
        {
            std::cout.write(reinterpret_cast<const char*>(e.get_map_Start_Addr()), e.get_size());
            std::cout.flush();
        }
        else
        {
            const int fd = open(filePath.c_str(), O_CREAT | O_RDWR,
            S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP);
            if (fd < 0)
            {
                FATAL_ERROR_WITH_MSG("Failed to open file " + filePath);
            }
            const size_t size = e.get_size();
            if (posix_fallocate(fd, 0, size) != 0)
            {
                FATAL_ERROR_WITH_MSG("Failed to allocate file " + filePath);
            }
            void *mapStartAddr = mmap(NULL, size, PROT_WRITE, MAP_SHARED, fd,
                    0);
            if (mapStartAddr == MAP_FAILED)
            {
                FATAL_ERROR_WITH_MSG("Failed to map file " + filePath);
            }
            memcpy(mapStartAddr, e.get_map_Start_Addr(), size);
            munmap(mapStartAddr, size);
            close(fd);
        }
    }
}

void IOCache::IOCacheOutputEntry::update(io_cache data)
{
    io_cache_get_output_file_as_mem(data, filename.c_str(), &mapStartAddr,
            &size);
}
