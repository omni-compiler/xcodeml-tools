/* Author: Mikhail Zhigun */
#include "io_cache.h"
#include "omni_errors.h"
#include <unordered_map>
#include <string>
#include <iostream>

namespace
{
    struct InputEntry
    {
        void* start;
        size_t size;
    };
    struct OutputEntry
    {
        OutputEntry(char* start, size_t size, FILE* handle):
            start{start},
            size{size},
            handle{handle}
        {}
        OutputEntry(const OutputEntry&) = delete;
        OutputEntry(OutputEntry&& e):
            start{e.start},
            size{e.size},
            handle{e.handle}
        {
            e.start = nullptr;
            e.size = 0;
            e.handle = nullptr;
        }
        char* start;
        size_t size;
        FILE* handle;
        ~OutputEntry()
        {
            if(start)
            {
                free(start);
            }
        }
    };
}

struct s_io_cache
{
    std::unordered_map<std::string, InputEntry> in_entries;
    std::unordered_map<std::string, OutputEntry> out_entries;
};

io_cache create_io_cache()
{
    return new s_io_cache;
}

void free_io_cache(io_cache cache)
{
    delete cache;
}

void io_cache_add_input_file(io_cache cache, const char* name, void* start, size_t size)
{
    cache->in_entries[std::string{name}] = InputEntry{start, size};
}

FILE* io_cache_get_input_file(io_cache cache, const char* name)
{
    auto entryIt = cache->in_entries.find(std::string{name});
    if(entryIt != cache->in_entries.end())
    {
        const auto& entry = (*entryIt).second;
        return fmemopen(entry.start, entry.size, "r");
    }
    else
    {
        return nullptr;
    }
}

bool io_cache_get_input_file_as_mem(io_cache cache, const char* name, void** startAddress, size_t* size)
{
    auto entryIt = cache->in_entries.find(std::string{name});
    if(entryIt != cache->in_entries.end())
    {
        const auto& entry = (*entryIt).second;
        *startAddress = entry.start;
        *size = entry.size;
        return true;
    }
    else
    {
        *startAddress = nullptr;
        *size = 0;
        return false;
    }
}

void io_cache_add_output_file(io_cache cache, const char* name)
{
    auto& entry = (*cache->out_entries.emplace(std::string{name}, OutputEntry{nullptr, 0, nullptr}).first).second;
    entry.handle = open_memstream(&entry.start, &entry.size);
}

FILE* io_cache_get_output_file(io_cache cache, const char* name)
{
    auto entryIt = cache->out_entries.find(std::string{name});
    if(entryIt != cache->out_entries.end())
    {
        const auto& entry = (*entryIt).second;
        return entry.handle;
    }
    else
    {
        return nullptr;
    }
}

void io_cache_get_output_file_as_mem(io_cache cache, const char* name, void** startAddress, size_t* size)
{
    auto entryIt = cache->out_entries.find(std::string{name});
    if(entryIt != cache->out_entries.end())
    {
        const auto& entry = (*entryIt).second;
        *startAddress = reinterpret_cast<void*>(entry.start);
        *size = entry.size;
    }
    else
    {
        *startAddress = nullptr;
        *size = 0;
    }
}


void debug_print_io_cache(io_cache cache)
{
    std::cerr << "Input entries:" << std::endl;
    for (auto const& it : cache->in_entries)
    {
        std::cerr << it.first  << " : " << it.second.start << " " << it.second.size << std::endl;
    }
    std::cerr << "Output entries:" << std::endl;
    for (auto const& it : cache->out_entries)
    {
        std::cerr << it.first  << " : " << it.second.start << " " << it.second.size << std::endl;
    }
    std::cerr << std::endl;
}


