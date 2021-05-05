/* Author: Mikhail Zhigun */
#include "io_cache.h"
#include "omni_errors.h"

#if !defined(__cplusplus)

io_cache create_io_cache()
{
    return NULL;
}

void free_io_cache(io_cache cache)
{}

void io_cache_add_input_file(io_cache cache, const char* filename, void* startAddress, size_t size)
{
    FATAL_ERROR_WITH_MSG("io_cache_add_input_file() not implemented in C build");
}

FILE* io_cache_get_input_file(io_cache cache, const char* filename)
{
    return NULL;
}

bool io_cache_get_input_file_as_mem(io_cache cache, const char* name, void** startAddress, size_t* size)
{
    *startAddress = NULL;
    *size = 0;
    return false;
}

void io_cache_add_output_file(io_cache cache, const char* filename)
{
    FATAL_ERROR_WITH_MSG("io_cache_add_output_file() not implemented in C build");
}

FILE* io_cache_get_output_file(io_cache cache, const char* filename)
{
    return NULL;
}

void io_cache_get_output_file_as_mem(io_cache cache, const char* filename, void** startAddress, size_t* size)
{
    FATAL_ERROR_WITH_MSG("io_cache_add_entry() not implemented in C build");
}

#endif

