/* Author: Mikhail Zhigun */
#ifndef IO_CACHE_H
#define IO_CACHE_H

/* IO cache provides storage and lookup for input files (src, xmods etc.) mapped and stored in memory,
 * as well as output files wrapped in C streams.
   It is only implemented in c++ build because functionality is only relevant for JNI interface.
*/

#include "bool.h"
#include <stdio.h>

struct s_io_cache;
typedef struct s_io_cache* io_cache;

io_cache create_io_cache();
void free_io_cache(io_cache cache);
void io_cache_add_input_file(io_cache cache, const char* filename, void* startAddress, size_t size);
FILE* io_cache_get_input_file(io_cache cache, const char* filename);
bool io_cache_get_input_file_as_mem(io_cache cache, const char* filename, void** startAddress, size_t* size);
void io_cache_add_output_file(io_cache cache, const char* filename);
FILE* io_cache_get_output_file(io_cache cache, const char* filename);
void io_cache_get_output_file_as_mem(io_cache cache, const char* filename, void** startAddress, size_t* size);
void debug_print_io_cache(io_cache cache);

#endif //IO_CACHE_H
