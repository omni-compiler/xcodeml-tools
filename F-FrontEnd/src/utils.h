#ifndef _F_UTILS_H_
#define _F_UTILS_H_

#include "sds/sds.h"
#include "bool.h"
#include <stdio.h>

#if defined(__cplusplus)
    #define THREAD_LOCAL thread_local
#elif __GNUC__
    #define THREAD_LOCAL __thread
#elif __STDC_VERSION__ >= 201112L
    #define THREAD_LOCAL _Thread_local
#elif defined(_MSC_VER)
    #define THREAD_LOCAL __declspec(thread)
#else
    #error Cannot define THREAD_LOCAL
#endif

typedef sds sds_string;
typedef sds* sds_string_vector;

void set_sds_string(sds_string* sds_str, const char* c_str);
void copy_sds_string_vector(sds_string_vector* to, const sds_string_vector from);
void free_sds_string_vector(sds_string_vector*);

bool file_exists(const char* path);

typedef enum {
    NONE_STREAM = 0,
    FILE_STREAM = 1,
    STD_IO_STREAM = 2,
    MEM_STREAM = 3
} stream_type;

typedef struct {
    stream_type type;
    FILE* handle;
} str_stream;

void init_str_stream(str_stream*);
void set_str_stream(str_stream*, stream_type type, FILE* handle);
void release_str_stream(str_stream*);
bool open_in_file_str_stream(str_stream*, const char* file_path);
bool open_out_file_str_stream(str_stream*, const char* file_path);

#endif //_F_UTILS_H_
