#include "utils.h"
#include "c_vector/vector.h"

void set_sds_string(sds_string* sds_str, const char* c_str)
{
    *sds_str = sdscpy(*sds_str, c_str);
}

void free_sds_string_vector(sds_string_vector* pv)
{
    size_t i, size = vector_size(*pv);
    for(i = 0; i < size; ++i)
    {
        sdsfree((*pv)[i]);
    }
    vector_free((*pv));
    *pv = NULL;
}

void copy_sds_string_vector(sds_string_vector* p_to, const sds_string_vector from)
{
    sds_string_vector to = *p_to;
    vector_clear(to);
    size_t i, size = vector_size(from);
    vector_reserve(to, size);
    for(i = 0; i < size; ++i)
    {
        vector_push_back(to, sdsnew(from[i]));
    }
    *p_to = to;
}

bool file_exists(const char* path)
{
    FILE* fp = fopen(path, "r");
    if (fp)
    {
        fclose(fp);
        return true;
    }
    else
    {
        return false;
    }
}

void init_str_stream(str_stream* strm)
{
    strm->type = NONE_STREAM;
    strm->handle = NULL;
}

void set_str_stream(str_stream* strm, stream_type type, FILE* handle)
{
    strm->type = type;
    strm->handle = handle;
}

void release_str_stream(str_stream* strm)
{
    if(strm->type != NONE_STREAM)
    {
        switch(strm->type)
        {
        case FILE_STREAM:
        case MEM_STREAM:
            fclose(strm->handle);
            break;
        default:
            break;
        }
        init_str_stream(strm);
    }
}

static bool open_file_str_stream(str_stream* strm, const char* file_path, const char* flags)
{
    strm->handle = fopen(file_path, flags);
    if (strm->handle != NULL)
    {
        strm->type = FILE_STREAM;
        return true;
    }
    else
    {
        return false;
    }
}

bool open_in_file_str_stream(str_stream* strm, const char* file_path)
{
    return open_file_str_stream(strm, file_path, "r");
}

bool open_out_file_str_stream(str_stream* strm, const char* file_path)
{
    return open_file_str_stream(strm, file_path, "w");
}

