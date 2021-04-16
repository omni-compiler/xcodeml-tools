#ifndef OMNI_ERRORS_H
#define OMNI_ERRORS_H

#define FATAL_ERROR() on_fatal_error(NULL, __FILE__, __LINE__)
#define FATAL_ERROR_WITH_MSG(msg) on_fatal_error(msg, __FILE__, __LINE__)

#ifndef __cplusplus

#include <stdio.h>
#include <stdlib.h>

static void on_fatal_error(const char* msg,
                           const char* fileName,
                           const size_t lineNumber)
{
    if(msg)
    {
        fprintf(stderr, "\n%s:%zu: FATAL ERROR: %s\n", fileName, lineNumber, msg);
    }
    else
    {
        fprintf(stderr, "\n%s:%zu: FATAL ERROR\n", fileName, lineNumber);
    }
    fflush(stderr);
    abort();
}

#else

#include <cstddef>

void on_fatal_error(const char* msg, const char *fileName, const size_t lineNumber);

#endif

#endif //OMNI_ERRORS_H
