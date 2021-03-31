#ifndef BOOL_H
#define BOOL_H

#include "config.h"

#ifndef __cplusplus
    #ifdef HAVE_STDBOOL_H
        #include <stdbool.h>
    #else
        typedef enum { false = 0, true = 1 } bool;
    #endif
#endif /* ! __cplusplus */

#endif //BOOL_H