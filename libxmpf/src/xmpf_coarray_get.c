/*
 *   COARRAY GET
 *
 */

#include <stdarg.h>
#include "xmpf_internal.h"

// communication schemes
#define GETSCHEME_Normal        0
#define GETSCHEME_RecvBuffer    1

static int _select_getscheme(char *result);

static void _getCoarray(int serno, char *baseAddr, int coindex, char *res,
                        int bytes, int rank, int skip[], int count[]);

static char *_getVectorIter(int serno, char *baseAddr, int bytes,
                            int coindex, char *dst,
                            int loops, int skip[], int count[]);

static void _getVectorByByte(int serno, char *baseAddr, int bytes,
                             int coindex, char *dst);
static void _getVectorByElement(char *desc, int start, int vlength,
                                int coindex, char *dst);


/***************************************************\
    entry
\***************************************************/

extern void xmpf_coarray_get_scalar_(int *serno, char *baseAddr, int *element,
                                     int *coindex, char *result)
{
  char *buf;

  int scheme = _select_getscheme(result);

  char *desc = _XMPF_get_coarrayDesc(*serno);
  int start = _XMPF_get_coarrayStart(*serno, baseAddr);

  switch (scheme) {
  case GETSCHEME_Normal:
    _getVectorByElement(desc, start, 1, *coindex, result);
    break;

  case GETSCHEME_RecvBuffer:
    buf = malloc((size_t)ROUND_UP_BOUNDARY(*element));
    _getVectorByElement(desc, start, 1, *coindex, buf);
    (void)memcpy(result, buf, *element);
    break;

  default:
    _XMP_fatal("undefined scheme number in " __FILE__);
  }
}


extern void xmpf_coarray_get_array_(int *serno, char *baseAddr, int *element,
                                    int *coindex, char *result, int *rank, ...)
{
  size_t bufsize;
  char *buf, *p;
  int i, nelems;

  int scheme = _select_getscheme(result);

  char *nextAddr;
  int skip[MAX_RANK];
  int count[MAX_RANK];
  va_list argList;
  va_start(argList, rank);

  if (*element % BOUNDARY_BYTE != 0) {
    _XMP_fatal("violation of boundary in get communication");
    return;
  }

  for (int i = 0; i < *rank; i++) {
    nextAddr = va_arg(argList, char*);
    skip[i] = nextAddr - baseAddr;
    count[i] = *(va_arg(argList, int*));
  }

  int bytes = _XMPF_get_coarrayElement(*serno);

  switch (scheme) {
  case GETSCHEME_Normal:
    _getCoarray(*serno, baseAddr, *coindex, result, bytes, *rank, skip, count);
    break;

  case GETSCHEME_RecvBuffer:
    bufsize = *element;
    for (i = 0; i < *rank; i++) {
      bufsize *= count[i];
    }
    buf = malloc(bufsize);
    _getCoarray(*serno, baseAddr, *coindex, buf, bytes, *rank, skip, count);
    (void)memcpy(result, buf, bufsize);
    break;

  default:
    _XMP_fatal("undefined scheme number in " __FILE__);
  }
}


int _select_getscheme(char *result)
{
  int scheme = GETSCHEME_Normal;

#ifdef _XMP_COARRAY_FJRDMA
  // if the address of result may not be written in:
  scheme = GETSCHEME_RecvBuffer;
#endif

  return scheme;
}



void _getCoarray(int serno, char *baseAddr, int coindex, char *result,
                 int bytes, int rank, int skip[], int count[])
{
  if (rank == 0) {  // fully contiguous after perfect collapsing
    if (_XMPF_coarrayMsg)
      fprintf(stderr, "**** %d bytes fully contiguous (%s)\n",
              bytes, __FILE__);

    _getVectorByByte(serno, baseAddr, bytes, coindex, result);
    return;
  }

  if (bytes == skip[0]) {  // contiguous
    _getCoarray(serno, baseAddr, coindex, result,
                bytes * count[0], rank - 1, skip + 1, count + 1);
    return;
  }

  // not contiguous any more
  char* dst = result;

  if (_XMPF_coarrayMsg) {
    char work[200];
    char* p;
    sprintf(work, "**** get, %d-byte contiguous", bytes);
    p = work + strlen(work);
    for (int i = 0; i < rank; i++) {
      sprintf(p, ", %d %d-byte skips", count[i], skip[i]);
      p += strlen(p);
    }
    fprintf(stderr, "%s (%s)\n", work, __FILE__);
  }

  dst = _getVectorIter(serno, baseAddr, bytes, coindex, dst,
                       rank, skip, count);

  if (_XMPF_coarrayMsg) {
    fprintf(stderr, "**** end get\n");
  }
}

  
char *_getVectorIter(int serno, char *baseAddr, int bytes,
                     int coindex, char *dst,
                     int loops, int skip[], int count[])
{
  char* src = baseAddr;
  int n = count[loops - 1];
  int gap = skip[loops - 1];

  if (loops == 1) {
    for (int i = 0; i < n; i++) {
      _getVectorByByte(serno, src, bytes, coindex, dst);
      dst += bytes;
      src += gap;
    }
  } else {
    for (int i = 0; i < n; i++) {
      dst = _getVectorIter(serno, baseAddr + i * gap, bytes,
                           coindex, dst,
                           loops - 1, skip, count);
    }
  }
  return dst;
}


void _getVectorByByte(int serno, char *src, int bytes,
                      int coindex, char *dst)
{
  char* desc = _XMPF_get_coarrayDesc(serno);
  int start = _XMPF_get_coarrayStart(serno, src);
  // The element that was recorded when the data was allocated is used.
  int element = _XMPF_get_coarrayElement(serno);
  int vlength = bytes / element;

  _getVectorByElement(desc, start, vlength, coindex, dst);
}


void _getVectorByElement(char *desc, int start, int vlength,
                         int coindex, char *dst)
{
  _XMP_coarray_rdma_coarray_set_1(start, vlength, 1);    // coindexed-object
  _XMP_coarray_rdma_array_set_1(0, vlength, 1, 1, 1);    // result
  _XMP_coarray_rdma_node_set_1(coindex);
  _XMP_coarray_rdma_do(COARRAY_GET_CODE, desc, dst, NULL);
}

