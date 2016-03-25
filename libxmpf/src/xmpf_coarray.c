#include "xmpf_internal.h"

#define DIV_CEILING(m,n)  (((m)-1)/(n)+1)

/* Threshold of memory size to share in the pool
 */
#define POOL_THRESHOLD (40*1024*1024)          // 40MB

/* Size of the communication buffer prepared for short communications
 * to avoid allocation and registration every communication time
 */
#define COMM_BUFF_SIZE  (40000)                // ~40kB


/*****************************************\
  static vars and functions
\*****************************************/

static int _XMPF_coarrayMsg = 0;          // default: message off
static int _XMPF_coarrayMsg_last;         // for _XMPF_set/reset_coarrayMsg()

//static int _XMPF_coarrayErr = 0;          // default: aggressive error check off
static unsigned _XMPF_poolThreshold = POOL_THRESHOLD;
static size_t _XMPF_commBuffSize = COMM_BUFF_SIZE;

static void _set_coarrayMsg(int sw)
{
  switch (sw) {
  case 0:
  default:
    if (_XMPF_coarrayMsg)
      _XMPF_coarrayDebugPrint("Switch _XMPF_coarrayMsg=0. Bye!\n");
    _XMPF_coarrayMsg = 0;
    return;

  case 1:
    if (_XMPF_coarrayMsg == 0)
      _XMPF_coarrayDebugPrint("Switch _XMPF_coarrayMsg=1.\n");
    _XMPF_coarrayMsg = 1;
    break;
  }
}


/*****************************************\
  static vars and functions
\*****************************************/

int _XMPF_get_coarrayMsg(void)
{
  return _XMPF_coarrayMsg;
}


void _XMPF_set_coarrayMsg(int sw)
{
  _XMPF_coarrayMsg_last = _XMPF_coarrayMsg;
  _XMPF_coarrayMsg = sw;
}

void _XMPF_reset_coarrayMsg(void)
{
  _XMPF_coarrayMsg = _XMPF_coarrayMsg_last;
}


void XMPF_set_poolThreshold(unsigned size)
{
  _XMPF_poolThreshold = size;

  _XMPF_coarrayDebugPrint("set _XMPF_poolThreshold = %u\n",
                          _XMPF_poolThreshold);
}


unsigned XMPF_get_poolThreshold(void)
{
  return _XMPF_poolThreshold;
}

size_t XMPF_get_commBuffSize(void)
{
  return _XMPF_commBuffSize;
}


/*****************************************\
  hidden API
\*****************************************/

/*
 *  hidden subroutine interface,
 *   which can be used in the user program
 */
void xmpf_coarray_msg_(int *sw)
{
  _set_coarrayMsg(*sw);
}


/*****************************************\
  initialization called in xmpf_main
\*****************************************/

/*  1. set static variable _this_image and _num_nodes
 *  2. read environment variable XMPF_COARRAY_MSG and set _XMPF_coarrayMsg
 *     usage: <v1><d><v2><d>...<vn>
 *        <vk>  value for image index k
 *        <d>   delimiter ',' or ' '
 *  3. read environmrnt variable XMPF_COARRAY_POOL and set
 *     _XMPF_poolThreshold
 *     usage: [0-9]+[kKmMgG]?
 */
void _XMPF_coarray_init(void)
{
  /*
   *  set who-am-i
   */
  _XMPF_set_initial_this_image();
  _XMPF_set_initial_num_images();

  /*
   * read environment variables
   */
  char *tok, *work, *env1, *env2;
  int i, stat;
  char delim[] = ", ";
  unsigned len;
  char c;

  env1 = getenv("XMPF_COARRAY_MSG");
  if (env1 != NULL) {
    work = strdup(env1);
    tok = strtok(work, delim);
    for (i = 1; tok != NULL; i++, tok = strtok(NULL, delim)) {
      if (_XMPF_get_current_this_image() == i)
        _set_coarrayMsg(atoi(tok));
    }
  }

  env2 = getenv("XMPF_COARRAY_POOL");
  if (env2 != NULL) {
    work = strdup(env2);
    stat = sscanf(work, "%u%c", &len, &c);

    switch (stat) {
    case EOF:
    case 0:
      // use default value of poolThread
      break;

    case 1:
      XMPF_set_poolThreshold(len);
      break;

    case 2:
      switch (c) {
      case 'k':
      case 'K':
        XMPF_set_poolThreshold(len * 1024);
        break;
      case 'm':
      case 'M':
        XMPF_set_poolThreshold(len * 1024 * 1024);
        break;
      case 'g':
      case 'G':
        XMPF_set_poolThreshold(len * 1024 * 1024 * 1024);
        break;
      default:
        _XMPF_coarrayFatal("Usage of XMPF_COARRAY_POOL: [0-9]+[kKmMgG]?");
        break;
      }
      break;

    default:
      _XMPF_coarrayFatal("Illegal value of environ variable XMPF_COARRAY_POOL.\n"
                         "  Usage: [0-9]+[kKmMgG]?");
      break;
    }
  }

  _XMPF_coarrayDebugPrint("Execution time environment\n"
                          "   communication layer  :  %s\n"
                          "   coarray boundary     :  %u bytes\n"
                          "   environment vars     :  XMPF_COARRAY_MSG=%s\n"
                          "                           XMPF_COARRAY_POOL=%s\n"
                          "   _XMPF_coarrayMsg     :  %d\n"
                          "   _XMPF_poolThreshold  :  %u bytes\n"
                          "   _XMPF_commBuffSize   :  %u bytes\n",
                          ONESIDED_COMM_LAYER, ONESIDED_BOUNDARY,
                          env1 ? env1 : "", env2 ? env2 : "",
                          _XMPF_get_coarrayMsg(),
                          XMPF_get_poolThreshold(),
                          XMPF_get_commBuffSize()
                          );
}


/* NOT USED
 */
void _XMPF_coarray_finalize(void)
{
  xmpf_sync_all_auto_();
}


/*****************************************\
  restriction checker
\*****************************************/

int _XMPF_nowInTask()
{
  return xmp_num_nodes() < xmp_all_num_nodes();
}

void _XMPF_checkIfInTask(char *msgopt)
{
  if (_XMPF_nowInTask())
    _XMPF_coarrayFatal("current restriction: "
                       "cannot use %s in any task construct\n",
                       msgopt);
}

void xmpf_coarray_fatal_with_len_(char *msg, int *msglen)
{
  _XMPF_coarrayFatal("FATAL ERROR: %*s\n", *msglen, msg);
}

void _XMPF_coarrayFatal(char *format, ...)
{
  char work[300];
  va_list list;
  va_start(list, format);
  vsprintf(work, format, list);
  fprintf(stderr, "CAF[%d] %s", _XMPF_get_current_this_image(), work);
  va_end(list);

  //xmpf_finalize_each__();   This causes deadlock sometimes.

  _XMP_fatal("fatal error detected by Coarray/F runtime.");
}

void _XMPF_coarrayDebugPrint(char *format, ...)
{
  if (!_XMPF_coarrayMsg)
    return;

  char work[300];
  va_list list;
  va_start(list, format);
  vsprintf(work, format, list);
  fprintf(stderr, "CAF[%d] %s", _XMPF_get_current_this_image(), work);
  va_end(list);
}

