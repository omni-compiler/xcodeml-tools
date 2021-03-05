#ifndef _C_OMP_H
#define _C_OMP_H

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

enum OMP_pragma {
    OMP_NONE = 0,
    OMP_PARALLEL = 1, 		/* parallel <clause_list> */
    OMP_FOR = 2,		/* loop <clause_list> */
    OMP_SECTIONS = 3,		/* sections <clause_list> */
    OMP_SECTION = 4,		/* section */
    OMP_SINGLE = 5,		/* single <clause list> */
    OMP_MASTER = 6,		/* master */
    OMP_CRITICAL = 7,		/* critical <name> <hint> */
    OMP_BARRIER = 8,		/* barrier */
    OMP_ATOMIC = 9,		/* atomic */
    OMP_FLUSH = 10,		/* flush <namelist> */
    OMP_ORDERED = 11,		/* ordered <clause> */
    OMP_THREADPRIVATE = 12,	/* threadprivate <namelist> */

    OMP_PARALLEL_FOR = 13, 	/* parallel for <clause_list> */
    OMP_PARALLEL_LOOP = 13,
    OMP_PARALLEL_SECTIONS = 14,	/* parallel sections <clause_list> */

    OMP_SIMD = 20,		/* simd <clause_list> */
    OMP_LOOP_SIMD = 21,		/* for simd <clause_list> */
    OMP_PARALLEL_LOOP_SIMD = 22,/* parallel for simd <clause_list> */
    OMP_DECLARE_SIMD = 23,	/* declare simd <clause_list> */

    OMP_DISTRIBUTE = 30,	/* distribute <clause_list> */
    OMP_DISTRIBUTE_SIMD = 31,	/* distribute simd <clause_list> */
    OMP_DISTRIBUTE_PARALLEL_LOOP = 32,			/* distribute parallel for <clause_list> */
    OMP_DISTRIBUTE_PARALLEL_LOOP_SIMD = 33,		/* distribute parallel for simd <clause_list> */

    OMP_TEAMS = 40,		/* teams <clause_list> */
    OMP_TEAMS_DISTRIBUTE = 41,				/* teams distribute <clause_list> */
    OMP_TEAMS_DISTRIBUTE_SIMD = 42,			/* teams distribute simd <clause_list> */
    OMP_TEAMS_DISTRIBUTE_PARALLEL_LOOP = 43,		/* teams distribute parallel for <clause_list> */
    OMP_TEAMS_DISTRIBUTE_PARALLEL_LOOP_SIMD = 44,	/* teams distribute parallel for simd <clause_list> */

    OMP_TARGET = 50,		/* target <clause_list> */
    OMP_TARGET_DATA = 51,	/* target data <clause_list> */
    OMP_TARGET_ENTER_DATA = 52,	/* target enter data <clause_list> */
    OMP_TARGET_EXIT_DATA = 53,	/* target exit data <clause_list> */
    OMP_TARGET_UPDATE = 54,	/* target update <clause_list> */

    OMP_TARGET_SIMD = 55,				/* target simd <clause_list> */
    OMP_TARGET_PARALLEL = 56,				/* target parallel <clause_list> */
    OMP_TARGET_PARALLEL_LOOP = 57,			/* target parallel for <clause_list> */
    OMP_TARGET_PARALLEL_LOOP_SIMD = 58,			/* target parallel for simd <clause_list> */

    OMP_TARGET_TEAMS = 59,				/* target teams <clause_list> */
    OMP_TARGET_TEAMS_DISTRIBUTE = 60,			/* target teams distribute <clause_list> */
    OMP_TARGET_TEAMS_DISTRIBUTE_SIMD = 61,		/* target teams distribute simd <clause_list> */
    OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_LOOP = 62,	/* target teams distribute parallel for <clause_list> */
    OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_LOOP_SIMD = 63,/* target teams distribute parallel for simd <clause_list> */
    OMP_DECLARE_TARGET = 64,	/* declare target <extended-list> <clause_list> */

    OMP_TASK = 70,		/* task <clause_list> */
    OMP_TASKLOOP = 71,		/* taskloop <clause_list> */
    OMP_TASKLOOP_SIMD = 72,	/* taskloop simd <clause_list> */
    OMP_TASKYIELD = 73,		/* taskyield */
    OMP_TASKWAIT = 74,		/* taskwait */
    OMP_TASKGROUP = 75,		/* taskgroup */
    OMP_CANCEL= 77,		/* cancel <type> <if-clause> */
    OMP_CANCELLATION_POINT= 78,	/* cancellation point <type> */
    OMP_DECLARE_REDUCTION= 79,	/* declare reduction <id> <type-list> <combiner> <init-clause> */
    OMP_DECLARE_TARGET_START = 80, /* declare target */
    OMP_DECLARE_TARGET_END = 81,   /* end declare target */
};

#define IS_OMP_PRAGMA_CODE(code) (((int)(code)) < 100)

enum OMP_pragma_clause {
    OMP_CLAUSE_NONE = 0, // reserved for none
    OMP_DATA_DEFAULT = 1,
    OMP_DATA_PRIVATE = 2,		/* private <namelist> */
    OMP_DATA_SHARED = 3,		/* shared <namelist> */
    OMP_DATA_FIRSTPRIVATE=4,
    OMP_DATA_LASTPRIVATE=5,
    OMP_DATA_COPYIN =6,

    OMP_DATA_REDUCTION_PLUS	=10,
    OMP_DATA_REDUCTION_MINUS	=11,
    OMP_DATA_REDUCTION_MUL	=12,
    OMP_DATA_REDUCTION_BITAND	=13,
    OMP_DATA_REDUCTION_BITOR	=14,
    OMP_DATA_REDUCTION_BITXOR	=15,
    OMP_DATA_REDUCTION_LOGAND	=16,
    OMP_DATA_REDUCTION_LOGOR	=17,
    OMP_DATA_REDUCTION_MIN	=18,
    OMP_DATA_REDUCTION_MAX	=19,

    OMP_DIR_ORDERED=20,
    OMP_DIR_IF=21,
    OMP_DIR_NOWAIT=22,
    OMP_DIR_SCHEDULE=23,
    OMP_DIR_NUM_THREADS=24,
    
    OMP_COLLAPSE=25,
    OMP_DEPEND=26,

    OMP_DATA_LINEAR=27,
    OMP_DATA_COPYPRIVATE=28,
    OMP_TARGET_DATA_MAP=29,
    OMP_DATA_DEFAULTMAP=30,
    OMP_DECLARE_TARGET_TO=31,
    OMP_DATA_DECALRE_LINK=32,
    OMP_TARGET_DEVICE=33,
    OMP_TARGET_SHADOW=34,
    OMP_TARGET_LAYOUT=35,

    OMP_FINAL = 36,
    OMP_PRIORITY = 37,
    OMP_UNTIED = 38,
    OMP_MERGEABLE = 39,
    OMP_GRAINSIZE = 40,
    OMP_NUM_TASKS = 41,
    OMP_NOGROUP = 42,
    OMP_IS_DEVICE_PTR = 43,
    OMP_USE_DEVICE_PTR = 44,
    OMP_TARGET_UPDATE_TO = 45,
    OMP_TARGET_UPDATE_FROM = 46,
    OMP_NUM_TEAMS = 47,
    OMP_THREAD_LIMIT = 48,
    OMP_DIST_SCHEDULE = 49,
    OMP_PROC_BIND = 50,
    OMP_SAFELEN = 51,
    OMP_SIMDLEN = 52,
    OMP_ALIGNED = 53,
};

enum OMP_sched_clause {
    OMP_SCHED_NONE = 0,
    OMP_SCHED_STATIC = 1,
    OMP_SCHED_DYNAMIC = 2,
    OMP_SCHED_GUIDED = 3,
    OMP_SCHED_RUNTIME = 4,
    OMP_SCHED_AFFINITY = 5,
    OMP_SCHED_AUTO = 6,
};

enum OMP_sched_modifier {
    OMP_SCHED_MODIFIER_NONE = 0,
    OMP_SCHED_MODIFIER_MONOTONIC = 1,
    OMP_SCHED_MODIFIER_NONMONOTONIC = 2,
    OMP_SCHED_MODIFIER_SIMD = 3,
};

enum OMP_data_default {
    OMP_DEFAULT_NONE = 0,
    OMP_DEFAULT_SHARED = 1,
    OMP_DEFAULT_PRIVATE = 2
};

enum OMP_proc_bind_clause {
     OMP_PROC_BIND_NONE = 0,
     OMP_PROC_BIND_MASTER = 1,
     OMP_PROC_BIND_CLOSE = 2,
     OMP_PROC_BIND_SPREAD = 3,
};

enum OMP_linear_modifier {
     OMP_LINEAR_MODIFIER_NONE = 0,
     OMP_LINEAR_MODIFIER_VAL = 1,
};

typedef enum {
    OMP_DATA_MAP_UNKNOWN = 0,
    OMP_DATA_MAP_TO = 1,
    OMP_DATA_MAP_FROM =	2,
    OMP_DATA_MAP_TOFROM	= 3,
    OMP_DATA_MAP_ALLOC = 4,
    OMP_DATA_MAP_RELEASE = 5,
    OMP_DATA_MAP_DELETE = 6,
    OMP_DATA_MAP_ALWAYS = 7,
} OMP_map_type;
#define N_OMP_DATA_MAP	(((int)OMP_DATA_MAP_ALWAYS) + 1)

typedef enum {
    OMP_DEPEND_UNKNOWN = 0,
    OMP_DEPEND_IN = 1,
    OMP_DEPEND_OUT = 2,
    OMP_DEPEND_INOUT = 3,
    OMP_DEPEND_SINK = 4,
    OMP_DEPEND_SOURCE = 5,
    OMP_DEPEND_MUTEXINOUTSET = 6,
    OMP_DEPEND_DEPOBJ = 7,
    OMP_DEPEND_ITERATOR = 8,
} OMP_depend_type;
#define N_OMP_DEPEND	(((int)OMP_DEPEND_ITERATOR) + 1)

	
/* protype */
char *ompDirectiveName(int c);
char *ompClauseName(int c);
char *ompScheduleName(int c);
char *ompDataDefaultName(int c);

const char *ompMapClauseTypeString(OMP_map_type mt);
const char *ompDependClauseTypeString(OMP_depend_type dt);
	
CExpr* lexParsePragmaOMP(char *p, int *token);
void out_OMP_PRAGMA(FILE *fp, int indent, int code, CExpr* expr);

#endif
