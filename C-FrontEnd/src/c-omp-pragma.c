/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil ; -*- */
#include <sys/param.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>

#include "c-expr.h"
#include "c-pragma.h"
#include "c-parser.h"
#include "c-const.h"
#include "c-option.h"
#include "c-omp.h"

/*
 * <OMPPragma> <string> directive_name </string> 
 *             [clauses] [body] </OMPPragma>
 * [clauses] = <list> [clause] </list>
 *   C_Front: (direcive clause1 clause2 ... )
 *
 * [data_clause] = 
 *     <list> <string> [data_clause_name] </string> [name_list] </list>
 *     [data_clause_name] = DATA_PRIVATE|OMP_DATA_SHARED|
 *               OMP_DATA_FIRSTPRIVATE|OMP_DATA_LASTPRIVATE|OMP_DATA_COPYIN|
 *               OMP_DATA_REDUCTION_***
 *     [name_list] = <list> variable ... </list>
 *  C_Front: (data_clause_name (LIST ident ... ))
 * 
 * [default_clause] = 
 *      <list> <string> OMP_DATA_DEFAULT </string> 
 *           <string> OMP_DEFAULT_*** </string> </list>
 *  C_Front: (OMP_DATA_DEFAULT (OMP_DEFAULT_*** null))
 * 
 * [if_clause] = <list> <string> OMP_DIR_IF </string> cond_expr </list>
 *  C_Front: (OMP_DIR_IF cond_expr)
 *
 * [schedule_clause] = 
 *       <list> <string> OMP_DIR_SCHEDULE </string>
 *           <list> <string> OMP_SCHED_*** </string> expr </list> </list>
 *  C_Front: (OMP_DIR_SCHEDULE (OMP_SCHED_*** expr))
 *
 * [ordered_clause] = <list> <string> OMP_DIR_ORDERED </strign> null </list>
 *  C_Front: (OMP_DIR_ORDERED null) 
 *
 * [nowait_clause] = <list> <string> OMP_DIR_NOWAIT </strign> null </list>
 *  C_Front: (OMP_DIR_NOWAIT null) 
 *
 * [num_threads_clause] = 
 *    <list> <string> OMP_DIR_NUM_THREADS </strign> expr </list>
 *  C_Front: (OMP_DIR_NUM_THREADS expr) 
 *
 */

static int parse_OMP_pragma(void);
static CExpr* parse_OMP_clauses(void);
static CExpr* parse_OMP_namelist(void);
static CExpr* parse_OMP_reduction_namelist(int *r);
static CExpr* parse_OMP_array_list(void);
static CExpr* parse_OMP_to(int *r);
static CExpr* parse_OMP_dist_schedule(void);
static CExpr* parse_OMP_proc_bind(void);
static CExpr* parse_OMP_if(int *r);
static CExpr* parse_OMP_aligned(void);
static CExpr* parse_OMP_linear_namelist(void);
static CExpr* parse_OMP_linear(void);

static int parse_OMP_linear_list(CExpr** modifiers,
                                 CExpr** linear_list);
static int parse_OMP_target_pragma(void);
static int parse_OMP_teams_pragma(void);
static int parse_OMP_distribute_pragma(void);
static int parse_OMP_parallel_for_SIMD_pragma(void);
static int parse_OMP_if_directive_name_modifier(int *r);
static int parse_OMP_declare_pragma(void);
static int parse_OMP_end_pragma(void);

#define OMP_PG_LIST(pg,args) _omp_pg_list(pg,args)

#define OMP_CLAUSE_DEVICE    0
#define OMP_CLAUSE_SHADOW    1

#define MAX_NUM_SCHEDULE_MODIFIER (2)

/*
 * map clause type - string
 */
typedef struct {
  OMP_map_type t;
  const char *map_type;
} map_type_entry_t;

map_type_entry_t map_table[] = {
  { OMP_DATA_MAP_UNKNOWN, "" },
  { OMP_DATA_MAP_TO, "to" },
  { OMP_DATA_MAP_FROM, "from" },
  { OMP_DATA_MAP_TOFROM, "tofrom" },
  { OMP_DATA_MAP_ALLOC, "alloc" },
  { OMP_DATA_MAP_RELEASE, "release" },
  { OMP_DATA_MAP_DELETE, "delete" },
  { OMP_DATA_MAP_ALWAYS, "always" },
  { -1, NULL }
};

static inline int get_map_type_enum(const char *str)
{
  OMP_map_type mt = OMP_DATA_MAP_UNKNOWN;
  map_type_entry_t *e = map_table;

  while ((int)(e->t) >= 0) {
    if (strcasecmp(e->map_type, str) == 0) {
      mt = e->t;
      break;
    }
    e++;
  }

  if (mt == OMP_DATA_MAP_ALWAYS) {
    return 1000;
  } else {
    return ((int)mt > 0) ? (int)mt : -1;
  }
}

const char *ompMapClauseTypeString(OMP_map_type mt)
{
  if ((int)mt >= 0 && (int)mt < N_OMP_DATA_MAP) {
    map_type_entry_t *e = &(map_table[(int)mt]);
    return e->map_type;
  }
  return NULL;
}

/*
 * depend clause type - string
 */
typedef struct {
  OMP_depend_type t;
  const char *depend_type;
} depend_type_entry_t;

depend_type_entry_t depend_table[] = {
  { OMP_DEPEND_UNKNOWN, "" },
  { OMP_DEPEND_IN, "in" },
  { OMP_DEPEND_OUT, "out" },
  { OMP_DEPEND_INOUT, "inout" },
  { OMP_DEPEND_SINK, "sink" },
  { OMP_DEPEND_SOURCE, "source" },
  { OMP_DEPEND_MUTEXINOUTSET, "mutexinoutset" },
  { OMP_DEPEND_DEPOBJ, "depobj" },
  { OMP_DEPEND_ITERATOR, "iterator" },
  { -1, NULL }
};

static inline int get_depend_type_enum(const char *str)
{
  OMP_map_type dt = OMP_DEPEND_UNKNOWN;
  depend_type_entry_t *e = depend_table;

  while ((int)(e->t) >= 0) {
    if (strcasecmp(e->depend_type, str) == 0) {
      dt = e->t;
      break;
    }
    e++;
  }
  return ((int)dt > 0) ? (int)dt : -1;
}

const char *ompDependClauseTypeString(OMP_depend_type dt)
{
  if ((int)dt >= 0 && (int)dt < N_OMP_DEPEND) {
    depend_type_entry_t *e = &(depend_table[(int)dt]);
    return e->depend_type;
  }
  return NULL;
}

static CExpr* _omp_pg_list(int omp_code,CExpr* args)
{
  CExprOfList *lp;
  lp = allocExprOfList1(EC_UNDEF,args);
  lp->e_aux = omp_code;
  return (CExpr *)lp;
}

#define EMPTY_LIST (CExpr *)allocExprOfList(EC_UNDEF)

#ifdef not
static expv compile_OMP_SECTIONS_statement(expr x);
static void compile_OMP_pragma_clause(expr x, int pragma, int is_parallel, expv *pc, expv *dc);
static void compile_OMP_name_list _ANSI_ARGS_((expr x));
#endif

static int pg_OMP_pragma;
CExpr* pg_OMP_list;

/*
 * for OpenMP directives
 */
CExpr*
lexParsePragmaOMP(char *p, int *token) // p is buffer
{
  //skip pragma[space]omp[space]*
  p = lexSkipSpace(lexSkipWordP(lexSkipSpace(lexSkipWord(lexSkipSpace(lexSkipSharp(lexSkipSpace(p)))))));

  pg_cp = p; // set the pointer

  *token = parse_OMP_pragma();

  if(pg_OMP_list == NULL) pg_OMP_list = EMPTY_LIST;
  ((CExprOfList *)pg_OMP_list)->e_aux = pg_OMP_pragma;
  
  return pg_OMP_list;
}

#undef ENABLE_MULTILINE_PRAGMA
int parse_OMP_pragma()
{
  int ret = PRAGMA_PREFIX; /* default */
#ifdef ENABLE_MULTILINE_PRAGMA
  CExpr *parsed_pragmas = NULL;
#endif /* ENABLE_MULTILINE_PRAGMA */
  pg_OMP_pragma = OMP_NONE;
  pg_OMP_list = NULL;

#ifdef ENABLE_MULTILINE_PRAGMA  
 pragma_start:
#endif /* ENABLE_MULTILINE_PRAGMA */
  pg_get_token();
  if(pg_tok != PG_IDENT) goto syntax_err;

  /* parallel block directive */
  if(PG_IS_IDENT("parallel")){
    pg_get_token();
    if(pg_tok == PG_IDENT){
      if(PG_IS_IDENT("for")){	/* parallel for */
	pg_OMP_pragma = OMP_PARALLEL_FOR;
	pg_get_token();
        if(pg_tok == PG_IDENT){
          if(PG_IS_IDENT("simd")){  /* parallel for simd */
            pg_OMP_pragma = OMP_PARALLEL_LOOP_SIMD;
            pg_get_token();
            if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
            goto chk_end;
          }
        }
	if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
	goto chk_end;
      }
      if(PG_IS_IDENT("sections")){	/* parallel for */
	pg_OMP_pragma = OMP_PARALLEL_SECTIONS;
	pg_get_token();
	if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
	goto chk_end;
      }
    }
    pg_OMP_pragma = OMP_PARALLEL;
    if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
    goto chk_end;
  }
  
  if(PG_IS_IDENT("for")){
    pg_OMP_pragma = OMP_FOR;
    pg_get_token();
    if(pg_tok == PG_IDENT){
        if(PG_IS_IDENT("simd")){  /* for simd */
          pg_OMP_pragma = OMP_LOOP_SIMD;
          pg_get_token();
          if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
          goto chk_end;
        }
    }
    if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
    goto chk_end;
  }

  if(PG_IS_IDENT("sections")){
    pg_OMP_pragma = OMP_SECTIONS;
    pg_get_token();
    if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
    goto chk_end;
  }

  if(PG_IS_IDENT("single")){
    pg_OMP_pragma = OMP_SINGLE;
    pg_get_token();
    if((pg_OMP_list = parse_OMP_clauses()) == NULL)  goto syntax_err;
    goto chk_end;
  }

  if(PG_IS_IDENT("master")){
    pg_OMP_pragma = OMP_MASTER;
    pg_get_token();
    goto chk_end;
  }

  if(PG_IS_IDENT("critical")){
    pg_OMP_pragma = OMP_CRITICAL;
    pg_get_token();
    if(pg_tok == '('){
      if((pg_OMP_list = parse_OMP_namelist()) == NULL) goto syntax_err;
    } else pg_OMP_list = NULL;
    goto chk_end;
  }

  if(PG_IS_IDENT("ordered")){
    pg_OMP_pragma = OMP_ORDERED;
    pg_get_token();
    goto chk_end;
  }

  if(PG_IS_IDENT("section")){
    pg_OMP_pragma = OMP_SECTION;
    pg_get_token();
    ret = PRAGMA_EXEC;
    goto chk_end;
  }

  if(PG_IS_IDENT("barrier")){
    pg_OMP_pragma = OMP_BARRIER;
    ret = PRAGMA_EXEC;
    pg_get_token();
    goto chk_end;
  }
  
  if(PG_IS_IDENT("atomic")){
      pg_OMP_pragma = OMP_ATOMIC;
      ret = PRAGMA_PREFIX;
      pg_get_token();
      goto chk_end;
  }

  if(PG_IS_IDENT("flush")){
      pg_OMP_pragma = OMP_FLUSH;
      pg_get_token();
      if(pg_tok == '('){
	  if((pg_OMP_list = parse_OMP_namelist()) == NULL) goto syntax_err;
      } else pg_OMP_list = NULL;
      ret= PRAGMA_EXEC;
      goto chk_end;
  }

  if(PG_IS_IDENT("threadprivate")){
      pg_OMP_pragma = OMP_THREADPRIVATE;
      pg_get_token();
      if((pg_OMP_list = parse_OMP_namelist()) == NULL) goto syntax_err;
      ret = PRAGMA_EXEC;
      goto chk_end;
  }

  if (PG_IS_IDENT("declare")) { /* declare */
    pg_get_token();
    if((ret = parse_OMP_declare_pragma()) == 0) goto syntax_err;
    goto chk_end;
  }

  if (PG_IS_IDENT("end")) { /* end */
    pg_get_token();
    if((ret = parse_OMP_end_pragma()) == 0) goto syntax_err;
    goto chk_end;
  }

  if(PG_IS_IDENT("simd")){
    pg_OMP_pragma = OMP_SIMD;
    pg_get_token();
    if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
    goto chk_end;
  }

  if(PG_IS_IDENT("task")){
    pg_OMP_pragma = OMP_TASK;
    pg_get_token();
    if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
    goto chk_end;
  }

  if(PG_IS_IDENT("target")){  /* target */
    pg_get_token();
    if(pg_tok == PG_IDENT){
      if((ret = parse_OMP_target_pragma()) == 0) goto syntax_err;
      goto chk_end;
    }
    pg_OMP_pragma = OMP_TARGET;
    if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
    goto chk_end;
  }

  if(PG_IS_IDENT("teams")){  /* teams */
    if((ret = parse_OMP_teams_pragma()) == 0) goto syntax_err;
    goto chk_end;
  }

  if(PG_IS_IDENT("distribute")){  /* distribute */
    if((ret = parse_OMP_distribute_pragma()) == 0) goto syntax_err;
    goto chk_end;
  }
  
  if(PG_IS_IDENT("taskloop")){
    pg_OMP_pragma = OMP_TASKLOOP;
    pg_get_token();
    if(pg_tok == PG_IDENT){
        if(PG_IS_IDENT("simd")){  /* taskloop simd */
          pg_OMP_pragma = OMP_TASKLOOP_SIMD;
          pg_get_token();
          if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
          goto chk_end;
        }
    }
    if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
    goto chk_end;
  }

  if(PG_IS_IDENT("taskwait")){
    pg_OMP_pragma = OMP_TASKWAIT;
    pg_get_token();
    if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
    ret = PRAGMA_EXEC;
    goto chk_end;
  }

  if(PG_IS_IDENT("taskgroup")){
    pg_OMP_pragma = OMP_TASKGROUP;
    pg_get_token();
    if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
    ret = PRAGMA_EXEC;
    goto chk_end;
  }

  if(PG_IS_IDENT("taskyield")){
    pg_OMP_pragma = OMP_TASKYIELD;
    pg_get_token();
    if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
    ret = PRAGMA_EXEC;
    goto chk_end;
  }

  addError(NULL,"OMP: unknown OMP directive, '%s'",pg_tok_buf);
 syntax_err:
  return 0;

 chk_end:
#ifndef ENABLE_MULTILINE_PRAGMA
  if(pg_tok != 0) addError(NULL,"OMP:extra arguments for OMP directive");
#else  
  if (pg_tok == '\\') {
    ret = PRAGMA_PREFIX;
    pg_OMP_pragma = OMP_NONE;
    if (pg_OMP_list != NULL) {
      parsed_pragmas = exprListAdd(EMPTY_LIST, pg_OMP_list);
      pg_OMP_list = NULL;
    }
    goto pragma_start;
  } else {
    if (pg_tok != 0) addError(NULL,"OMP:extra arguments for OMP directive");
  }

  if (parsed_pragmas != NULL) {
    pg_OMP_list = parsed_pragmas;
  }

#endif /* ! ENABLE_MULTILINE_PRAGMA */
  return ret;
}

int parse_OMP_target_pragma()
{
  int ret = PRAGMA_PREFIX; /* default */

  if(PG_IS_IDENT("data")){ /* target data */
    ret = PRAGMA_EXEC;
    pg_OMP_pragma = OMP_TARGET_DATA;
    pg_get_token();
    goto chk_end;
  }
  else if(PG_IS_IDENT("enter")){ 
    ret = PRAGMA_EXEC;
    pg_get_token();
    if(pg_tok == PG_IDENT && PG_IS_IDENT("data")){  /* target enter data */
      pg_OMP_pragma = OMP_TARGET_ENTER_DATA;
      pg_get_token();
      goto chk_end;
    }
    goto syntax_err;
  }
  else if(PG_IS_IDENT("exit")){ 
    ret = PRAGMA_EXEC;
    pg_get_token();
    if(pg_tok == PG_IDENT && PG_IS_IDENT("data")){ /* target exit data */
        pg_OMP_pragma = OMP_TARGET_EXIT_DATA;
        pg_get_token();
        goto chk_end;
    }
    goto syntax_err;
  }
  else if(PG_IS_IDENT("update")){ 
    ret = PRAGMA_EXEC;
    pg_OMP_pragma = OMP_TARGET_UPDATE;
    pg_get_token();
    goto chk_end;
  }

  if(parse_OMP_teams_pragma() == 0) goto syntax_err;

 switch(pg_OMP_pragma){
 case OMP_TEAMS:
   pg_OMP_pragma = OMP_TARGET_TEAMS;
    break;
 case OMP_TEAMS_DISTRIBUTE:
   pg_OMP_pragma = OMP_TARGET_TEAMS_DISTRIBUTE;
    break;
 case OMP_TEAMS_DISTRIBUTE_PARALLEL_LOOP:
    pg_OMP_pragma = OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_LOOP;
    break;
  case OMP_TEAMS_DISTRIBUTE_PARALLEL_LOOP_SIMD:
    pg_OMP_pragma = OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_LOOP_SIMD;
    break;
  case OMP_TEAMS_DISTRIBUTE_SIMD:
    pg_OMP_pragma = OMP_TARGET_TEAMS_DISTRIBUTE_SIMD;
    break;

  case OMP_PARALLEL:
    pg_OMP_pragma = OMP_TARGET_PARALLEL;
    break;
  case OMP_PARALLEL_LOOP:
    pg_OMP_pragma = OMP_TARGET_PARALLEL_LOOP;
    break;
  case OMP_PARALLEL_LOOP_SIMD:
    pg_OMP_pragma = OMP_TARGET_PARALLEL_LOOP_SIMD;
    break;
  case OMP_SIMD:
    pg_OMP_pragma = OMP_TARGET_SIMD;
    break;

  case OMP_NONE:
    pg_OMP_pragma = OMP_TARGET;
    break;

  case OMP_DISTRIBUTE_PARALLEL_LOOP:
  case OMP_DISTRIBUTE_PARALLEL_LOOP_SIMD:
  case OMP_DISTRIBUTE_SIMD:
  default:
    goto syntax_err;
  }
 return ret;
  
 chk_end:
  if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
  return ret;

 syntax_err:
  return 0;
}

int parse_OMP_teams_pragma()
{
  int have_teams = FALSE;
  int ret = PRAGMA_PREFIX; /* default */

  if(pg_tok == PG_IDENT && PG_IS_IDENT("teams")){ /* teams ... */
    have_teams = TRUE;
    pg_get_token();
  }

  if(parse_OMP_distribute_pragma() == 0) goto syntax_err;

  if(have_teams){
    switch(pg_OMP_pragma){
    case OMP_NONE:
      pg_OMP_pragma = OMP_TEAMS;
      break;

    case OMP_DISTRIBUTE:
      pg_OMP_pragma = OMP_TEAMS_DISTRIBUTE;
      break;
    case OMP_DISTRIBUTE_PARALLEL_LOOP:
      pg_OMP_pragma = OMP_TEAMS_DISTRIBUTE_PARALLEL_LOOP;
      break;
    case OMP_DISTRIBUTE_PARALLEL_LOOP_SIMD:
      pg_OMP_pragma = OMP_TEAMS_DISTRIBUTE_PARALLEL_LOOP_SIMD;
      break;
    case OMP_DISTRIBUTE_SIMD:
      pg_OMP_pragma = OMP_TEAMS_DISTRIBUTE_SIMD;
      break;

    case OMP_PARALLEL_LOOP:
    case OMP_PARALLEL_LOOP_SIMD:
    case OMP_SIMD:
    default:
      goto syntax_err;
    }
  }
  return ret;

 syntax_err:
   return 0;
}

int parse_OMP_distribute_pragma()
{
  int have_distribute = FALSE;
  int ret = PRAGMA_PREFIX; /* default */  

  if(pg_tok == PG_IDENT && PG_IS_IDENT("distribute")){ /* distribute ... */
    have_distribute = TRUE;
    pg_get_token();
  }

  if(parse_OMP_parallel_for_SIMD_pragma() == 0) goto syntax_err;

  if(have_distribute){
    switch(pg_OMP_pragma){
    case OMP_NONE:
      pg_OMP_pragma = OMP_DISTRIBUTE;
      break;

    case OMP_PARALLEL_LOOP:
      pg_OMP_pragma = OMP_DISTRIBUTE_PARALLEL_LOOP;
      break;
    case OMP_PARALLEL_LOOP_SIMD:
      pg_OMP_pragma = OMP_DISTRIBUTE_PARALLEL_LOOP_SIMD;
      break;
    case OMP_SIMD:
      pg_OMP_pragma = OMP_DISTRIBUTE_SIMD;
      break;

    default:
      goto syntax_err;
    }
  }
  return ret;

 syntax_err:
   return 0;
}

int parse_OMP_parallel_for_SIMD_pragma()
{
  int ret = PRAGMA_PREFIX; /* default */

  if(pg_tok == PG_IDENT){
    if(PG_IS_IDENT("parallel")){
      pg_OMP_pragma = OMP_PARALLEL;
      pg_get_token();
      if(pg_tok == PG_IDENT && PG_IS_IDENT("for")){ /* parallel for */
        pg_OMP_pragma = OMP_PARALLEL_LOOP;
        pg_get_token();
        if(pg_tok == PG_IDENT && PG_IS_IDENT("simd")){ /* parallel for simd */
          pg_OMP_pragma = OMP_PARALLEL_LOOP_SIMD;
          pg_get_token();
        }
        goto chk_end;
      } 
    } 
    else if(PG_IS_IDENT("simd")){  /* simd */
      pg_get_token();
      pg_OMP_pragma = OMP_SIMD;
      goto chk_end;
    }
  }

 chk_end:
  if((pg_OMP_list = parse_OMP_clauses()) == NULL) goto syntax_err;
  return ret;

 syntax_err:
  return 0;
}

static int parse_OMP_declare_pragma() {
  int ret = OMP_NONE;
  int r;
  CExpr* v = NULL;

  if (PG_IS_IDENT("target")) {  /* declare target */
    pg_OMP_pragma = OMP_DECLARE_TARGET;
    pg_get_token();
    if (pg_tok == PG_IDENT) {
      /* declare target clause[ [, ]clause ...] */
      if ((pg_OMP_list = parse_OMP_clauses()) == NULL) {
        goto end;
      }
      ret = PRAGMA_EXEC;
    } else if (pg_tok == '(') {
      /* declare target (extended-list) */
      if ((v = parse_OMP_to(&r)) == NULL) {
        goto end;
      }
      if((pg_OMP_list = exprListAdd(EMPTY_LIST,
                                    OMP_PG_LIST(r, v))) == NULL) {
        goto end;
      }
      ret = PRAGMA_EXEC;
    } else if (pg_tok == '\0') {
      /* declare target ... end declare target */
      if (expr_num_nested_defs == 0ULL) {
        pg_OMP_pragma = OMP_DECLARE_TARGET_START;
        ret = PRAGMA_EXEC;
      } else {
        addError(NULL, "OMP: OpenMP declare directive: "
                 "Nested and local definitions are not supported.");
      }
    } else {
     addError(NULL, "OMP: OpenMP declare directive: "
               "unsupported : '%s'", pg_tok_buf);
    }
  } else {
    addError(NULL, "OMP: OpenMP declare directive: "
             "unsupported : '%s'", pg_tok_buf);
  }

 end:
  return ret;
}

static int parse_OMP_end_pragma() {
  int ret = OMP_NONE;

  if (PG_IS_IDENT("declare")) {
    pg_get_token();
    if (PG_IS_IDENT("target")) {
      /* declare target ... end declare target */
      pg_get_token();
      if (expr_num_nested_defs == 0ULL) {
        pg_OMP_pragma = OMP_DECLARE_TARGET_END;
        ret = PRAGMA_EXEC;
      } else {
        addError(NULL, "OMP: OpenMP declare directive: "
                 "Nested and local definitions are not supported.");
      }
    } else {
      addError(NULL, "OMP: OpenMP declare directive: "
               "unsupported : '%s'", pg_tok_buf);
    }
  } else {
    addError(NULL, "OMP: OpenMP end: "
             "unsupported : '%s'", pg_tok_buf);
  }

  return ret;
}

CExpr *parse_range_expr(int clause)
{
  CExpr *list = EMPTY_LIST, *v1, *v2;

  pg_get_token();
  while(1){
    v1 = v2 = NULL;
    switch(pg_tok){
    case ')': goto err;
    case '(': goto err;
    case ':':
      v1 = (CExpr*)allocExprOfNumberConst2(0, BT_INT);
      break;
    case '*':
      v1 = (CExpr *)allocExprOfStringConst(EC_STRING_CONST, "* @{ASTERISK}@", CT_UNDEF);
      pg_get_token();
      goto next;
      break;
    default:
      v1 = pg_parse_expr();
    }

    if(pg_tok != ':'){
      if(clause == OMP_CLAUSE_DEVICE)
	v2 = (CExpr*)allocExprOfNumberConst2(1, BT_INT);
      else // (clause == OMP_CLAUSE_SHADOW)
	v2 = v1;
      goto next;
    }

    pg_get_token();
    if(pg_tok == ':')
      goto err;
    else
      v2 = pg_parse_expr();

  next:
    if (v1 == NULL && v2 == NULL)
      list = exprListAdd(list, NULL);
    else
      list = exprListAdd(list, (CExpr*)allocExprOfList2(EC_UNDEF,v1,v2));

    if(pg_tok == ')'){
      pg_get_token();
      break;
    }
    
    if(pg_tok == ',')  pg_get_token();
    else goto err;
  }

  return list;
 err:
  addError(NULL, "Syntax error in device clause");
  return NULL;
}

static CExpr* parse_layout_expr()
{
  CExpr *list = EMPTY_LIST, *v;
  pg_get_token();
  
  while(1){
    if(pg_tok == '*'){
      pg_get_token();
      v = (CExpr *)allocExprOfStringConst(EC_STRING_CONST, "* @{ASTERISK}@", CT_UNDEF);
    }
    else if(PG_IS_IDENT("block")){
      pg_get_token();
      v = (CExpr *)allocExprOfStringConst(EC_STRING_CONST, "block", CT_UNDEF);
    }
    else goto err;

    list = exprListAdd(list, v);

    if(pg_tok == ')'){
      pg_get_token();
      break;
    }
    else if(pg_tok == ','){
      pg_get_token();
      continue;
    }
    else goto err;
  }
  
  return list;

 err:
  addError(NULL, "syntax error in layout clause");
  return NULL;
}

static CExpr* parse_OMP_C_subscript_list()
{
  CExpr* list = EMPTY_LIST, *v1, *v2;

  if(pg_tok != '[') {
    addError(NULL, "parse_OMP_C_subscript_list: first token= '['");
  }
  pg_get_token();

  while(1){
    v1 = v2 = NULL;
    switch(pg_tok){
    case ']':  goto err;
    case ',':  goto err;
      break;
    case ':':
      v1 = (CExpr*)allocExprOfNumberConst2(0, BT_INT);
      break;
    default:
      v1 = pg_parse_expr();
    }

    if(pg_tok == ':') goto subarray;
    list = exprListAdd(list, v1);
    goto next;
    
  subarray:
    pg_get_token();
    if(pg_tok != ']'){
      v2 = pg_parse_expr();
    }
    list = exprListAdd(list, (CExpr*)allocExprOfList2(EC_UNDEF,v1,v2));
    
  next:
    if(pg_tok == ']'){
      pg_get_token();
    }
    else goto err;
    
    if(pg_tok != '['){
      break;
    }
    else{
      pg_get_token();
    }
  }

  return list;

 err:
  addError(NULL, "Syntax error in scripts of OpenMP directive");
  return NULL;
}

typedef enum {
  MAP_TYPE_SEQ_UNKNOWN = 0,
  MAP_TYPE_SEQ_GOT_COLON,
  MAP_TYPE_SEQ_NEXT_VAR,
  MAP_TYPE_SEQ_ERROR,
} map_type_seq_result_t;

static inline map_type_seq_result_t
parse_map_type_seq(pg_token_context_t *prevctx)
{
  map_type_seq_result_t ret = MAP_TYPE_SEQ_UNKNOWN;
  pg_token_context_t ctx = *prevctx;
  char map_type[32 + 1];
  size_t cplen;

  pg_peek_token(&ctx);
  cplen = ((sizeof(map_type) -1) < ctx.token_len) ?
    (sizeof(map_type) -1) : ctx.token_len;
  memcpy(map_type, ctx.token, cplen);
  map_type[cplen] = '\0';

  if (strcasecmp(map_type, "to") == 0 ||
      strcasecmp(map_type, "from") == 0 ||
      strcasecmp(map_type, "tofrom") == 0 ||
      strcasecmp(map_type, "alloc") == 0 ||
      strcasecmp(map_type, "release") == 0 ||
      strcasecmp(map_type, "delete") == 0 ||
      strcasecmp(map_type, "always") == 0) {

    pg_peek_token(&ctx);		/* peek 1 */

    if (strncasecmp(ctx.token, ",", ctx.token_len) == 0 ||
        strncasecmp(ctx.token, "[", ctx.token_len) == 0) {
      ret = MAP_TYPE_SEQ_NEXT_VAR;
    } else if (strncasecmp(ctx.token, ":", ctx.token_len) == 0) {
      if (strcasecmp(map_type, "always") == 0) {
        /*
         * got "map(always, always :" ... error
         */
        ret = MAP_TYPE_SEQ_ERROR;
      } else {
        ret = MAP_TYPE_SEQ_GOT_COLON;
      }
    }
  } else {
    ret = MAP_TYPE_SEQ_NEXT_VAR;
  }

  *prevctx = ctx;
  return ret;
}

static CExpr *parse_array_list()
{
  CExpr *args = EMPTY_LIST;
  pg_token_context_t pgctx;
  char map_type[32 + 1];
  int map_type_val = -1;
  int is_first = 1;
  CExpr *v = NULL;
  int got_map_type = 0;

  pg_token_context_init(&pgctx);

  if (pg_tok != '(') {
    addError(NULL,"OMP: OpenMP map clause: requires at least a "
             "name list.");
    return NULL;
  }

  pg_get_token();
  if (pg_tok != PG_IDENT) {
    if (pg_tok == ':') {
      addError(NULL, "OMP: OpenMP map clause: no map-type is yet specified.");
    } else if (pg_tok == ')') {
      addError(NULL, "OMP: OpenMP map clause: empty name list.");
    } else {
      addError(NULL, "OMP: OpenMP map clause: invalid map-type.");
    }
    return NULL;
  }

 parse_map_type:
  if (is_first == 1) {
    snprintf(map_type, sizeof(map_type), "%s", pg_tok_buf);
    is_first = 0;
    pg_token_context_init(&pgctx);
  } else {
    size_t cplen = ((sizeof(map_type) -1) < pgctx.token_len) ?
      (sizeof(map_type) -1) : pgctx.token_len;
    memcpy(map_type, pgctx.token, cplen);
    map_type[cplen] = '\0';
  }

  if (strcasecmp(map_type, "to") == 0 ||
      strcasecmp(map_type, "from") == 0 ||
      strcasecmp(map_type, "tofrom") == 0 ||
      strcasecmp(map_type, "alloc") == 0 ||
      strcasecmp(map_type, "release") == 0 ||
      strcasecmp(map_type, "delete") == 0 ||
      strcasecmp(map_type, "always") == 0) {

    pg_peek_token(&pgctx);		/* peek 1 */

    if (strncasecmp(pgctx.token, ",", pgctx.token_len) == 0 ||
        strncasecmp(pgctx.token, "[", pgctx.token_len) == 0 ||
        strncasecmp(pgctx.token, ")", pgctx.token_len) == 0) {
      if (strncasecmp(pgctx.token, ",", pgctx.token_len) == 0 &&
          strcasecmp(map_type, "always") == 0) {
        /*
         * "always, ": The most ambiguous case.
         *
         *	1) "alwyas" is a var.
         *	2) "always" is a map_type_modifier with ignorable ','.
         */
        pg_token_context_t ctx = pgctx;
        map_type_seq_result_t res = parse_map_type_seq(&ctx);

        if (res == MAP_TYPE_SEQ_GOT_COLON) {
          pg_peek_token(&pgctx);
          goto about_to_var;
        } else if (res == MAP_TYPE_SEQ_NEXT_VAR) {
          goto no_map_type;
        } else {
          char buf[32 + 1];
          size_t cplen = ((sizeof(buf) -1) < ctx.token_len) ?
            (sizeof(buf) -1) : ctx.token_len;
          memcpy(buf, ctx.token, cplen);
          buf[cplen] = '\0';
          addError(NULL, "OMP: OpenMP map clause: an invalid "
                   "map-type/map-type-modifier sequence between "
                   "\"%s\" and \"%s\".", map_type, buf);
          return NULL;
        }
      } else {
        /*
         * got "map(%map_type%," ... %map_type% is var/array.
         */
        goto no_map_type;
      }
    } else if (strncasecmp(pgctx.token, ":", pgctx.token_len) == 0) {
      /*
       * got "map(%map_type% :" ... OK (if %map_type% != "always")
       */

      /*
       * error
       */
      if (got_map_type == 1) {
        pg_seek_token(&pgctx);
        addError(NULL, "OMP: OpenMP map clause: map-type is already given.");
        return NULL;
      } else if (strcasecmp(map_type, "always") == 0) {
        pg_seek_token(&pgctx);
        addError(NULL, "OMP: OpenMP map clause: need a map-type following "
                 "\"always\"");
        return NULL;
      } else {
        /*
         * try to parse var
         */
        goto about_to_var;
      }
    } else {
      int mtv = -1;
     about_to_var:
      mtv = get_map_type_enum(map_type);
      if (mtv < 0) {
        addError(NULL, "OMP: OpenMP map clause: invalid map-type \"%s\".",
                 map_type);
        return NULL;
      }
      if (strcasecmp(map_type, "always") == 0) {
        map_type_val = mtv;
        goto parse_map_type;
      } else {
        got_map_type = 1;

        /*
         * one more check.
         */
        if (strncasecmp(pgctx.token, ",", pgctx.token_len) != 0 &&
            strncasecmp(pgctx.token, "[", pgctx.token_len) != 0 &&
            strncasecmp(pgctx.token, ")", pgctx.token_len) != 0 &&
            strncasecmp(pgctx.token, ":", pgctx.token_len) != 0) {
          char buf[32 + 1];
          size_t cplen = ((sizeof(buf) -1) < pgctx.token_len) ?
            (sizeof(buf) -1) : pgctx.token_len;
          memcpy(buf, pgctx.token, cplen);
          buf[cplen] = '\0';
          addError(NULL, "OMP: OpenMP map clause: an invalid "
                   "map-type/map-type-modifier sequence, having "
                   "both \"%s\" and \"%s\".",
                   map_type, buf);
          return NULL;
        }

        if (map_type_val < 0) {
          map_type_val = mtv;
        } else {
          map_type_val += mtv;
        }
        if (map_type_val < 0) {
          addError(NULL, "OMP: OpenMP map clause: an internal error around "
                   "map-type/map-type-modifer combination analisys.");
          return NULL;
        }
        pg_seek_token(&pgctx);
        args = exprListAdd(args,
                           (CExpr *)allocExprOfNumberConst2(-map_type_val,
                                                            BT_INT));
      }
    }
  } else {
    CExpr *mtNode;

  no_map_type:
    if (pgctx.token == NULL) {
      pg_token_context_t ctx;
      pg_token_context_init(&ctx);
      pg_peek_token(&ctx);
      if (strncasecmp(ctx.token, ":", ctx.token_len) == 0) {
        addError(NULL, "OMP: OpenMP map clause: got ':' in name list.");
        return NULL;
      }
    }
    /*
     * Add default map_type tofrom node.
     * Reference:
     *	p.218, 13, "2.15.5.1 map Clause", "OpenMP Application Programming
     *	Interface Version 4.5 November 2015"
     */
    mtNode = (CExpr *)allocExprOfNumberConst2(-((int)OMP_DATA_MAP_TOFROM),
                                              BT_INT);
    args = exprListAdd(args, mtNode);
  }

  v = pg_tok_val;

  /*
   * var list check.
   */
 var_list:
  pg_get_token();

  if (pg_tok != '[') {
    CExpr *varRef = EMPTY_LIST;
    varRef = exprListAdd(varRef, v);
    varRef = exprListAdd(varRef, EMPTY_LIST);
    args = exprListAdd(args, varRef);
  } else{
    CExpr *list = parse_OMP_C_subscript_list();
    CExpr *arrayRef = exprBinary(EC_ARRAY_REF, v, list);

    args = exprListAdd(args, arrayRef);
  }

  if (pg_tok == ',') {
    pg_get_token();
    v = pg_tok_val;
    goto var_list;
  } else if (pg_tok == ')') {
    pg_get_token();

    if (pg_tok == ',') {
      /*
       * Ignore trailing ','.
       */
      pg_get_token();
    }
    return args;
  } else if (pg_tok == PG_IDENT) {
    addError(NULL, "OMP: OpenMP map clause: need a ',' before \"%s\".",
             pg_tok_buf);
    return NULL;
  } else if (pg_tok == ':') {
    addError(NULL, "OMP: OPenMP map clause: only a single map-type must be "
             "specified in a name list.");
    return NULL;
  }

  addError(NULL, "OMP: OpenMP map clause: unhandled syntax error.");
  return NULL;
}


/*
  depend([depend-modifier,] dependency-type : locator-list)
*/
static CExpr* parse_depend_expr()
{
  int got_source = 0;
  int got_sink = 0;
  CExpr* args = EMPTY_LIST;
  int depend_type = -1;

  if(pg_tok != '('){
    addError(NULL,"OMP: OpenMP depend clause: requires a name list.");
    return NULL;
  }
  pg_get_token();

  if(pg_tok != PG_IDENT){
    addError(NULL, "OMP: OpenMP depend clause: empty name list.");
    return NULL;
  }

  // todo: implement depend-modifier introduced in OpenMP 5.0
  if (PG_IS_IDENT("iterator")) {
    addError(NULL, "depend-modifier in depend clause is not implemented yet");
    return NULL;
  }

  // in, out, inout is introduced in OpenMP 4.0
  // sink, source in OpenMP 4.5
  // mutexinoutset, depobj is introduced in OpenMP 5.0
  if (pg_tok == 'I' &&
      (depend_type = get_depend_type_enum(pg_tok_buf)) > 0) {
    args = exprListAdd(args,
                       (CExpr *)allocExprOfNumberConst2(-depend_type,
                                                        BT_INT));
    switch (depend_type) {
    case OMP_DEPEND_SINK:
      got_sink = 1;
      break;
    case OMP_DEPEND_SOURCE:
      got_source = 1;
      break;
    default:
      break;
    }
  } else {
    addError(NULL, "OMP: OpenMP depend clause: unknown depend-type \"%s\".",
             pg_tok_buf);
    return NULL;
  }

  pg_get_token();

  if (got_source == 1) {
    if (pg_tok == ')') {
      pg_get_token();
      return args;
    } else {
      addError(NULL, "OMP: OpenMP depend clause: depend-type \"source\" "
               "must not has parameters.");
      return NULL;
    }
  } else if (pg_tok != ':') {
    goto err;
  }

  pg_get_token();

  CExpr* v; 
  CExpr *locatorList = EMPTY_LIST;

nextLocator:
  if (got_sink == 1) {
    v = pg_parse_expr();
    if (EXPR_CODE(v) == EC_PLUS || EXPR_CODE(v) == EC_MINUS ||
        EXPR_CODE(v) == EC_IDENT) {
      locatorList = exprListAdd(locatorList, v);
    } else {
      addError(NULL, "OMP: OpenMP depend clause: depend-type \"sink\" must "
               "has vector list "
               "(list of \"<var>\", \"<var> + <int>\" or \"<var> - <int>\").");
      return NULL;
    }
  } else {
    v = pg_tok_val;
    pg_get_token();

    if (pg_tok != '[') {
      // not array expression
      CExpr *varRef = EMPTY_LIST;
      varRef = exprListAdd(varRef, v);
      varRef = exprListAdd(varRef, EMPTY_LIST);
      locatorList = exprListAdd(locatorList, varRef);
    } else {
      // array expression
      CExpr *list     = parse_OMP_C_subscript_list();
      CExpr* arrayRef = exprBinary(EC_ARRAY_REF, v, list);
      locatorList = exprListAdd(locatorList, arrayRef);
    }
  }

  if(pg_tok == ','){
    pg_get_token();
    goto nextLocator;
  }
  else if(pg_tok == ')'){
    pg_get_token();

    args = exprListAdd(args, locatorList);
    return args;
  }
  
 err:
  addError(NULL,"OMP: syntax error in OpenMP pragma clause");
  return NULL;

}

static CExpr* parse_OMP_array_list()
{
  CExpr* v = NULL;
  CExpr* arrayRef = NULL;
  CExpr* list = EMPTY_LIST;
  CExpr* subscript_list = EMPTY_LIST;

next:
  if (pg_tok != PG_IDENT) {
    addError(NULL, "OpenMP: empty name list in OpenMP directive clause");
    return NULL;
  }

  v = pg_tok_val;
  pg_get_token();
  if (pg_tok == '[') {
    // array expression
    subscript_list = parse_OMP_C_subscript_list();
    if (subscript_list == NULL) {
      addError(NULL, "OpenMP: invalid name list in OpenMP directive clause");
      return NULL;
    }
    arrayRef = exprBinary(EC_ARRAY_REF, v, subscript_list);
    list = exprListAdd(list, arrayRef);
  } else{
    // not array expression
    list = exprListAdd(list, v);
  }

  if (pg_tok == ',') {
    pg_get_token();
    goto next;
  }

  return list;
}

static CExpr* parse_OMP_defaultmap()
{
  CExpr* args = EMPTY_LIST;

  if (pg_tok != PG_IDENT) {
    addError(NULL, "OMP: OpenMP defaultmap clause: requires implicit-behavior "
             "and variable-category");
    return NULL;
  }

  if (PG_IS_IDENT("tofrom")) {
    args = exprListAdd(args, pg_parse_expr());
  } else {
    addError(NULL, "OMP: OpenMP defaultmap clause: "
             "unsupported implicit-behavior: '%s'", pg_tok_buf);
    return NULL;
  }

  if (pg_tok != ':') {
    addError(NULL, "OMP: OpenMP defaultmap clause: requires ':'");
    return NULL;
  }

  pg_get_token();
  if (PG_IS_IDENT("scalar")) {
    args = exprListAdd(args, pg_parse_expr());
  } else {
    addError(NULL, "OMP: OpenMP defaultmap clause: "
             "unsupported variable-category: '%s'", pg_tok_buf);
    return NULL;
  }

  return args;
}

static CExpr* parse_OMP_to(int *r)
{
  CExpr* v = NULL;

  switch (pg_OMP_pragma) {
  case OMP_DECLARE_TARGET:
    *r = OMP_DECLARE_TARGET_TO;
    return parse_OMP_namelist();
  case OMP_TARGET_UPDATE:
    if (pg_tok != '(') {
      addError(NULL, "OMP: OpenMP to clause: requires arguments");
      return NULL;
    }

    pg_get_token();
    *r = OMP_TARGET_UPDATE_TO;
    if ((v = parse_OMP_array_list()) == NULL) {
      addError(NULL, "OMP: OpenMP to clause: "
               "invalid list");
      return NULL;
    }

    if (pg_tok != ')') {
      addError(NULL, "OMP: OpenMP to clause: not terminated");
      return NULL;
    }
    pg_get_token();
    return v;
  default:
    addError(NULL, "OMP: OpenMP to clause: This directive "
             "does not support to clause");
    return NULL;
  }
}

static CExpr* parse_OMP_proc_bind() {
  CExpr* args = EMPTY_LIST;
  int kind = OMP_PROC_BIND_NONE;

  if (pg_tok != PG_IDENT) {
    addError(NULL, "OMP: OpenMP proc_bind clause: requires arguments");
    return NULL;
  }

  if (PG_IS_IDENT("master")) {
    kind = OMP_PROC_BIND_MASTER;
  } else if (PG_IS_IDENT("close")) {
    kind = OMP_PROC_BIND_CLOSE;
  } else if (PG_IS_IDENT("spread")) {
    kind = OMP_PROC_BIND_SPREAD;
  } else {
    addError(NULL, "OMP: OpenMP proc_bind clause: "
             "unsupported kind: '%s'", pg_tok_buf);
    return NULL;
  }

  args = OMP_PG_LIST(kind, EMPTY_LIST);
  pg_get_token();

  return args;
}

static CExpr* parse_OMP_dist_schedule() {
  CExpr* args = EMPTY_LIST;
  // 'modifiers' is always empty.
  // Because it uses the same output format as 'schedule' clause.
  CExpr* modifiers = EMPTY_LIST;
  CExpr* chunk_size_expr = EMPTY_LIST;
  int kind = OMP_SCHED_NONE;

  if (pg_tok != PG_IDENT) {
    addError(NULL, "OMP: OpenMP dist_schedule clause: requires kind");
    return NULL;
  }

  // kind.
  if (PG_IS_IDENT("static")) {
    kind = OMP_SCHED_STATIC;
  } else {
    addError(NULL, "OMP: OpenMP dist_schedule clause: "
             "unsupported kind: '%s'", pg_tok_buf);
    return NULL;
  }

  pg_get_token();
  if (pg_tok == ',') {
    // chunk_size.
    pg_get_token();
    if((chunk_size_expr = pg_parse_expr()) == NULL) {
      addError(NULL, "OMP: OpenMP dist_schedule clause: "
               "invalid chunk_size expression");
      return NULL;
    }
  }

  if (pg_tok != ')') {
    addError(NULL, "OMP: OpenMP dist_schedule clause: many arguments "
             "or not terminated");
    return NULL;
  }

  args = exprListAdd(args, modifiers);
  args = exprListAdd(args, chunk_size_expr);
  args = OMP_PG_LIST(kind, args);

  return args;
}

static int parse_OMP_schedule_modifier(char *token, int token_len,
                                       CExpr** modifiers) {
  pg_token_context_t ctx;
  int modifier = OMP_SCHED_MODIFIER_NONE;
  int count_schedule_modifier = 0;

  (void) pg_token_context_init(&ctx);

 next:
  if (count_schedule_modifier >= MAX_NUM_SCHEDULE_MODIFIER) {
    addError(NULL, "OMP: OpenMP schedule clause: "
             "Number of appearances of modifier exceeds the upper limit");
    return 0;
  }

  if (PG_TOKEN_STR_IS_EQUAL(token, token_len, "monotonic")) {
    modifier = OMP_SCHED_MODIFIER_MONOTONIC;
  } else if (PG_TOKEN_STR_IS_EQUAL(token, token_len, "nonmonotonic")) {
    modifier = OMP_SCHED_MODIFIER_NONMONOTONIC;
  } else if (PG_TOKEN_STR_IS_EQUAL(token, token_len, "simd")) {
    modifier = OMP_SCHED_MODIFIER_SIMD;
  } else {
    if (count_schedule_modifier == 0) {
      return 1;
    }
    addError(NULL, "OMP: OpenMP schedule clause: "
             "unsupported modifier: '%s'", pg_tok_buf);
    return 0;
  }

  (void) pg_peek_token(&ctx);

  *modifiers = exprListAdd(*modifiers,
                           (CExpr *)allocExprOfNumberConst2(modifier, BT_INT));
  if (ctx.token != NULL && *(ctx.token) != '\0') {
    if (*(ctx.token) == ':') {
      pg_seek_token(&ctx);
    } else if (*(ctx.token) == ',') {
      (void) pg_peek_token(&ctx);
      token = ctx.token;
      token_len = ctx.token_len;
      count_schedule_modifier++;
      goto next;
    } else {
      addError(NULL, "OMP: OpenMP schedule clause: ':' or ',' not found");
      return 0;
    }
  } else {
    addError(NULL, "OMP: OpenMP schedule clause: requires modifier or kind");
    return 0;
  }

  return 1;
}

static CExpr* parse_OMP_schedule() {
  CExpr* args = EMPTY_LIST;
  CExpr* modifiers = EMPTY_LIST;
  CExpr* chunk_size_expr = EMPTY_LIST;
  int kind = OMP_SCHED_NONE;

  if (pg_tok != PG_IDENT) {
    addError(NULL, "OMP: OpenMP schedule clause: requires kind");
    return NULL;
  }

  if (parse_OMP_schedule_modifier(pg_tok_buf, strlen(pg_tok_buf),
                                  &modifiers) == 0) {
    return NULL;
  }

  // kind.
  if (PG_IS_IDENT("static")) {
    kind = (int)OMP_SCHED_STATIC;
  } else if (PG_IS_IDENT("dynamic")) {
    kind = (int)OMP_SCHED_DYNAMIC;
  }else if (PG_IS_IDENT("guided")) {
    kind = (int)OMP_SCHED_GUIDED;
  } else if (PG_IS_IDENT("runtime")) {
    kind = (int)OMP_SCHED_RUNTIME;
  } else if (PG_IS_IDENT("affinity")) {
    kind = (int)OMP_SCHED_AFFINITY;
  } else if (PG_IS_IDENT("auto")) {
    kind = (int)OMP_SCHED_AUTO;
  } else {
    addError(NULL, "OMP: OpenMP schedule clause: "
             "unsupported kind: '%s'", pg_tok_buf);
    return NULL;
  }

  pg_get_token();
  if (pg_tok == ',') {
    // chunk_size.
    pg_get_token();
    if ((chunk_size_expr = pg_parse_expr()) == NULL) {
      addError(NULL, "OMP: OpenMP schedule clause: "
               "invalid chunk_size expression");
      return NULL;
    }
  }

  if (pg_tok != ')') {
    addError(NULL, "OMP: OpenMP schedule clause: many arguments "
             "or not terminated");
    return NULL;
  }

  args = exprListAdd(args, modifiers);
  args = exprListAdd(args, chunk_size_expr);
  args = OMP_PG_LIST(kind, args);

  return args;
}

static CExpr* parse_OMP_linear_namelist() {
  CExpr* list = EMPTY_LIST;

 next:
  if (pg_tok != PG_IDENT){
    addError(NULL,"OMP: empty name list in OMP directive clause");
    return NULL;
  }

  list = exprListAdd(list, pg_tok_val);
  pg_get_token();
  if (pg_tok == ',') {
    pg_get_token();
    goto next;
  }

  return list;
}

static int parse_OMP_linear_list(CExpr** modifiers,
                                 CExpr** linear_list)
{
  pg_token_context_t ctx;

  // modifier(list)
  if (PG_IS_IDENT("val")) {
    (void) pg_token_context_init(&ctx);
    (void) pg_peek_token(&ctx);

    if (ctx.token != NULL && *(ctx.token) != '\0') {
      if (*(ctx.token) == '(') {
        *modifiers = exprListAdd(*modifiers,
                                 (CExpr *)allocExprOfNumberConst2(OMP_LINEAR_MODIFIER_VAL,
                                                                  BT_INT));

        pg_seek_token(&ctx);
        if ((*linear_list = parse_OMP_linear_namelist()) == NULL) {
          return 0;
        }
        if (pg_tok != ')') {
          addError(NULL, "OMP: OpenMP linear clause: many arguments "
                   "or not terminated");
          return 0;
        }
        pg_get_token();
     } else {
        // list
        if ((*linear_list = parse_OMP_linear_namelist()) == NULL) {
          return 0;
        }
      }
    } else {
      addError(NULL, "OMP: OpenMP linear clause: not terminated");
      return 0;
    }
  } else {
    // list
    if ((*linear_list = parse_OMP_linear_namelist()) == NULL) {
        return 0;
    }
  }

  return 1;
}

static CExpr* parse_OMP_linear()
{
  CExpr* args = EMPTY_LIST;
  CExpr* modifiers = EMPTY_LIST;
  CExpr* linear_list = EMPTY_LIST;
  CExpr* linear_step_expr = EMPTY_LIST;

  if (pg_tok != PG_IDENT) {
    addError(NULL, "OMP: OpenMP linear clause: requires list");
    return NULL;
  }

  if (parse_OMP_linear_list(&modifiers, &linear_list) == 0) {
    return NULL;
  }

  if (pg_tok == ':') {
    pg_get_token();
    if ((linear_step_expr = pg_parse_expr()) == NULL) {
      addError(NULL, "OMP: OpenMP linear clause: "
               "invalid linear_step expression");
      return NULL;
    }
  }

  if (pg_tok != ')') {
    addError(NULL, "OMP: OpenMP linear clause: many arguments "
             "or not terminated");
    return NULL;
  }

  args = exprListAdd(args, modifiers);
  args = exprListAdd(args, linear_list);
  args = exprListAdd(args, linear_step_expr);

  return args;
}

static CExpr* parse_OMP_aligned()
{
  CExpr* args = EMPTY_LIST;
  CExpr* aligned_list = EMPTY_LIST;
  CExpr* alignment_expr = EMPTY_LIST;

  if (pg_tok != PG_IDENT) {
    addError(NULL, "OMP: OpenMP aligned clause: requires list");
    return NULL;
  }

  if ((aligned_list = parse_OMP_array_list()) == NULL) {
    addError(NULL, "OMP: OpenMP aligned clause: "
             "invalid list");
    return NULL;
  }

  if (pg_tok == ':') {
    pg_get_token();
    if ((alignment_expr = pg_parse_expr()) == NULL) {
      addError(NULL, "OMP: OpenMP aligned clause: "
               "invalid alignment expression");
      return NULL;
    }
  }

  if (pg_tok != ')') {
    addError(NULL, "OMP: OpenMP aligned clause: many arguments "
             "or not terminated");
    return NULL;
  }

  args = exprListAdd(args, aligned_list);
  args = exprListAdd(args, alignment_expr);

  return args;
}

static CExpr* parse_OMP_clauses()
{
  CExpr *args=EMPTY_LIST, *v, *c;
  int r = 0;

  while(pg_tok == PG_IDENT){
    if(PG_IS_IDENT("private")){
      pg_get_token();
      if((v = parse_OMP_namelist()) == NULL) goto syntax_err;
      c = OMP_PG_LIST(OMP_DATA_PRIVATE,v);
    } else if(PG_IS_IDENT("shared")){
      pg_get_token();
      if((v = parse_OMP_namelist()) == NULL) goto syntax_err;
      c = OMP_PG_LIST(OMP_DATA_SHARED,v);
    } else if(PG_IS_IDENT("firstprivate")){
      pg_get_token();
      if((v = parse_OMP_namelist()) == NULL) goto syntax_err;
      c = OMP_PG_LIST(OMP_DATA_FIRSTPRIVATE,v);
    } else if(PG_IS_IDENT("lastprivate")){
      pg_get_token();
      if((v = parse_OMP_namelist()) == NULL) goto syntax_err;
      c = OMP_PG_LIST(OMP_DATA_LASTPRIVATE,v);
    } else if(PG_IS_IDENT("copyin")){
      pg_get_token();
      if((v = parse_OMP_namelist()) == NULL) goto syntax_err;
      c = OMP_PG_LIST(OMP_DATA_COPYIN,v);
    } else if(PG_IS_IDENT("copyprivate")){
      pg_get_token();
      if((v = parse_OMP_namelist()) == NULL) goto syntax_err;
      c = OMP_PG_LIST(OMP_DATA_COPYPRIVATE,v);
    } else if(PG_IS_IDENT("reduction")){
      pg_get_token();
      if((v = parse_OMP_reduction_namelist(&r)) == NULL) goto syntax_err;
      c = OMP_PG_LIST(r,v);
    } else if(PG_IS_IDENT("default")){
      pg_get_token();
      if(pg_tok != '(') goto syntax_err;
      pg_get_token();
      if(pg_tok != PG_IDENT) goto syntax_err;
      if(PG_IS_IDENT("shared")) 
	r = OMP_DEFAULT_SHARED;
      else if(PG_IS_IDENT("private")) 
	r = OMP_DEFAULT_PRIVATE;
      else if(PG_IS_IDENT("none"))
	r = OMP_DEFAULT_NONE;
      else goto syntax_err;
      pg_get_token();
      if(pg_tok != ')') goto syntax_err;
      pg_get_token();
      v = OMP_PG_LIST(r,EMPTY_LIST);
      c = OMP_PG_LIST(OMP_DATA_DEFAULT,v);
    } else if(PG_IS_IDENT("if")){
      pg_get_token();
      if(pg_tok != '(') {
        addError(NULL, "OMP: OpenMP if clause: requires arguments");
        goto syntax_err;
      }

      pg_get_token();
      if ((v = parse_OMP_if(&r)) == NULL) goto syntax_err;

      if(pg_tok != ')') {
        addError(NULL, "OMP: OpenMP if clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();

      if (r != OMP_NONE) {
        v = OMP_PG_LIST(r, v);
      }
      c = OMP_PG_LIST(OMP_DIR_IF, v);
    } else if(PG_IS_IDENT("schedule")){
      pg_get_token();
      if(pg_tok != '(') {
        addError(NULL, "OMP: OpenMP schedule clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if((v = parse_OMP_schedule()) == NULL) goto syntax_err;
      if(pg_tok != ')') {
        addError(NULL, "OMP: OpenMP schedule clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_DIR_SCHEDULE,v);
    } else if(PG_IS_IDENT("ordered")){
      pg_get_token();
      c = OMP_PG_LIST(OMP_DIR_ORDERED,NULL);
    } else if(PG_IS_IDENT("nowait")){
      pg_get_token();
      c = OMP_PG_LIST(OMP_DIR_NOWAIT,NULL);
    } else if(PG_IS_IDENT("num_threads")){
      pg_get_token();
      if(pg_tok != '(') {
        addError(NULL, "OMP: OpenMP num_threads clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if((v = pg_parse_expr()) == NULL) goto syntax_err;
      if(pg_tok != ')') {
        addError(NULL, "OMP: OpenMP num_threads clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_DIR_NUM_THREADS,v);
    } else if(PG_IS_IDENT("collapse")){
      pg_get_token();
      if(pg_tok != '(') {
        addError(NULL, "OMP: OpenMP collapse clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if((v = pg_parse_expr()) == NULL) goto syntax_err;
      if(pg_tok != ')') {
        addError(NULL, "OMP: OpenMP collapse clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_COLLAPSE,v);
    }
    else if(PG_IS_IDENT("map")){
      pg_get_token();
      if(pg_tok != '(') goto syntax_err;
      if((v = parse_array_list()) == NULL) goto syntax_err;
      c = OMP_PG_LIST(OMP_TARGET_DATA_MAP, v);
    } else if(PG_IS_IDENT("to")){
      pg_get_token();
      if ((v = parse_OMP_to(&r)) == NULL) goto syntax_err;
      c = OMP_PG_LIST(r, v);
    } else if(PG_IS_IDENT("device")){
      pg_get_token();
      if(pg_tok != '(') goto syntax_err;
      if((v = parse_range_expr(OMP_CLAUSE_DEVICE)) == NULL) goto syntax_err;
      c = OMP_PG_LIST(OMP_TARGET_DEVICE,v);
    } else if(PG_IS_IDENT("shadow")){
      pg_get_token();
      if(pg_tok != '(') goto syntax_err;
      if((v = parse_range_expr(OMP_CLAUSE_SHADOW)) == NULL) goto syntax_err;
      c = OMP_PG_LIST(OMP_TARGET_SHADOW,v);
    } else if(PG_IS_IDENT("layout")){
      pg_get_token();
      if(pg_tok != '(') goto syntax_err;
      if((v = parse_layout_expr()) == NULL) goto syntax_err;
      c = OMP_PG_LIST(OMP_TARGET_LAYOUT,v);
    } else if (PG_IS_IDENT("depend")) {
      pg_get_token();
      if (pg_tok != '(') goto syntax_err;
      if((v = parse_depend_expr()) == NULL) goto syntax_err;
      c = OMP_PG_LIST(OMP_DEPEND, v);
    } else if (PG_IS_IDENT("final")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP final clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = pg_parse_expr()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP final clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_FINAL, v);
    } else if (PG_IS_IDENT("priority")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP priority clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = pg_parse_expr()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP priority clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_PRIORITY,v);
    } else if (PG_IS_IDENT("untied")) {
      pg_get_token();
      c = OMP_PG_LIST(OMP_UNTIED, NULL);
    } else if (PG_IS_IDENT("mergeable")) {
      pg_get_token();
      c = OMP_PG_LIST(OMP_MERGEABLE, NULL);
    } else if (PG_IS_IDENT("grainsize")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP grainsize clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = pg_parse_expr()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP grainsize clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_GRAINSIZE,v);
    } else if (PG_IS_IDENT("num_tasks")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP num_tasks clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = pg_parse_expr()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP num_tasks clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_NUM_TASKS,v);
    } else if (PG_IS_IDENT("nogroup")) {
      pg_get_token();
      c = OMP_PG_LIST(OMP_NOGROUP, NULL);
    } else if (PG_IS_IDENT("is_device_ptr")) {
      pg_get_token();
      if (pg_tok != '('){
        addError(NULL, "OMP: OpenMP is_device_ptr clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = parse_OMP_array_list()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP is_device_ptr clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_IS_DEVICE_PTR, v);
    } else if (PG_IS_IDENT("use_device_ptr")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP use_device_ptr clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = parse_OMP_array_list()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP use_device_ptr clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_USE_DEVICE_PTR, v);
    } else if (PG_IS_IDENT("defaultmap")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP defaultmap clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = parse_OMP_defaultmap()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP defaultmap clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_DATA_DEFAULTMAP, v);
    } else if(PG_IS_IDENT("from")){
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP from clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = parse_OMP_array_list()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP from clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_TARGET_UPDATE_FROM, v);
    } else if(PG_IS_IDENT("link")){
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP link clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = parse_OMP_array_list()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP link clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_DATA_DECALRE_LINK, v);
    } else if (PG_IS_IDENT("num_teams")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP num_teams clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = pg_parse_expr()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP num_teams clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_NUM_TEAMS, v);
    } else if (PG_IS_IDENT("thread_limit")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP thread_limit clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = pg_parse_expr()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP thread_limit clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_THREAD_LIMIT, v);
    } else if (PG_IS_IDENT("dist_schedule")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP dist_schedule clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = parse_OMP_dist_schedule()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP dist_schedule clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_DIST_SCHEDULE, v);
    } else if (PG_IS_IDENT("proc_bind")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP proc_bind clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = parse_OMP_proc_bind()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP proc_bind clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_PROC_BIND, v);
    } else if (PG_IS_IDENT("safelen")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP safelen clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = pg_parse_expr()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP safelen clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_SAFELEN, v);
    } else if (PG_IS_IDENT("simdlen")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP simdlen clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = pg_parse_expr()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP simdlen clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_SIMDLEN, v);
    } else if (PG_IS_IDENT("linear")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP linear clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = parse_OMP_linear()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP linear clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_DATA_LINEAR, v);
    } else if (PG_IS_IDENT("aligned")) {
      pg_get_token();
      if (pg_tok != '(') {
        addError(NULL, "OMP: OpenMP aligned clause: requires arguments");
        goto syntax_err;
      }
      pg_get_token();
      if ((v = parse_OMP_aligned()) == NULL) goto syntax_err;
      if (pg_tok != ')') {
        addError(NULL, "OMP: OpenMP aligned clause: not terminated");
        goto syntax_err;
      }
      pg_get_token();
      c = OMP_PG_LIST(OMP_ALIGNED, v);
    }
    else {
      addError(NULL,"unknown OMP directive clause '%s'", pg_tok_buf);
      goto syntax_err;
    }
    args = exprListAdd(args, c);
  }

  return args;
    
 syntax_err:
    addError(NULL,"OMP: syntax error in OMP pragma clause");
    return NULL;
}

static int parse_OMP_if_directive_name_modifier(int *r)
{
  pg_token_context_t ctx;
  int modifier = OMP_NONE;

  (void) pg_token_context_init(&ctx);

  if (PG_IS_IDENT("task")) {
    (void) pg_peek_token(&ctx);
    modifier = OMP_TASK;
  } else if (PG_IS_IDENT("taskloop")) {
    (void) pg_peek_token(&ctx);
    modifier = OMP_TASKLOOP;
  } else if (PG_IS_IDENT("target")) {
    (void) pg_peek_token(&ctx);
    modifier = OMP_TARGET;

    if (ctx.token != NULL && *(ctx.token) != '\0') {
      if (PG_TOKEN_CONTEXT_STR_IS_EQUAL(&ctx, "update")) {
        (void) pg_peek_token(&ctx);
        modifier = OMP_TARGET_UPDATE;
      } else if (PG_TOKEN_CONTEXT_STR_IS_EQUAL(&ctx, "data")) {
        (void) pg_peek_token(&ctx);
        modifier = OMP_TARGET_DATA;
      } else if (PG_TOKEN_CONTEXT_STR_IS_EQUAL(&ctx, "enter")) {
        (void) pg_peek_token(&ctx);

        if (ctx.token != NULL && *(ctx.token) != '\0') {
          if (PG_TOKEN_CONTEXT_STR_IS_EQUAL(&ctx, "data")) {
            (void) pg_peek_token(&ctx);
            modifier = OMP_TARGET_ENTER_DATA;
          }
        } else {
          addError(NULL,"OMP if clause requires modifier or expression.");
          return 0;
        }
      } else if (PG_TOKEN_CONTEXT_STR_IS_EQUAL(&ctx, "exit")) {
        (void) pg_peek_token(&ctx);

        if (ctx.token != NULL && *(ctx.token) != '\0') {
          if (PG_TOKEN_CONTEXT_STR_IS_EQUAL(&ctx, "data")) {
            (void) pg_peek_token(&ctx);
            modifier = OMP_TARGET_EXIT_DATA;
          }
        } else {
          addError(NULL,"OMP if clause requires modifier or expression.");
          return 0;
        }
      }
    } else {
      addError(NULL,"OMP if clause requires modifier or expression.");
      return 0;
    }
  } else if (PG_IS_IDENT("parallel")) {
    (void) pg_peek_token(&ctx);
    modifier = OMP_PARALLEL_FOR;
  }

  if (modifier != OMP_NONE) {
    if (ctx.token == NULL || *(ctx.token) == '\0') {
      addError(NULL,"OMP if clause requires modifier or expression.");
      return 0;
    }
    if (*(ctx.token) == ':') {
      *r = modifier;

      // Next token. Skip tokens for peek.
      pg_seek_token(&ctx);
    }
  }

  return 1;
}

static CExpr* parse_OMP_if(int *r) {
  CExpr* v = NULL;
  *r = OMP_NONE;

  if (parse_OMP_if_directive_name_modifier(r) == 0) {
    return NULL;
  }

  if ((v = pg_parse_expr()) == NULL) {
    addError(NULL, "OMP: OpenMP if clause: "
             "invalid expression");
    return NULL;
  }

  return v;
}

static CExpr* parse_OMP_namelist()
{
    CExpr* args = EMPTY_LIST;
    if(pg_tok != '(') {
      addError(NULL,"OMP: OMP directive clause requires name list");
      return NULL;
    }
    pg_get_token();
    
 next:
    if(pg_tok != PG_IDENT){
      addError(NULL,"OMP: empty name list in OMP directive clause");
	return NULL;
    }

    args = exprListAdd(args, pg_tok_val);
    pg_get_token();
    if(pg_tok == ','){
      pg_get_token();
      goto next;
    }
    else if(pg_tok == ')'){
      pg_get_token();
      return args;
    } 

    addError(NULL,"OMP: syntax error in OMP pragma clause");
    return NULL;
}

static CExpr* parse_OMP_reduction_namelist(int *r)
{
  CExpr* args = EMPTY_LIST;
    if(pg_tok != '('){
      addError(NULL,"OMP reduction clause requires name list");
	return NULL;
    }
    pg_get_token();
    switch(pg_tok){
    case '+': *r = OMP_DATA_REDUCTION_PLUS;         break;
    case '-': *r = OMP_DATA_REDUCTION_MINUS;        break;
    case '*': *r = OMP_DATA_REDUCTION_MUL;          break;
    case '&': *r = OMP_DATA_REDUCTION_BITAND;       break;
    case '|': *r = OMP_DATA_REDUCTION_BITOR;        break;
    case '^': *r = OMP_DATA_REDUCTION_BITXOR;       break;
    case PG_ANDAND: *r = OMP_DATA_REDUCTION_LOGAND; break;
    case PG_OROR:   *r = OMP_DATA_REDUCTION_LOGOR;  break;
    case PG_IDENT:
      if(PG_IS_IDENT("max")) { *r = OMP_DATA_REDUCTION_MAX; break; }
      if(PG_IS_IDENT("min")) { *r = OMP_DATA_REDUCTION_MIN; break; }
    default:
      return NULL;	/* syntax error */
    }
    pg_get_token();
    if(pg_tok != ':') return NULL;
    pg_get_token();

 next:
    if(pg_tok != PG_IDENT){
      addError(NULL,"empty name list in OMP reduction clause");
	return NULL;
    }
    args = exprListAdd(args,pg_tok_val);
    pg_get_token();
    if(pg_tok == ','){
	pg_get_token();
	goto next;
    } else if(pg_tok == ')'){
	pg_get_token();
	return args;
    } 

    addError(NULL,"syntax error in OMP directive clause");
    return NULL;
}

#ifdef not
/*
 * compile pragma, called from compile_statement 
 */

expv compile_OMP_pragma(enum OMP_pragma pragma,expr x)
{
    expv v,c;
    expv pclause,dclause;

    switch(pragma){
    case OMP_PARALLEL: 		/* parallel <clause_list> */
	compile_OMP_pragma_clause(EXPR_ARG2(x),OMP_PARALLEL,TRUE,
				  &pclause,&dclause);
	v = compile_statement(EXPR_ARG3(x));
	return elist3(EXPR_LINE(x),OMP_PRAGMA,EXPR_ARG1(x),pclause,v);

    case OMP_PARALLEL_FOR:	/* parallel for <clause_list> */
	compile_OMP_pragma_clause(EXPR_ARG2(x),OMP_FOR,TRUE,
				  &pclause,&dclause);
	v = compile_statement(EXPR_ARG3(x));
	return elist3(EXPR_LINE(x),OMP_PRAGMA,
		      make_enode(INT_CONSTANT, (void *)OMP_PARALLEL), pclause,
		      elist3(EXPR_LINE(x),OMP_PRAGMA,
			     make_enode(INT_CONSTANT, (void *)OMP_FOR),
			     dclause,v));

    case OMP_PARALLEL_SECTIONS: /* parallel sections <clause_list> */
	compile_OMP_pragma_clause(EXPR_ARG2(x),OMP_SECTIONS,TRUE,
				  &pclause,&dclause);
	v = compile_OMP_SECTIONS_statement(EXPR_ARG3(x));
	return elist3(EXPR_LINE(x),OMP_PRAGMA,
		      make_enode(INT_CONSTANT, (void *)OMP_PARALLEL), pclause,
		      elist3(EXPR_LINE(x),OMP_PRAGMA,
			     make_enode(INT_CONSTANT, (void *)OMP_SECTIONS),
			     dclause, v));

    case OMP_FOR:		/* for <clause_list> */
	compile_OMP_pragma_clause(EXPR_ARG2(x),OMP_FOR,FALSE,
				  &pclause,&dclause);
	v = compile_statement(EXPR_ARG3(x));
	return elist3(EXPR_LINE(x),OMP_PRAGMA,EXPR_ARG1(x),dclause,v);
		     
    case OMP_SECTIONS:		/* sections <clause_list> */
	compile_OMP_pragma_clause(EXPR_ARG2(x),OMP_SECTIONS,FALSE,
				  &pclause,&dclause);
	if((v = compile_OMP_SECTIONS_statement(EXPR_ARG3(x))) == NULL)
	  break;
	return elist3(EXPR_LINE(x),OMP_PRAGMA,EXPR_ARG1(x),dclause,v);

    case OMP_SINGLE:		/* single <clause list> */
	compile_OMP_pragma_clause(EXPR_ARG2(x),OMP_SINGLE,FALSE,
				  &pclause,&dclause);
	v = compile_statement(EXPR_ARG3(x));
	return elist3(EXPR_LINE(x),OMP_PRAGMA,EXPR_ARG1(x),dclause,v);

    case OMP_MASTER:		/* master */
    case OMP_ORDERED:		/* ordered */
	v = compile_statement(EXPR_ARG3(x));
	return elist3(EXPR_LINE(x),OMP_PRAGMA,EXPR_ARG1(x),NULL,v);

    case OMP_CRITICAL:		/* critical <name> */
	v = compile_statement(EXPR_ARG3(x));
	c = EXPR_ARG2(x);
	if(c != NULL && LIST_NEXT(EXPR_LIST(c)) != NULL){
	    error_at_node(x,"bad critical section name");
	    break;
	}
	return elist3(EXPR_LINE(x),OMP_PRAGMA,EXPR_ARG1(x),c,v);

    case OMP_ATOMIC:		/* atomic */
	/* should check next statment */
	if((v = compile_statement(EXPR_ARG3(x))) == NULL) 
	  break;
	if(EXPV_CODE(v) != EXPR_STATEMENT){
	    error_at_node(x,"bad statement for OMP atomic directive");
	    break;
	}
	return elist3(EXPR_LINE(x),OMP_PRAGMA,EXPR_ARG1(x),NULL,v);

    case OMP_FLUSH:		/* flush <namelist> */
	c = EXPR_ARG2(x);
	compile_OMP_name_list(c);
	return elist3(EXPR_LINE(x),OMP_PRAGMA,EXPR_ARG1(x),c,NULL);

    case OMP_SECTION:		/* section */
	/* section directive must appear in section block */
	error_at_node(x,"'section' directive in SECTIONS");
	break;

    case OMP_BARRIER:		/* barrier */
	return elist3(EXPR_LINE(x),OMP_PRAGMA,EXPR_ARG1(x),NULL,NULL);

    case OMP_THREADPRIVATE:
	c = EXPR_ARG2(x);
	compile_OMP_name_list(c);
	return elist3(EXPR_LINE(x),OMP_PRAGMA,EXPR_ARG1(x),c,NULL);

    case OMP_DECLARE_TARGET:
      c = EXPR_ARG2(x);
      compile_OMP_name_list(c);
      return elist3(EXPR_LINE(x),OMP_PRAGMA,EXPR_ARG1(x),c,NULL);
      
    default:
	fatal("compile_pragma_line: unknown pragma %d",pragma);
    }
    return NULL;
}

static expv compile_OMP_SECTIONS_statement(expr x)
{
    expr xx;
    expv section_list,current_section;
    list lp;

    if(EXPR_CODE(x) != COMPOUND_STATEMENT){
	error_at_node(x,"sections directive must be followed by compound statement block");
	return NULL;
    }
    xx = EXPR_ARG1(x);
    if(xx != NULL){
	error_at_node(xx,"declarations in sections block");
	return NULL;
    }
    section_list = EMPTY_LIST;
    current_section = NULL;
    FOR_ITEMS_IN_LIST(lp,EXPR_ARG2(x)){
	xx = LIST_ITEM(lp);
	if(EXPR_CODE(xx) == PRAGMA_LINE &&
	   EXPR_INT(EXPR_ARG1(xx)) == OMP_SECTION){
	    if(current_section != NULL){
		current_section = list3(COMPOUND_STATEMENT,
					list0(ID_LIST),list0(LIST),
					current_section);
		section_list = exprListAdd(section_list,current_section);
	    }
	    current_section = EMPTY_LIST;
	    continue;
	}
	if(current_section == NULL){
	    /* error_at_node(xx,"statement is not in any section");
	    return NULL; */
	    current_section = EMPTY_LIST;
	}
	current_section = exprListAdd(current_section,
					compile_statement(xx));
    }
    current_section = list3(COMPOUND_STATEMENT,
			    list0(ID_LIST),list0(LIST),
			    current_section);
    section_list = exprListAdd(section_list,current_section);
    return section_list;
}

/* PARALLEL - private,firstprivate,reduction,default,shared,copyin,if
 * FOR      - private,firstprivate,lastprivate,reduction,ordered,shed,nowait
 * SECTIONS - private,firstprivate,lastprivate,reduction,nowait
 * SINGLE   - private,firstprivate,nowait
 */
static void compile_OMP_pragma_clause(expr x, int pragma, int is_parallel,
				      expv *pc,expv *dc)
{
    list lp;
    expr c,v;
    expv pclause = NULL;
    expv dclause;

    if(is_parallel) pclause = EMPTY_LIST;
    dclause = EMPTY_LIST;
    FOR_ITEMS_IN_LIST(lp,x){
      c = LIST_ITEM(lp);
      switch(EXPR_INT(EXPR_ARG1(c))){
      case OMP_DATA_DEFAULT:	/* default(shared|none) */
	if(!is_parallel){
	  error_at_node(x,"'default' clause must be in PARALLEL");
	  break;
	}
	pclause = exprListAdd(pclause,c);
	break;
      case OMP_DATA_SHARED:
	compile_OMP_name_list(EXPR_ARG2(c));
	if(!is_parallel){
	  error_at_node(x,"'shared' clause must be in PARALLEL");
	  break;
	}
	pclause = exprListAdd(pclause,c);
	break;
      case OMP_DATA_COPYIN:
	compile_OMP_name_list(EXPR_ARG2(c));
	if(!is_parallel){
	  error_at_node(x,"'copyin' clause must be in PARALLEL");
	  break;
	}
	pclause = exprListAdd(pclause,c);
	break;
      case OMP_DIR_IF:
	if(!is_parallel){
	  error_at_node(x,"'if' clause must be in PARALLEL");
	  break;
	}
	v = compile_expression(EXPR_ARG2(c));
	pclause = exprListAdd(pclause, list2(LIST,EXPR_ARG1(c),v));
	break;
      case OMP_DATA_PRIVATE:
      case OMP_DATA_FIRSTPRIVATE:
	/* all pragma can have these */
	compile_OMP_name_list(EXPR_ARG2(c));
	if(pragma == OMP_PARALLEL)
	  pclause = exprListAdd(pclause,c);
	else     
	  dclause = exprListAdd(dclause,c);
	break;
	
      case OMP_DATA_LASTPRIVATE:
	compile_OMP_name_list(EXPR_ARG2(c));
	if(pragma != OMP_FOR && pragma != OMP_SECTIONS){
	  error_at_node(x,"'lastprivate' clause must be in FOR or SECTIONS");
	  break;
	}
	dclause = exprListAdd(dclause,c);
	break;
	
      case OMP_DATA_REDUCTION_PLUS:
      case OMP_DATA_REDUCTION_MINUS:
      case OMP_DATA_REDUCTION_MUL:
      case OMP_DATA_REDUCTION_BITAND:
      case OMP_DATA_REDUCTION_BITOR:
      case OMP_DATA_REDUCTION_BITXOR:
      case OMP_DATA_REDUCTION_LOGAND:
      case OMP_DATA_REDUCTION_LOGOR:
      case OMP_DATA_REDUCTION_MIN:
      case OMP_DATA_REDUCTION_MAX:
	compile_OMP_name_list(EXPR_ARG2(c));
	if(pragma == OMP_PARALLEL)
	  pclause = exprListAdd(pclause,c);
	else if(pragma == OMP_FOR || pragma == OMP_SECTIONS)
	  dclause = exprListAdd(dclause,c);
	else 
	  error_at_node(x,"'reduction' clause must not be in SINGLE");
	break;
	
      case OMP_DIR_ORDERED:
	if(pragma != OMP_FOR){
	  error_at_node(x,"'ordered' clause must be in FOR");
	  break;
	}
	dclause = exprListAdd(dclause,c);
	break;
	
      case OMP_DIR_SCHEDULE:
	if(pragma != OMP_FOR){
	  error_at_node(x,"'schedule' clause must be in FOR");
	  break;
	}
	v = EXPR_ARG2(EXPR_ARG2(c));
	if(v != NULL && 
	   EXPR_INT(EXPR_ARG1(EXPR_ARG2(c))) != (int)OMP_SCHED_AFFINITY){
	  v = compile_expression(v);
	  c = list2(LIST,EXPR_ARG1(c),
		    list2(LIST,EXPR_ARG1(EXPR_ARG2(c)),v));
	}
	dclause = exprListAdd(dclause,c);
	break;
	
      case OMP_DIR_NOWAIT:
	if(is_parallel){
	  error_at_node(x,"'nowait' clause must not be in PARALLEL");
	  break;
	}
	dclause = exprListAdd(dclause,c);
	break;
	
      default:
	fatal("compile_OMP_paragma_clause");
      }
    }

    /* combination with PARALLEL, don't have to wait */
    if(is_parallel && (pragma != OMP_PARALLEL))
      dclause = exprListAdd(dclause, OMP_PG_LIST(OMP_DIR_NOWAIT, NULL));

    *pc = pclause;
    *dc = dclause;
}

static CExpr* compile_OMP_name_list(expr x)
{
    list lp;
    expr v;
    ID id;
    TYPE_DESC tp;

    FOR_ITEMS_IN_LIST(lp,x){
	v = LIST_ITEM(lp);
	id = lookup_ident(v);
	if(id == NULL){
	    error_at_node(x, "undefined variable, %s in pragma", 
			  SYM_NAME(EXPR_SYM(v)));
	    continue;
	}
	switch(ID_CLASS(id)){
	case AUTO:	/* auto variable */
	case PARAM:	/* paramter */
	case EXTERN:	/* extern variable */
	case EXTDEF:	/* external defition */
	case STATIC:	/* static variable */
	case REGISTER:	/* register variable */
	    tp = ID_TYPE(id);
	    if ( IS_FUNCTION(tp) ){
		error_at_node(x, "function name, %s in pragma", 
			      SYM_NAME(EXPR_SYM(v)));
	    }
	    break;
	default:
	  error_at_node(x, "identifer, %s is not variable in pragma",
			SYM_NAME(EXPR_SYM(v)));
	}
    }
}

#endif
