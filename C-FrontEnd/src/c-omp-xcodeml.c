/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil ; -*- */
#include <stdlib.h>
#include <stdarg.h> 
#include <wchar.h>
#include "c-comp.h"
#include "c-option.h"
#include "c-omp.h"

#include "c-xcodeml.h"

void outx_OMP_Clause(FILE *fp, int indent, CExprOfList* clause);
void out_OMP_name_list(FILE *fp,int indent, CExprOfList *list);
void out_ACC_subscript(FILE *fp,int indent, CExpr *subscript);
void out_ACC_arrayRef(FILE *fp,int indent, CExprOfBinaryNode *arrayRef);
void out_OMP_IF(FILE *fp, int indent, CExpr *arg);

static void out_OMP_map(FILE *fp, int indent, CExprOfList *list);

void
out_OMP_PRAGMA(FILE *fp, int indent, int pragma_code, CExpr* expr)
{
    CExprOfList *body = (CExprOfList *)expr;
    CExprOfList *clauseList = (CExprOfList *)body->e_aux_info;

    const char *ompPragmaTag = "OMPPragma";
    int indent1 = indent + 1;

    CCOL_DListNode *ite;
    outxTagForStmt(fp, indent,(CExpr*)clauseList, ompPragmaTag,0, NULL);
    outxPrint(fp,indent1,"<string>%s</string>\n",
	      ompDirectiveName(pragma_code));
    switch(pragma_code){
    case OMP_CRITICAL:
    case OMP_FLUSH:
    case OMP_THREADPRIVATE:
	out_OMP_name_list(fp, indent1, clauseList);
	goto end;
    }

    outxPrint(fp,indent1,"<list>\n");
    EXPR_FOREACH(ite, clauseList){
      CExpr *node = EXPR_L_DATA(ite);
      outx_OMP_Clause(fp,indent1+1,(CExprOfList *)node);
    }
    outxPrint(fp,indent1,"</list>\n");

  end:
    if(EXPR_L_SIZE(expr) != 0) outxChildren(fp,indent1,expr);
    outxTagClose(fp, indent,ompPragmaTag);
}

void
outx_OMP_Clause(FILE *fp, int indent, CExprOfList* clause)
{
  int indent1 = indent+1;
  CExpr *arg;
  CExprOfList *namelist;

  outxPrint(fp,indent,"<list>\n");
  outxPrint(fp,indent1,"<string>%s</string>\n",
	    ompClauseName(clause->e_aux));
  arg = exprListHeadData((CExpr *)clause);

  switch(clause->e_aux){
  case OMP_DIR_ORDERED:
  case OMP_DIR_NOWAIT:
  case OMP_UNTIED:
  case OMP_MERGEABLE:
  case OMP_NOGROUP:
      break;

  case OMP_DIR_IF:
      out_OMP_IF(fp, indent1 + 1, arg);
      break;

  case OMP_DIR_NUM_THREADS:
  case OMP_COLLAPSE:
  case OMP_FINAL:
  case OMP_PRIORITY:
  case OMP_GRAINSIZE:
  case OMP_NUM_TASKS:
      outxContext(fp,indent1+1,arg);
      break;

  case OMP_TARGET_DEVICE:
  case OMP_TARGET_SHADOW:
    {
      CCOL_DListNode *ite;
      EXPR_FOREACH(ite, arg){
	CExpr *node = EXPR_L_DATA(ite);
	out_ACC_subscript(fp,indent1+1,node);
      }
      break;
    }

  case OMP_DIR_SCHEDULE:
      outxPrint(fp,indent1,"<list>\n");
      outxPrint(fp,indent1+1,"<string>%s</string>\n",
		ompScheduleName(((CExprOfList *)arg)->e_aux));
      outxContext(fp,indent1+1,exprListHeadData(arg));
      outxPrint(fp,indent1,"</list>\n");
      break;

  case OMP_DEPEND:
    /*
      <string>DEPEND</string>
      <list>
          <list>...</list> // depend-modifier, tentative expression
          <string>in</string>
          <list> // locator-list
            <Var>i</Var>
            <list>
              <Var>a</Var>
              <Var type="int">i</Var>
            <list>
          </list>
        </list> // end of locator-list
      </list> 
    */

    namelist = (CExprOfList *)arg;
    if (EXPR_L_SIZE(namelist) == 0) {
      break;
    }

    // for depend-modifier, currently it is not implemented
    outxPrint(fp,indent1,"<list></list>\n"); 
    // for dependence-type
    outxPrint(fp,indent1,"<string>%s</string>\n",
              ((CExprOfSymbol *)EXPR_L_DATA(EXPR_L_AT(namelist, 1)))->e_symName);
    // for locator list
    out_OMP_name_list(fp, indent1, (CExprOfList *)EXPR_L_DATA(EXPR_L_AT(namelist, 2)));
    break;

  case OMP_DATA_DEFAULT:
      outxPrint(fp,indent1+1,"<string>%s</string>\n",
		ompDataDefaultName(((CExprOfList *)arg)->e_aux));
      break;

  case OMP_TARGET_DATA_MAP:
    namelist = (CExprOfList *)arg;
    if(EXPR_L_SIZE(namelist) != 0)
      out_OMP_map(fp, indent1, namelist);
    break;
      
  default:
    namelist = (CExprOfList *)arg;
    if(EXPR_L_SIZE(namelist) != 0)
      out_OMP_name_list(fp, indent1, namelist);
  }
  outxPrint(fp,indent,"</list>\n");
}

void out_OMP_IF(FILE *fp, int indent, CExpr *arg)
{
  switch(((CExprOfList *)arg)->e_aux) {
  case OMP_TASK:
  case OMP_TASKLOOP:
  case OMP_TARGET:
  case OMP_TARGET_UPDATE:
  case OMP_TARGET_DATA:
  case OMP_TARGET_ENTER_DATA:
  case OMP_TARGET_EXIT_DATA:
  case OMP_PARALLEL_FOR:
    outxPrint(fp, indent,"<list>\n");
    outxPrint(fp, indent + 1,"<string>%s</string>\n",
              ompDirectiveName(((CExprOfList *)arg)->e_aux));
    outxPrint(fp, indent,"</list>\n");
    outxContext(fp, indent, exprListHeadData(arg));
    break;
  default:
    outxPrint(fp,indent,"<list></list>\n");
    outxContext(fp, indent, arg);
    break;
  }
}

void out_OMP_name_list(FILE *fp,int indent, CExprOfList *list)
{
    int indent1 = indent+1;
    CCOL_DListNode *ite;
    outxPrint(fp,indent,"<list>\n");
    EXPR_FOREACH(ite, list) {
      CExpr *node = EXPR_L_DATA(ite);
      if(EXPR_CODE(node) == EC_ARRAY_REF){
        out_ACC_arrayRef(fp,indent1, (CExprOfBinaryNode*)node);
      }
      else{
        outxPrint(fp,indent1,"<Var>%s</Var>\n",
                  ((CExprOfSymbol *)node)->e_symName);
      }
    }
    outxPrint(fp,indent,"</list>\n");
}

static void out_OMP_map(FILE *fp,int indent, CExprOfList *list)
{
  int indent1 = indent+1;
  CCOL_DListNode *ite;

  outxPrint(fp, indent, "<list>\n");

  EXPR_FOREACH(ite, list) {
    CExpr *mapEntry = EXPR_L_DATA(ite);
    CExpr *mapTypeNode = EXPR_L_DATA(EXPR_L_AT(mapEntry, 0));
    CExpr *varNode = EXPR_L_DATA(EXPR_L_AT(mapEntry, 1));

    outxPrint(fp, indent1, "<list>\n");

    if (EXPR_CODE(mapTypeNode) == EC_NUMBER_CONST) {
      const char *map_type;
      const char *always;
      long long l = EXPR_NUMBERCONST(mapTypeNode)->e_numValue.ll;
      int i = (int)(-l);

      if (i < 0) {
       unknown_map:
        map_type = "??unknown map type??";
      } else if (i == 0) {
        always = "";
        map_type = "";
        goto emit_map_type;
      } else if (i >= 1000) {
        always = "always ";
        i -= 1000;
      } else {
        always = "";
      }
      map_type = ompMapClauseTypeString((OMP_map_type)i);
     emit_map_type:
      outxPrint(fp, indent1 + 1,
                "<string>%s%s</string>\n", always, map_type);
    } else {
      goto unknown_map;
    }

    if (EXPR_CODE(varNode) == EC_ARRAY_REF) {
      out_ACC_arrayRef(fp, indent1 + 1, (CExprOfBinaryNode *)varNode);
    } else {
      CExpr *varNameNode = EXPR_L_DATA(EXPR_L_AT(varNode, 0));

      outxPrint(fp, indent1 + 1, "<list>\n");      
      outxPrint(fp, indent1 + 2, "<Var>%s</Var>\n",
                ((CExprOfSymbol *)varNameNode)->e_symName);
      outxPrint(fp, indent1 + 2, "<list></list>\n");
      outxPrint(fp, indent1 + 1, "</list>\n");      
    }

    outxPrint(fp, indent1, "</list>\n");
  }

  outxPrint(fp, indent, "</list>\n");
}

char *ompDirectiveName(int c)
{
  switch(c){
  case OMP_PARALLEL:          return "PARALLEL";
  case OMP_FOR:               return "FOR";
  case OMP_SECTIONS:          return "SECTIONS";
  case OMP_SECTION:           return "SECTION";
  case OMP_SINGLE:            return "SINGLE";
  case OMP_MASTER:            return "MASTER";
  case OMP_CRITICAL:          return "CRITICAL";
  case OMP_BARRIER:           return "BARRIER";
  case OMP_ATOMIC:            return "ATOMIC";
  case OMP_FLUSH:             return "FLUSH";
  case OMP_ORDERED:           return "ORDERED";
  case OMP_THREADPRIVATE:     return "THREADPRIVATE";
  case OMP_PARALLEL_FOR:      return "PARALLEL_FOR";
  case OMP_PARALLEL_SECTIONS: return "PARALLEL_SECTIONS";


  case OMP_SIMD:  return "SIMD";
  case OMP_LOOP_SIMD: return "LOOP_SIMD";
  case OMP_PARALLEL_LOOP_SIMD: return "PARALLEL_LOOP_SIMD";
  case OMP_DECLARE_SIMD: return "DECLARE_SIMD";

  case OMP_DISTRIBUTE: return "DISTRIBUTE";
  case OMP_DISTRIBUTE_SIMD:  return "DISTRIBUTE_SIMD";
  case OMP_DISTRIBUTE_PARALLEL_LOOP: return "DISTRIBUTE_PARALLEL_LOOP";
  case OMP_DISTRIBUTE_PARALLEL_LOOP_SIMD: return "DISTRIBUTE_PARALLEL_LOOP_SIMD";


  case OMP_TEAMS: return "TEAMS";
  case OMP_TEAMS_DISTRIBUTE: return "TEAMS_DISTRIBUTE";
  case OMP_TEAMS_DISTRIBUTE_SIMD: return "TEAMS_DISTRIBUTE_SIMD";
  case OMP_TEAMS_DISTRIBUTE_PARALLEL_LOOP: return "TEAMS_DISTRIBUTE_PARALLEL_LOOP";
  case OMP_TEAMS_DISTRIBUTE_PARALLEL_LOOP_SIMD: return "TEAMS_DISTRIBUTE_PARALLEL_LOOP_SIMD";

  case OMP_TARGET:  return "TARGET";
  case OMP_TARGET_DATA: return "TARGET_DATA";
  case OMP_TARGET_ENTER_DATA: return "TARGET_ENTER_DATA";
  case OMP_TARGET_EXIT_DATA: return "TARGET_EXIT_DATA";
  case OMP_TARGET_UPDATE: return "TARGET_UPDATE";

  case OMP_TARGET_SIMD: return "TARGET_SIMD";
  case OMP_TARGET_PARALLEL: return "TARGET_PARALLEL";
  case OMP_TARGET_PARALLEL_LOOP: return "TARGET_PARALLEL_LOOP";
  case OMP_TARGET_PARALLEL_LOOP_SIMD: return "TARGET_PARALLEL_LOOP_SIMD";

  case OMP_TARGET_TEAMS: return "TARGET_TEAMS";
  case OMP_TARGET_TEAMS_DISTRIBUTE: return "TARGET_TEAMS_DISTRIBUTE";
  case OMP_TARGET_TEAMS_DISTRIBUTE_SIMD: return "TARGET_TEAMS_DISTRIBUTE_SIMD";
  case OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_LOOP:
    return "TARGET_TEAMS_DISTRIBUTE_PARALLEL_LOOP";
  case OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_LOOP_SIMD:
    return "OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_LOOP_SIMD";

  case OMP_DECLARE_TARGET:    return "DECLARE_TARGET";

  case OMP_TASK: return "TASK";
  case OMP_TASKLOOP: return "TASKLOOP";
  case OMP_TASKLOOP_SIMD: return "TASKLOOP_SIMD";
  case OMP_TASKYIELD: return "TASKYIELD";
  case OMP_TASKWAIT: return "TASKWAIT";
  case OMP_TASKGROUP: return "TASKGROUP";
  case OMP_CANCEL: return "CANCEL";
  case OMP_CANCELLATION_POINT: return "CANCELLATION_POINT";
  case OMP_DECLARE_REDUCTION: return "DECLARE_REDUCTION";
  case OMP_DECLARE_TARGET_START: return "DECLARE_TARGET_START";
  case OMP_DECLARE_TARGET_END: return "OMP_DECLARE_TARGET_END";

  default:
    return "OMP???";
  }
}

char *ompClauseName(int c)
{
  switch(c){
  case OMP_DATA_DEFAULT:          return "DATA_DEFAULT";
  case OMP_DATA_PRIVATE:          return "DATA_PRIVATE";
  case OMP_DATA_SHARED:           return "DATA_SHARED";
  case OMP_DATA_FIRSTPRIVATE:     return "DATA_FIRSTPRIVATE";
  case OMP_DATA_LASTPRIVATE:      return "DATA_LASTPRIVATE";
  case OMP_DATA_COPYIN:           return "DATA_COPYIN";

  case OMP_DATA_REDUCTION_PLUS:   return "DATA_REDUCTION_PLUS";
  case OMP_DATA_REDUCTION_MINUS:  return "DATA_REDUCTION_MINUS";
  case OMP_DATA_REDUCTION_MUL:    return "DATA_REDUCTION_MUL";
  case OMP_DATA_REDUCTION_BITAND: return "DATA_REDUCTION_BITAND";
  case OMP_DATA_REDUCTION_BITOR:  return "DATA_REDUCTION_BITOR";
  case OMP_DATA_REDUCTION_BITXOR: return "DATA_REDUCTION_BITXOR";
  case OMP_DATA_REDUCTION_LOGAND: return "DATA_REDUCTION_LOGAND";
  case OMP_DATA_REDUCTION_LOGOR:  return "DATA_REDUCTION_LOGOR";
  case OMP_DATA_REDUCTION_MIN:    return "DATA_REDUCTION_MIN";
  case OMP_DATA_REDUCTION_MAX:    return "DATA_REDUCTION_MAX";

  case OMP_DIR_ORDERED:           return "DIR_ORDERED";
  case OMP_DIR_IF:                return "DIR_IF";
  case OMP_DIR_NOWAIT:            return "DIR_NOWAIT";
  case OMP_DIR_SCHEDULE:          return "DIR_SCHEDULE";
  case OMP_DIR_NUM_THREADS:       return "DIR_NUM_THREADS";
  case OMP_COLLAPSE:              return "COLLAPSE";
  case OMP_DECLARE_TARGET_TO:     return "DECLARE_TARGET_TO";
  case OMP_TARGET_DEVICE:         return "TARGET_DEVICE";
  case OMP_TARGET_SHADOW:         return "TARGET_SHADOW";
  case OMP_TARGET_LAYOUT:         return "TARGET_LAYOUT";
  case OMP_TARGET_DATA_MAP:       return "TARGET_DATA_MAP";
  case OMP_DEPEND:                return "DEPEND";
  case OMP_FINAL:                 return "FINAL";
  case OMP_PRIORITY:              return "PRIORITY";
  case OMP_UNTIED:                return "UNTIED";
  case OMP_MERGEABLE:             return "MERGEABLE";
  case OMP_GRAINSIZE:             return "GRAINSIZE";
  case OMP_NUM_TASKS:             return "NUM_TASKS";
  case OMP_NOGROUP:               return "NOGROUP";
  case OMP_IS_DEVICE_PTR:         return "IS_DEVICE_PTR";
  case OMP_USE_DEVICE_PTR:        return "USE_DEVICE_PTR";
  case OMP_DATA_DEFAULTMAP:       return "DEFAULTMAP";
  case OMP_TARGET_UPDATE_TO:      return "TARGET_UPDATE_TO";
  case OMP_TARGET_UPDATE_FROM:    return "TARGET_UPDATE_FROM";
  case OMP_DATA_DECALRE_LINK:     return "DATA_DECALRE_LINK";
  default:                        return "???OMP???";
  }
}

char *ompScheduleName(int c)
{
    switch(c){
    case  OMP_SCHED_NONE: return "SCHED_NONE";
    case OMP_SCHED_STATIC: return "SCHED_STATIC";
    case OMP_SCHED_DYNAMIC: return "SCHED_DYNAMIC";
    case OMP_SCHED_GUIDED: return "SCHED_GUIDED";
    case OMP_SCHED_RUNTIME: return "SCHED_RUNTIME";
    case OMP_SCHED_AFFINITY: return "SCHED_AFFINITY";
    case OMP_SCHED_AUTO: return "SCHED_AUTO";
    default: 
	return "SCHED_???";
    }
}

char *ompDataDefaultName(int c)
{
    switch(c){
    case OMP_DEFAULT_NONE:  return "DEFAULT_NONE";
    case OMP_DEFAULT_SHARED:  return "DEFAULT_SHARED";
    case OMP_DEFAULT_PRIVATE:  return "DEFAULT_PRIVATE";
    default:
	return "DEFAULT_???";
    }
}
