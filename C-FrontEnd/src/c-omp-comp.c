/**
 * \file c-omp-comp.c
 */

#include "c-comp.h"
#include "c-omp.h"

PRIVATE_STATIC void
out_OMP_subscript(CExpr *subscript, CExpr *parent)
{
  if(EXPR_CODE(subscript) != EC_UNDEF){
    compile1(subscript, parent);
  }else{
    CExpr *lower = exprListHeadData(subscript);
    if(! EXPR_ISNULL(lower)){
      compile1(lower, parent);
    }

    if(EXPR_L_SIZE(subscript) > 1){
      CExpr *length = exprListNextNData(subscript, 1);
      if(! EXPR_ISNULL(length)){
	compile1(length, parent);
      }
    }
  }
}


PRIVATE_STATIC void
compile_OMP_arrayRef(CExprOfBinaryNode *arrayRef, CExpr *parent)
{
  CExpr *arrayExpr = arrayRef->e_nodes[0];
  CExpr *subscripts = arrayRef->e_nodes[1];

  compile1(arrayExpr, parent);

  CCOL_DListNode *ite;
  EXPR_FOREACH(ite, subscripts){
    CExpr *node = EXPR_L_DATA(ite);
    out_OMP_subscript(node, parent);
  }
}

PRIVATE_STATIC void
compile_OMP_name_list(CExprOfList *name_list, CExpr *parent)
{
  CCOL_DListNode *ite;

  EXPR_FOREACH(ite, name_list) {
    CExpr *node = EXPR_L_DATA(ite);

    if(EXPR_CODE(node) == EC_ARRAY_REF){
      compile_OMP_arrayRef((CExprOfBinaryNode*)node, parent);
    }else{
      compile1(node, parent);
    }
  }
}

PRIVATE_STATIC void
compile_OMP_clause(CExpr *clause, CExpr *parent)
{
  enum OMP_pragma_clause clause_code = ((CExprOfList*)clause)->e_aux;
  CExpr *arg = exprListHeadData(clause);

  switch(clause_code){
  case OMP_DATA_PRIVATE:
  case OMP_DATA_FIRSTPRIVATE:
  case OMP_DATA_LASTPRIVATE:
  case OMP_DATA_SHARED:
  case OMP_DATA_COPYIN:
  case OMP_DATA_COPYPRIVATE:
  case OMP_THREADPRIVATE:
    {
      /* name list */
      CExprOfList *name_list = (CExprOfList *)arg;
      if(EXPR_L_ISNULL(name_list)) return;
      compile_OMP_name_list(name_list, parent);
    }
    break;

  case OMP_DIR_NUM_THREADS:
  case OMP_COLLAPSE:
  case OMP_FINAL:
  case OMP_PRIORITY:
  case OMP_GRAINSIZE:
  case OMP_NUM_TASKS:
  case OMP_NUM_TEAMS:
  case OMP_THREAD_LIMIT:
  case OMP_SAFELEN:
  case OMP_SIMDLEN:
    compile1(arg, parent);
    break;

  case OMP_DIR_IF:
    compile1(arg, parent);
    break;

  case OMP_TARGET_DEVICE:
  case OMP_TARGET_SHADOW:
    /* { */
    /*   CCOL_DListNode *ite; */
    /*   EXPR_FOREACH(ite, arg){ */
    /* 	CExpr *node = EXPR_L_DATA(ite); */
    /* 	out_ACC_subscript(fp,indent1+1,node); */
    /*   } */
    /*   break; */
    /* } */
  case OMP_DIR_SCHEDULE:
  case OMP_DIST_SCHEDULE:
    // out_OMP_schedule(fp, indent1, arg);
    break;

  case OMP_DEPEND:
    // namelist = (CExprOfList *)arg;
    // if(EXPR_L_SIZE(namelist) != 0)
    // out_OMP_depend(fp, indent1, namelist);
    break;

  case OMP_TARGET_DATA_MAP:
    {
      CExprOfList *map_namelist = (CExprOfList *)arg;
      if(EXPR_L_SIZE(map_namelist) == 0) break;

      CCOL_DListNode *ite;

#if 1
    EXPR_FOREACH(ite, map_namelist) {
	CExpr *node = EXPR_L_DATA(ite);
	if(EXPR_CODE(node) == EC_ARRAY_REF){
	  compile_OMP_arrayRef((CExprOfBinaryNode*)node, parent);
	} else {
	  compile1(node, parent);
	}
    }
#else      
      EXPR_FOREACH(ite, map_namelist) {
	CExpr *v = EXPR_L_DATA(ite);
	if (EXPR_CODE(v) == EC_NUMBER_CONST) continue;
	compile1(v, parent);
      }
#endif
    }
    break;

  case OMP_DATA_LINEAR:
    // out_OMP_linear(fp, indent1, arg);
    break;

  case OMP_ALIGNED:
    // out_OMP_aligned(fp, indent1, arg);
    break;
  default:
    break;
  }
}

/**
 * \brief
 * compile #pragma omp
 *
 * @param parent
 *      parent node
 */
void
compile_OMP_pragma(CExpr *expr, CExpr *parent)
{
  CExprOfList *body = (CExprOfList *)expr;
  CExprOfList *clauseList = (CExprOfList *)body->e_aux_info;

  int pragma_code = clauseList->e_aux;

  switch(pragma_code){
  case OMP_ATOMIC:
    break;
  case OMP_CRITICAL:
  case OMP_FLUSH:
  case OMP_THREADPRIVATE:
    compile_OMP_name_list(clauseList, parent);
    break;
  default:
    {
      CCOL_DListNode *ite;
      EXPR_FOREACH(ite, clauseList) {
	CExpr *clause = EXPR_L_DATA(ite);
	compile_OMP_clause(clause, parent);
      }
    }
  }
}
