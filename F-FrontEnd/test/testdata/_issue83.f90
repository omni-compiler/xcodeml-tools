MODULE issue83
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: p_gatherv
       
  INTERFACE p_gatherv
    MODULE PROCEDURE p_gatherv_int
  END INTERFACE
 CONTAINS
  SUBROUTINE p_gatherv_int()
  END SUBROUTINE p_gatherv_int
END MODULE issue83
