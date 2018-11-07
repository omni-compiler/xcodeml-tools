MODULE mod1
  USE issue83

  INTERFACE p_gatherv
    MODULE PROCEDURE p_gatherv_1d
  END INTERFACE p_gatherv

CONTAINS
  SUBROUTINE p_gatherv_1d()
  END SUBROUTINE p_gatherv_1d
END MODULE mod1
