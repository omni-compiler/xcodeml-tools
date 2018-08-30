MODULE mo_var_list
  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL_T = 2

  PRIVATE

CONTAINS
  SUBROUTINE add_var_list_element_5d(ndims, data_type, name)
    INTEGER, INTENT(IN) :: ndims 
    INTEGER, INTENT(IN) :: data_type
    CHARACTER(len=*), INTENT(IN) :: name
  END SUBROUTINE add_var_list_element_5d

  SUBROUTINE sub1(data_type_dd)
    INTEGER, INTENT(IN) :: data_type_dd
  END SUBROUTINE sub1

  SUBROUTINE add_var_list_element_r4d(name)
    CHARACTER(len=*), INTENT(IN) :: name
    INTEGER :: ndims

    CALL add_var_list_element_5d(ndims, REAL_T, name)
    CALL sub1(data_type_dd = REAL_T)
  END SUBROUTINE add_var_list_element_r4d

END MODULE mo_var_list
