MODULE mo_var_list
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: find_list_element

  INTERFACE find_element
    MODULE PROCEDURE find_list_element
  END INTERFACE find_element

CONTAINS

  SUBROUTINE get_var_list_element_info()
    REAL, POINTER :: element
    element => find_list_element ()
  END SUBROUTINE get_var_list_element_info

  FUNCTION find_list_element () RESULT(this_list_element)
     REAL, POINTER :: this_list_element
  END FUNCTION find_list_element
END MODULE mo_var_list
