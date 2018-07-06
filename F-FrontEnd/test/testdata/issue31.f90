MODULE mod1

  IMPLICIT NONE

  PRIVATE

  TYPE type1
  END TYPE type1

  TYPE type2
    TYPE(type1),  ALLOCATABLE :: prog(:)
  END TYPE type2

  PUBLIC :: compute_tendencies

CONTAINS

  SUBROUTINE compute_tendencies(n_new,n_now,n_new_rcf,n_now_rcf)
    INTEGER,  INTENT(IN) :: n_new,n_now
    INTEGER,  INTENT(IN) :: n_new_rcf,n_now_rcf

    TYPE(type2), POINTER :: p_nh
    TYPE(type1),  POINTER :: p_prog_now
    TYPE(type1),  POINTER :: p_prog_new
    TYPE(type1),  POINTER :: p_prog_now_rcf
    TYPE(type1),  POINTER :: p_prog_new_rcf


    p_prog_now     => p_nh%prog(n_now)
    p_prog_new     => p_nh%prog(n_new)
    p_prog_now_rcf => p_nh%prog(n_now_rcf)
    p_prog_new_rcf => p_nh%prog(n_new_rcf)

  END SUBROUTINE compute_tendencies

END MODULE mod1

