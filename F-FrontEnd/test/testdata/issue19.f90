MODULE mod1
  IMPLICIT NONE
  PRIVATE

  TYPE t_lnd_prog
  END TYPE t_lnd_prog
  
  TYPE t_lnd_state
    TYPE(t_lnd_prog), ALLOCATABLE  :: prog_lnd(:)
  END TYPE t_lnd_state

CONTAINS

  SUBROUTINE save_initial_state(p_lnd)
    TYPE(t_lnd_state), TARGET, INTENT(IN) :: p_lnd(:)
    INTEGER :: jg

    TYPE(t_lnd_prog), POINTER :: lnd_prog

    lnd_prog => p_lnd(jg)%prog_lnd(1)
  END SUBROUTINE save_initial_state
END MODULE mod1
