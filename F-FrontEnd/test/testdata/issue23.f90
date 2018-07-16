MODULE mod1
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: compute1
CONTAINS
  SUBROUTINE setup_comm(a)
    INTEGER, INTENT(IN) :: a(:)
    print*,a
  END SUBROUTINE setup_comm 

  SUBROUTINE compute1
    INTEGER :: i, jg, my_id, nthis
    my_id = 11
    nthis = 5

    DO i=1, 2
      DO jg=1, 2
        CALL setup_comm((/(my_id, i = 1, nthis)/))
      END DO
    END DO
  END SUBROUTINE compute1
END MODULE mod1
