MODULE issue27_mod
  IMPLICIT NONE

  TYPE distrib
  CONTAINS
    PROCEDURE localsize => localsize
  END TYPE distrib
CONTAINS
    FUNCTION blk_no(i) result(no)
      INTEGER, INTENT(IN) :: i
      INTEGER :: no
      no = 20
    END FUNCTION blk_no

    FUNCTION localsize(this) result(lsize)
      CLASS(distrib), INTENT(IN) :: this
      INTEGER :: lsize
      lsize = 10
    END FUNCTION localsize

    SUBROUTINE sub1(nproma, distribution)
      INTEGER, INTENT(IN) :: nproma
      CLASS(distrib), INTENT(IN) :: distribution
      REAL, POINTER :: val(:,:)
      ALLOCATE(val(nproma, blk_no(distribution%localSize())))
    END SUBROUTINE sub1
END MODULE issue27_mod
