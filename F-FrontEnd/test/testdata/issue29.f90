MODULE mod1

  IMPLICIT NONE

  SAVE
  INTEGER, PARAMETER :: wp = selected_real_kind(12,307)

  REAL(wp), PARAMETER :: tlbound =  50.0_wp
  REAL(wp), PARAMETER :: tubound = 400.0_wp
  REAL(wp), PARAMETER :: rfdeltat = 1000.0_wp
  INTEGER,  PARAMETER :: jptlucu1 = NINT(rfdeltat*tlbound)
  INTEGER,  PARAMETER :: jptlucu2 = NINT(rfdeltat*tubound)
  REAL(wp) :: tlucua(jptlucu1-1:jptlucu2+1)
  REAL(wp) :: tlucuaw(jptlucu1-1:jptlucu2+1)
  REAL(wp) :: tlucuad(jptlucu1-1:jptlucu2+1)
  REAL(wp) :: tlucuawd(jptlucu1-1:jptlucu2+1)
  REAL(wp) :: tlucub(jptlucu1-1:jptlucu2+1)
  REAL(wp) :: tlucubw(jptlucu1-1:jptlucu2+1)
  REAL(wp) :: tlucuc(jptlucu1-1:jptlucu2+1)
  REAL(wp) :: tlucucw(jptlucu1-1:jptlucu2+1)

CONTAINS
  SUBROUTINE init_convect_tables
    REAL(wp) :: zalvdcp, zalsdcp, rv, rd
    REAL(wp) :: ztt, zldcp
    REAL(wp) :: zminner,zdminner,zlinner,zdlinner
    INTEGER :: it
    DO it = jptlucu1-1, jptlucu2+1
      tlucua(it)  = EXP(zminner)*rd/rv
      tlucuaw(it) = EXP(zlinner)*rd/rv

      tlucuad(it)  = zdminner*EXP(zminner)*rd/rv
      tlucuawd(it) = zdlinner*EXP(zlinner)*rd/rv

      tlucub(it)  = zldcp*zdminner
      tlucubw(it) = zalvdcp*zdlinner

      tlucuc(it)  = zldcp
      tlucucw(it) = zalvdcp
    END DO
  END SUBROUTINE init_convect_tables
END MODULE mod1

