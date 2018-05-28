MODULE mod1

  IMPLICIT NONE

  SAVE
  INTEGER, PARAMETER :: wp = selected_real_kind(12,307)

  REAL(wp), PARAMETER :: tlbound =  50.0_wp  ! lower bound [K]
  REAL(wp), PARAMETER :: tubound = 400.0_wp  ! upper bound [K]
  REAL(wp), PARAMETER :: rfdeltat = 1000.0_wp
  INTEGER,  PARAMETER :: jptlucu1 = NINT(rfdeltat*tlbound) ! lookup table lower bound
  INTEGER,  PARAMETER :: jptlucu2 = NINT(rfdeltat*tubound) ! lookup table upper bound
  REAL(wp) :: tlucua(jptlucu1-1:jptlucu2+1)     ! table - Es*Rd/Rv, mixed phases
  REAL(wp) :: tlucuaw(jptlucu1-1:jptlucu2+1)    ! table - Es*Rd/Rv, water phase only
  REAL(wp) :: tlucuad(jptlucu1-1:jptlucu2+1)    ! table - dEs/dT*Rd/Rv, mixed phases
  REAL(wp) :: tlucuawd(jptlucu1-1:jptlucu2+1)   ! table - dEs/dT*Rd/Rv, water phase only
  REAL(wp) :: tlucub(jptlucu1-1:jptlucu2+1)     ! table - inner dEs/dT*L/cp, mixed phases
  REAL(wp) :: tlucubw(jptlucu1-1:jptlucu2+1)    ! table - inner dEs/dT*L/cp, water phase only
  REAL(wp) :: tlucuc(jptlucu1-1:jptlucu2+1)     ! table - L/cp, mixed phases
  REAL(wp) :: tlucucw(jptlucu1-1:jptlucu2+1)    ! table - L/cp, water phase only

CONTAINS
  SUBROUTINE init_convect_tables
    REAL(wp) :: zalvdcp, zalsdcp
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

