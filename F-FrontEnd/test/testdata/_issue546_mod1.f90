module issue546_mod1
  implicit none
  private
  INTEGER, PARAMETER :: ps =   6
  INTEGER, PARAMETER :: rs =  37
  INTEGER, PARAMETER :: pd =  12
  INTEGER, PARAMETER :: rd = 307
  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(ps,rs)
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(pd,rd)
  INTEGER, PARAMETER :: wp = dp
  PUBLIC :: sp, dp, wp
end module issue546_mod1
