module mod1
  use issue546_mod2, only: wp
  implicit none
  private
  REAL(wp), PARAMETER :: test = 1.0_wp
end module mod1 
