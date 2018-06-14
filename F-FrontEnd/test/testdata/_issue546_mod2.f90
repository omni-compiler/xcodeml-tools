module issue546_mod2
  use issue546_mod1, only: dp
  implicit none
  public
  integer, parameter :: wp = dp
end module issue546_mod2
