module issue133_1
  implicit none
  private
  integer, parameter :: pd = 12
  integer, parameter :: rd = 307
  integer, parameter :: dp = selected_real_kind(pd,rd)
  integer, parameter :: wp = dp
  public :: wp
end module issue133_1
