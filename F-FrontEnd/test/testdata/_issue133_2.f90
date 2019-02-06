module issue133_2
  USE issue133_1, ONLY: wp
  implicit none
  public
  real(wp), parameter :: rdaylen=86400._wp
  real(wp), parameter :: rdaylen2=86400._8
end module issue133_2
