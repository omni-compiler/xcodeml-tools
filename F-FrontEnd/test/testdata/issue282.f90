module mod1
  use mo_issue282 ! R504 use statement must be here
  implicit none


  real(kind=wp), parameter :: c1 = 0._wp
contains
  subroutine sub1()
    implicit none
    real(kind=wp) :: gcc(4) = (/c0, 500._wp, c0, c1/)
  end subroutine sub1
end module mod1
