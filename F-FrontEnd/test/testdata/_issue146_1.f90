module mod146_1
  interface sub
     module procedure sub1
  end interface sub
contains
  subroutine sub1(val)
    real :: val
  end subroutine sub1
end module mod146_1
