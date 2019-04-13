module m1
  interface sub
     module procedure sub1
  end interface sub
contains
  subroutine sub1(val)
    real :: val
  end subroutine sub1
end module m1

module m2
  interface sub
     module procedure sub2
  end interface sub
contains
  subroutine sub2(val)
    integer :: val
  end subroutine sub2
end module m2

program test
  use m1
  use m2
  call sub(5)
end program test
