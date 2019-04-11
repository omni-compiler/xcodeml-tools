module mod146_2
  interface sub
     module procedure sub2
  end interface sub
contains
  subroutine sub2(val)
    integer :: val
  end subroutine sub2
end module mod146_2
