module mod1

type :: type1
  real, allocatable :: mem1(:)
end type type1

contains
  subroutine sub1()
    type(type1) :: a
    type(type1) :: b

    associate(am => a%mem1(1:5), bm => b%mem1(1:5))
    end associate
  end subroutine sub1
end module mod1 
