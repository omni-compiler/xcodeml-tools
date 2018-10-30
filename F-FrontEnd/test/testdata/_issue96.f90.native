module mo_operator
  implicit none
  private

  public :: operator(>)
  public :: operator(.lt.)

  interface operator(>)
    module procedure greater_than
  end interface operator(>)

  interface operator(.lt.)
    module procedure less_than
  end interface operator(.lt.)

  type :: t1
    integer :: id
  end type t1

contains

  function greater_than(lhs, rhs) result(gt) 
    logical :: gt
    type(t1), intent(in) :: lhs
    type(t1), intent(in) :: rhs
    gt = lhs%id > rhs%id
  end function greater_than

  function less_than(lhs, rhs) result(lt) 
    logical :: lt
    type(t1), intent(in) :: lhs
    type(t1), intent(in) :: rhs
    lt = lhs%id < rhs%id
  end function greater_than
end module mo_operator
