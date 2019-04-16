module op_module
  implicit none
  interface operator (.feq.)
    module procedure equal
  end interface
contains
  function equal(a,b) result(res)
    real, intent(in) :: a
    real, intent(in) :: b
    logical :: res
    res = abs( a - b ) < spacing( max( abs( a ), abs( b ) ) )
  end function
end module op_module

