module mod1
implicit none
private
contains

  subroutine sub1(dim_c, dim_k)
    integer, intent(in) :: dim_c
    integer, intent(in) ::  dim_k
    integer :: work(8*min(dim_c, dim_k))
  end subroutine sub1

end module mod1
