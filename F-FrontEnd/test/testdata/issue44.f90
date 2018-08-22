module mod1
  implicit none
  private 
  public :: toupper
contains
  function toupper (lowercase)
    character(len=*), intent(in) :: lowercase
    character(len=len_trim(lowercase)) :: toupper
  end function toupper
end module mod1
