module issue78_mod
  implicit none
  
  private

  public :: t_tileinfo

  type, abstract:: t_tileinfo_elt
  end type t_tileinfo_elt
 
  type, extends(t_tileinfo_elt) :: t_tileinfo
    integer :: idx
  end type t_tileinfo

end module issue78_mod
