module issue124_mod
implicit none
private

public fckit_shared_ptr
public fckit_shared_ptr__final_auto

type :: fckit_shared_ptr
  logical, private :: return_value = .false.

contains
  procedure, public :: final => fckit_shared_ptr__final

  final :: fckit_shared_ptr__final_auto
end type
contains

subroutine fckit_shared_ptr__final_auto(this)
  type(fckit_shared_ptr), intent(inout) :: this
  call this%final()
end subroutine

subroutine fckit_shared_ptr__final(this)
  class(fckit_shared_ptr), intent(inout) :: this
end subroutine

end module
