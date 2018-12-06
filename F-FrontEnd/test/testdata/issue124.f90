module fckit_shared_object_module
use issue124_mod
implicit none
private

public :: fckit_shared_object

type, extends(fckit_shared_ptr) :: fckit_shared_object
end type

end module
