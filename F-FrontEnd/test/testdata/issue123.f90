module fckit_shared_object_module
use fckit_shared_ptr_module, only : fckit_shared_ptr
implicit none
private

public :: fckit_shared_object

type, extends(fckit_shared_ptr) :: fckit_shared_object
end type

end module
