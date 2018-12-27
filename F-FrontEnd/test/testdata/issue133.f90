module mod1
use issue133_1, only: jprb=>wp
use issue133_2, only: rday => rdaylen
contains
  elemental function rteta(ptime)
    real(kind=jprb)             :: rteta
    real(kind=jprb), intent(in) :: ptime
    rteta = ptime/(rday*365.25_jprb)
  end function rteta
end module mod1
