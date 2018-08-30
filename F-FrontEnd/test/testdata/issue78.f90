module mod1
  use issue78_mod
  implicit none

contains

  subroutine sub1 ()
    type(t_tileinfo) :: tileinfo
    tileinfo = t_tileinfo(1)
  end subroutine sub1
END MODULE mod1
