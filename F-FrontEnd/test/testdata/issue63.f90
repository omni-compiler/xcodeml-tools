module mod1
  implicit none
  private 

  integer :: calendar
  character(len=32) :: ini_datetime_string, end_datetime_string

  namelist /time_nml/ calendar, ini_datetime_string, end_datetime_string

end module mod1
