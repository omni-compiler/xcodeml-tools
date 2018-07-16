module mtime_print_by_callback
  implicit none
  private
  public :: register_print_mtime_procedure

  abstract interface
    subroutine registered_print_mtime_procedure(leading_text, message_text)
      character(len=*), intent(in) :: leading_text
      character(len=*), intent(in) :: message_text    
    end subroutine registered_print_mtime_procedure
  end interface

  procedure(registered_print_mtime_procedure), pointer :: print_message => null()
  
contains
  subroutine register_print_mtime_procedure(message_procedure)
    procedure(registered_print_mtime_procedure) :: message_procedure
    print_message => message_procedure
  end subroutine register_print_mtime_procedure

  subroutine print_mtime_datetime(leading_text, message_text)
    character(len=*), intent(in) :: leading_text
    character(len=*), intent(in) :: message_text    
    character(len=60)  :: dstring
    call print_message(trim(leading_text), trim(message_text)//' '//trim(dstring)) 
  end subroutine print_mtime_datetime
end module mtime_print_by_callback
