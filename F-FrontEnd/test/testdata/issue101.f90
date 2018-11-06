module mod1

contains
  function strip_from_modifiers(dt_string)
    character(LEN=*), INTENT(IN) :: dt_string
    character(LEN=LEN_TRIM(dt_string)) :: strip_from_modifiers
    character :: char
 
    char = strip_from_modifiers(1:1)
    select case(char)
      case ('>','<')
        strip_from_modifiers = strip_from_modifiers(2:)
    end select
  end function strip_from_modifiers
end module mod1
