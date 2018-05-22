module mod1
  integer, parameter :: sp = selected_real_kind(6,37)
  integer, parameter :: dp = selected_real_kind(12,307)

contains

  subroutine sub1(arg1)
    class(*), intent(in) :: arg1

    select type(arg1)
      type is(real(sp))
        print*,'arg is sp'
      type is(real(dp))
        print*,'arg is dp'
    end select
   
  end subroutine sub1

end module mod1
