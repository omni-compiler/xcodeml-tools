module mod1

contains

  subroutine sub1(arg1)
    class(*), intent(in) :: arg1

    select type(arg1)
      type is(real(4))
        print*,'arg is sp'
      type is(real(8))
        print*,'arg is dp'
    end select
   
  end subroutine sub1

end module mod1
