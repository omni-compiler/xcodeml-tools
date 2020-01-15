module mod1
  ! 16.9.170 - 3 At least one argument shall be present.
  integer, parameter :: r1 = selected_real_kind(6)
  integer, parameter :: sp = selected_real_kind(6, 37)
  integer, parameter :: dp = selected_real_kind(12,307)
  integer, parameter :: wp = dp

  integer, parameter :: i1 = selected_int_kind(4);
  integer, parameter :: i2 = selected_int_kind(8);

contains

  subroutine sub1(arg1)
    class(*), intent(in) :: arg1

    select type(arg1)
      type is(real(sp))
        print*,'arg is sp'
      type is(real(dp))
        print*,'arg is dp'
      type is(integer(i1))
        print*,'arg is i1'
      type is(integer(i2))
        print*,'arg is i2'
    end select

   select type(arg1)
      type is(real(r1))
        print*,'arg is r1'
      type is(real(dp))
        print*,'arg is dp'
    end select

   select type(arg1)
      type is(integer)
        print*,'arg is integer'
      type is(real(wp))
        print*,'arg is wp'
      type is(real(sp))
        print*,'arg is sp'
    end select
  end subroutine sub1
end module mod1
