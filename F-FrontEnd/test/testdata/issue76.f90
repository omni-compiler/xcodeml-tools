module mod1
  implicit none

  private

  type :: t_comm_pattern
  end type

  class(t_comm_pattern), pointer :: comm_pat_glb_to_loc_c
end module mod1

