module mod1
  USE mpi42
  IMPLICIT NONE
  PRIVATE
  INTEGER, PARAMETER :: ppm_address_kind = mpi_address_kind
  INTEGER(ppm_address_kind), PARAMETER :: ppm_maximum_alignment = 64
contains
  subroutine sub1
   if(IAND(ppm_maximum_alignment, ppm_maximum_alignment - 1_ppm_address_kind) == 0_ppm_address_kind) THEN
   end if 
  end subroutine sub1
end module mod1
