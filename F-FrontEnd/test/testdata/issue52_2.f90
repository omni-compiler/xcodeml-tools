MODULE mod1

  
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: process_phy_nml

  REAL :: sample

  NAMELIST /phy_nml/ sample

CONTAINS

  SUBROUTINE process_nml( filename, nml_name, nml_read, nml_write )
 
     CHARACTER(LEN=*), INTENT(IN) :: filename
     CHARACTER(LEN=*), INTENT(IN) :: nml_name
 
     INTERFACE
        !
        SUBROUTINE nml_read (funit)
          INTEGER, INTENT(in) :: funit
        END SUBROUTINE nml_read
        !
        SUBROUTINE nml_write(funit)
          INTEGER, INTENT(in) :: funit
        END SUBROUTINE nml_write
        !
     END INTERFACE

  END SUBROUTINE process_nml

  SUBROUTINE process_phy_nml(filename)
    !
    CHARACTER(LEN=*), INTENT(in) :: filename
    !
    !
    CALL process_nml(filename, 'phy_nml', nml_read, nml_write)
    !
  CONTAINS
    !
    SUBROUTINE nml_read(funit)
      INTEGER, INTENT(in) :: funit
      READ(funit, NML=phy_nml)
    END SUBROUTINE nml_read
    !
    SUBROUTINE nml_write(funit)
      INTEGER, INTENT(in) :: funit
      WRITE(funit, NML=phy_nml)
    END SUBROUTINE nml_write
    !
  END SUBROUTINE process_phy_nml

END MODULE mod1
