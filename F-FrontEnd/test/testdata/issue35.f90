MODULE mo_sw_test
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: init_will3_test

  PUBLIC :: symmetric_u_velo
  CONTAINS

  SUBROUTINE init_will3_test ()
    IMPLICIT NONE
    REAL :: z_rotlat, z_uu, z_hh, u0
    
    z_hh = geostr_balance(symmetric_u_velo)
    z_uu = symmetric_u_velo(z_rotlat, u0) * z_uu
  END SUBROUTINE init_will3_test

  FUNCTION geostr_balance(func)  RESULT(p_hh)
    INTERFACE                        ! selected function
      FUNCTION func(p_t, u0) RESULT(p_vv)
        REAL, INTENT(in) :: p_t, u0
        REAL             :: p_vv
      END FUNCTION func
    END INTERFACE
    REAL :: p_hh
  END FUNCTION geostr_balance

  FUNCTION symmetric_u_velo(p_rlatd, u0) RESULT (p_usres)
    REAL,INTENT(in) :: p_rlatd, u0
    REAL :: p_usres
  END FUNCTION symmetric_u_velo
END MODULE mo_sw_test


