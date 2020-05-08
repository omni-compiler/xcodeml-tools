MODULE mhl

  IMPLICIT NONE

  PUBLIC :: t_dt

  TYPE t_dt

    INTEGER :: i1
    INTEGER :: i2
    
  CONTAINS
    
    PROCEDURE :: equal_dt            => t_dt_equal
    PROCEDURE :: not_equal_dt        => t_dt_not_equal
    PROCEDURE :: less_than_dt        => t_dt_less_than
    PROCEDURE :: greater_than_dt     => t_dt_greater_than
    PROCEDURE :: less_or_equal_dt    => t_dt_less_or_equal
    PROCEDURE :: greater_or_equal_dt => t_dt_greater_or_equal

    GENERIC   :: OPERATOR(==)        => equal_dt
    GENERIC   :: OPERATOR(/=)        => not_equal_dt
    GENERIC   :: OPERATOR(<)         => less_than_dt
    GENERIC   :: OPERATOR(>)         => greater_than_dt
    GENERIC   :: OPERATOR(<=)        => less_or_equal_dt
    GENERIC   :: OPERATOR(>=)        => greater_or_equal_dt
    
  END TYPE t_dt

CONTAINS

  LOGICAL FUNCTION t_dt_equal(this, dt) result(eq)
    CLASS (t_dt),  INTENT(in), target :: this
    CLASS (t_dt),  INTENT(in), target :: dt
    if ((this%i1 == dt%i1) .and. (this%i2 == dt%i2)) then
      eq = .true.
    else
      eq = .false.
    endif    
  END FUNCTION t_dt_equal

  LOGICAL FUNCTION t_dt_not_equal(this, dt)
    CLASS (t_dt),  INTENT(in) :: this
    CLASS (t_dt),  INTENT(in) :: dt
    t_dt_not_equal = .not. (this == dt)
  END FUNCTION t_dt_not_equal

  LOGICAL FUNCTION t_dt_less_than(this, dt)  result(lt)
    CLASS (t_dt),  INTENT(in), target :: this
    CLASS (t_dt),  INTENT(in), target :: dt
    if ((this%i1 + this%i2) < (dt%i1 + dt%i2)) then
      lt = .true.
    else
      lt = .false.
    endif    
  END FUNCTION t_dt_less_than

  LOGICAL FUNCTION t_dt_greater_than(this, dt)  result(gt)
    CLASS (t_dt),  INTENT(in), target :: this
    CLASS (t_dt),  INTENT(in), target :: dt
    if ((this%i1 + this%i2) > (dt%i1 + dt%i2)) then
      gt = .true.
    else
      gt = .false.
    endif    
  END FUNCTION t_dt_greater_than

  LOGICAL FUNCTION t_dt_less_or_equal(this, dt)
    CLASS (t_dt),  INTENT(in) :: this
    CLASS (t_dt),  INTENT(in) :: dt
    t_dt_less_or_equal = .not. (this > dt)
  END FUNCTION t_dt_less_or_equal

  LOGICAL FUNCTION t_dt_greater_or_equal(this, dt)
    CLASS (t_dt),  INTENT(in) :: this
    CLASS (t_dt),  INTENT(in) :: dt
    t_dt_greater_or_equal = .not. (this < dt)
  END FUNCTION t_dt_greater_or_equal
  
END MODULE mhl
