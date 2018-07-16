MODULE mod1

  IMPLICIT NONE

  TYPE :: t_destructible
  END TYPE t_destructible

  TYPE, EXTENDS(t_destructible) :: t_axis
    INTEGER :: axis_type
  END TYPE t_axis

CONTAINS

  INTEGER FUNCTION list_hashKey(key) RESULT(res)
    CLASS(t_destructible), POINTER, INTENT(in) :: key

    SELECT TYPE(key)
      TYPE IS(t_axis)
        res = key%axis_type
      CLASS DEFAULT
        print*,'default'
      END SELECT
  END FUNCTION list_hashKey

END MODULE mod1
