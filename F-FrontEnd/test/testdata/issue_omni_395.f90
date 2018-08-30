PROGRAM main
  CALL sub()
END PROGRAM main

MODULE m
  INTEGER :: hoge
END MODULE m

SUBROUTINE sub ( )
  USE m
  PRINT * , hoge
END SUBROUTINE sub
