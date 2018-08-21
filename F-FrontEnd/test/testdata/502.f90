program main
  interface
     SUBROUTINE s0(f)
       INTERFACE
          FUNCTION f()
            INTEGER :: f
          END FUNCTION f
       END INTERFACE
       OPTIONAL :: f
     END SUBROUTINE s0
  end interface
  call s0
end program main

SUBROUTINE s0(f)
  INTERFACE
     FUNCTION f()
       INTEGER :: f
     END FUNCTION f
  END INTERFACE
  OPTIONAL :: f
END SUBROUTINE s0
