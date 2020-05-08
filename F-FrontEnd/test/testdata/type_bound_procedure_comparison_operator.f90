PROGRAM test_mhl

  USE mhl

  TYPE(t_dt) :: a
  TYPE(t_dt) :: b

  a = t_dt(3, 4)
  b = t_dt(2, 3)

  print *, a == b, ' expect false' 
  print *, a /= b, ' expect true' 
  print *, a < b,  ' expect false' 
  print *, a > b,  ' expect true' 
  print *, a <= b, ' expect false' 
  print *, a >= b, ' expect true'   
  
END PROGRAM test_mhl
