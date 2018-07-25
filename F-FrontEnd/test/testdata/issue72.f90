module mtime_datetime
  implicit none
  private
  public :: min, max
  public :: assignment(=)
  public :: operator(>)
  public :: operator(<)
  public :: operator(<=)
  public :: operator(>=)
  public :: operator(==)
  public :: operator(/=)

  interface min
  end interface min
  
  interface max
  end interface max
  
  interface assignment (=)
  end interface assignment (=)
  
  interface operator (>)
  end interface operator (>)
  
  interface operator (<)
  end interface operator (<)
  
  interface operator (<=)
  end interface operator (<=)
  
  interface operator (>=)
  end interface operator (>=)
  
  interface operator (==)
  end interface operator (==)
  
  interface operator (/=)
  end interface operator (/=)
end module mtime_datetime

module mtime_timedelta
  use mtime_datetime
  implicit none
  private

  public :: operator(+)
  public :: operator(-)
  public :: operator(*)
  public :: operator(>)
  public :: operator(<)
  public :: operator(<=)
  public :: operator(>=)
  public :: operator(==)
  public :: operator(/=)
   
  interface operator (+)
  end interface operator (+)
  
  interface operator (-)
  end interface operator (-)
  
  interface operator (*)
  end interface operator (*)
  
  interface operator (>)
  end interface operator (>)
  
  interface operator (<)
  end interface operator (<)
  
  interface operator (<=)
  end interface operator (<=)
  
  interface operator (>=)
  end interface operator (>=)
  
  interface operator (==)
  end interface operator (==)
  
  interface operator (/=)
  end interface operator (/=)
  
end module mtime_timedelta
