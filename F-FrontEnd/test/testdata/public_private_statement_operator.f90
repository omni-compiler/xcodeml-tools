module mtime_timedelta
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
