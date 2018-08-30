module mod1
contains
  subroutine sub1()
    write (0,*) "\begin{varlist}{\grpname{group1}}"
    write (0,*) 'GROUP "', 'group1', '":'
  end subroutine sub1
end module mod1
