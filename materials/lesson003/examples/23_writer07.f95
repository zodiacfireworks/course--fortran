! ------------------------------------------------------------------------------
! Programa:
!   Writter07
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Writter07
implicit none
integer :: fluid
real :: litres
real :: pints
  open (unit=1,file='ch1209.txt')
  write(unit=1,fmt=200)
  200 format(' Pints       Litres')
  do fluid=1,10
    litres = fluid / 1.75
    pints  = fluid * 1.75
    write (unit=1,fmt=100) pints,fluid,litres
    100 format(' ',f7.3,' ',i3,' ',f7.3)
  end do
  close(1)
end program Writter07
