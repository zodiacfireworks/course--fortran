! ------------------------------------------------------------------------------
! Programa:
!   Writter03
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Writter03
implicit none
integer :: fluid
real :: litres
real :: pints
  do fluid=1,10
    litres = fluid / 1.75
    pints  = fluid * 1.75
    print 100 , pints,fluid,litres
    100 format(' ',F7.3,' ',I3,' ',F7.3)
  end do
end program Writter03
