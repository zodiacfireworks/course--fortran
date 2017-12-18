! ------------------------------------------------------------------------------
! Programa:
!   Reader02
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Reader02
implicit none
integer , parameter :: n=10
real , dimension(1:n) :: h
real , dimension(1:n) :: w
real , dimension(1:n) :: bmi
integer :: i
  do i=1,n
    read 100, h(i),w(i)
    100 format(f4.2,2x,f3.0)
  end do
  bmi=w/(h*h)
  do i=1,n
    print 200,bmi(i)
    200 format(2x,f5.0)
  end do
end program Reader02
