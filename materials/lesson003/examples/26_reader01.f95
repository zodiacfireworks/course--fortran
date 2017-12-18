! ------------------------------------------------------------------------------
! Programa:
!   Reader01
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Reader01
implicit none
integer , parameter :: n=12
integer :: i
integer , dimension(1:n) :: x
integer , dimension(1:n) :: y
  do i=1,n
    read  100,x(i),y(i)
    100 format(2x,i2,9x,i3)
    print 200,x(i),y(i)
    200 format(1x,i3,2x,i3)
  end do
end program Reader01
