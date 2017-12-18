! ------------------------------------------------------------------------------
! Programa:
!   Writter02
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Writter02
implicit none
integer :: big=10
integer :: i
  do i=1,40
    print 100,i,big
    100 format(1x,i3,2x,i12)
    big=big*10
  end do
end program Writter02
