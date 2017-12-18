! ------------------------------------------------------------------------------
! Programa:
!   Writter05
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Writter05
implicit none
integer :: i
real    :: small = 1.0
real    :: big   = 1.0
  do i=1,50
    print 100,i,small,big
    100 format(' ',i3,' ',e10.4,' ',e10.4)
    small=small/10.0
    big=big*10.0
  end do
end program Writter05
