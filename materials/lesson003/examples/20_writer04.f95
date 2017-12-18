! ------------------------------------------------------------------------------
! Programa:
!   Writter04
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Writter04
implicit none
integer :: i
real    :: small = 1.0
real    :: big   = 1.0
  do i=1,50
    print 100,i,small,big
    100 format(' ',i3,' ',f7.3,' ',f7.3)
    small=small/10.0
    big=big*10.0
  end do
end program Writter04
