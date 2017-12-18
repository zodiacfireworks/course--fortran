! ------------------------------------------------------------------------------
! Programa:
!   Subroutines12
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Subroutines12
!
! Program to test array subscript checking
! when the array is passed as an argument.
!
implicit none
integer , parameter :: array_size=10
integer :: i
integer , dimension(array_size) :: a
  do i=1,array_size
    a(i)=i
  end do
  call sub01(a,array_size)
end program Subroutines12

subroutine sub01(a,array_size)
implicit none
integer , intent(in) :: array_size
integer , intent(in) , dimension(array_size) :: a
integer :: i
integer :: atotal=0
integer :: rtotal=0
  do i=1,array_size
    rtotal=rtotal+a(i)
  end do
  do i=1,array_size+1
    atotal=atotal+a(i)
  end do
  print *,' Apparent total is ' , atotal
  print *,'     real total is ' , rtotal
end subroutine sub01
