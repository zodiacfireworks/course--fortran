! ------------------------------------------------------------------------------
! Programa:
!   Subroutines11
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Subroutines11
implicit none
integer :: n
integer :: i
real , allocatable , dimension(:) :: x
  print *,' how many values ?'
  read *,n
  allocate(x(1:n))
  call random_number(x)
  x=x*1000
  open(unit=10,file='random.txt')
  do i=1,n
    write(10, 100)x(i)
    100 format(f8.3)
  end do
end program Subroutines11
