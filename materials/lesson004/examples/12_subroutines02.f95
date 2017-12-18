! ------------------------------------------------------------------------------
! Programa:
!   Subroutines02
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Subroutines02
implicit none
real :: a,b,c
  a = 1000.0
  b =   20.0
  call divide(a,b,c)
  print *,c
end program Subroutines02

subroutine divide(a,b,c)
implicit none
integer , intent(in) :: a
integer , intent(in) :: b
integer , intent(out):: c
  c=a/b
end subroutine divide
