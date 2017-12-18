! ------------------------------------------------------------------------------
! Programa:
!   Pointers05
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Pointers05
INTEGER , POINTER :: A=>NULL(),B=>NULL()
INTEGER , TARGET :: C
INTEGER :: D
  ALLOCATE(B)
  C = 1
  A => C
  C = 2
  B = A
  D = A + B
  PRINT *,A,B,C,D
END PROGRAM Pointers05
