! ------------------------------------------------------------------------------
! Programa:
!   Pointers04
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Pointers04
INTEGER , POINTER :: A=>NULL(),B=>NULL()
INTEGER , TARGET :: C
INTEGER :: D
  C = 1
  A = 21
  C = 2
  B => C
  D = A + B
  PRINT *,A,B,C,D
END PROGRAM Pointers04
