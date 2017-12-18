! ------------------------------------------------------------------------------
! Programa:
!   Pointers01
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Pointers01
  INTEGER , POINTER :: A,B
  INTEGER , TARGET :: C
  INTEGER :: D
  C = 1
  A => C
  C = 2
  B => C
  D = A + B
  PRINT *,A,B,C,D
END PROGRAM Pointers01
