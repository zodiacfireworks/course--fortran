! ------------------------------------------------------------------------------
! Programa:
!   Pointers13
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Pointers13
INTEGER , POINTER :: A,B
INTEGER , TARGET :: C
INTEGER :: D
  PRINT *,LOC(a)
  PRINT *,LOC(b)
  PRINT *,LOC(c)
  PRINT *,LOC(d)
  C = 1
  A => C
  C = 2
  B => C
  D = A + B
  PRINT *,A,B,C,D
  PRINT *,LOC(a)
  PRINT *,LOC(b)
  PRINT *,LOC(c)
  PRINT *,LOC(d)
END PROGRAM Pointers13
