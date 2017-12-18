! ------------------------------------------------------------------------------
! Programa:
!   Pointers03
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Pointers03
INTEGER , POINTER :: A,B
INTEGER , TARGET :: C
INTEGER :: D
  PRINT *,ASSOCIATED(A)
  PRINT *,ASSOCIATED(B)
  PRINT *,A
  PRINT *,B
  C = 1
  A => C
  C = 2
  B => C
  D = A + B
  PRINT *,A,B,C,D
  PRINT *,ASSOCIATED(A)
  PRINT *,ASSOCIATED(B)
END PROGRAM Pointers03
