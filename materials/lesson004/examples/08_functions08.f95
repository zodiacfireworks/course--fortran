! ------------------------------------------------------------------------------
! Programa:
!   Functions08
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Functions08
IMPLICIT NONE
INTEGER :: I,J,Result
INTEGER :: GCD
  PRINT *,' Type in two integers'
  READ *,I,J
  Result=GCD(I,J)
  PRINT *,' GCD is ',Result
END PROGRAM Functions08

RECURSIVE INTEGER FUNCTION GCD(I,J) RESULT(Answer)
IMPLICIT NONE
INTEGER , INTENT(IN) :: I,J
  IF (J==0) THEN
    Answer=I
  ELSE
    Answer=GCD(J,MOD(I,J))
  ENDIF
END FUNCTION GCD
