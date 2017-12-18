! ------------------------------------------------------------------------------
! Programa:
!   Functions07
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Functions07
IMPLICIT NONE
INTEGER :: I, F, Factorial
  PRINT *,' Type in the number, integer only'
  READ *,I
  DO WHILE(I<0)
    PRINT *,' Factorial only defined for '
    PRINT *,' positive integers: Re-input'
    READ *,I
  END DO
  F=Factorial(I)
  PRINT *,' Answer is', F
END PROGRAM Functions07

RECURSIVE INTEGER FUNCTION Factorial(I) RESULT(Answer)
IMPLICIT NONE
INTEGER , INTENT(IN):: I
  IF (I==0) THEN
    Answer=1
  ELSE
    Answer=I*Factorial(I-1)
  END IF
END FUNCTION Factorial
