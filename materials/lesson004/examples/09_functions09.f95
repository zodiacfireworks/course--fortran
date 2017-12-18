! ------------------------------------------------------------------------------
! Programa:
!   Functions09
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Functions09
IMPLICIT NONE
INTEGER :: I,J,Result
INTEGER :: GCD
  PRINT *,' Type in two integers'
  READ *,I,J
  Result=GCD(I,J)
  PRINT *,' GCD is ',Result
END PROGRAM Functions09

INTEGER FUNCTION GCD(I,J)
IMPLICIT NONE
INTEGER , INTENT(INOUT) :: I,J
INTEGER :: Temp
  DO WHILE (J/=0)
    Temp=MOD(I,J)
    I=J
    J=Temp
  END DO
  GCD=I
END FUNCTION GCD
