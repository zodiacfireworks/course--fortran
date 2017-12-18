! ------------------------------------------------------------------------------
! Programa:
!   WindDirection
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM WindDirection
IMPLICIT NONE
REAL , DIMENSION(-180:180) :: Time=0
INTEGER :: Degree,Strip
REAL :: Value
CHARACTER (LEN=1) , DIMENSION(-180:180) &
    :: Direction=' '
  DO Degree=-180,165,15
    Value=Degree/15.
    DO Strip=0,14
      Time(Degree+Strip)=Value
    ENDDO
  ENDDO
  DO Degree=-180,180
    PRINT *,Degree,' ',Time(Degree)
  END DO
  WHERE (Time > 0.0)
    Direction='E'
  ELSEWHERE (Time < 0.0)
    Direction='W'
  ENDWHERE
  PRINT *,direction
END PROGRAM WindDirection
