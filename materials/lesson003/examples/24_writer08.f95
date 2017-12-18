! ------------------------------------------------------------------------------
! Programa:
!   Writter08
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Writter08
REAL , DIMENSION(10,10) :: Y
INTEGER :: NROWS=6
INTEGER :: NCOLS=7
INTEGER :: I,J
INTEGER :: K=0

  DO I=1,NROWS
    DO J=1,NCOLS
      K=K+1
      Y(I,J)=K
    END DO
  END DO

  WRITE(UNIT=*,FMT=100)Y
  100 FORMAT(1X,10F10.4)

END PROGRAM Writter08
