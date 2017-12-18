! ------------------------------------------------------------------------------
! Programa:
!   BasicStats
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM BasicStats
    IMPLICIT NONE
    REAL :: Mean=0.0,SSQ=0.0,X,W,SD,R
    INTEGER :: I,N

    PRINT *,' ENTER THE NUMBER OF READINGS'
    READ*,N
    PRINT*,' ENTER THE ',N,' VALUES, ONE PER LINE'

    DO I=1,N
      READ*,X
      W=X-Mean
      R=I-1
      Mean=(R*Mean+X)/I
      SSQ=SSQ+W*W*R/I
    ENDDO

    SD=(SSQ/R)**0.5

    PRINT *,' Mean is ',Mean
    PRINT *,' Standard deviation is ',SD
END PROGRAM BasicStats
