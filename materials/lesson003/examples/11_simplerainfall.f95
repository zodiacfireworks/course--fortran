! ------------------------------------------------------------------------------
! Programa:
!   SimpleRainFall
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM SimpleRainFall
    IMPLICIT NONE
    integer , parameter :: n=12
    REAL :: Total=0.0, Average=0.0
    REAL , DIMENSION(1:n) :: RainFall = (/3.1,2.0,2.4,2.1,2.2,2.2,1.8,2.2,2.7,2.9,3.1,3.1/)
    INTEGER :: Month
    DO Month=1,n
      Total = Total + RainFall(Month)
    ENDDO

    Average = Total / n

    PRINT *,' Average monthly rainfall was'
    PRINT *, Average
END PROGRAM SimpleRainFall
