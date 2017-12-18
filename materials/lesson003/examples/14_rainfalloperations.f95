! ------------------------------------------------------------------------------
! Programa:
!   RainFallOperations
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM RainFallOperations
IMPLICIT NONE
REAL :: Total=0.0, Average=0.0
REAL , DIMENSION(12) :: RainFall = &
  (/3.1,2.0,2.4,2.1,2.2,2.2,1.8,2.2,2.7,2.9,3.1,3.1/)
INTEGER :: Month
  Total = SUM(RainFall)
  Average = Total / 12
  PRINT *,' Average monthly rainfall was'
  PRINT *, Average
END PROGRAM RainFallOperations
