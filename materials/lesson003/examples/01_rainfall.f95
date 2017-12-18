! ------------------------------------------------------------------------------
! Programa:
!   RainFall
!
! Descripción:
!   Calcula el promedio de las precipitaciones de un año
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM RainFall
    IMPLICIT NONE
    REAL :: Total=0.0
    REAL ::Average=0.0
    REAL, DIMENSION(1:12) :: MonthlyRainFall
    INTEGER :: Month
    WRITE(*, '(A)') 'Ingrese los valores de las precipitaciones mensuales'

    DO Month=1,12
      WRITE(*,'(A,I2,A)', advance='no') "Mes ", Month, ": "
      READ(*, *) MonthlyRainFall(Month)
    ENDDO

    DO Month=1,12
      Total = Total + MonthlyRainFall(Month)
    ENDDO

    Average = Total / 12
    WRITE(*, '(A)', advance='no') 'Average monthly rainfall was: '
    WRITE(*, *) Average
END PROGRAM RainFall
