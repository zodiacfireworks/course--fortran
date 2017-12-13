! ------------------------------------------------------------------------------
! Programa:
!   LineEquation
! 
! Descripción:
!   Dados dos puntos (x, y) determina la ecuación de la recta
!   y = m * x + b
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM LineEquation
    IMPLICIT NONE
    CHARACTER(len= 4), PARAMETER :: prompt = ">>> "
    REAL(kind=4) :: X1, Y1, X2, Y2
    REAL(kind=4) :: Slope, Intercept

    WRITE(unit=*, fmt='(2A)') prompt, "Ecuación de la Recta"
    
    WRITE(unit=*, fmt='(2A)') prompt, "Punto 1"
    WRITE(unit=*, fmt='(2A)', advance='no') prompt, "X1 = "
    READ (unit=*, fmt=*) X1
    WRITE(unit=*, fmt='(2A)', advance='no') prompt, "Y1 = "
    READ (unit=*, fmt=*) Y1

    WRITE(unit=*, fmt='(2A)') prompt, "Punto 2"
    WRITE(unit=*, fmt='(2A)', advance='no') prompt, "X2 = "
    READ (unit=*, fmt=*) X2
    WRITE(unit=*, fmt='(2A)', advance='no') prompt, "Y2 = "
    READ (unit=*, fmt=*) Y2

    Slope = (Y2 - Y1)/(X2 - X1)
    Intercept = Y1 - Slope*X1

    WRITE(unit=*, fmt='(2A)') prompt, "La ecuación de la recta es"
    WRITE(unit=*, fmt='(2A,F8.4,A,F8.4)') prompt, "Y = ", Slope, " * X + ", Intercept
END PROGRAM LineEquation
