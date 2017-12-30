! ------------------------------------------------------------------------------
! Programa:
!   Truncation
! 
! Descripción:
!   Muestra los efectos de truncar los valores obtenidos en una operacion 
!   arimetica
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Truncation
    IMPLICIT NONE

    REAL :: A, B, C, D
    INTEGER :: I, J, K, L

    WRITE(unit=*, fmt='(A)') "----------------------------------------"
    WRITE(unit=*, fmt='(A)', advance='no') "[REAL    ] A = "
    READ (unit=*, fmt=*) A

    WRITE(unit=*, fmt='(A)', advance='no') "[REAL    ] B = "
    READ (unit=*, fmt=*) B

    C = A / B ! División real almancenada en un número real
    I = A / B ! División real almancenada en un número entero

    WRITE(unit=*, fmt='(A)', advance='no') "[REAL    ] A / B = "
    WRITE(unit=*, fmt=*) C

    WRITE(unit=*, fmt='(A)', advance='no') "[TRUNCADO] A / B = "
    WRITE(unit=*, fmt=*) I

    WRITE(unit=*, fmt='(A)') "----------------------------------------"
    WRITE(unit=*, fmt='(A)', advance='no') "[INTEGER ] J = "
    READ (unit=*, fmt=*) J

    WRITE(unit=*, fmt='(A)', advance='no') "[INTEGER ] K = "
    READ (unit=*, fmt=*) K

    WRITE(unit=*, fmt='(A)', advance='no') "[INTEGER ] L = "
    READ (unit=*, fmt=*) L

    D = (J / K) * L ! Operación entre enteros asignada a un número real

    WRITE(unit=*, fmt='(A)', advance='no') "[INTEGER ] (J / K) * L = "
    WRITE(unit=*, fmt=*) D

    WRITE(unit=*, fmt='(A)') "----------------------------------------"
END PROGRAM Truncation
