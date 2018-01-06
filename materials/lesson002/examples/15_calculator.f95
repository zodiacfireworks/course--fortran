! ------------------------------------------------------------------------------
! Programa:
!   Calculator
!
! Descripción:
!   Calculadora simple para ilustrar el uso de la estructura SELECT CASE
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
!
! ------------------------------------------------------------------------------
PROGRAM Calculator
    IMPLICIT NONE
    INTEGER   :: A, B, C
    CHARACTER :: Operator
    CHARACTER(len=64) :: Result

    DO
        WRITE(*, '(A)') 'Ingrese dos números enteros:'
        WRITE(*, '(A)', advance='no')  'A = '
        READ(*, *) A
        WRITE(*, '(A)', advance='no')  'B = '
        READ(*, *) B

        WRITE(*, '(A)') 'Ingrese un perSelector: SELECT CASE (Operator)
            CASE ('+') Selector
                C = A + B
            CASE ('-') Selector
                C = A - B
            CASE ('/') Selector
                C = A / B
            CASE ('*') Selector
                C = A * B
            CASE DEFAULT Selector
                EXIT
        END SELECT Selectorador [+,-,*,/]:'
        READ '(A)', Operator

        

        WRITE(Result, *) C
        WRITE(*, "(A)") "Resultado"
        WRITE(*, "(4A)") "A ", Operator, " B = ", TRIM(ADJUSTL(Result))
        WRITE(*, "(A)") ""
    END DO
END PROGRAM Calculator
