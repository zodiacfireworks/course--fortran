! ------------------------------------------------------------------------------
! Programa:
!   Fibonacci
!
! Descripción:
!   Calcula el los N primeros número de Fibonacci. N es ingresado por el
!   usuario
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
!
! ------------------------------------------------------------------------------
PROGRAM Fibonacci
    IMPLICIT NONE
    INTEGER :: I
    INTEGER :: N
    INTEGER(kind=16) :: Fibn
    INTEGER(kind=16) :: Fib1 = 1
    INTEGER(kind=16) :: Fib2 = 1
    CHARACTER(len=64) :: strFibn

    ! Do Infinitio
    ! Ideal para solicitudes de entrada persistentes con validación
    DO
        WRITE(*, '(A)') "¿Cuantos números de Fibonacci desea?"
        READ(*, *) N

        IF (N <= 0) THEN
            WRITE(*, '(A)') "ERROR::Valor inválido"
        ELSE
            WRITE(*, '(A,2X,A)') "    N", "Fib(N)"

            WRITE(strFibn, '(I64)') Fib1
            WRITE(*, '(I5,2X,A)') 1, ADJUSTL(strFibn)
            IF (N == 1) EXIT

            WRITE(strFibn, '(I64)') Fib2
            WRITE(*, '(I5,2X,A)') 2, ADJUSTL(strFibn)
            IF (N == 2) EXIT

            ! Do finito
            ! Igual que un bucle FOR en otros lenguajes
            DO I = 3, N, 1
                Fibn = Fib2 + Fib1

                IF (Fibn < 0) THEN
                    WRITE(*, '(I5,2X,A)') I, "Overflow error!"
                    EXIT
                END IF

                WRITE(strFibn, '(I64)') Fibn
                WRITE(*, '(I5,2X,A)') I, ADJUSTL(strFibn)

                Fib1 = Fib2
                Fib2 = Fibn
            END DO

            EXIT

        END IF
    END DO
END PROGRAM Fibonacci
