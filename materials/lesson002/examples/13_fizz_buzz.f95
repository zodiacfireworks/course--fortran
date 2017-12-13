! ------------------------------------------------------------------------------
! Programa:
!   FizzBuzz
!
! Descripción:
!   Pide un número al usuario y retorna Fizz si solo es multiplo de 3, Buzz si
!   solo es multiplo de 5, FizzBuzz si es multiplo de 3 y 5 al mismo tiempo
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
!
! ------------------------------------------------------------------------------
PROGRAM FizzBuzz
    IMPLICIT NONE
    INTEGER :: I
    CHARACTER(len=40) :: CharacterI

    WRITE(*, '(A)') "Ingrese un número entero:"
    READ(*, *) I

    IF (mod(I,3) == 0 .AND. mod(I,5) == 0) THEN
        WRITE(*, '(A)') 'FizzBuzz'
    ELSE IF (mod(I,3) == 0) THEN
        WRITE(*, '(A)') 'Fizz'
    ELSE IF (mod(I,5) == 0) THEN
        WRITE(*, '(A)') 'Buzz'
    ELSE
        WRITE(CharacterI, '(I10)') I
        WRITE(*, '(A)') ADJUSTL(CharacterI)
    END IF
END PROGRAM FizzBuzz
