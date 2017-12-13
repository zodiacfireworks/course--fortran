! ------------------------------------------------------------------------------
! Programa:
!   Echo
!
! Descripción:
!   Emulador del comando echo
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
!
! Notas:
!   Las funciones GETARG y IARGC son funciones implementadas como extensiones
!   los compiladores GNU. No son parte del ningún estándar.
!
! ------------------------------------------------------------------------------
PROGRAM Echo
    IMPLICIT NONE
    INTEGER :: i
    CHARACTER(len=128) :: Arg

    DO i = 1, IARGC()
        CALL GETARG(i, Arg)
        IF(i == IARGC()) THEN
            WRITE(*, '(A)') TRIM(Arg)
        ELSE
            WRITE(*, '(2A)', advance='no') TRIM(Arg), " "
        END IF
    END DO
END PROGRAM Echo
