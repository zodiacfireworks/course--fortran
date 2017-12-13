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
!   Las funciones COMMAND_ARGUMENT_COUNT y GET_COMMAND_ARGUMENT son funciones
!    implementadas en el estándar Fortran 2003
!
! ------------------------------------------------------------------------------
PROGRAM Echo
    IMPLICIT NONE
    INTEGER :: i
    CHARACTER(len=128) :: Arg

    DO i = 1, COMMAND_ARGUMENT_COUNT()
        CALL GET_COMMAND_ARGUMENT(i, Arg)
        IF(i == IARGC()) THEN
            WRITE(*, '(A)') TRIM(Arg)
        ELSE
            WRITE(*, '(2A)', advance='no') TRIM(Arg), " "
        END IF
    END DO
END PROGRAM Echo
