! ------------------------------------------------------------------------------
! Programa:
!   Kinds
! 
! Descripción:
!   Muestra el uso de la función KIND (Fortran 95 y posteriores)
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Kinds
    IMPLICIT NONE
    INTEGER   :: I
    REAL      :: R
    CHARACTER :: C
    LOGICAL   :: L
    COMPLEX   :: Z

    WRITE(*, '(A,I2)') 'I -> Integer : kind = ', KIND(I)
    WRITE(*, '(A,I2)') 'R -> Real    : kind = ', KIND(R)
    WRITE(*, '(A,I2)') 'C -> Char    : kind = ', KIND(C)
    WRITE(*, '(A,I2)') 'L -> Logical : kind = ', KIND(L)
    WRITE(*, '(A,I2)') 'Z -> Complex : kind = ', KIND(Z)
END PROGRAM Kinds
