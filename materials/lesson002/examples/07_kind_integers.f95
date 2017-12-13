! ------------------------------------------------------------------------------
! Programa:
!   KindIntegers
!
! Descripción:
!   Uso de la función SELECTED_INT_KIND (Fortran 95 y posteriores) y los
!   maximos y minimos de un tipo de dato determinado.
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
!
! ------------------------------------------------------------------------------
PROGRAM KindIntegers
    INTEGER, PARAMETER :: K02 = SELECTED_INT_KIND(2)
    INTEGER, PARAMETER :: K04 = SELECTED_INT_KIND(4)
    INTEGER, PARAMETER :: K08 = SELECTED_INT_KIND(8)
    INTEGER, PARAMETER :: K16 = SELECTED_INT_KIND(16)

    INTEGER(kind=K02) :: I02
    INTEGER(kind=K04) :: I04
    INTEGER(kind=K08) :: I08
    INTEGER(kind=K16) :: I16

    WRITE(*,'(A5,A26)') "Kind", "Máximo"
    WRITE(*,'(I5,I25)') K02, HUGE(I02)
    WRITE(*,'(I5,I25)') K04, HUGE(I04)
    WRITE(*,'(I5,I25)') K08, HUGE(I08)
    WRITE(*,'(I5,I25)') K16, HUGE(I16)
END PROGRAM KindIntegers
