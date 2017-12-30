! ------------------------------------------------------------------------------
! Programa:
!   KindIntegers
! 
! Descripción:
!   Uso de la función SELECTED_INT_KIND (Fortran 95 y posteriores, variante con
!   el parametro radix en Fortran 08) y los valores maximos.
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
!
! ------------------------------------------------------------------------------
PROGRAM KindReal
    INTEGER, PARAMETER :: p04 = SELECTED_REAL_KIND( 6,   37, 0)
    INTEGER, PARAMETER :: p08 = SELECTED_REAL_KIND(15,  307, 0)
    INTEGER, PARAMETER :: p16 = SELECTED_REAL_KIND(18, 4931, 0)
    INTEGER, PARAMETER :: p32 = SELECTED_REAL_KIND(33, 4931, 0)

    REAL(kind=p04) :: X04
    REAL(kind=p08) :: X08
    REAL(kind=p16) :: X16
    REAL(kind=p32) :: X32

    WRITE(*, '(A5,A12,A8,A8,3X,A)') "Kind", "Precision", "Range", "Radix", "Máx Value"
    WRITE(*, '(I5,I12,I8,I8)', advance='no') KIND(X04), PRECISION(X04), RANGE(X04), RADIX(X04)
    WRITE(*, *) HUGE(X04)
    WRITE(*, '(I5,I12,I8,I8)', advance='no') KIND(X08), PRECISION(X08), RANGE(X08), RADIX(X08)
    WRITE(*, *) HUGE(X08)
    WRITE(*, '(I5,I12,I8,I8)', advance='no') KIND(X16), PRECISION(X16), RANGE(X16), RADIX(X16)
    WRITE(*, *) HUGE(X16)
    WRITE(*, '(I5,I12,I8,I8)', advance='no') KIND(X32), PRECISION(X32), RANGE(X32), RADIX(X32)
    WRITE(*, *) HUGE(X32)
END PROGRAM KindReal