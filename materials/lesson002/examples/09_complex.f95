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
PROGRAM ComplexTest
    IMPLICIT NONE
    INTEGER :: re = 25
    REAL(kind= 4) :: im04 = 3.141592
    REAL(kind= 8) :: im08 = 3.141592
    REAL(kind=10) :: im10 = 3.141592
    REAL(kind=16) :: im16 = 3.141592
    COMPLEX(kind=16) :: z16 = (25, 3.141592)

    WRITE(*, '(3A8,3X,A)') "REAL", "IMAG", "COMPLEX", "COMPLEX"
    WRITE(*, '(3A8,3X,A)') "KIND", "KIND", "KIND", "NUMBER"
    WRITE(*, '(3I8,2X)', advance='no') KIND(re), KIND(im04), KIND(COMPLEX(re, im04))
    WRITE(*, *) COMPLEX(re, im04)
    WRITE(*, '(3I8,2X)', advance='no') KIND(re), KIND(im08), KIND(COMPLEX(re, im08))
    WRITE(*, *) COMPLEX(re, im08)
    WRITE(*, '(3I8,2X)', advance='no') KIND(re), KIND(im10), KIND(COMPLEX(re, im10))
    WRITE(*, *) COMPLEX(re, im10)
    WRITE(*, '(3I8,2X)', advance='no') KIND(re), KIND(im16), KIND(COMPLEX(re, im16))
    WRITE(*, *) COMPLEX(re, im16)
    WRITE(*, '(3I8,2X)', advance='no') KIND(REAL(z16)), KIND(AIMAG(z16)), KIND(z16)
    WRITE(*, *) z16
END PROGRAM ComplexTest