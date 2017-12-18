! ------------------------------------------------------------------------------
! Programa:
!   Initializer
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM ch1106
    IMPLICIT NONE
    integer , parameter :: n=10
    integer :: i
    INTEGER , dimension(1:n) :: Litre=(/(i,i=1,n)/)
    REAL    , dimension(1:n) :: Gallon,USGallon
    Gallon   = Litre * 0.2641925
    USGallon = Litre * 0.220022
    DO i = 1,n
      PRINT *,Litre(i), ' ',Gallon(i),' ',USGallon(i)
    END DO
END PROGRAM ch1106
