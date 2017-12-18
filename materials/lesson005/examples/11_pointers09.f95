! ------------------------------------------------------------------------------
! Programa:
!   Pointers09
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Pointers09
IMPLICIT NONE
TYPE Ragged
  REAL , DIMENSION(:) , POINTER :: Ragged_row
END TYPE
INTEGER :: i
INTEGER , PARAMETER :: n=3
TYPE (Ragged) , DIMENSION(1:n) :: Lower_Diag
  DO i=1,n
    ALLOCATE(Lower_Diag(i)%Ragged_Row(1:i))
    PRINT *,' Type in the values for row ' , i
    READ *,Lower_Diag(I)%Ragged_Row(1:i)
  END DO
  DO i=1,n
    PRINT *,Lower_Diag(i)%Ragged_Row(1:i)
  END DO
END PROGRAM Pointers09
