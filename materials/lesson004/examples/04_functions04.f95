! ------------------------------------------------------------------------------
! Programa:
!   Functions04
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Functions04
IMPLICIT NONE
REAL , DIMENSION(5) :: X = (/1.0,2.0,3.0,4.0,5.0/)
  PRINT *,' Dot product of X with X is'
  PRINT *,' ',DOT_PRODUCT(X,X)
END PROGRAM Functions04
