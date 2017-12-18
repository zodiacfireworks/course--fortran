! ------------------------------------------------------------------------------
! Programa:
!   Functions03
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Functions03
IMPLICIT NONE
REAL , DIMENSION(5) :: X = (/1.0,2.0,3.0,4.0,5.0/)
! Elemental function
  PRINT *,' Sine of ', X ,' = ',SIN(X)
! Transformational function
  PRINT *,' Sum of ', X ,' = ',SUM(X)
END PROGRAM Functions03
