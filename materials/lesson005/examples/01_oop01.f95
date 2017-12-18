! ------------------------------------------------------------------------------
! Programa:
!   OOP01
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM OOP01
IMPLICIT NONE
TYPE Date
  INTEGER :: Day=1
  INTEGER :: Month=1
  INTEGER :: Year=2000
END TYPE Date
TYPE (Date) :: D
  PRINT *,D%Day, D%Month, D%Year
  PRINT *,' Type in the date, day, month, year'
  READ *,D%Day, D%Month, D%Year
  PRINT *,D%Day, D%Month, D%Year
END PROGRAM OOP01
