! ------------------------------------------------------------------------------
! Programa:
!   Functions05
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Functions05
IMPLICIT NONE
INTEGER :: Year, Metcyc, Century, Error1, Error2, Day
INTEGER :: Epact, Luna, Temp
! A program to calculate the date of Easter
  PRINT *,' Input the year for which Easter'
  PRINT *,' is to be calculated'
  PRINT *,' enter the whole year, e.g. 1978 '
  READ *,Year
! calculating the year in the 19 year
! metonic cycle using variable metcyc
  Metcyc = MOD(Year,19)+1
  IF(Year <= 1582)THEN
    Day = (5*Year)/4
    Epact = MOD(11*Metcyc-4,30)+1
  ELSE
!        calculating the Century-century
    Century = (Year/100)+1
!        accounting for arithmetic inaccuracies
!        ignores leap years etc.
    Error1 = (3*Century/4)-12
    Error2 = ((8*Century+5)/25)-5
!        locating Sunday
    Day = (5*Year/4)-Error1-10
!        locating the epact(full moon)
    Temp = 11 * Metcyc + 20 + Error2 - Error1
    Epact = MOD(Temp,30)
    IF(Epact <= 0) THEN
      Epact = 30 + Epact
    ENDIF
    IF((Epact == 25 .AND. Metcyc > 11) &
    .or. Epact == 24)THEN
      Epact = Epact+1
    ENDIF
  ENDIF
!     finding the full moon
  Luna= 44 - epact
  IF (Luna < 21) THEN
    Luna = Luna+30
  ENDIF
!     locating Easter Sunday
  Luna = Luna+7-(MOD(Day+Luna,7))
!     locating the correct month
  IF(Luna > 31)THEN
    Luna = Luna - 31
    PRINT *,' for the year ',YEAR
    PRINT *,' Easter falls on April ',Luna
  ELSE
    PRINT *,' for the year ',YEAR
    PRINT *,' Easter falls on march ',Luna
  ENDIF
END PROGRAM Functions05
