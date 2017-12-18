! ------------------------------------------------------------------------------
! Programa:
!   Pointers10
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Pointers10
IMPLICIT NONE
TYPE Ragged
  REAL , DIMENSION(:) , POINTER :: rainfall
END TYPE
INTEGER :: i
INTEGER , PARAMETER :: nr=5
INTEGER , DIMENSION (1:nr) :: nc
TYPE (ragged) ,  DIMENSION(1:nr) :: station
  DO i=1,nr
    PRINT *,' enter the number of data values', &
     ' for station ',i
    READ *,nc(i)
    ALLOCATE(station(i)%rainfall(1:nc(i)))
    PRINT *,' Type in the values for station ' , i
    READ *,station(i)%rainfall(1:nc(i))
  END DO
  DO i=1,nr
    PRINT *,station(i)%rainfall(1:nc(i))
  END DO
END PROGRAM Pointers10
