! ------------------------------------------------------------------------------
! Programa:
!   Writter06
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Writter06
IMPLICIT NONE
CHARACTER (LEN=15) :: Firstname
INTEGER :: age
REAL :: weight
CHARACTER (LEN=1) :: sex
  PRINT *,' Type in your first name '
  READ *,Firstname
  PRINT *,' type in your age in years'
  READ *,age
  PRINT *,' type in your weight in kilos'
  READ *,weight
  PRINT *,' type in your sex (f/m)'
  READ *,sex
  PRINT *,' your personal details are'
  PRINT *
  PRINT 100
  100 FORMAT(4x,'first name', 4x , 'age' , 1x , &
     'weight' , 2x , 'sex')
  PRINT 200 , firstname, age , weight , sex
  200 FORMAT(1x , a , 2x , i3 , 2x , f5.2 , 2x, a)
END PROGRAM Writter06
