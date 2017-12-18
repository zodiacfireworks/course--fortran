! ------------------------------------------------------------------------------
! Programa:
!   Writter01
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Writter01
INTEGER :: T
  PRINT *,' '
  PRINT *,' Twelve times table'
  PRINT *,' '
  DO T=1,12
    PRINT 100, T,T*12
    100 FORMAT(' ',I3,' *  12 = ',I3)
  END DO
END PROGRAM Writter01
