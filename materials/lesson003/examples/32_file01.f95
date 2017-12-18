! ------------------------------------------------------------------------------
! Programa:
!   File01
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM File01
IMPLICIT NONE
REAL :: X
CHARACTER (7) :: WHICH
  OPEN(UNIT=5,FILE='INPUT')
  DO
    WRITE(UNIT=6,FMT='('' DATA SET NAME, OR END'')')
    READ(UNIT=5,FMT='(A)') WHICH
    IF(WHICH == 'END') EXIT
    OPEN(UNIT=1,FILE=WHICH)
    READ(UNIT=1,FMT=100) X
!    ...
    CLOSE(UNIT=1)
  END DO
END PROGRAM File01
