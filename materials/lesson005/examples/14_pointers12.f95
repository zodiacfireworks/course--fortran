! ------------------------------------------------------------------------------
! Programa:
!   Pointers12
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Pointers12
IMPLICIT NONE
INTEGER :: Allocate_status=0
REAL , DIMENSION(:) , POINTER :: X
REAL , DIMENSION(1:10) , TARGET :: Y
INTEGER , PARAMETER :: SIZE=10000000
INTEGER :: I
  DO
    ALLOCATE(X(1:SIZE),STAT=Allocate_status)
    IF (allocate_status > 0) THEN
      PRINT *,' Allocate failed. Program ends.'
      STOP
    ENDIF

! initialise the memory that x points to
    DO I=1,SIZE
      X(I)=I
    END DO
! print out the first 10 values
    DO I=1,10
      PRINT *,X(I)
    END DO
! initialise the array y
    DO I=1,10
      Y(I)=I*I
    END DO
! print out y
    DO I=1,10
      PRINT *,Y(I)
    END DO
! x now points to y
    X=>Y
! print out what x now points to
    DO I=1,10
      PRINT *,X(I)
    END DO
! what has happened to the memory that x
! used to point to?
  end do
END PROGRAM Pointers12
