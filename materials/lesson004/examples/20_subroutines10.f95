! ------------------------------------------------------------------------------
! Programa:
!   Subroutines10
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Subroutines10
IMPLICIT NONE
INTEGER                            :: How_Many
CHARACTER   (LEN=20)               :: File_Name
REAL , ALLOCATABLE , DIMENSION(:)  :: Raw_Data
integer , dimension(8) :: timing

INTERFACE
  SUBROUTINE Read_Data(File_Name,Raw_Data,How_Many)
    IMPLICIT NONE
    CHARACTER (LEN=*) , INTENT(IN) :: File_Name
    INTEGER , INTENT(IN) :: How_Many
    REAL , INTENT(OUT) , &
    DIMENSION(:) :: Raw_Data
  END SUBROUTINE Read_Data
END INTERFACE

INTERFACE
  SUBROUTINE Sort_Data(Raw_Data,How_Many)
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: How_Many
    REAL , INTENT(INOUT) , &
    DIMENSION(:) :: Raw_Data
  END SUBROUTINE Sort_Data
END INTERFACE

INTERFACE
  SUBROUTINE Print_Data(Raw_Data,How_Many)
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: How_Many
    REAL , INTENT(IN) , &
    DIMENSION(:) :: Raw_Data
  END SUBROUTINE Print_Data
END INTERFACE

  PRINT * , ' How many data items are there?'
  READ  * , How_Many
  PRINT * , ' What is the file name?'
  READ '(A)',File_Name
  call date_and_time(values=timing)
  print *,' initial'
  print *,timing(6),timing(7),timing(8)
  ALLOCATE(Raw_Data(How_Many))
  call date_and_time(values=timing)
  print *,' allocate'
  print *,timing(6),timing(7),timing(8)
  CALL Read_Data(File_Name,Raw_Data,How_Many)
  call date_and_time(values=timing)
  print *,' read'
  print *,timing(6),timing(7),timing(8)
  CALL Sort_Data(Raw_Data,How_Many)
  call date_and_time(values=timing)
  print *,' sort'
  print *,timing(6),timing(7),timing(8)
  CALL Print_Data(Raw_Data,How_Many)
  call date_and_time(values=timing)
  print *,' print'
  print *,timing(6),timing(7),timing(8)
  PRINT * , ' '
  PRINT *, ' Data written to file SORTED.DAT'

END PROGRAM Subroutines10

SUBROUTINE Read_Data(File_Name,Raw_Data,How_Many)
IMPLICIT NONE
CHARACTER (LEN=*) , INTENT(IN) :: File_Name
INTEGER , INTENT(IN) :: How_Many
REAL , INTENT(OUT) , DIMENSION(:) :: Raw_Data
! Local variables
INTEGER :: I

  OPEN(FILE=File_Name,UNIT=1)
  DO I=1,How_Many
    READ (UNIT=1,FMT=*) Raw_Data(I)
  ENDDO

END SUBROUTINE Read_Data

SUBROUTINE Sort_Data(Raw_Data,How_Many)
IMPLICIT NONE
INTEGER , INTENT(IN) :: How_Many
REAL , INTENT(INOUT) , DIMENSION(:) :: Raw_Data

  CALL QuickSort(1,How_Many)

CONTAINS

  RECURSIVE SUBROUTINE QuickSort(L,R)
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: L,R
  ! Local variables
  INTEGER :: I,J
  REAL :: V,T

  i=l
  j=r
  v=raw_data( int((l+r)/2) )
  do
    do while (raw_data(i) < v )
        i=i+1
    enddo
    do while (v < raw_data(j) )
        j=j-1
    enddo
    if (i<=j) then
      t=raw_data(i)
      raw_data(i)=raw_data(j)
      raw_data(j)=t
      i=i+1
      j=j-1
    endif
    if (i>j) exit
  enddo

  if (l<j) then
    call quicksort(l,j)
  endif

  if (i<r) then
    call quicksort(i,r)
  endif

  END SUBROUTINE QuickSort

END SUBROUTINE Sort_Data

SUBROUTINE Print_Data(Raw_Data,How_Many)
IMPLICIT NONE
INTEGER , INTENT(IN) :: How_Many
REAL , INTENT(IN) , DIMENSION(:) :: Raw_Data
! Local variables
INTEGER :: I
  OPEN(FILE='SORTED.DAT',UNIT=2)
  DO I=1,How_Many
    WRITE(UNIT=2,FMT=*) Raw_Data(I)
  ENDDO
  CLOSE(2)
END SUBROUTINE Print_Data
