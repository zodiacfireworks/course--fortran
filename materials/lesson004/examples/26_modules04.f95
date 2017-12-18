! ------------------------------------------------------------------------------
! Programa:
!   Modules04
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
module read_data

contains

  SUBROUTINE Read(File_Name,Raw_Data,How_Many)
  IMPLICIT NONE
  CHARACTER (LEN=*) , INTENT(IN) :: File_Name
  INTEGER , INTENT(IN) :: How_Many
  REAL , INTENT(OUT) , DIMENSION(:) :: Raw_Data

  INTEGER :: I

    OPEN(FILE=File_Name,UNIT=1)
    DO I=1,How_Many
      READ (UNIT=1,FMT=*) Raw_Data(I)
    ENDDO
  END SUBROUTINE Read

end module read_data

module sort_data

contains

  SUBROUTINE Sort(Raw_Data,How_Many)
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: How_Many
  REAL , INTENT(INOUT) , DIMENSION(:) :: Raw_data

    CALL QuickSort(1,How_Many)

  CONTAINS

    RECURSIVE SUBROUTINE QuickSort(L,R)
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: L,R
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

  END SUBROUTINE Sort

end module sort_data

module print_data

contains

  SUBROUTINE Print(Raw_Data,How_Many)
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: How_Many
  REAL , INTENT(IN) , DIMENSION(:) :: Raw_data
  INTEGER :: I
    OPEN(FILE='SORTED.DAT',UNIT=2)
    DO I=1,How_Many
      WRITE(UNIT=2,FMT=*) Raw_data(I)
    ENDDO
    CLOSE(2)
  END SUBROUTINE Print

end module print_data

PROGRAM Modules04
use read_data
use sort_data
use print_data
IMPLICIT NONE
INTEGER                            :: How_Many
CHARACTER   (LEN=20)               :: File_Name
REAL , ALLOCATABLE , DIMENSION(:)  :: Raw_data
integer , dimension(8)             :: dt

  PRINT * , ' How many data items are there?'
  READ  * , How_Many
  PRINT * , ' What is the file name?'
  READ '(A)',File_Name

  call date_and_time(values=dt)
  PRINT 100 , dt(6),dt(7),dt(8)
  100 FORMAT(' Initial cpu time    = ',3(2x,i10))

  ALLOCATE(Raw_data(How_Many))

  call date_and_time(values=dt)
  PRINT 110 , dt(6),dt(7),dt(8)
  110 FORMAT(' Allocate cpu time   = ',3(2x,i10))

  CALL Read(File_Name,Raw_Data,How_Many)

  call date_and_time(values=dt)
  PRINT 120 , dt(6),dt(7),dt(8)
  120 FORMAT(' Read data cpu time  = ',3(2x,i10))

  CALL Sort(Raw_Data,How_Many)

  call date_and_time(values=dt)
  PRINT 130 , dt(6),dt(7),dt(8)
  130 FORMAT(' Quick sort cpu time = ',3(2x,i10))

  CALL Print(Raw_Data,How_Many)

  call date_and_time(values=dt)
  PRINT 140 , dt(6),dt(7),dt(8)
  140 FORMAT(' Write data cpu time = ',3(2x,i10))

  PRINT * , ' '
  PRINT *, ' Data written to file SORTED.DAT'

END PROGRAM Modules04
