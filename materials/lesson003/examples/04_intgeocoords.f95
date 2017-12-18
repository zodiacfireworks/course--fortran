! ------------------------------------------------------------------------------
! Programa:
!   IntGeoCoords
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM IntGeoCoords
IMPLICIT NONE
    INTEGER , PARAMETER :: Size = 3
    INTEGER  :: Lat , Long
    REAL , DIMENSION(1:Size,1:Size) :: Height
    REAL , PARAMETER :: Correct = 10.0

    DO Lat = 1,Size
        DO Long = 1,Size
            READ * , Height(Lat,Long)
            Height(Lat,Long) = Height(Lat,Long) + Correct
        ENDDO
    ENDDO

    DO Lat = 1,Size
        PRINT * , (Height(Lat,Long),Long=1,3)
    ENDDO
END PROGRAM IntGeoCoords
