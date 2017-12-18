! ------------------------------------------------------------------------------
! Programa:
!   GeoCoords
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM GeoCoords
    IMPLICIT NONE
    INTEGER , PARAMETER :: Size = 3
    INTEGER  :: Lat , Long
    REAL , DIMENSION(1:Size,1:Size) :: Height
    REAL , PARAMETER :: Correct = 10.0
    DO Lat = 1,Size
        DO Long = 1,Size
            PRINT *,' Type in value at ',Lat,' ',Long
            READ * , Height(Lat,Long)
        ENDDO
    ENDDO
    DO Lat = 1,Size
        DO Long = 1,Size
            Height(Lat,Long) = Height(Lat,Long) + Correct
        ENDDO
    ENDDO
    PRINT * , ' Corrected data is '
    DO Lat = 1,Size
        DO Long = 1,Size
            PRINT * , Height(Lat,Long)
        ENDDO
    ENDDO
END PROGRAM GeoCoords
