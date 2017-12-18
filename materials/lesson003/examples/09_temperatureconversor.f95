! ------------------------------------------------------------------------------
! Programa:
!   TemperatureConversor
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM TemperatureConversor
    IMPLICIT NONE
    REAL, DIMENSION (1:5,1:5) :: Fahrenheit, Celsius
    INTEGER :: Long, Lat

    DO Lat=1,5
        PRINT *, ' For Latitude= ',Lat
        DO Long=1,5
            PRINT *, ' For Longitude', Long
            READ *,Fahrenheit( Long, Lat)
        END DO
    END DO

    Celsius = 5.0/9.0 * (Fahrenheit - 32.0)
    PRINT * , Celsius
    PRINT * , Fahrenheit
END PROGRAM TemperatureConversor
