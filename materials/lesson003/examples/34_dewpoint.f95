! Programa:
!   DewPoint
!
! Descripcion:
!   Calculo del punto de rocio de los registros de tempratura y humedad
!   dados en un archivo CSV
!
! Referencia:
!   Funcion DewPoint de la NOAA
!   [1] http://wahiduddin.net/calc/density_algorithms.htm
!   [2] http://www.colorado.edu/geography/weather_station/Geog_site/about.htm
!
PROGRAM DewPoint
    ! Parte NO ejecutable ------------------------------------------------------
    ! Declaracion de varaibles
    IMPLICIT NONE
    REAL :: temperature       ! [C] Temperatura (grados celsius)
    REAL :: humidity          ! [%] Humedad Relativa
    REAL :: ratio             ! variable temporal
    REAL :: rhs               ! variable temporal
    REAL :: rhs2              ! variable temporal
    REAL :: vapor_pressure
    REAL :: dew_point

    CHARACTER(len=200) :: DataLine
    INTEGER :: nLine = 0

    ! len: length, longitud del nombre del archivo
    CHARACTER(len=128) :: FileInName
    LOGICAL :: FileInExists
    INTEGER :: FileInState

    INTEGER, PARAMETER :: FileInUnit = 20
    INTEGER, PARAMETER :: FileOutUnit = 30

    WRITE(unit=*, fmt='(A)', advance='no') "Archivo de datos: "
    READ(*, *) FileInName

    INQUIRE(exist=FileInExists, file=FileInName)

    IF (FileInExists) THEN
        OPEN(unit=FileInUnit, file=FileInName, iostat=FileInState)
        DO
            ! unit: Unidad del archivo de lectura (FileInUnit)
            ! fmt: formato de lectura ('(A)')
            ! iostat: Input/Output State, Estado de la lectura del archivo
            READ(unit=FileInUnit, fmt='(A)', iostat=FileInState) DataLine
            IF(FileInState /= 0) THEN
                EXIT
            ELSE
                nLine = nLine + 1
                WRITE(*, '(I5,A,A80)') nLine, "    ", DataLine
            END IF
        END DO
        CLOSE(FileInUnit)
    ELSE
        WRITE(unit=*, fmt="(A,A,A)") "El archivo '", FileInName, "' no existe"
    END IF
END PROGRAM DewPoint
