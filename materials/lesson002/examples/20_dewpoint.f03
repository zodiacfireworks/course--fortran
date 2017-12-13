! Programa:
!   DewPoint
!
! Descripcion:
!   Calculo del punto de rocio por linea de comandos ingresando temperatura y
!   la humedad relativa como argumentos
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
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
    CHARACTER(len=32) :: Arg  ! Para leer los argumentos por linea de comandos
    CHARACTER(len=32) :: Res  ! Salida en forma de text
    ! Parte ejecutable----------------------------------------------------------
    ! Adquisicion de datos
    ! Verificar la cantidad de argumentos
    IF(COMMAND_ARGUMENT_COUNT() < 2) THEN
        WRITE(*, '(A)') "Argumentos insuficientes"
        STOP -1
    ELSE IF(COMMAND_ARGUMENT_COUNT() > 2) THEN
        WRITE(*, '(A)') "Exceso de argumentos"
        STOP -1
    END IF

    ! Obtenermos la temperatura como primer argumento
    CALL GET_COMMAND_ARGUMENT(1, Arg)
    READ(Arg, *) temperature

    ! Obtenermos la Humedad como segundo argumento
    CALL GET_COMMAND_ARGUMENT(2, Arg)
    READ(Arg, *) humidity

    ! Procesamiento de datos
    ! (1) Calculo de la presion de Vapor de Saturación
    ratio = 373.15 / ( 273.15 + temperature)
    rhs = -7.90298 * (ratio - 1)
    rhs = rhs + 5.02808 * LOG10(ratio)
    rhs = rhs - 1.3816e-7 * (10 ** (11.344 * (1 - 1/ratio)))
    rhs = rhs + 8.1328e-3 * (10 ** (-3.49149 * (ratio - 1)))
    rhs = rhs + LOG10(1013.246)
    vapor_pressure = (10 ** (rhs - 3)) * humidity

    ! (2) dew_point = f(vapor_pressure)
    rhs2 = LOG(vapor_pressure/0.61078)
    dew_point = (241.88 * rhs2)/(17.552 - rhs2)

    ! Salida de datos
    WRITE(Res, *) dew_point
    WRITE(*, '(A)') TRIM(ADJUSTL(Res))
END PROGRAM DewPoint
