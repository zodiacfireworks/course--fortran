! Programa:
!   DewPoint
!
! Descripcion:
!   Calculo del punto de rocio
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

    ! Parte ejecutable----------------------------------------------------------
    ! Adquisicion de datos
    WRITE (*, '(A)')  "Cálculo del punto de rocío"
    WRITE (*, '(2A)') "--------------------------", NEW_LINE('A')
    WRITE (*, '(A)', advance='no') "Ingrese el valor de la temperatura [C]: "
    READ (*, *) temperature

    WRITE (*, '(A)', advance='no') "Ingrese el valor de la humedad relativa [%]: "
    READ (*, *) humidity

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
    WRITE (*, '(A)', advance='no') "El valor del punto de rocio [C] es:"
    WRITE(*, *) dew_point
END PROGRAM DewPoint
