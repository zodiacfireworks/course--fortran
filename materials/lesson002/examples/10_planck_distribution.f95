! ------------------------------------------------------------------------------
! Programa:
!   PlanckDistribution
! 
! Descripción:
!   Determina el valor de la distribución de Planck para la radiancia espectral
!   B(f, T) = [2*h*(f^3)/(c^2)][1/(exp(h*f/(k_b*T)) - 1)]
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM PlanckDisribution
    ! > Declaración de vconstantes y varaibles
    IMPLICIT NONE
    ! Constantes físicas 
    REAL(KIND=8), PARAMETER :: kBoltzmann = 8.6173303e-5 ! [eV * K^-1]
    REAL(KIND=8), PARAMETER :: hPlanck = 4.135667662e-15 ! [eV * s]
    REAL(KIND=8), PARAMETER :: speedOfLight = 299792458  ! [m * s^-1]
    ! Constantes de conversion
    REAL(KIND=8), PARAMETER :: eV2J = 1.602176565e-19    ! 1eV = 1.602176565e-19J
    ! Varaibles de entrada
    REAL(KIND=8) :: temperature
    REAL(KIND=8) :: frequency
    ! Varaibles de salida
    REAL(KIND=8) :: spectralRadiance

    ! > Cuerpo del programa
    ! Entrada de datos
    WRITE(*, *) "Ingrese la temperatura [K]"
    READ(*, *) temperature
    WRITE(*, *) "Ingrese la frcuencia [Hz]"
    READ(*, *) frequency
    
    ! Procesamiento
    spectralRadiance = (2 * kBoltzmann * (frequency ** 2) / speedOfLight ** 2)
    spectralRadiance = spectralRadiance / (-1 + &
    & EXP((hPlanck / kBoltzmann) * (frequency / temperature)) )
    spectralRadiance = spectralRadiance * eV2J
    ! Salida
    WRITE(*, *) "La rediancia espectral es [W sr^−1 m^−2 Hz^−1]:"
    WRITE(*, *) spectralRadiance

END PROGRAM PlanckDisribution