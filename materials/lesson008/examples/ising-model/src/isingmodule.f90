MODULE ISINGMODULE
    ! AVOIDING IMPLICIT DECLARATIONS ===============================================================
    IMPLICIT NONE

    ! PARAMETERS ===================================================================================
    ! Couple constant for the Ising model, it can be only +1 or -1
    REAL (KIND = 4) , PARAMETER :: cplConst = 1.0

    CONTAINS
        ! FUNCTIONS ================================================================================

        ! NearLocation *****************************************************************************
        ! * Description :
        !   This funtion returns the nearest location "nloc" in a "lim"-length one-dimensional array
        !   placed at "add" positions from "loc" postion  considering periodical bound condition.
        !
        !   Here we ar considering that array's index is running from 1 to "lim" so both "loc" and
        !   "nloc" are running from 1 to "lim"
        !
        ! * Input:
        ! >>> loc    --> Location index.
        ! >>> lim    --> 1D lenght of the array.
        ! >>> add    --> Places distand from "loc" position.
        !
        ! * Output:
        ! >>> nloc   --> Nearest location to "loc" placed at "add" places.
        !
        !*******************************************************************************************

        FUNCTION NearLocation( loc, lim, add ) &
            RESULT( nloc )
            ! Funtion Variables ********************************************************************
            ! Input Varibles
            INTEGER( KIND = 4 ) :: loc,lim,add

            ! Output Varibles
            INTEGER( KIND = 4 ) :: nloc

            ! Function Body ************************************************************************
            ! Calculatring nloc
            nloc = MOD( (loc + lim + add - 1) , lim ) + 1

        END FUNCTION NearLocation

        ! Function Energy **************************************************************************
        ! * Description :
        !   Given the spin system configuration, this function devolve the systems energy for that
        !   configuration. This energy is calculated acording Isng Hamiltonian with periodic bondary
        !   conditions.
        !
        ! * Input:
        !   >>> sysConfig   --> System Configuration
        !   >>> sys1DLen    --> System Places
        !   >>> sysDim      --> System Dimension
        !
        ! * Output:
        !   >>> sysEnergy   --> System Energy
        !
        ! * References:
        !*******************************************************************************************

        FUNCTION Energy( sysConfig, xLim, yLim ) &
            RESULT( sysEnergy )

            ! FUNCTION VARIABLES ===================================================================
            ! * Input Variables
            REAL    (KIND = 8) , ALLOCATABLE :: sysConfig(:,:)

            ! * Output Variables
            REAL    (KIND = 8) :: sysEnergy

            ! * Internal Variables
            INTEGER (KIND = 4) :: i, xLim, nXloc
            INTEGER (KIND = 4) :: j, yLim, nYloc

            ! FUNTION BODY =========================================================================
            ! Setting up the energy of the system
            sysEnergy = 0.0

            ! Running over all spines in th system to calculate energy
            DO i = 1,xLim
                DO j = 1,yLim
                    nXloc = NearLocation(i,xLim,1)
                    nYloc = NearLocation(j,yLim,1)

                    IF(yLim .EQ. 1)THEN
                        sysEnergy = sysEnergy - cplConst*sysConfig(i,1)*sysConfig(nXloc,1)
                    ELSE
                        sysEnergy = sysEnergy - cplConst*sysConfig(  i  ,   j   )*( &
                                                         sysConfig(nXloc,   j   )+  &
                                                         sysConfig(  i  , nYloc ) )
                    END IF

                END DO
            END DO

        END FUNCTION Energy

        ! Function Magnet **************************************************************************
        ! * Description :
        !   Given the spin system configuration, this function devolve the systems magnetization
        !   for that configuration.
        !
        ! * Input:
        !   >>> sysConfig   --> System Configuration
        !   >>> sys1DLen    --> System Places
        !   >>> sysDim      --> System Dimension
        !
        ! * Output:
        !   >>> sysMagnet   --> System Magnetization
        !
        ! * References:
        !*******************************************************************************************

        FUNCTION Magnet( sysConfig, xLim, yLim ) &
            RESULT( sysMagnet )

            ! FUNCTION VARIABLES ===================================================================
            ! * Input Variables
            REAL    (KIND = 8) , ALLOCATABLE :: sysConfig(:,:)

            ! * Output Variables
            REAL    (KIND = 8) :: sysMagnet

            ! * Internal Variables
            INTEGER (KIND = 4) :: i, xLim
            INTEGER (KIND = 4) :: j, yLim

            ! FUNTION BODY =========================================================================
            ! Setting up the magnetization of the system
            sysMagnet = 0.0

            ! Running over all spines in th system to calculate magnetization
            DO i = 1,xLim
                DO j = 1,yLim
                    sysMagnet = sysMagnet + sysConfig(  i  ,   j   )
                END DO
            END DO
        END FUNCTION Magnet

        ! SUBROUTINES ==============================================================================

        ! InitializeSystem **********************************************************************100
        !
        ! This subroutine initializes the system by allocating "sysConfig" system configuration
        ! array and assigning values ​​to the system spins placed on system configuration array. This
        ! this process can be donein two ways acording to state type "stateType" optional character
        ! varialbe.
        !
        ! If stateType = "rnd" then a random state is generated and asigned to system configuration,
        ! else if stateType = "gnd" then system configuration is set up to system ground state. If
        ! stateType is not given, it is assumed as "rnd" by default.
        !
        ! sysConfig -> Integer and allocatable 2D-array that contains system spins.
        ! sys1DLen  -> 1D-length of the system.
        ! sysDim    -> System dimension.
        ! stateType   -> System configuration time indicator.
        !
        !****************************************************************************************100

        SUBROUTINE InitializeSystem( sysConfig, xLim, yLim, sysTemp )

            ! SUROUTINE VARIABLES ***************************************************************100
            ! Input Varibles
            REAL     ( KIND = 8 ) , ALLOCATABLE :: sysConfig(:,:)
            REAL     ( KIND = 8 ) :: sysTemp

            ! Internal Varibles
            ! REAL     ( KIND = 8 ) :: rnd
            INTEGER  ( KIND = 4 ) :: i, Xlim, j, Ylim

            ! SUROUTINE BODY ********************************************************************100
            CALL SetRnd( )
            
            DO i = 1,Xlim
                DO j = 1,Ylim
                    
                                        
                    IF(sysTemp < 1.5)THEN
                        !CALL GetRnd( rnd )
                        sysConfig(i,j) = +1
                        !IF(rnd < 0.5) sysConfig(i,j) = -1
                    END IF
                END DO
            END DO

            RETURN
        END SUBROUTINE InitializeSystem

END MODULE ISINGMODULE
