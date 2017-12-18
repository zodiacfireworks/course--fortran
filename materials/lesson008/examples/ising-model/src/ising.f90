PROGRAM ISING

    USE DATETIMEMODULE
    USE ISINGMODULE

    IMPLICIT NONE


    REAL     (KIND = 8) , ALLOCATABLE :: SysConfig(:,:)
    REAL     (KIND = 8) :: SysParams(5), SysAux(5)
    REAL     (KIND = 8) :: SysEnergyDif, SysEnergy, SysMagnet, SysTemp
    INTEGER  (KIND = 4) :: SysDim, SysNSpin, SysX, SysY, Sys1DLen

    REAL     (KIND = 8) :: EnergyDif(-8:8)

    REAL     (KIND = 8) :: TempI, TempF, TempD
    INTEGER  (KIND = 4) :: TempS

    INTEGER  (KIND = 4) :: MCSteps

    INTEGER  (KIND = 4) :: i, j, t, mc, k

    REAL     (KIND = 8) :: rnd
    INTEGER  (KIND = 4) :: XSpin, YSpin, NXSpin, NYSpin

    CHARACTER(LEN = 15) :: FlatDate
    CHARACTER(LEN = 30) :: FullDate

    CHARACTER(LEN  = :) , ALLOCATABLE :: SimFile


    FlatDate = TimeStampStr('flat')
    FullDate = TimeStampStr('full')

    WRITE (*,*) ""
    WRITE (*,*) "Simulating the Ising model for ferromagnetic materials"
    WRITE (*,*) "======================================================"
    WRITE (*,*) ""
    WRITE (*,*) "Program Started at ", FullDate
    WRITE (*,*) ""
    WRITE (*,*) "Please, enter the information requested below : "
    WRITE (*,*) ""
    WRITE (*,"(A43)", ADVANCE='NO') "* System dimension            [1 or 2] : "
    READ  (*,*) SysDim
    WRITE (*,"(A43)", ADVANCE='NO') "* One Dimensional Latice sites         : "
    READ  (*,*) sys1DLen
    WRITE (*,"(A43)", ADVANCE='NO') "* Initial Temperature   [Energy Units] : "
    READ  (*,*) TempI
    WRITE (*,"(A43)", ADVANCE='NO') "* Final   Temperature   [Energy Units] : "
    READ  (*,*) TempF
    WRITE (*,"(A43)", ADVANCE='NO') "* Temperature Steps                    : "
    READ  (*,*) TempS
    WRITE (*,"(A43)", ADVANCE='NO') "* Montecarlo steps                     : "
    READ  (*,*) MCSteps
    WRITE (*,*)


    SysX = Sys1DLen
    SysY = Sys1DLen
    IF( SysDim .EQ. 1 ) SysY = 1
    SysNSpin = SysX*SysY
    ALLOCATE(SysConfig(SysX, SysY))
    DO k = 1,5
        SysParams(k) = 0.0
    END DO
    DO k = 1,5
        SysAux(k) = 0.0
    END DO
    DO k = -8,8
        EnergyDif(k) = 0.0
    END DO
    TempD = (TempF - TempI)/DBLE(TempS)


    CALL DTTic( )

    ALLOCATE( CHARACTER(LEN = 34) :: SimFile )
    WRITE(SimFile,"(I3.3,A,I3.3,3A)") Sys1DLen,'x',Sys1DLen,'_IMSim_',FlatDate,'.dat'
    OPEN (UNIT=10, FILE=SimFile)
    WRITE(10,*) "# Ising Model Simulation File"
    WRITE(10,*) "# Program executed at ", FullDate
    WRITE(10,*) "# Simulation Parameters"
    WRITE(10,*) "#   Model Dimension              : ", SysDim
    WRITE(10,*) "#   One Dimensional Latice sites : ", Sys1DLen
    WRITE(10,*) "#   Initial Temperature          : ", TempI
    WRITE(10,*) "#   Final Temperature            : ", TempF
    WRITE(10,*) "#   Temperature Step             : ", TempS
    WRITE(10,*) "#   Monte Carlo Steps            : ", MCSteps
    WRITE(10,*) "# Data Table (All data is per spin)"
    WRITE(10,*) "#             T             <E> ", &
                       "            <M>           <|M|> ", &
                       "             Cv               X "


    CALL SetRnd( )

    DO t = 0,TempS

        SysTemp = TempI + t*TempD


        DO k = -8,8,4
            EnergyDif(k) = EXP(-DBLE(k)/SysTemp)
        END DO

        CALL InitializeSystem(SysConfig, SysX, SysY, SysTemp)

        SysEnergy = Energy(SysConfig, SysX, SysY)
        SysMagnet = Magnet(SysConfig, SysX, SysY)

        DO mc = 1, MCSteps

            DO i = 1,SysX
                DO j = 1,SysY

                    CALL GetRnd(rnd)
                    XSpin =  INT(rnd*(SysX - 1) + 1)
                    CALL GetRnd(rnd)
                    YSpin =  INT(rnd*(SysY - 1) + 1)

                    SysConfig(XSpin,YSpin) = -1*sysConfig(XSpin,YSpin)

                    SysEnergyDif = 0.0

                    IF( SysY .EQ. 1) THEN
                        SysEnergyDif = SysEnergyDif - 2*cplConst*sysConfig(  XSpin  , YSpin)*( &
                                                                 sysConfig(XSpin - 1, YSpin)+  &
                                                                 sysConfig(XSpin + 1, YSpin) )
                    ELSE
                        NXSpin = NearLocation(XSpin, SysX, -1)
                        NYSpin = NearLocation(YSpin, SysY, -1)
                        SysEnergyDif = SysEnergyDif - 2*cplConst*sysConfig(XSpin , YSpin )*( &
                                                                 sysConfig(NXSpin, YSpin )+  &
                                                                 sysConfig(XSpin , NYSpin) )
                        NXSpin = NearLocation(XSpin, SysX, +1)
                        NYSpin = NearLocation(YSpin, SysY, +1)
                        SysEnergyDif = SysEnergyDif - 2*cplConst*sysConfig(XSpin , YSpin )*( &
                                                                 sysConfig(NXSpin, YSpin )+  &
                                                                 sysConfig(XSpin , NYSpin) )
                    END IF

                    CALL GetRnd(rnd)
                    IF(rnd >= EnergyDif(INT(SysEnergyDif)) .AND. SysEnergyDif > 0) THEN

                        sysConfig(XSpin,YSpin) = -1*sysConfig(XSpin,YSpin)
                    ELSE

                        SysEnergy = SysEnergy + SysEnergyDif
                        SysMagnet = SysMagnet + 2*SysConfig(XSpin, YSpin)
                    END IF
                END DO
            END DO

            SysAux(1) = SysAux(1) + SysEnergy
            SysAux(2) = SysAux(2) + SysMagnet
            SysAux(3) = SysAux(3) + ABS(SysMagnet)
            SysAux(4) = SysAux(4) + SysEnergy*SysEnergy
            SysAux(5) = SysAux(5) + SysMagnet*SysMagnet
        END DO

        SysAux(1) = SysAux(1)/DBLE(MCSteps)
        SysAux(2) = SysAux(2)/DBLE(MCSteps)
        SysAux(3) = SysAux(3)/DBLE(MCSteps)
        SysAux(4) = SysAux(4)/DBLE(MCSteps)
        SysAux(5) = SysAux(5)/DBLE(MCSteps)

        SysParams(1) = SysAux(1)/SysNSpin

        SysParams(2) = SysAux(2)/SysNSpin

        SysParams(3) = SysAux(3)/SysNSpin

        SysParams(4) = (SysAux(4) - SysAux(1)*SysAux(1))/SysNSpin/SysTemp/SysTemp

        SysParams(5) = (SysAux(5) - SysAux(2)*SysAux(2))/SysNSpin/SysTemp
        WRITE(10,120) SysTemp, SysParams(1), SysParams(2), SysParams(3), &
                                       SysParams(4), SysParams(5)
        120 FORMAT(1X,E15.5,1X,E15.5,1X,E15.5,1X,E15.5,1X,E15.5,1X,E15.5)
        WRITE(*,"(1X,A,F6.2,A)") "PROGRESS : ", 100*t/DBLE(TempS), "%"
    END DO
    Close(10)
    WRITE(*,*)
    CALL DTToc( )
END PROGRAM ISING
