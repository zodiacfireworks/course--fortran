! ------------------------------------------------------------------------------
! Programa:
!   RainFallConverter
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM ch1101
    IMPLICIT NONE
    INTEGER , PARAMETER :: N=12
    REAL , DIMENSION(1:N) :: RainFall_ins=0.0
    REAL , DIMENSION(1:N) :: RainFall_cms=0.0
    INTEGER :: Month

    PRINT *, ' Input the rainfall values in inches'

    READ *, RainFall_ins
    RainFall_cms=RainFall_ins * 2.54

    DO Month=1,N
        PRINT * , ' ', Month , ' ' , &
            RainFall_ins(Month) , ' ' , &
            RainFall_cms(Month)
    END DO
END PROGRAM ch1101
