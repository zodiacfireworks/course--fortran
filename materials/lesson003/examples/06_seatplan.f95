! ------------------------------------------------------------------------------
! Programa:
!   SeatPlan
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM SeatPlan
    IMPLICIT NONE
    INTEGER  , PARAMETER :: NR=5
    INTEGER  , PARAMETER :: NC=10
    INTEGER  , PARAMETER :: NF=3
    INTEGER :: Row,Column,Floor
    CHARACTER*1 , DIMENSION(1:NR,1:NC,1:NF) :: Seats=' '

    DO  Floor=1,NF
        DO  Row=1,NR
            READ *,(Seats(Row,Column,Floor),Column=1,NC)
        ENDDO
    ENDDO

    PRINT *,' Seat plan is'

    DO  Floor=1,NF
        PRINT *,' Floor = ',Floor
        DO  Row=1,NR
            PRINT *,(Seats(Row,Column,Floor),Column=1,NC)
        ENDDO
    ENDDO
END PROGRAM SeatPlan
