! ------------------------------------------------------------------------------
! Programa:
!   PeopleWeight
!
! Descripción:
!   The program reads up to number_of_people weights
!   into the array Weight
!   Variables used
!   Weight, holds the weight of the people
!   Person, an index into the array
!   Total, total weight
!   Average, average weight of the people
!   Parameters used
!   NumberOfPeople ,10 in this case.
!   The weights are written out so that
!   they can be checked
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM PeopleWeight
    IMPLICIT NONE
    INTEGER , PARAMETER :: Number_Of_People = 10
    REAL ::  Total = 0.0, Average = 0.0
    INTEGER :: Person
    REAL , DIMENSION(1:Number_of_People) :: Weight
    DO Person=1,Number_Of_People
        PRINT *, ' Type in the weight for person ',Person
        READ *,Weight(Person)
        Total = Total + Weight(Person)
    ENDDO
    Average = Total / Number_Of_People
    PRINT *,' The total of the weights is ',Total
    PRINT *,' Average Weight is ',Average
    PRINT *,' ',Number_of_People,' Weights were '
    DO Person=1,Number_Of_People
      PRINT *,Weight(Person)
    ENDDO
END PROGRAM PeopleWeight
