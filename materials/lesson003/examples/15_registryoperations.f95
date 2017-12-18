! ------------------------------------------------------------------------------
! Programa:
!   RegistryOperations
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM RegistryOperations
IMPLICIT NONE
INTEGER , PARAMETER :: nrow=5
INTEGER , PARAMETER :: ncol=6
REAL , DIMENSION(1:nrow*ncol)   :: results = &
              (/50 , 47 , 28 , 89 , 30 , 46 , &
                37 , 67 , 34 , 65 , 68 , 98 , &
                25 , 45 , 26 , 48 , 10 , 36 , &
                89 , 56 , 33 , 45 , 30 , 65 , &
                68 , 78 , 38 , 76 , 98 , 65/)
REAL , DIMENSION(1:nrow,1:ncol) :: Exam_Results &
   = 0.0
REAL , DIMENSION(1:nrow)       :: People_average &
   = 0.0
REAL , DIMENSION(1:ncol)        :: Subject_Average &
   = 0.0
INTEGER :: r,c
  exam_results = &
    reshape(results,(/nrow,ncol/),(/0.0,0.0/),(/2,1/))
  Exam_Results(1:nrow,3) = 2.5 * Exam_Results(1:nrow,3)
  subject_average = sum(exam_results,dim=1)
  people_average  = sum(exam_results,dim=2)
  people_average  = people_average  / ncol
  subject_average = subject_average / nrow
  print *,' People averages'
  print *, people_average
  print *, ' Subject averages'
  print *, subject_average
END PROGRAM RegistryOperations
