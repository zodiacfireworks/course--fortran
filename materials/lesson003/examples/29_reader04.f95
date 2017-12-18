! ------------------------------------------------------------------------------
! Programa:
!   Reader04
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Reader04
implicit none
integer , parameter :: nrow=5
integer , parameter :: ncol=6
REAL , DIMENSION(1:nrow,1:ncol) :: Exam_Results    = 0.0
real , dimension(1:nrow)        :: People_average  = 0.0
real , dimension(1:ncol)        :: Subject_Average = 0.0
integer :: r,c
  do r=1,nrow
    read 100,(exam_results(r,1:ncol)),people_average(r)
    100 format(1x,6(1x,f5.1),4x,f6.2)
  end do
  read *
  read 110, subject_average(1:ncol)
  110 format(1x,6(1x,f5.1))
  do r=1,nrow
    print 200,(exam_results(r,c),c=1,ncol),people_average(r)
    200 format(1x,6(1x,f5.1),'  = ',f6.2)
  end do
  print *,'  ====  ====  ====  ====  ====  ===='
  print 210, subject_average(1:ncol)
  210 format(1x,6(1x,f5.1))
end program Reader04
