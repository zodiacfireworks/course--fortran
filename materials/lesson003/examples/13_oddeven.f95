! ------------------------------------------------------------------------------
! Programa:
!   OddEven
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program OddEven
    implicit none
    integer :: i
    integer , dimension(1:10) :: x=(/(i,i=1,10)/)
    integer , dimension(1:5)  :: odd=(/(i,i=1,10,2)/)
    integer , dimension(1:5)  :: even
    even = x(2:10:2)
    print *,' x'
    print *, x
    print *,' odd'
    print *, odd
    print *,' even'
    print *, even
end program OddEven
