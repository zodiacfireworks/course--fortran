! ------------------------------------------------------------------------------
! Programa:
!   Subroutines05
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Subroutines05

implicit none
integer                        :: n
real , allocatable , dimension(:)  :: x
real :: m,sd

interface
  subroutine stats(x,n,mean,std_dev)
    implicit none
    integer , intent(in)               :: n
    real    , intent(in) , dimension(:) :: x
    real    , intent(out)             :: mean
    real    , intent(out)             :: std_dev
  end subroutine stats
end interface

  print *,' type in n'
  read *,n
  allocate(x(1:n))
  call random_number(x)
  x=x*100
  call stats(x,n,m,sd)
  print *,' numbers were '
  print *,x
  print *,' Mean =               ',m
  print *,' Standard deviation = ',sd

end program Subroutines05

subroutine stats(x,n,mean,std_dev)
implicit none
integer , intent(in)                :: n
real    , intent(in) , dimension(:)   :: x
real    , intent(out)               :: mean
real    , intent(out)               :: std_dev
real :: variance
real:: sumxi,sumxi2
integer :: i

  variance=0.0
  sumxi=0.0
  sumxi2=0.0
  do i=1,n
     sumxi = sumxi+ x(i)
     sumxi2 = sumxi2 + x(i)*x(i)
  end do
  mean=sumxi/n
  variance = (sumxi2 - sumxi*sumxi/n)/(n-1)
  std_dev=sqrt(variance)
end subroutine stats
