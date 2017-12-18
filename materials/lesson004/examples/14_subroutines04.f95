! ------------------------------------------------------------------------------
! Programa:
!   Subroutines04
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Subroutines04
implicit none
integer , parameter      :: n=10
real , dimension(1:n)    :: x
real , dimension(-4:5)   :: y
real , dimension(10)     :: z
real , allocatable , dimension(:) :: t
real :: m,sd
integer :: i

interface
  subroutine stats(x,n,mean,std_dev)
    implicit none
    integer , intent(in)                :: n
    real    , intent(in) , dimension(1:n) :: x
    real    , intent(out)               :: mean
    real    , intent(out)               :: std_dev
  end subroutine stats
end interface

  do i=1,n
    x(i)=real(i)
  end do
  call stats(x,n,m,sd)
  print *,' x'
  print *,' mean = ',m
  print *,' Standard deviation = ',sd
  y=x
  call stats(y,n,m,sd)
  print *,' y'
  print *,' mean = ',m
  print *,' Standard deviation = ',sd
  z=x
  call stats(z,10,m,sd)
  print *,' z'
  print *,' mean = ',m
  print *,' Standard deviation = ',sd
  allocate(t(10))
  t=x
  call stats(t,10,m,sd)
  print *,' t'
  print *,' mean = ',m
  print *,' Standard deviation = ',sd
end program Subroutines04

subroutine stats(x,n,mean,std_dev)
implicit none
integer , intent(in)                  :: n
real    , intent(in) , dimension(1:n) :: x
real    , intent(out)                 :: mean
real    , intent(out)                 :: std_dev
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
