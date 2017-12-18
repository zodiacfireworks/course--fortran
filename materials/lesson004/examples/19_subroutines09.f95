! ------------------------------------------------------------------------------
! Programa:
!   Subroutines09
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Subroutines09

implicit none
integer :: n
real , allocatable , dimension(:) :: x
real :: m,sd,median
integer , dimension(8) :: timing

interface
  subroutine stats(x,n,mean,std_dev,median)
    implicit none
    integer , intent(in)                :: n
    real    , intent(in) , dimension(:)   :: x
    real    , intent(out)               :: mean
    real    , intent(out)               :: std_dev
    real    , intent(out)               :: median
  end subroutine stats
end interface

  n=1000
  do
    print *,' n = ',n
    allocate(x(1:n))
    call random_number(x)
    x=x*1000
    call date_and_time(values=timing)
    print *,' initial '
    print *,timing(6),timing(7),timing(8)
    call stats(x,n,m,sd,median)
    print *,' mean = ',m
    print *,' Standard deviation = ',sd
    print *,' median is = ',median
    call date_and_time(values=timing)
    print *,' after sort'
    print *, timing(6),timing(7),timing(8)
    n=n*10
    deallocate(x)
  end do

end program Subroutines09

subroutine stats(x,n,mean,std_dev,median)
implicit none
integer , intent(in)                :: n
real    , intent(in) , dimension(:)   :: x
real    , intent(out)               :: mean
real    , intent(out)               :: std_dev
real    , intent(out)               :: median
real    , dimension(1:n)            :: y
real :: variance
real    :: sumxi, sumxi2
integer:: k
  sumxi=0.0
  sumxi2=0.0
  variance=0.0
  sumxi=sum(x)
  sumxi2=sum(x*x)
  mean=sumxi/n
  variance=(sumxi2-sumxi*sumxi/n)/(n-1)
  std_dev = sqrt(variance)
  y=x
  if (mod(n,2) == 0) then
    median = ( find(n/2)+find((n/2)+1) )/2
  else
    median=find((n/2)+1)
  endif

  contains

    real function find(k)
    implicit none
    integer , intent(in) :: k
    integer :: l,r,i,j
    real :: t1,t2
      l=1
      r=n
      do while (l<r)
        t1=y(k)
        i=l
        j=r
        do
          do while (y(i)<t1)
            i=i+1
          end do
          do while (t1<y(j))
            j=j-1
          end do
          if (i<=j) then
            t2=y(i)
            y(i)=y(j)
            y(j)=t2
            i=i+1
            j=j-1
          end if
          if (i>j) exit
        end do
        if (j<k) then
          l=i
        end if
        if (k<i) then
          r=j
        end if
      end do
      find=y(k)
    end function find

end subroutine stats
