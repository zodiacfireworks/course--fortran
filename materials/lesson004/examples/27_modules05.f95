! ------------------------------------------------------------------------------
! Programa:
!   Modules05
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
module statistics

contains

  real function mean(x,n)
  implicit none
  integer , intent(in)                :: n
  real    , intent(in) , dimension(:)   :: x
  integer :: i
  real    :: total
    total=0
    do i=1,n
      total=total+x(i)
    end do
    mean=total/n
  end function mean

  real function std_dev(x,n,mean)
  integer , intent(in)                :: n
  real    , intent(in) , dimension(:)   :: x
  real    , intent(in)                :: mean
  real                              :: variance
    variance=0
    do i=1,n
      variance=variance + (x(i)-mean)**2
    end do
    variance=variance/(n-1)
    std_dev=sqrt(variance)
  end function std_dev

  real function median(x,n)
  integer , intent(in)                :: n
  real    , intent(in) , dimension(:)   :: x
  real    , dimension(1:n)            :: y
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

  end function median

end module statistics

program Modules05

use statistics

implicit none
integer :: n
real , allocatable , dimension(:) :: x
real :: m,sd,med
integer , dimension(8) :: v

  print *,' How many values ?'
  read *,n
  call date_and_time(values=v)
  print *,' initial              ',v(6),v(7),v(8)
  allocate(x(1:n))
  call date_and_time(values=v)
  print *,' allocate             ',v(6),v(7),v(8)
  call random_number(x)
  call date_and_time(values=v)
  print *,' random               ',v(6),v(7),v(8)
  x=x*1000
  call date_and_time(values=v)
  print *,' output               ',v(6),v(7),v(8)
  m=mean(x,n)
  call date_and_time(values=v)
  print *,' mean                 ',v(6),v(7),v(8)
  print *,' mean               = ',m
  sd=std_dev(x,n,m)
  call date_and_time(values=v)
  print *,' standard deviation   ',v(6),v(7),v(8)
  print *,' Standard deviation = ',sd
  med = median(x,n)
  call date_and_time(values=v)
  print *,' median               ',v(6),v(7),v(8)
  print *,' median is          = ',med
end program Modules05
