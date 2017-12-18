! ------------------------------------------------------------------------------
! Programa:
!   Modules02
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
module data
  implicit none
  integer , parameter   :: n=12
  real , dimension(1:n) :: rainfall
  real , dimension(1:n) :: sorted
end module data

program Modules02
use data
implicit none

  call readdata
  call sortdata
  call printdata

end program Modules02

subroutine readdata
use data
implicit none
integer :: i
character (len=40) :: filename
  print *,' What is the filename ?'
  read *,filename
  open(unit=100,file=filename)
  do i=1,n
    read (100,*) rainfall(i)
  end do
end subroutine readdata

subroutine sortdata
use data
  sorted=rainfall
  call selection

  contains

    subroutine selection
    implicit none
    integer :: i,j,k
    real :: minimum
      do i=1,n-1
        k=i
        minimum=sorted(i)
        do j=i+1,n
          if (sorted(j) < minimum) then
            k=j
            minimum=sorted(k)
          end if
        end do
        sorted(k)=sorted(i)
        sorted(i)=minimum
      end do
    end subroutine selection

end subroutine sortdata

subroutine printdata
use data
implicit none
integer :: i
  print *,' original data is '
  do i=1,n
    print 100,rainfall(i)
    100 format(1x,f7.1)
  end do
  print *,' Sorted data is '
  do i=1,n
    print 100,sorted(i)
  end do
end subroutine printdata
