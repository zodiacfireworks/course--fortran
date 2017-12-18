! ------------------------------------------------------------------------------
! Programa:
!   Reader05
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
program Reader05
implicit none
integer , parameter :: n=1000000
integer , dimension(1:n) :: x
real    , dimension(1:n) :: y
integer :: i
real :: t,t1,t2,t3,t4,t5
character*10 :: comment
  open(unit=10,file='ch1306.txt')
  call cpu_time(t)
  t1=t
  comment=' Intial '
  print 100,comment,t1
  do i=1,n
    read(10,200) x(i)
    200 format(1x,i10)
  end do
  call cpu_time(t)
  t2=t-t1
  comment = ' i read '
  print 100,comment,t2
  do i=1,n
    read(10,300) y(i)
    300 format(1x,f10.0)
  end do
  call cpu_time(t)
  t3=t-t1-t2
  comment = ' r read '
  print 100,comment,t3
  100 format(1x,a,2x,f7.3)
  do i=1,10
    print *,x(i), ' ' , y(i)
  end do
end program Reader05
