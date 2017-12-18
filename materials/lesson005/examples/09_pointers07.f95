! ------------------------------------------------------------------------------
! Programa:
!   Pointers07
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Pointers07

TYPE Link
  REAL :: N
  TYPE (Link) , POINTER   :: Next
END TYPE Link

TYPE (Link) , POINTER :: Root, Current

INTEGER :: I=0
integer :: error=0
INTEGER :: IO_Stat_Number=0
integer :: blank_lines=0

real , allocatable , dimension(:) :: x

  ALLOCATE(Root)
  READ (UNIT = *, FMT = *, IOSTAT = IO_Stat_Number) Root%N
  IF (IO_Stat_Number > 0) THEN
    error=error+1
  else if (io_stat_number == -1) then
    NULLIFY(Root%Next)
  else if (io_stat_number == -2) then
    blank_lines=blank_lines+1
  ELSE
    i=i+1
    ALLOCATE(Root%Next)
  ENDIF

  Current => Root

  DO WHILE (ASSOCIATED(Current%Next))

    Current => Current%Next

    READ (UNIT=*,FMT=*, IOSTAT=IO_Stat_Number) Current%N

    IF (IO_Stat_Number > 0) THEN
      error=error+1
    else if (io_stat_number == -1) then
      NULLIFY(current%Next)
    else if (io_stat_number == -2) then
      blank_lines=blank_lines+1
    ELSE
      i=i+1
      ALLOCATE(current%Next)
    ENDIF

  END DO

  print *,i,' items read'
  print *,blank_lines,' blank lines'
  print *,error,' items in error'

  allocate(x(1:i))
  i=1
  Current => Root

  DO WHILE (ASSOCIATED(Current%Next))
    x(i)=current%n
    i=i+1
    PRINT * , Current%N
    Current => Current%Next
  END DO

  print *,x

END PROGRAM Pointers07
