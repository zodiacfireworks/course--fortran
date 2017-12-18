! ------------------------------------------------------------------------------
! Programa:
!   Pointers08
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Pointers08
use f90_iostat
TYPE Link
  REAL :: N
  TYPE (Link) , POINTER   :: Next
END TYPE Link

TYPE (Link) , POINTER :: Root, Current

INTEGER :: I=0
INTEGER :: IO_Stat_Number=0

  ALLOCATE(Root)
  READ (UNIT = *, FMT = *, IOSTAT = IO_Stat_Number) Root%N
  if (io_stat_number == ioerr_eof) then
    NULLIFY(Root%Next)
  ELSE if(io_stat_number == ioerr_ok) then
    i=i+1
    ALLOCATE(Root%Next)
  ENDIF

  Current => Root

  DO WHILE (ASSOCIATED(Current%Next))

    Current => Current%Next

    READ (UNIT=*,FMT=*, IOSTAT=IO_Stat_Number) Current%N

    if (io_stat_number == ioerr_eof) then
      NULLIFY(current%Next)
    ELSE if(io_stat_number == ioerr_ok) then
      i=i+1
      ALLOCATE(current%Next)
    ENDIF

  END DO

  print *,i,' items read'

  Current => Root

  DO WHILE (ASSOCIATED(Current%Next))
    PRINT * , Current%N
    Current => Current%Next
  END DO

END PROGRAM Pointers08
