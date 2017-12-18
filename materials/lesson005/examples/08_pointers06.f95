! ------------------------------------------------------------------------------
! Programa:
!   Pointers06
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Pointers06
TYPE Link
  CHARACTER :: C
  TYPE (Link) , POINTER   :: Next
END TYPE Link
TYPE (Link) , POINTER :: Root , Current
INTEGER :: IO_Stat_Number=0
  ALLOCATE(Root)
  READ (UNIT = *, FMT = '(A)' , ADVANCE = 'NO' , &
    IOSTAT = IO_Stat_Number) Root%C
  IF (IO_Stat_Number == -1) THEN
    NULLIFY(Root%Next)
  ELSE
    ALLOCATE(Root%Next)
  ENDIF
  Current => Root
  DO WHILE (ASSOCIATED(Current%Next))
    Current => Current%Next
    READ (UNIT=*,FMT='(A)',ADVANCE='NO', &
    IOSTAT=IO_Stat_Number) Current%C
    IF (IO_Stat_Number == -1) THEN
      NULLIFY(Current%Next)
    ELSE
      ALLOCATE(Current%Next)
    ENDIF
  END DO
  Current => Root
  DO WHILE (ASSOCIATED(Current%Next))
    PRINT * , Current%C
    Current => Current%Next
  END DO
END PROGRAM Pointers06
