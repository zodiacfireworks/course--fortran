! ------------------------------------------------------------------------------
! Programa:
!   Subroutines03
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Subroutines03
IMPLICIT NONE
! Simple example of the use of a main program and two
! subroutines. One interacts with the user and the
! second solves a quadratic equation,
! based on the use input.

REAL :: P, Q, R, Root1, Root2
INTEGER :: IFail=0
LOGICAL :: OK=.TRUE.
  CALL Interact(P,Q,R,OK)
  IF (OK) THEN
    CALL Solve(P,Q,R,Root1,Root2,IFail)
    IF (IFail == 1) THEN
      PRINT *,' Complex roots, calculation abandoned'
    ELSE
      PRINT *,' Roots are  ',Root1,'  ',Root2
    ENDIF
  ELSE
    PRINT*,' Error in data input program ends'
  ENDIF

contains

SUBROUTINE Interact(A,B,C,OK)
  IMPLICIT NONE
  REAL , INTENT(OUT) :: A
  REAL , INTENT(OUT) :: B
  REAL , INTENT(OUT) :: C
  LOGICAL , INTENT(OUT) :: OK
  INTEGER :: IO_Status=0
  PRINT*,' Type in the coefficients A, B AND C'
  READ(UNIT=*,FMT=*,IOSTAT=IO_Status)A,B,C
  IF (IO_Status == 0) THEN
    OK=.TRUE.
  ELSE
    OK=.FALSE.
  ENDIF
END SUBROUTINE Interact

SUBROUTINE Solve(E,F,G,Root1,Root2,IFail)
  IMPLICIT NONE
  REAL , INTENT(IN) :: E
  REAL , INTENT(IN) :: F
  REAL , INTENT(IN) :: G
  REAL , INTENT(OUT) :: Root1
  REAL , INTENT(OUT) :: Root2
  INTEGER , INTENT(INOUT) :: IFail
! Local variables
  REAL :: Term
  REAL :: A2
  Term = F*F - 4.*E*G
  A2 = E*2.0
! if term < 0, roots are complex
  IF(Term < 0.0)THEN
    IFail=1
  ELSE
    Term = SQRT(Term)
    Root1 = (-F+Term)/A2
    Root2 = (-F-Term)/A2
  ENDIF
END SUBROUTINE Solve

END PROGRAM Subroutines03
