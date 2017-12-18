! ------------------------------------------------------------------------------
! Programa:
!   Modules06
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
MODULE Precisions
INTEGER,PARAMETER:: Long=SELECTED_REAL_KIND(15,307)
END MODULE Precisions

PROGRAM Modules06

  USE Precisions
  IMPLICIT NONE
  INTEGER :: I,N
  REAL (Long), ALLOCATABLE:: A(:,:),B(:),X(:)
  LOGICAL:: Singular

INTERFACE

  SUBROUTINE Gaussian_Elimination(A,N,B,X,Singular)
    USE Precisions
    IMPLICIT NONE
    INTEGER, INTENT(IN)::N
    REAL (Long), INTENT (INOUT) :: A(:,:),B(:)
    REAL (Long), INTENT(OUT)::X(:)
    LOGICAL, INTENT(OUT) :: Singular
  END SUBROUTINE Gaussian_Elimination

END INTERFACE

  PRINT *,'Number of equations?'
  READ *,N
  ALLOCATE(A(1:N,1:N),B(1:N),X(1:N))
  DO I=1,N
    PRINT *,'Input elements of row ',I,' of A'
    READ*,A(I,1:N)
    PRINT*,'Input element ',I,' of B'
    READ *,B(I)
  END DO
  CALL Gaussian_Elimination(A,N,B,X,Singular)
  IF(Singular) THEN
    PRINT*, 'Matrix is singular'
  ELSE
    PRINT*, 'Solution X:'
    PRINT*,X(1:N)
  ENDIF
END PROGRAM Modules06

SUBROUTINE Gaussian_Elimination(A,N,B,X,Singular)
! Routine to solve a system Ax=b
! using Gaussian Elimination
! with partial pivoting
! The code is based on the Linpack routines
! SGEFA and SGESL
! and operates on columns rather than rows!
  USE Precisions
  IMPLICIT NONE
! Matrix A and vector B are over-written
! Arguments
  INTEGER, INTENT(IN):: N
  REAL (Long),INTENT(INOUT):: A(:,:),B(:)
  REAL (Long),INTENT(OUT)::X(:)
  LOGICAL,INTENT(OUT)::Singular
! Local variables
  INTEGER::I,J,K,Pivot_row
  REAL (Long):: Pivot,Multiplier,Sum,Element
  REAL (Long),PARAMETER::Eps=1.E-13_Long
!
! Work through the matrix column by column
!
  DO K=1,N-1
!
!  Find largest element in column K for pivot
!
  Pivot_row = MAXVAL( MAXLOC( ABS( A(K:N,K) ) ) ) &
    + K - 1
!
! Test to see if A is singular
! if so return to main program
!
    IF(ABS(A(Pivot_row,K)) <= Eps) THEN
      Singular=.TRUE.
      RETURN
    ELSE
      Singular = .FALSE.
    ENDIF
!
! Exchange elements in column K if largest is
! not on the diagonal
!
    IF(Pivot_row /= K) THEN
      Element=A(Pivot_row,K)
      A(Pivot_Row,K)=A(K,K)
      A(K,K)=Element
      Element=B(Pivot_row)
      B(Pivot_row)=B(K)
      B(K)=Element
    ENDIF
!
! Compute multipliers
! elements of column K below diagonal
! are set to these multipliers for use
! in elimination later on
!
    A(K+1:N,K) = A(K+1:N,K)/A(K,K)
!
! Row elimination performed by columns for efficiency
!
    DO J=K+1,N
      Pivot = A(Pivot_row,J)
      IF(Pivot_row /= K) THEN
!       Swap if pivot row is not K
        A(Pivot_row,J)=A(K,J)
        A(K,J)=Pivot
      ENDIF
      A(K+1:N,J)=A(K+1:N,J)-Pivot* A(K+1:N,K)
    END DO
!
! Apply same operations to B
!
    B(K+1:N)=B(K+1:N)-A(K+1:N,K)*B(K)
  END DO
!
! Backward substitution
!
  DO I=N,1,-1
    Sum = 0.0
    DO J= I+1,N
      Sum=Sum+A(I,J)*X(J)
    END DO
    X(I)=(B(I)-Sum)/A(I,I)
  END DO
END SUBROUTINE Gaussian_Elimination
