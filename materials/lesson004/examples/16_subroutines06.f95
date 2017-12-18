! ------------------------------------------------------------------------------
! Programa:
!   Subroutines06
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Subroutines06
  IMPLICIT NONE
  REAL , ALLOCATABLE , DIMENSION &
  (:,:)::One,Two,Three,One_T
  INTEGER :: I,N
  INTERFACE
    SUBROUTINE Matrix_bits(A,B,C,A_T,N)
    IMPLICIT NONE
    INTEGER, INTENT(IN):: N
      REAL, DIMENSION (:,:), INTENT(IN) :: A,B
      REAL, DIMENSION (:,:), INTENT(OUT) :: C,A_T
    END SUBROUTINE Matrix_bits
  END INTERFACE
    PRINT *,'Input size of matrices'
    READ*,N
    ALLOCATE(One(1:N,1:N))
    ALLOCATE(Two(1:N,1:N))
    ALLOCATE(Three(1:N,1:N))
    ALLOCATE(One_T(1:N,1:N))
    DO I=1,N
      PRINT*, 'Input row ', I,' of One'
      READ*,One(I,1:N)
    END DO
    DO I=1,N
      PRINT*, 'Input row ', I,' of Two'
      READ*,Two(I,1:N)
    END DO
    CALL Matrix_bits(One,Two,Three,One_T,N)
    PRINT*,' Matrix Three:'
    DO I=1,N
      PRINT *,Three(I,1:N)
    END DO
    PRINT *,' Matrix One_T:'
    DO I=1,N
      PRINT *,One_T(I,1:N)
    END DO
END PROGRAM Subroutines06

SUBROUTINE Matrix_bits(A,B,C,A_T,N)
  IMPLICIT NONE
  INTEGER, INTENT(IN):: N
  REAL, DIMENSION (:,:), INTENT(IN) :: A,B
  REAL, DIMENSION (:,:), INTENT(OUT) :: C,A_T
  INTEGER:: I,J, K
  REAL:: Temp
!
! matrix multiplication C=AB
!
   DO I=1,N
      DO J=1,N
         Temp=0.0
         DO K=1,N
            Temp = Temp + A(I,K) * B (K,J)
         END DO
            C(I,J) = Temp
      END DO
   END DO
!
! Calculate A_T transpose of A
!
!
! set A_T to be transpose matrix A
  DO I=1,N
     DO J=1,N
        A_T(I,J) = A(J,I)
     END DO
 END DO
END SUBROUTINE Matrix_bits
