! ------------------------------------------------------------------------------
! Programa:
!   Subroutines07
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Subroutines07
  IMPLICIT NONE
  REAL , ALLOCATABLE , DIMENSION &
  (:,:)::One,Two,Three,One_T
  INTEGER :: I,N
  INTERFACE
    SUBROUTINE Matrix_bits(A,B,C,A_T)
      IMPLICIT NONE
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
    CALL Matrix_bits(One,Two,Three,One_T)
    PRINT*,' Matrix Three:'
    DO I=1,N
      PRINT *,Three(I,1:N)
    END DO
    PRINT *,' Matrix One_T:'
    DO I=1,N
      PRINT *,One_T(I,1:N)
    END DO
END PROGRAM Subroutines07

SUBROUTINE Matrix_bits(A,B,C,A_T)
  IMPLICIT NONE
  REAL, DIMENSION (:,:), INTENT(IN) :: A,B
  REAL, DIMENSION (:,:), INTENT(OUT) :: C,A_T
    C=MATMUL(A,B)
    A_T=TRANSPOSE(A)
END SUBROUTINE Matrix_bits
