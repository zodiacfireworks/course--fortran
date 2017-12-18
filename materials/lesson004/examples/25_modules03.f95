! ------------------------------------------------------------------------------
! Programa:
!   Modules03
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
MODULE Personal_details
  IMPLICIT NONE
  TYPE Person
    REAL:: Weight
    INTEGER :: Age
    CHARACTER :: Sex
  END TYPE Person
END MODULE Personal_details

PROGRAM Modules03
  USE Personal_details
  IMPLICIT NONE
  INTEGER ,PARAMETER:: Max_no=100
  TYPE (Person), DIMENSION(1:Max_no) :: Patient
  INTEGER :: No_of_patients
  REAL :: Male_average, Female_average
INTERFACE

  SUBROUTINE Read_data(Data,Max_no,No)
    USE Personal_details
    IMPLICIT NONE
    TYPE (Person), DIMENSION (:), INTENT(OUT):: Data
    INTEGER, INTENT(OUT):: No
    INTEGER, INTENT(IN):: Max_no
  END SUBROUTINE Read_Data

  SUBROUTINE Stats(Data,No,M_a,F_a)
    USE Personal_details
    IMPLICIT NONE
    TYPE(Person), DIMENSION (:) :: Data
    REAL:: M_a,F_a
    INTEGER :: No
  END SUBROUTINE Stats

END INTERFACE
!
  CALL Read_data(Patient,Max_no,No_of_patients)
  CALL Stats(  Patient , No_of_patients , &
          Male_average , Female_average)
  PRINT*, 'Average male weight is ',Male_average
  PRINT*, 'Average female weight is ',Female_average
END PROGRAM Modules03

SUBROUTINE Read_Data(Data,Max_no,No)
  USE Personal_details
  IMPLICIT NONE
  TYPE (PERSON), DIMENSION (:), INTENT(OUT)::Data
  INTEGER, INTENT(OUT):: No
  INTEGER, INTENT(IN):: Max_no
  INTEGER :: I
  DO
    PRINT *,'Input number of patients'
    READ *,No
    IF ( No > 0 .AND. No <= Max_no) EXIT
  END DO
  DO I=1,No
    PRINT *,'For person ',I
    PRINT *,'Weight ?'
    READ*,Data(I)%Weight
    PRINT*,'Age ?'
    READ*,Data(I)%Age
    PRINT*,'Sex ?'
    READ*,Data(I)%Sex
  END DO
END SUBROUTINE Read_Data

SUBROUTINE Stats(Data,No,M_a,F_a)
  USE Personal_details
  IMPLICIT NONE
  TYPE(Person), DIMENSION(:)::Data
  REAL :: M_a,F_a
  INTEGER:: No
  INTEGER :: I,No_f,No_m
  M_a=0.0; F_a=0.0;No_f=0; No_m =0
  DO I=1,No
    IF ( Data(I)%Sex == 'M' &
    .OR. Data(I)%Sex == 'm') THEN
      M_a=M_a+Data(I)%Weight
      No_m=No_m+1
    ELSEIF(Data(I)%Sex == 'F' &
      .OR. Data(I)%Sex == 'f') THEN
      F_a=F_a +Data(I)%Weight
      No_f=No_f+1
    ENDIF
  END DO
  IF (No_m > 0 ) THEN
    M_a = M_a/No_m
  ENDIF
  IF (No_f > 0 ) THEN
    F_a = F_a/No_f
  ENDIF
END SUBROUTINE Stats
