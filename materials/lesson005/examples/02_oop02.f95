! ------------------------------------------------------------------------------
! Programa:
!   OOP02
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM OOP02
IMPLICIT NONE
TYPE Address
  CHARACTER (LEN=60)  :: Street
  CHARACTER (LEN=60)  :: District
  CHARACTER (LEN=60)  :: City
  CHARACTER (LEN=8 )  :: Post_Code
END TYPE Address
TYPE Date_Of_Birth
  INTEGER :: Day
  INTEGER :: Month
  INTEGER :: Year
END TYPE Date_Of_Birth
TYPE Personal
  CHARACTER (LEN=20)   :: First_Name
  CHARACTER (LEN=20)   :: Other_Names
  CHARACTER (LEN=40)   :: Surname
  TYPE (Date_Of_Birth) :: DOB
  CHARACTER (LEN=1)    :: Sex
  TYPE (Address)       :: Addr
END TYPE Personal
INTEGER , PARAMETER :: N_People=2
TYPE (Personal) , DIMENSION(N_People) :: P
INTEGER :: I
  OPEN(UNIT=1,FILE='PERSON.DAT')
  DO I=1,N_People
    READ(1,FMT=10)  P(I)%First_Name,&
              P(I)%Other_Names,&
              P(I)%Surname,&
              P(I)%DOB%Day,&
              P(I)%DOB%Month,&
              P(I)%DOB%Year,&
              P(I)%Sex,&
              P(I)%Addr%Street,&
              P(I)%Addr%District,&
              P(I)%Addr%City,&
    P(I)%Addr%Post_Code
    10 FORMAT(  A20,/,&
            A20,/,&
            A40,/,&
            I2,1X,I2,1X,I4,/,&
            A1,/,&
            A60,/,&
            A60,/,&
            A60,/,&
            A8)
  END DO
  DO I=1,N_People
    WRITE(*,FMT=20)  P(I)%First_Name,&
                P(I)%Other_Names,&
                P(I)%Surname,&
                P(I)%DOB%Day,&
                 P(I)%DOB%Month,&
                P(I)%DOB%Year,&
                P(I)%Sex,&
                P(I)%Addr%Street,&
                P(I)%Addr%District,&
                P(I)%Addr%City,&
                P(I)%Addr%Post_Code
    20 FORMAT(      A20,A20,A40,/,&
                I2,1X,I2,1X,I4,/,&
                A1,/,&
                A60,/,&
                A60,/,&
                A60,/,&
                A8)
  END DO
END PROGRAM OOP02
