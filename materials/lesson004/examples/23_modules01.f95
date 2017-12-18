! ------------------------------------------------------------------------------
! Programa:
!   Modules01
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
module precision_definition
  implicit none
  integer , parameter :: long=selected_real_kind(15,307)
end module precision_definition

module maths_constants
  use precision_definition
  implicit none
  real (long) , parameter :: c = 299792458.0_long
  ! units m s-1
  real (long) , parameter :: &
    e = 2.71828182845904523_long
  real (long) , parameter :: g = 9.812420_long
  ! 9.780 356 m s-2 at sea level on the equator
  ! 9.812 420 m s-2 at sea level in London
  ! 9.832 079 m s-2 at sea level at the poles
  real (long) , parameter :: &
    pi = 3.14159265358979323_long
end module maths_constants

PROGRAM Modules01
  USE Precision_definition
  IMPLICIT NONE
  INTERFACE
    SUBROUTINE Sub1(Radius,Area,Circum)
    USE Precision_definition
    IMPLICIT NONE
    REAL(Long),INTENT(IN)::Radius
    REAL(Long),INTENT(OUT)::Area,Circum
    END SUBROUTINE Sub1
  END INTERFACE
  REAL(Long)::R,A,C
  INTEGER ::I
  DO I=1,10
    PRINT*,'Radius?'
    READ*,R
    CALL Sub1(R,A,C)
    PRINT *,' For radius   = ',R
    PRINT *,' Area        = ',A
    PRINT *,' Circumference = ',C
  END DO
END PROGRAM Modules01

SUBROUTINE Sub1(Radius,Area,Circum)
  USE Precision_definition
  use maths_constants
  IMPLICIT NONE
  REAL(Long),INTENT(IN)::Radius
  REAL(Long),INTENT(OUT)::Area,Circum
  Area=Pi*Radius*Radius
  Circum=2.0_Long*Pi*Radius
END SUBROUTINE Sub1
