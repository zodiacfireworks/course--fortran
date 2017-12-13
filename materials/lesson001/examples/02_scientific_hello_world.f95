! ------------------------------------------------------------------------------
! Programa:
!   Scientific Hello World
! 
! Descripción:
!   El clásico hello world, con un toque científico
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
!
! Notas:
!   Basado en Morten Hjorth-Jensen, Computational Physics, Lecture Notes Fall
!   2015, Capítulo 2 : program1.f90
! ------------------------------------------------------------------------------
PROGRAM hello_world
    IMPLICIT NONE

    ! Angulo de entrada
    REAL(KIND=4) :: theta
    
    ! Resultado de aplicar la función seno 
    REAL(KIND=4) :: sin_of_theta
    
    ! Mensaje
    CHARACTER(len=*), PARAMETER :: Message = 'Hello World'

    WRITE(*, *) 'Ingrese un ángulo [rad]: '
    READ(*, *) theta
    sin_of_theta = SIN(theta)
    
    WRITE(*, *) Message
    WRITE(*, *) "sin(", theta, ") = ", sin_of_theta
END PROGRAM hello_world
