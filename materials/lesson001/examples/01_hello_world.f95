! ------------------------------------------------------------------------------
! Programa:
!   Hello World
! 
! Descripción:
!   El clásico hello world
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM hello_world
    CHARACTER(len=*), PARAMETER :: Message = 'Hello World'
    WRITE(*, *) Message
END PROGRAM hello_world
