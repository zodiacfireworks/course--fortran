! ------------------------------------------------------------------------------
! Programa:
!   Strings
! 
! Descripción:
!   Muestra la declaración y manipulación básica de cadenas de caracteres
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
! ------------------------------------------------------------------------------
PROGRAM Strings
    IMPLICIT NONE
    CHARACTER(len= 4), PARAMETER :: prompt = ">>> "
    CHARACTER(len= *), PARAMETER :: message = "Ingresa tu primer nombre [máx 20 caracteres]:"
    CHARACTER(len=20) :: FirstName

    ! Fortran 2003 y posteriores
    ! El carater de salto de linea se especifíca mediante la función NEW_LINE
    WRITE(unit=*, fmt='(3A)', advance='no') prompt, message, NEW_LINE('(A)')
    WRITE(unit=*, fmt='(A)', advance='no') prompt
    
    ! Lee todo hasta llenar los 20 caracteres de FirstName o hasta el primer 
    ! espacio que encuentra
    READ (unit=*, fmt=*) FirstName

    ! Lee todo hasta llenar los 20 caracteres de FirstName
    ! READ (unit=*, fmt='(A)') FirstName
    
    WRITE(unit=*, fmt='(2A)') prompt, FirstName
END PROGRAM Strings
