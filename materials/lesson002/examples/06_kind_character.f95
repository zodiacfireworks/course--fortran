! ------------------------------------------------------------------------------
! Programa:
!   KindCharacter
! 
! Descripción:
!   Uso de la función SELECTED_CHAR_KIND (Fortran 2003 y posteriores) y el 
!   manejo de la codificación en Fortran.
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
!
! Notas:
!   ISO_FORTRAN_ENV: Módulo intrínseco
!   OUTPUT_UNIT: Identificador del la salida estándar. Usualmente empleado 
!   cuando se escriben expresiones como WRITE(unit=*, ...). Este valor esta 
!   declarado en el módulo ISO_FORTRAN_ENV.
! ------------------------------------------------------------------------------
PROGRAM KindCharacter
    USE ISO_FORTRAN_ENV
    IMPLICIT NONE
    INTEGER, PARAMETER :: ascii = SELECTED_CHAR_KIND('ascii')
    INTEGER, PARAMETER :: ucs4  = SELECTED_CHAR_KIND('ISO_10646')

    CHARACTER(kind=ascii, len=26) :: Alphabet
    CHARACTER(kind= ucs4, len=30) :: HelloWorld

    Alphabet = ascii_'abcdefghijklmnopqrstuvwxyz'
    HelloWorld = ucs4_'Hello World and Ni Hao -- ' // char (int (z'4F60'), ucs4) // char (int (z'597D'), ucs4)

    WRITE(*, '(A)') alphabet
    WRITE(*, '(A)') TRIM(HelloWorld)
    OPEN(OUTPUT_UNIT, encoding='UTF-8')
    WRITE(*, '(A)') TRIM(HelloWorld)
END PROGRAM KindCharacter
