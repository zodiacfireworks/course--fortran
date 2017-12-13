! ------------------------------------------------------------------------------
! Programa:
!   Atoms
!
! Descripción:
!   Define un nuevo tipo de dato Atomo, muestra como se definen nuevos tipos de
!   datos.
!
! Autor:
!   Martin Vuelta <martin.vuelta@gmail.com>
!
! ------------------------------------------------------------------------------
PROGRAM Atoms
    IMPLICIT NONE
    TYPE Atom
        CHARACTER(len= 2) :: Symbol ! Símbolo
        CHARACTER(len=20) :: Name   ! Nombre
        INTEGER(kind=4)   :: Z      ! Número atómico
        INTEGER(kind=4)   :: A      ! Número de masa
        REAL(kind=4)      :: m      ! Masa en uma
    END TYPE Atom
    TYPE (Atom) :: Helium
    Helium%Symbol = "He"
    Helium%Name   = "Helium"
    Helium%Z      = 2
    Helium%A      = 4
    Helium%m      = 4

    WRITE(*, *) "Helium data [Ordered]"
    WRITE(*, *) "Symbol : ", TRIM(Helium%Symbol)
    WRITE(*, *) "Name   : ", TRIM(Helium%Name)
    WRITE(*, *) "Z      : ", Helium%Z
    WRITE(*, *) "A      : ", Helium%A
    WRITE(*, *) "m      : ", Helium%m
    WRITE(*, *) "Helium data [Raw]"
    WRITE(*, *) Helium
END PROGRAM Atoms
