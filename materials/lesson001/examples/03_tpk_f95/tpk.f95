! ------------------------------------------------------------------------------
! Programa:
!       TPK95
! 
! Descripción:
!       El algorimo TPK en Fortran 95
!
! Autor:
!       Martin Vuelta <martin.vuelta@gmail.com>
!
! Notas:
!   El formato libre de Fortran se emplea en la actualidad desde la 
!   especificación de Fortran 90. Las reglas que sigue son las siguientes
!
!   a. Máximo 132 caracteres por linea. 
!   b. El catacter `!` marca los comentarios. Cualquier cosa colocada despues 
!      `!` sera tratado como un comentario
!
!      ```
!      <instruccion> ! <comentario>
!      ```
!
!   c. Se pueden colocar varias instrucciones en la misma línea y separarlas 
!      empleando el caracter `;`
!
!      ```
!      <instrucción>; <instrucción>
!      ```
!
!   d. Las instrucciones muy largar pueden ser continuadas en la siguiente 
!      linea indicando con un `&` el final de una linea y otro al inicio de su
!      continuación
!      
!      ```
!      <instrucción 1> &
!      & <instrucción 1> &
!      & <instrucción 1>
!      <instrucción 2>
!      ```  
!
!   e. Las variables y unidades programaticas deben de ser nobradas empleando 
!      una combinacion de maximo 31 caracteres siendo el primer caracter una 
!      letra
!
!      ```
!      PROGRAM TPK95
!      ```
!
! ------------------------------------------------------------------------------
MODULE Functions
    PUBLIC :: fun
    CONTAINS
        FUNCTION fun(t) result (r)
            REAL, INTENT(IN) :: t
            REAL  :: r
            r = SQRT(ABS(t)) + 5.0*t**3
        END FUNCTION fun
END MODULE Functions

PROGRAM TPK95
    ! The TPK Algorithm
    ! F style
    USE Functions
    INTEGER :: i
    REAL    :: y
    REAL, DIMENSION(0:10) :: a
    READ *, a
    DO i = 10, 0, -1 ! Bucle DO con contador hacia atrás
        y = fun(a(i))
        IF ( y < 400.0 ) THEN
            PRINT *, i, y
        ELSE
            PRINT *, i, " Too large"
        END IF
    END DO
END PROGRAM tpk95
