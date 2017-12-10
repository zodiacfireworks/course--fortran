C     ------------------------------------------------------------------
C     Programa:
C           TPK77
C     
C     Descripción:
C           El algorimo TPK en FORTRAN 77
C
C     Autor:
C           Martin Vuelta <martin.vuelta@gmail.com>
C
C     Notas:
C           El formato fijo de Fortran se empleo hasta antes de
C           Fortran 90 y seguia las siguientes reglas
C           
C           a. Maximo permite 72 caracteres por linea
C           b. El comentario se marca colocando la letra "c en el 
C              primer"
C              caracter
C
C              ```
C              c --- Comentario
C              ```
C
C           c. Las instrucciones inician en el caracter numero 7
C
C              ```
C              c23456789
C                    <Instruccion>
C              ```
C
C           d. Las instrucciones muy largas (mas de 72 caracteres)se
C              continuan en la linea siguiente colocando un caracter
C              no alfanumerico (comunmente se utilizan "*") y "&"
C
C              ```
C              c23456789
C                    <Instruccion muy larga>
C                   &<Continuacion de instruccion muy larga>
C              ```
C
C           e. Los primeros 5 caracteres se emplean para etiqueta de 
C              una linea emplenando numeros mayores a 0 y hasta 
C              99999
C
C              ```
C              c23456789
C              75289 <Instruccion muy larga>
C                   &<Continuacion de instruccion muy larga>
C              ```
C
C           f. Las variables deben de ser nobradas empleando una
C              combinacion de maximo 6 caracteres siendo el primer
C              caracter una letra
C
C              ```
C              c23456789
C                 12 REAL*8 FUNCTION Fib
C              ```
C     ------------------------------------------------------------------
      PROGRAM TPK
      REAL A(0:10)
      READ (5,*) A
      DO 10 I=10,0,-1
      Y=FUN(A(I))
      IF(Y .LT. 400) THEN
      WRITE (6, *) I, Y
      ELSE
 5    FORMAT(I10, ' TOO LARGE')
      WRITE (6,5)
      ENDIF
 10   CONTINUE
      END

      REAL FUNCTION FUN(T)
      REAL T
      FUN = SQRT(ABS(T)) + 5.0*T**3
      END
