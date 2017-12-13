C     ------------------------------------------------------------------
C     Programa:
C           TPK77
C
C     Descripción:
C           Uso de la sentencia GO TO para imprimir los numeros del 0 al 9
C
C     Autor:
C           Martin Vuelta <martin.vuelta@gmail.com>
C
C     ------------------------------------------------------------------
      PROGRAM GOTOER
  1   WRITE(*, *) 0
      GOTO 2
  9   WRITE(*, *) 8
      GOTO 10
  4   WRITE(*, *) 3
      GOTO 5
  5   WRITE(*, *) 4
      GOTO 6
  6   WRITE(*, *) 5
      GOTO 7
  3   WRITE(*, *) 2
      GOTO 4
  8   WRITE(*, *) 7
      GOTO 9
  2   WRITE(*, *) 1
      GOTO 3
  7   WRITE(*, *) 6
      GOTO 8
 10   WRITE(*, *) 9
      END PROGRAM GOTOER
