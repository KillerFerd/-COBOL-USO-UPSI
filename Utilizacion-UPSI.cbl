      ******************************************************************
      * FECHA       : 14/03/2022                                       *
      * PROGRAMADOR : JOSUE FERNANDO DONIS ALVARADO                    *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDUI3097                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : USO UPSI PARA NOMBRES DIAS DE LA SEMANA          *
      * ARCHIVOS    : SIN ARCHIVOS                                     *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EDUI3097.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           UPSI-0 IS SW-0 ON  STATUS IS ENGLISH
                         OFF STATUS IS SPANISH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *VARIABLES DE REGISTRO*
       01 REGISTRO.
           02 FECHA-INGRESADA.
              04 ANIO-INGRESADO             PIC 9(04).
              04 MES-INGRESADO              PIC 9(02).
              04 DIA-INGRESADO              PIC 9(02).
           02 FECHA-INGRESADA-NUM REDEFINES FECHA-INGRESADA
                                            PIC 9(08).
           02 FILLER                        PIC X(12).
       01 REDEFINES REGISTRO.
           02                               PIC X(03).
              88 FIN-FECHAS                           VALUE 'FIN'.
           02                               PIC X(17).

      *VARIABLES DE TRABAJO*
       01 CAMPOS-DE-TRABAJO.
          02 CONTADORES.
              04 CUENTA-FECHAS              PIC 9(02) VALUE  0.
              04 CUENTA-PROCESOS            PIC 9(02) VALUE  0.
                 88 LLEGO-AL-LIMITE                   VALUE  11.

          02 FORMATO                        PIC 99/99/9999.
          02 FECHA-GUARDADA                 PIC 9(08) OCCURS 10.
          02 FECHA-CONSTANTE                PIC 9(08) VALUE  19900101.

      *VARIABLES PARA NUMERAR LA FECHA*
          02 NUM-FECHA.
              04 NUM-DIA                    PIC 9(02) OCCURS 10.
              04 NUM-MES                    PIC 9(02) OCCURS 10.
              04 NUM-ANIO                   PIC 9(04) OCCURS 10.
      *VARIABLES PARA NOMBRAR LOS DIAS Y MESES*
          02 NOM-FECHA.
              04 NUM-DIA-SEM                PIC 9(02).
              04 NOM-DIA                    PIC X(10) OCCURS 10.
              04 NOM-MES                    PIC X(10) OCCURS 10.

      *VARIABLES PARA OBTENER DIFERENCIA DE FECHAS*
          02 CALCULAR-DIF.
              04 DI-DIA                     PIC 9(04) OCCURS 10.
              04 ULT-DIA-MES                PIC 9(02) OCCURS 10.

      *VARIABLE PARA CONTENER LA FECHA FINAL*
          02 FECHA-FINAL.
              04 F-DIA                      PIC 9(02)/.
              04 F-MES                      PIC 9(02)/.
              04 F-ANIO                     PIC 9(04)/.
      *VARIABLES PARA GUARDAR EL ULTIMO DÍA DEL MES*
          02 ULTIMO-DIA-MES.
              04 ENERO                      PIC 9(02) VALUE 31.
              04 FEBRERO.
                   05 FEBRERO-A             PIC 9(02) VALUE 28.
                   05 FEBRERO-B             PIC 9(02) VALUE 29.
              04 MARZO                      PIC 9(02) VALUE 31.
              04 ABRIL                      PIC 9(02) VALUE 30.
              04 MAYO                       PIC 9(02) VALUE 31.
              04 JUNIO                      PIC 9(02) VALUE 30.
              04 JULIO                      PIC 9(02) VALUE 31.
              04 AGOSTO                     PIC 9(02) VALUE 31.
              04 SEPTIEMBRE                 PIC 9(02) VALUE 30.
              04 OCTUBRE                    PIC 9(02) VALUE 31.
              04 NOVIEMBRE                  PIC 9(02) VALUE 30.
              04 DICIEMBRE                  PIC 9(02) VALUE 31.
       PROCEDURE DIVISION.

      *SECCION PRINCIPAL*
       PRINCIPAL SECTION.
           PERFORM INGRESAR-FECHAS
           DISPLAY "----->" CUENTA-FECHAS
           PERFORM PROCESAR-FECHAS
           DISPLAY "----->" CUENTA-PROCESOS
           STOP RUN.


      *SECCION INGRESAR FECHAS*
       INGRESAR-FECHAS SECTION.
           MOVE 0 TO CUENTA-FECHAS
           PERFORM UNTIL FIN-FECHAS
              ACCEPT REGISTRO FROM SYSIN
              IF REGISTRO NOT EQUAL 'FIN'
              ADD 1 TO CUENTA-FECHAS
              MOVE FECHA-INGRESADA-NUM TO FECHA-GUARDADA(CUENTA-FECHAS)
              MOVE DIA-INGRESADO       TO NUM-DIA(CUENTA-FECHAS)
              MOVE MES-INGRESADO       TO NUM-MES(CUENTA-FECHAS)
              MOVE ANIO-INGRESADO      TO NUM-ANIO(CUENTA-FECHAS)
              END-IF
           END-PERFORM.
       INGRESAR-FECHAS-E. EXIT.


      *SECCION PROCESAR FECHAS*
       PROCESAR-FECHAS SECTION.
           MOVE 1 TO CUENTA-PROCESOS
           COMPUTE FECHA-CONSTANTE =
                   FUNCTION INTEGER-OF-DATE(FECHA-CONSTANTE)
           PERFORM UNTIL CUENTA-PROCESOS GREATER CUENTA-FECHAS
                                         OR LLEGO-AL-LIMITE

           DISPLAY "--------------------------------------------------"
           DISPLAY "FECHA " CUENTA-PROCESOS ": "
                   PERFORM ENCONTRAR-DIA-SEMANA
                   PERFORM ENCONTRAR-DIF-DIA
                   PERFORM ENCONTRAR-MES-ANIO
                   PERFORM DESPLEGAR-MENSAJE

                   ADD 1 TO CUENTA-PROCESOS
           END-PERFORM.
       PROCESAR-FECHAS-E. EXIT.


      *SECCION ENCONTRAR DIA SEMANA*
       ENCONTRAR-DIA-SEMANA SECTION.
           COMPUTE NUM-DIA-SEM = FUNCTION MOD
                   (FUNCTION INTEGER-OF-DATE
                   (FECHA-GUARDADA(CUENTA-PROCESOS)) 7)
           EVALUATE NUM-DIA-SEM

                    WHEN 0
                    IF SPANISH
                        MOVE 'DOMINGO'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH
                        MOVE 'SUNDAY'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF

                    WHEN 1
                    IF SPANISH
                        MOVE 'LUNES'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH
                        MOVE 'MONDAY'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF

                    WHEN 2
                    IF SPANISH
                        MOVE 'MARTES'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH
                        MOVE 'TUESDAY'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF

                    WHEN 3
                    IF SPANISH
                        MOVE 'MIERCOLES'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH
                        MOVE 'WEDNESDAY'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF

                    WHEN 4
                    IF SPANISH
                        MOVE 'JUEVES'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH
                        MOVE 'THURSDAY'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF

                    WHEN 5
                    IF SPANISH
                        MOVE 'VIERNES'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH
                        MOVE 'FRIDAY'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF

                    WHEN 6
                    IF SPANISH
                        MOVE 'SABADO'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH
                        MOVE 'SATURDAY'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF

                    WHEN 7
                    IF SPANISH
                        MOVE 'DOMINGO'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH
                        MOVE 'SUNDAY'
                        TO NOM-DIA(CUENTA-PROCESOS)
                    END-IF

           END-EVALUATE.
       ENCONTRAR-DIA-SEMANA-E. EXIT.


      *SECCION ENCONTRAR MES DEL ANIO*
       ENCONTRAR-MES-ANIO SECTION.
           EVALUATE NUM-MES(CUENTA-PROCESOS)

                    WHEN 1
                    IF SPANISH MOVE 'ENERO'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH MOVE 'JANUARY'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF

                    MOVE ENERO TO ULT-DIA-MES(CUENTA-PROCESOS)

                    WHEN 2
                    IF SPANISH MOVE 'FEBRERO'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH MOVE 'FEBRUARY'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF

                    IF FUNCTION MOD(NUM-ANIO(CUENTA-PROCESOS) 4)
                       EQUAL 0
                       IF FUNCTION MOD(NUM-ANIO(CUENTA-PROCESOS) 100)
                       EQUAL 0
                          IF FUNCTION MOD(NUM-ANIO(CUENTA-PROCESOS) 400)
                             EQUAL 0
                             MOVE FEBRERO-B
                             TO ULT-DIA-MES(CUENTA-PROCESOS)
                          ELSE
                             MOVE FEBRERO-A
                             TO ULT-DIA-MES(CUENTA-PROCESOS)
                          END-IF
                       ELSE
                          MOVE FEBRERO-B
                          TO ULT-DIA-MES(CUENTA-PROCESOS)
                       END-IF
                    ELSE
                       MOVE FEBRERO-A
                       TO ULT-DIA-MES(CUENTA-PROCESOS)
                    END-IF

                    WHEN 3
                    IF SPANISH MOVE 'MARZO'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH MOVE 'MARCH'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    MOVE MARZO TO ULT-DIA-MES(CUENTA-PROCESOS)

                    WHEN 4
                    IF SPANISH MOVE 'ABRIL'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH MOVE 'APRIL'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    MOVE ABRIL TO ULT-DIA-MES(CUENTA-PROCESOS)

                    WHEN 5
                    IF SPANISH MOVE 'MAYO'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH MOVE 'MAY'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    MOVE MAYO TO ULT-DIA-MES(CUENTA-PROCESOS)

                    WHEN 6
                    IF ENGLISH MOVE 'JUNIO'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF SPANISH MOVE 'JUNE'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    MOVE JUNIO TO ULT-DIA-MES(CUENTA-PROCESOS)

                    WHEN 7
                    IF SPANISH MOVE 'JULIO'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH MOVE 'JULY'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    MOVE JULIO TO ULT-DIA-MES(CUENTA-PROCESOS)

                    WHEN 8
                    IF SPANISH MOVE 'AGOSTO'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH MOVE 'AUGUST'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    MOVE AGOSTO TO ULT-DIA-MES(CUENTA-PROCESOS)

                    WHEN 9
                    IF SPANISH MOVE 'SEPTIEMBRE'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH MOVE 'SEPTEMBER'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    MOVE SEPTIEMBRE TO ULT-DIA-MES(CUENTA-PROCESOS)

                    WHEN 10
                    IF SPANISH MOVE 'OCTUBRE'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH MOVE 'OCTOBER'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    MOVE OCTUBRE TO ULT-DIA-MES(CUENTA-PROCESOS)

                    WHEN 11
                    IF SPANISH MOVE 'NOVIEMBRE'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH MOVE 'NOVEMBER'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    MOVE NOVIEMBRE TO ULT-DIA-MES(CUENTA-PROCESOS)

                    WHEN 12
                    IF SPANISH MOVE 'DICIEMBRE'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    IF ENGLISH MOVE 'DECEMBER'
                    TO NOM-MES(CUENTA-PROCESOS)
                    END-IF
                    MOVE DICIEMBRE TO ULT-DIA-MES(CUENTA-PROCESOS)
           END-EVALUATE.
       ENCONTRAR-MES-ANIO-E. EXIT.


      *SECCION ENCONTRAR DIFERENCIA DIA*
       ENCONTRAR-DIF-DIA SECTION.
           COMPUTE DI-DIA(CUENTA-PROCESOS) =
                   FUNCTION INTEGER-OF-DATE
                   (FECHA-GUARDADA(CUENTA-PROCESOS)) -
                   FECHA-CONSTANTE.
       ENCONTRAR-DIF-DIA-E. EXIT.


      *SECCION DESPLEGAR-MENSAJE*
       DESPLEGAR-MENSAJE SECTION.
           MOVE ULT-DIA-MES(CUENTA-PROCESOS) TO F-DIA
           MOVE NUM-MES(CUENTA-PROCESOS) TO F-MES
           MOVE NUM-ANIO(CUENTA-PROCESOS) TO F-ANIO

           MOVE FECHA-FINAL TO FORMATO

           IF SPANISH
           DISPLAY "HACE "
                    DI-DIA(CUENTA-PROCESOS)
                    " DIAS FUE 1 DE ENERO DE 1990, Y HOY ES "
                    NOM-DIA(CUENTA-PROCESOS)
                    " DIA DE "
           DISPLAY  NOM-MES(CUENTA-PROCESOS)
                    " DE "
                    NUM-ANIO(CUENTA-PROCESOS)
                    " Y EL ULTIMO DIA DE ESTE MES SERA "
                    FORMATO
            END-IF
            IF ENGLISH
            DISPLAY DI-DIA(CUENTA-PROCESOS)
                    " DAYS AGO WAS 1 OF JANUARY OF 1990, AN TODAY IS "
                    NOM-DIA(CUENTA-PROCESOS)
                    ", DAY OF "
            DISPLAY NOM-MES(CUENTA-PROCESOS)
                    " OF "
                    NUM-ANIO(CUENTA-PROCESOS)
                    " AND THE LAST DAY OF THIS MOTH WILL BE "
                    FORMATO
            END-IF.
       DESPLEGAR-MENSAJE-E. EXIT.

