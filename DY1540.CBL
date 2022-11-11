       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1540.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCRD        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CRD
                       FILE          STATUS  IS  FST-CRD.

           SELECT      CADFAT        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-FAT
                       FILE          STATUS  IS  FST-FAT.

           SELECT      CADNTA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTA
                       FILE          STATUS  IS  FST-NTA.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCRD.CPY.
       COPY  CADFAT.CPY.
       COPY  CADNTA.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-CRD          PIC  X(02).
           03  FST-FAT          PIC  X(02).
           03  FST-NTA          PIC  X(02).

           03  PRX-AUX          PIC  9(01).
           03  LIN-AUX          PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  ACM-QTD          PIC  9(04).
           03  ACM-VAL          PIC  9(07)V99.
           03  TOT-VAL          PIC  9(08)V99.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  47.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1540".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 05 COLUMN 09 VALUE " Administra‡Æo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 06 COLUMN 11 VALUE " Controle de Clientes ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 05 COLUMN 31 VALUE "ß".
               05  LINE 06 COLUMN 33 VALUE "ß".
               05  LINE 06 COLUMN 10 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 07 COLUMN 09 VALUE " É                        
      -            "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ» ".
               05  LINE 08 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 09 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 10 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 11 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 12 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 13 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 14 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 15 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 16 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 17 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 18 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 19 COLUMN 09 VALUE " º
      -            "                                 º ".
               05  LINE 20 COLUMN 09 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -            "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 13 VALUE " Posi‡Æo do Movimento ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 08 COLUMN 72 VALUE  "  ".
               05  LINE 09 COLUMN 72 VALUE  "  ".
               05  LINE 10 COLUMN 72 VALUE  "  ".
               05  LINE 11 COLUMN 72 VALUE  "  ".
               05  LINE 12 COLUMN 72 VALUE  "  ".
               05  LINE 13 COLUMN 72 VALUE  "  ".
               05  LINE 14 COLUMN 72 VALUE  "  ".
               05  LINE 15 COLUMN 72 VALUE  "  ".
               05  LINE 16 COLUMN 72 VALUE  "  ".
               05  LINE 17 COLUMN 72 VALUE  "  ".
               05  LINE 18 COLUMN 72 VALUE  "  ".
               05  LINE 19 COLUMN 72 VALUE  "  ".
               05  LINE 20 COLUMN 72 VALUE  "  ".
               05  LINE 21 COLUMN 11 PIC X(63) FROM SPACES.

           03  FOR-CLR  07.
               05  LINE 08 COLUMN 13 PIC X(54) FROM ALL "ß" BCK-CLR 02.
               05  LINE 16 COLUMN 13 PIC X(01) FROM     "Ü" BCK-CLR 02.
               05  LINE 17 COLUMN 13 PIC X(54) FROM ALL "ß" BCK-CLR 01.
               05  LINE 19 COLUMN 13 PIC X(01) FROM     "Ü" BCK-CLR 01.

           03  FOR-CLR 15.
               05  BCK-CLR 02 LINE 09 COLUMN 13 PIC X(54) FROM
                              "  REQUISI€åES · FATURAR ÄÄÄ        R$".
               05  BCK-CLR 02 LINE 10 COLUMN 13 PIC X(54) FROM
                              "  FATURAS PENDENTES ÄÄÄÄÄÄÄ        R$".
               05  BCK-CLR 02 LINE 11 COLUMN 13 PIC X(54) FROM
                              "  CHEQUES PR-DATADOS ÄÄÄÄÄ        R$".
               05  BCK-CLR 02 LINE 12 COLUMN 13 PIC X(54) FROM
                              "  AMERICAN EXPRESS CARD ÄÄÄ        R$".
               05  BCK-CLR 02 LINE 13 COLUMN 13 PIC X(54) FROM
                              "  MASTERCARD E DINER'S ÄÄÄÄ        R$".
               05  BCK-CLR 02 LINE 14 COLUMN 13 PIC X(54) FROM
                              "  CARTÇO VISA E OUTROS ÄÄÄÄ        R$".
               05  BCK-CLR 02 LINE 15 COLUMN 13 PIC X(54) FROM
                              "  VALES FRETES DIVERSOS ÄÄÄ        R$".
               05  BCK-CLR 01 LINE 18 COLUMN 13 PIC X(54) FROM
                              "  TOTAL DE CRDITOS PENDENTES ..... R$".
           03  BCK-CLR  00.
               05  LINE 16 COLUMN 14 PIC X(53) FROM ALL "ß" FOR-CLR 02.
               05  LINE 19 COLUMN 14 PIC X(53) FROM ALL "ß" FOR-CLR 01.
               05  LINE 09 COLUMN 67 PIC X(01) FROM " ".
               05  LINE 10 COLUMN 67 PIC X(01) FROM " ".
               05  LINE 11 COLUMN 67 PIC X(01) FROM " ".
               05  LINE 12 COLUMN 67 PIC X(01) FROM " ".
               05  LINE 13 COLUMN 67 PIC X(01) FROM " ".
               05  LINE 14 COLUMN 67 PIC X(01) FROM " ".
               05  LINE 15 COLUMN 67 PIC X(01) FROM " ".
               05  LINE 16 COLUMN 67 PIC X(01) FROM " ".
               05  LINE 18 COLUMN 67 PIC X(01) FROM " ".
               05  LINE 19 COLUMN 67 PIC X(01) FROM " ".

       01  ENTR-01.
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE LIN-AUX COLUMN 42 PIC Z.ZZ9        FROM ACM-QTD.
               05  LINE LIN-AUX COLUMN 53 PIC Z.ZZZ.ZZ9,99 FROM ACM-VAL.
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 18      COLUMN 53 PIC Z.ZZZ.ZZ9,99 FROM TOT-VAL.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           OPEN     INPUT    CADCRD   CADFAT   CADNTA.

       ROT-ZERA-00.
           MOVE     09       TO  LIN-AUX
           MOVE     SPACES   TO  CHV-NTA  CHV-FAT
           MOVE     ZEROS    TO  ACM-QTD
                                 ACM-VAL  TOT-VAL.
       ROT-SOMA-00.
           START    CADNTA       KEY  >   CHV-NTA
           IF  FST-NTA   =  "23" GO       ROT-SOMA-10.
           READ     CADNTA  NEXT
           ADD      1        TO  ACM-QTD
           ADD      VAL-NTA  TO  ACM-VAL  TOT-VAL
           DISPLAY  ENTR-01      GO       ROT-SOMA-00.

       ROT-SOMA-10.
           DISPLAY  ENTR-01      ADD   1  TO  LIN-AUX
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL.

       ROT-SOMA-20.
           START    CADFAT       KEY  >   CHV-FAT
           IF  FST-FAT   =  "23" GO       ROT-SOMA-30.
           READ     CADFAT  NEXT
           ADD      1        TO  ACM-QTD
           ADD      VAL-FAT  TO  ACM-VAL  TOT-VAL
           DISPLAY  ENTR-01      GO       ROT-SOMA-20.

       ROT-SOMA-30.
           DISPLAY  ENTR-01      ADD   1  TO  LIN-AUX
           MOVE     ZEROS    TO  CHV-CRD
                                 ACM-QTD  ACM-VAL
           IF  LIN-AUX   >   15  GO       ROT-EXIT-00.
           COMPUTE  PRX-CRD   =  LIN-AUX  -   9
           MOVE     PRX-CRD  TO  PRX-AUX.

       ROT-SOMA-40.
           START    CADCRD       KEY  >   CHV-CRD
           IF  FST-CRD   =  "23" GO       ROT-SOMA-30.
           READ     CADCRD  NEXT
           IF  PRX-CRD   >       PRX-AUX
                                 GO       ROT-SOMA-30.
           ADD      1        TO  ACM-QTD
           ADD      VAL-CRD  TO  ACM-VAL  TOT-VAL
           DISPLAY  ENTR-01      GO       ROT-SOMA-40.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-EXIT-00.
           MOVE 50  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  GO       ROT-HELP-00.
           IF  TCL-MSG   >   00  OR
               HEX-MSG   =   13  GO       ROT-EXIT-00.

       ROT-EXIT-10.
           CLOSE    CADCRD   CADFAT   CADNTA   GOBACK.
