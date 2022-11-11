       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1440.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADBCO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-BCO
                       FILE          STATUS  IS  FST-BCO.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC
                       FILE          STATUS  IS  FST-LBC.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADBCO.CPY.
       COPY  CADLBC.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-BCO          PIC  X(02).
           03  FST-LBC          PIC  X(02).

           03  IND1             PIC  9(01).
           03  LIN1             PIC  9(02).
           03  TECLADO          PIC  9(02).

           03  TAB-ACM          OCCURS      06.
               05  ACM-QTD      PIC  9(03).
               05  ACM-VAL      PIC S9(08)V99.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  35.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1440".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR  04 FOR-CLR   15.
               05  LINE 06 COLUMN 10 VALUE " Administraฦo Global ".
           03  BCK-CLR  01 FOR-CLR   15.
               05  LINE 07 COLUMN 12 VALUE " Controle C. Banc ria ".
           03  BCK-CLR  00 FOR-CLR   08.
               05  LINE 06 COLUMN 32 VALUE "฿".
               05  LINE 07 COLUMN 34 VALUE "฿".
               05  LINE 07 COLUMN 11 VALUE "Ü".
           03  BCK-CLR 7 FOR-CLR 14.
               05  LINE 08 COLUMN 10 VALUE " ษ                        
      -            "ออออออออออออออออออออออออออออออออป ".
               05  LINE 09 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 10 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 11 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 12 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 13 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 14 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 15 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 16 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 17 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 18 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 19 COLUMN 10 VALUE " ศออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 08 COLUMN 14 VALUE " Posiฦo do Movimento ".
           03  BCK-CLR 00.
               05  LINE 09 COLUMN 72 VALUE "  ".
               05  LINE 10 COLUMN 72 VALUE "  ".
               05  LINE 11 COLUMN 72 VALUE "  ".
               05  LINE 12 COLUMN 72 VALUE "  ".
               05  LINE 13 COLUMN 72 VALUE "  ".
               05  LINE 14 COLUMN 72 VALUE "  ".
               05  LINE 15 COLUMN 72 VALUE "  ".
               05  LINE 16 COLUMN 72 VALUE "  ".
               05  LINE 17 COLUMN 72 VALUE "  ".
               05  LINE 18 COLUMN 72 VALUE "  ".
               05  LINE 19 COLUMN 72 VALUE "  ".
               05  LINE 20 COLUMN 12 PIC X(62) FROM SPACES.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 14 PIC X(53) FROM ALL "฿".
               05  LINE 15 COLUMN 14 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 10 COLUMN 14 VALUE "  SALDO INICIAL ฤฤฤฤฤฤฤฤฤ
      -            " 0  R$          0,00 (+)  ".
               05  LINE 11 COLUMN 14 VALUE "  DEPเSITOS EM CONTA ฤฤฤฤ
      -            " 0  R$          0,00 (+)  ".
               05  LINE 12 COLUMN 14 VALUE "  GANHOS COM CAPITAL ฤฤฤฤ
      -            " 0  R$          0,00 (+)  ".
               05  LINE 13 COLUMN 14 VALUE "  CHEQUES DEVOLVIDOS ฤฤฤฤ
      -            " 0  R$          0,00 (-)  ".
               05  LINE 14 COLUMN 14 VALUE "  CHEQUES EMITIDOS ฤฤฤฤฤฤ
      -            " 0  R$          0,00 (-)  ".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 10 COLUMN 67 VALUE " ".
               05  LINE 11 COLUMN 67 VALUE " ".
               05  LINE 12 COLUMN 67 VALUE " ".
               05  LINE 13 COLUMN 67 VALUE " ".
               05  LINE 14 COLUMN 67 VALUE " ".
               05  LINE 15 COLUMN 67 VALUE " ".
               05  LINE 15 COLUMN 15 PIC X(52) FROM ALL "฿".
           03  BCK-CLR 01 FOR-CLR 07.
               05  LINE 16 COLUMN 14 PIC X(53) FROM ALL "฿".
               05  LINE 18 COLUMN 14 PIC X(01) FROM     "Ü".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 17 COLUMN 14 VALUE "  SALDO FINAL NO PERึODO ...
      -            "... R$         0,00  (=)  ".
           03  BCK-CLR 00 FOR-CLR 01.
               05  LINE 17 COLUMN 67 PIC X(01) FROM     " ".
               05  LINE 18 COLUMN 67 PIC X(01) FROM     " ".
               05  LINE 18 COLUMN 15 PIC X(52) FROM ALL "฿".

       01  ENTR-01 BCK-CLR 02 FOR-CLR 15.
           03  LINE LIN1 COLUMN 41 PIC            ZZ9 FROM
                         ACM-QTD (IND1).
           03  LINE LIN1 COLUMN 48 PIC ---.---.--9,99 FROM
                         ACM-VAL (IND1).

       01  ENTR-02 BCK-CLR 01 FOR-CLR 15.
           03  LINE  17  COLUMN 48 PIC ---.---.--9,99 FROM ACM-VAL (6).

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"      USING    PRM-OPR
                                          PRM-SPL
           IF  LCK-SPL   =    1  GOBACK.
           OPEN     INPUT        CADBCO   CADLBC
           MOVE     1  TO  IND1  DISPLAY  TELA-01.

       ROT-ZERA-00.
           MOVE     ZEROS    TO  ACM-QTD  (IND1)
                                 ACM-VAL  (IND1)
           IF  IND1    <      6  ADD      1  TO  IND1
                                 GO       ROT-ZERA-00.
           MOVE     ZEROS    TO  CHV-BCO.

       ROT-EXBE-00.
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-EXIT-00.
           READ     CADBCO  NEXT
           MOVE     10       TO  LIN1
           MOVE     1        TO  IND1
           ADD      1        TO  ACM-QTD  (IND1)
           ADD      SDO-BCO  TO  ACM-VAL  (IND1)
                                 ACM-VAL  (6)
           DISPLAY  ENTR-01      ENTR-02
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC.

       ROT-EXBE-10.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-EXBE-00.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO
                                 GO       ROT-EXBE-00.
           IF  COD-LBC   =    0  OR   6
                                 GO       ROT-EXBE-10.
           IF  COD-LBC   <    3  MOVE     2  TO  IND1
                                 MOVE     11 TO  LIN1.
           IF  COD-LBC   =    3  MOVE     3  TO  IND1
                                 MOVE     12 TO  LIN1.
           IF  COD-LBC   =    4  MOVE     4  TO  IND1
                                 MOVE     13 TO  LIN1.
           IF  COD-LBC   =    5  MOVE     5  TO  IND1
                                 MOVE     14 TO  LIN1.
           ADD      1        TO  ACM-QTD  (IND1)
           ADD      VAL-LBC  TO  ACM-VAL  (IND1)
           IF  COD-LBC   <    4  COMPUTE  ACM-VAL (6)   =
                                          ACM-VAL (6)   +  VAL-LBC
           ELSE
                                 COMPUTE  ACM-VAL (6)   =
                                          ACM-VAL (6)   -  VAL-LBC.
           DISPLAY  ENTR-01      ENTR-02
           GO       ROT-EXBE-10.

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
           CLOSE    CADBCO   CADLBC   GOBACK.
