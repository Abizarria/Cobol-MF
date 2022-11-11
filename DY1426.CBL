       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1426.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC
                       FILE          STATUS  IS  FST-LBC.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADLBC.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-LBC          PIC  X(02).

           03  TECLADO          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1426".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       LINKAGE         SECTION.

       01  PRM-LCT.
           03  BCO-LCT          PIC  9(03).
           03  DTA-LCT          PIC  9(06).

       SCREEN          SECTION.

       01  ENTR-01    AUTO.
           03  DOC-E01 LINE 13 COLUMN 60 PIC ZZZZZZ9      USING DOC-LBC.
           03  HST-E01 LINE 15 COLUMN 29 PIC X(25)        USING HST-LBC.
           03  VAL-E01 LINE 17 COLUMN 29 PIC Z.ZZZ.ZZ9,99 USING VAL-LBC.

       01  TELA-01  BCK-CLR 01 FOR-CLR 15.
           03  DOC-T01 LINE 13 COLUMN 60 PIC 9(07)         FROM DOC-LBC.
           03  HST-T01 LINE 15 COLUMN 29 PIC X(25)         FROM HST-LBC.
           03  VAL-T01 LINE 17 COLUMN 29 PIC Z.ZZZ.ZZ9,99  FROM VAL-LBC.

       PROCEDURE       DIVISION  USING  PRM-LCT.

       ROT-0000-00.
           OPEN     I-O          CADLBC
           MOVE     SPACES   TO  HST-LBC
           MOVE     ZEROS    TO  LCT-LBC
                                 DOC-LBC  VAL-LBC.
       ROT-ENTR-00.
           ACCEPT   DOC-E01      DISPLAY  DOC-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-00.
           IF  DOC-LBC   =    0  GO       ROT-ENTR-00.

       ROT-ENTR-10.
           ACCEPT   HST-E01      DISPLAY  HST-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
           IF  HST-LBC   =       SPACES
                                 GO       ROT-ENTR-10.
       ROT-ENTR-20.
           ACCEPT   VAL-E01      DISPLAY  VAL-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-20.
           IF  VAL-LBC   =    0  GO       ROT-ENTR-20.

       ROT-ENTR-30.
           MOVE 10  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-30.
           IF  TCL-MSG   >   00  GO       ROT-ENTR-30.
           IF  HEX-MSG   =   27  GO       ROT-ENTR-20.
           MOVE     BCO-LCT  TO  BCO-LBC
           MOVE     DTA-LCT  TO  DTA-LBC
           MOVE     5        TO  COD-LBC.

       ROT-ENTR-40.
           WRITE    REG-LBC      ADD  2   TO  LCT-LBC
           IF  FST-LBC   =  "22" GO       ROT-ENTR-40.

       ROT-EXIT-00.
           CLOSE    CADLBC   GOBACK.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.
