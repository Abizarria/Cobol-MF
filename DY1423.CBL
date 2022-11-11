       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1423.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCDV        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CDV
                       FILE          STATUS  IS  FST-CDV.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC
                       FILE          STATUS  IS  FST-LBC.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCDV.CPY.
       COPY  CADLBC.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-CDV          PIC  X(02).
           03  FST-LBC          PIC  X(02).

           03  TECLADO          PIC  9(02).
           03  CPF-AUX          PIC  9(11).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1423".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-SCR.
           03  FLG-SCR          PIC  X(01).
           03  P01-SCR          PIC  X(01).
           03  P02-SCR          PIC S9(04).
           03  POS-SCR.
               05  LIN-SCR      PIC  9(02)  COMP-X.
               05  COL-SCR      PIC  9(02)  COMP-X.
           03  P03-SCR          PIC X(2000).
           03  P04-SCR          PIC X(2000).
           03  P05-SCR          PIC  9(04)  COMP-X.

       LINKAGE         SECTION.

       01  PRM-LCT.
           03  BCO-LCT          PIC  9(03).
           03  DTA-LCT          PIC  9(06).

       SCREEN          SECTION.

       01  TELA-01.
           03  BCK-CLR 04 FOR-CLR 01.
               05  LINE 14 COLUMN 18 PIC X(45) FROM ALL "ß".
               05  LINE 17 COLUMN 18 PIC X(01) FROM     "Ü".
           03  BCK-CLR 04 FOR-CLR 14.
               05  LINE 15 COLUMN 18 PIC X(46) FROM "  N£mero CPF ...:".
               05  LINE 16 COLUMN 18 PIC X(46) FROM "  Nome Emitente :".
           03  BCK-CLR 00 FOR-CLR 04.
               05  LINE 15 COLUMN 63 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 63 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 63 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 19 PIC X(44) FROM ALL "ß".

       01  ENTR-01     AUTO.
           03  CPF-E01 LINE 15 COLUMN 36 PIC 999.999.999.99
                                                          USING CPF-AUX.
           03  NOM-E01 LINE 16 COLUMN 36 PIC X(25)        USING NOM-CDV.
           03  DOC-E01 LINE 13 COLUMN 60 PIC ZZZZZ9       USING DOC-LBC.
           03  HST-E01 LINE 15 COLUMN 29 PIC X(25)        USING HST-LBC.
           03  VAL-E01 LINE 17 COLUMN 29 PIC Z.ZZZ.ZZ9,99 USING VAL-LBC.

       01  ENTR-02  BCK-CLR 01 FOR-CLR 15.
           03  DOC-E02 LINE 13 COLUMN 60 PIC 9(06)         FROM DOC-LBC.
           03  HST-E02 LINE 15 COLUMN 29 PIC X(25)         FROM HST-LBC.
           03  VAL-E02 LINE 17 COLUMN 29 PIC Z.ZZZ.ZZ9,99  FROM VAL-LBC.

       01  ENTR-04  BCK-CLR 04 FOR-CLR 15.
           03  CPF-E04 LINE 15 COLUMN 36 PIC 999.999.999.99
                                                           FROM CPF-AUX.
           03  NOM-E04 LINE 16 COLUMN 36 PIC X(25)         FROM NOM-CDV.

       PROCEDURE       DIVISION  USING  PRM-LCT.

       ROT-0000-00.
           OPEN     I-O          CADLBC   CADCDV
           PERFORM  ROT-GTLA-00.

       ROT-ENTR-00.
           DISPLAY  TELA-01      MOVE  0  TO  CPF-AUX.

       ROT-ENTR-10.
           ACCEPT   CPF-E01      DISPLAY  CPF-E04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
           IF  CPF-AUX   =    0  GO       ROT-ENTR-10.
           MOVE     SPACES   TO  CHV-CDV  NOM-CDV
           MOVE     CPF-AUX  TO  CPF-CDV
           START    CADCDV       KEY  >   CHV-CDV
           IF  FST-CDV   =  "23" GO       ROT-ENTR-20.
           READ     CADCDV  NEXT
           IF  CPF-CDV   =       CPF-AUX
                                 GO       ROT-ENTR-20.
           MOVE     SPACES   TO  NOM-CDV.

       ROT-ENTR-20.
           ACCEPT   NOM-E01      DISPLAY  NOM-E04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-20.
           IF  NOM-CDV   =       SPACES
                                 GO       ROT-ENTR-20.
           PERFORM  ROT-RTLA-00
           MOVE     SPACES   TO  HST-LBC
           MOVE     ZEROS    TO  LCT-LBC
                                 DOC-LBC  VAL-CDV.
       ROT-ENTR-30.
           ACCEPT   DOC-E01      DISPLAY  DOC-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  TELA-01
                                          CPF-E04
                                 GO       ROT-ENTR-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-30.
           IF  DOC-LBC   =    0  GO       ROT-ENTR-30.
           MOVE     CPF-AUX  TO  CPF-CDV
           MOVE     DTA-LCT  TO  DTA-CDV
           MOVE     DOC-LBC  TO  NUM-CDV
           READ     CADCDV       MOVE 37  TO  IND-MSG
           IF  FST-CDV   =  "00" PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-30.
       ROT-ENTR-40.
           ACCEPT   HST-E01      DISPLAY  HST-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-40.
           IF  HST-LBC   =       SPACES
                                 GO       ROT-ENTR-40.
       ROT-ENTR-50.
           ACCEPT   VAL-E01      DISPLAY  VAL-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-40.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-50.
           IF  VAL-LBC   =    0  GO       ROT-ENTR-50.

       ROT-ENTR-60.
           MOVE 10  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-60.
           IF  TCL-MSG   >   00  GO       ROT-ENTR-60.
           IF  HEX-MSG   =   27  GO       ROT-ENTR-50.
           MOVE     BCO-LCT  TO  BCO-LBC
           MOVE     DTA-LCT  TO  DTA-LBC
           MOVE     4        TO  COD-LBC
           MOVE     VAL-LBC  TO  VAL-CDV.

       ROT-ENTR-70.
           WRITE    REG-LBC      ADD   2  TO  LCT-LBC
           IF  FST-LBC   =  "22" GO       ROT-ENTR-70.
           WRITE    REG-CDV      GO       ROT-EXIT-00.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-EXIT-00.
           CLOSE    CADCDV   CADLBC  GOBACK.
