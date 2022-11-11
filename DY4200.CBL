       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY4200.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADOPR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-OPR
                       FILE          STATUS  IS  FST-OPR.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADOPR.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-OPR          PIC  X(02).
           03  TECLADO          PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  OPR-AUX          PIC  X(10).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "4200".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  61.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  24.
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

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 08 COLUMN 20 VALUE " Manutenฦo de Senhas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 08 COLUMN 42 VALUE "฿".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 09 COLUMN 20 VALUE
                   " ษ                        ออออออออออออป ".
               05  LINE 10 COLUMN 20 VALUE
                   " บ                                      บ ".
               05  LINE 11 COLUMN 20 VALUE
                   " บ                                      บ ".
               05  LINE 12 COLUMN 20 VALUE
                   " บ                                      บ ".
               05  LINE 13 COLUMN 20 VALUE
                   " บ                                      บ ".
               05  LINE 14 COLUMN 20 VALUE
                   " บ                                      บ ".
               05  LINE 15 COLUMN 20 VALUE
                   " บ                                      บ ".
               05  LINE 16 COLUMN 20 VALUE
                   " ศออออออออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 09 COLUMN 24 VALUE " Inclusฦo de Operador ".
           03 BCK-CLR 00.
               05  LINE 10 COLUMN 62 VALUE "  ".
               05  LINE 11 COLUMN 62 VALUE "  ".
               05  LINE 12 COLUMN 62 VALUE "  ".
               05  LINE 13 COLUMN 62 VALUE "  ".
               05  LINE 14 COLUMN 62 VALUE "  ".
               05  LINE 15 COLUMN 62 VALUE "  ".
               05  LINE 16 COLUMN 62 VALUE "  ".
               05  LINE 17 COLUMN 22 PIC X(42) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 10 COLUMN 24 PIC X(33) FROM ALL "฿".
               05  LINE 12 COLUMN 24 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 11 COLUMN 24 PIC X(33) FROM
                   "  Nome do Operador :".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 11 COLUMN 57 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 57 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 25 PIC X(32) FROM ALL "฿".

       01  ENTR-01 LINE 11 COLUMN 45 PIC X(10) USING CHV-OPR.
       01  ENTR-02 BCK-CLR 02 FOR-CLR 15
                   LINE 11 COLUMN 45 PIC X(10) USING CHV-OPR.

       01  TELA-03.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 13 COLUMN 24 PIC X(33) FROM ALL "฿".
               05  LINE 15 COLUMN 24 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 14 COLUMN 24 PIC X(33) FROM
                   "    Confirma a Inclusฦo ? [ ]".
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 14 COLUMN 57 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 57 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 25 PIC X(32) FROM ALL "฿".

       01  ENTR-03 BCK-CLR 05 FOR-CLR 11
                   LINE 14 COLUMN 51 PIC X(01) USING RSP-OPC AUTO.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-000-000.
           CALL    "DYSPOL"  USING   PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK. DISPLAY  TELA-01
           OPEN     I-O      CADOPR  DISPLAY  TELA-02.

       ROT-OPER-00.
           MOVE     SPACES   TO  CHV-OPR.

       ROT-OPER-10.
           ACCEPT   ENTR-01      DISPLAY  ENTR-02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-OPER-10.
           IF  CHV-OPR   =       SPACES
                                 GO       ROT-OPER-10.
           READ     CADOPR
           IF  FST-OPR   =  "23" GO       ROT-OPER-20.
           PERFORM  ROT-MSGM-00  GO       ROT-OPER-00.


       ROT-OPER-20.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-03.

       ROT-OPER-30.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   ENTR-03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-OPER-40.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-OPER-30.
           IF  RSP-OPC  NOT  =   "S"
                                 GO       ROT-OPER-30.
           MOVE     CHV-OPR  TO  OPR-AUX
           MOVE     PRM-OPR  TO  CHV-OPR
           READ     CADOPR
           MOVE     OPR-AUX  TO  CHV-OPR
           MOVE     "N"      TO  PRG-OPR (081)
           MOVE     SPACES   TO  COD-OPR
           WRITE    REG-OPR.

       ROT-OPER-40.
           PERFORM  ROT-RTLA-00  GO       ROT-OPER-00.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-EXIT-00.
           CLOSE    CADOPR   GOBACK.
