       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYOPER.
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
           03  IND1             PIC  9(03).
           03  TECLADO          PIC  9(02).
           03  FST-OPR          PIC  X(02).
           03  COD-AUX          PIC  X(10).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "OPER".
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

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 09 COLUMN 21 VALUE
                   " ษอ                 อออออออออออออออป ".
               05  LINE 10 COLUMN 21 VALUE
                   " บ                                   บ ".
               05  LINE 11 COLUMN 21 VALUE
                   " บ                                   บ ".
               05  LINE 12 COLUMN 21 VALUE
                   " บ                                   บ ".
               05  LINE 13 COLUMN 21 VALUE
                   " บ                                   บ ".
               05  LINE 14 COLUMN 21 VALUE
                   " บ                                   บ ".
               05  LINE 15 COLUMN 21 VALUE
                   " บ                                   บ ".
               05  LINE 16 COLUMN 21 VALUE
                   " ศอออออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 09 COLUMN 26 VALUE " Identificaฦo ".
           03 BCK-CLR 00.
               05  LINE 10 COLUMN 60 VALUE "  ".
               05  LINE 11 COLUMN 60 VALUE "  ".
               05  LINE 12 COLUMN 60 VALUE "  ".
               05  LINE 13 COLUMN 60 VALUE "  ".
               05  LINE 14 COLUMN 60 VALUE "  ".
               05  LINE 15 COLUMN 60 VALUE "  ".
               05  LINE 16 COLUMN 60 VALUE "  ".
               05  LINE 17 COLUMN 23 PIC X(39) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 10 COLUMN 25 PIC X(30) FROM ALL "฿".
               05  LINE 12 COLUMN 25 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 11 COLUMN 25 PIC X(30) FROM "  Nome Operador :".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 11 COLUMN 55 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 55 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 26 PIC X(29) FROM ALL "฿".

       01  ENTR-01 LINE 11 COLUMN 43 PIC X(10) USING CHV-OPR.
       01  ENTR-02 BCK-CLR 02 FOR-CLR 15
                   LINE 11 COLUMN 43 PIC X(10) USING CHV-OPR.

       01  TELA-03.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 13 COLUMN 25 PIC X(30) FROM ALL "฿".
               05  LINE 15 COLUMN 25 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 14 COLUMN 25 PIC X(30) FROM "  Senha Secreta :".
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 14 COLUMN 55 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 55 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 26 PIC X(29) FROM ALL "฿".

       01  ENTR-03  NO-ECHO
                    LINE 14 COLUMN 43 PIC X(10) USING COD-AUX.
       01  ENTR-04  BCK-CLR 05 FOR-CLR 15
                    LINE 14 COLUMN 43 PIC X(10) FROM  ALL "*".

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           OPEN     INPUT        CADOPR
           DISPLAY  TELA-01      TELA-02
           MOVE     SPACES   TO  CHV-OPR
           START    CADOPR       KEY  >   CHV-OPR
           IF  FST-OPR   =  "00" GO       ROT-OPER-00.
           CLOSE    CADOPR       OPEN     I-O  CADOPR.

       ROT-NOVO-00.
           MOVE     SPACES   TO  CHV-OPR  COD-OPR
           ACCEPT   ENTR-01      DISPLAY  ENTR-02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-NOVO-00.
           IF  CHV-OPR   =       SPACES
                                 GO       ROT-NOVO-00.
           MOVE     1        TO  IND1.

       ROT-NOVO-10.
           MOVE     "S"      TO  PRG-OPR (IND1)
           IF  IND1      <  100  ADD      1  TO  IND1
                                 GO       ROT-NOVO-10.
           MOVE     CHV-OPR  TO  PRM-OPR
           WRITE    REG-OPR      GO       ROT-EXIT-00.

       ROT-OPER-00.
           MOVE     SPACES   TO  CHV-OPR
           ACCEPT   ENTR-01      DISPLAY  ENTR-02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-OPER-00.
           IF  CHV-OPR   =       SPACES
                                 GO       ROT-OPER-00.
           READ     CADOPR       MOVE 23  TO  IND-MSG
           IF  FST-OPR   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-OPER-00.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-03.

       ROT-OPER-10.
           MOVE     SPACES   TO  COD-AUX
           ACCEPT   ENTR-03      DISPLAY  ENTR-04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-OPER-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-OPER-10.
           MOVE     25       TO  IND-MSG
           IF  COD-AUX  NOT  =   COD-OPR
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-OPER-10.
           MOVE     CHV-OPR  TO  PRM-OPR
           GO       ROT-EXIT-00.

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
           CLOSE    CADOPR   GOBACK.
