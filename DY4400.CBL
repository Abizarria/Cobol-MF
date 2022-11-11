       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY4400.
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
           03  IND1             PIC  9(02).
           03  IND2             PIC  9(01).
           03  IND3             PIC  9(02).
           03  IND4             PIC  9(02).
           03  IND5             PIC  9(02).
           03  IND6             PIC  9(01).
           03  IND7             PIC  9(02).
           03  IND8             PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  FST-OPR          PIC  X(02).

           03  FILLER           VALUE  "55526525565256262452".
               05  QTD-TAB      PIC  9(01)  OCCURS  20.

           03  TB-FLAG.
               05  M-0000  PIC  X(12)  VALUE  "995499999900".
               05  M-1000  PIC  X(12)  VALUE  "999999999900".
               05  M-1100  PIC  X(12)  VALUE  "019904059900".
               05  M-1120  PIC  X(12)  VALUE  "020300000000".
               05  M-1150  PIC  X(12)  VALUE  "060708091011".
               05  M-1200  PIC  X(12)  VALUE  "129915169900".
               05  M-1220  PIC  X(12)  VALUE  "131400000000".
               05  M-1250  PIC  X(12)  VALUE  "171819202100".
               05  M-1300  PIC  X(12)  VALUE  "222324259900".
               05  M-1350  PIC  X(12)  VALUE  "262728293031".
               05  M-1400  PIC  X(12)  VALUE  "323334359900".
               05  M-1450  PIC  X(12)  VALUE  "363700000000".
               05  M-1500  PIC  X(12)  VALUE  "389999479900".
               05  M-1520  PIC  X(12)  VALUE  "394041424344".
               05  M-1530  PIC  X(12)  VALUE  "454600000000".
               05  M-1550  PIC  X(12)  VALUE  "484950515253".
               05  M-3000  PIC  X(12)  VALUE  "559900000000".
               05  M-3200  PIC  X(12)  VALUE  "565758590000".
               05  M-4000  PIC  X(12)  VALUE  "606162636400".
               05  M-5000  PIC  X(12)  VALUE  "656600000000".
           03  RD-FLAG   REDEFINES   TB-FLAG.
               05  TELA-TB           OCCURS  20.
                   07  PROG-TB       OCCURS  06.
                       09  FLAG-TB   PIC  9(02).

       COPY  TBMENU.CPY.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "4400".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  63.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-SCR              OCCURS  05.
           03  FLG-SCR          PIC  X(0001).
           03  P01-SCR          PIC  X(0001).
           03  P02-SCR          PIC S9(0004).
           03  POS-SCR.
               05  LIN-SCR      PIC  9(0002)  COMP-X.
               05  COL-SCR      PIC  9(0002)  COMP-X.
           03  P03-SCR          PIC  X(2000).
           03  P04-SCR          PIC  X(2000).
           03  P05-SCR          PIC  9(0004)  COMP-X.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 06 COLUMN 09 VALUE " Manutenฦo de Senhas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 06 COLUMN 31 VALUE "฿".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 07 COLUMN 09 VALUE " ษ                        
      -            "อออออออออออออออออออออออออออออออออป ".
               05  LINE 08 COLUMN 09 VALUE " บ
      -            "                                 บ ".
               05  LINE 09 COLUMN 09 VALUE " บ
      -            "                                 บ ".
               05  LINE 10 COLUMN 09 VALUE " บ
      -            "                                 บ ".
               05  LINE 11 COLUMN 09 VALUE " บ
      -            "                                 บ ".
               05  LINE 12 COLUMN 09 VALUE " บ
      -            "                                 บ ".
               05  LINE 13 COLUMN 09 VALUE " บ
      -            "                                 บ ".
               05  LINE 14 COLUMN 09 VALUE " บ
      -            "                                 บ ".
               05  LINE 15 COLUMN 09 VALUE " บ
      -            "                                 บ ".
               05  LINE 16 COLUMN 09 VALUE " บ
      -            "                                 บ ".
               05  LINE 17 COLUMN 09 VALUE " บ
      -            "                                 บ ".
               05  LINE 18 COLUMN 09 VALUE " ศออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 07 COLUMN 13 VALUE " Alteraฦo dos Nกveis ".
           03  BCK-CLR  00.
               05  LINE 08 COLUMN 72 VALUE "  ".
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
               05  LINE 19 COLUMN 11 PIC X(63) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 08 COLUMN 13 PIC X(24) FROM ALL "฿".
               05  LINE 10 COLUMN 13 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 09 COLUMN 13 PIC X(24) FROM "  Operador:".
           03  BCK-CLR 02 FOR-CLR 00.
               05  LINE 09 COLUMN 37 VALUE "Û".
               05  LINE 10 COLUMN 37 VALUE "Û".
               05  LINE 10 COLUMN 14 PIC X(23) FROM ALL "Ü".

       01  OPR-E02 LINE 09 COLUMN 25 PIC X(10) USING CHV-OPR.
       01  OPR-T02 BCK-CLR 02 FOR-CLR 15
                   LINE 09 COLUMN 25 PIC X(10) USING CHV-OPR.
       01  TELA-03.
           03  BCK-CLR 03.
               05  LINE 09 COLUMN 40 PIC X(27) FROM SPACES.
               05  LINE 10 COLUMN 40 PIC X(27) FROM SPACES.
               05  LINE 11 COLUMN 40 PIC X(27) FROM SPACES.
               05  LINE 12 COLUMN 40 PIC X(27) FROM SPACES.
               05  LINE 13 COLUMN 40 PIC X(27) FROM SPACES.
               05  LINE 14 COLUMN 40 PIC X(27) FROM SPACES.
               05  LINE 15 COLUMN 40 PIC X(27) FROM SPACES.
               05  LINE 16 COLUMN 40 PIC X(27) FROM SPACES.
           03  BCK-CLR 00 FOR-CLR 07.
               05  LINE 09 COLUMN 67 VALUE "฿".
               05  LINE 10 COLUMN 67 VALUE " ".
               05  LINE 11 COLUMN 67 VALUE " ".
               05  LINE 12 COLUMN 67 VALUE " ".
               05  LINE 13 COLUMN 67 VALUE " ".
               05  LINE 14 COLUMN 67 VALUE " ".
               05  LINE 15 COLUMN 67 VALUE " ".
               05  LINE 16 COLUMN 67 VALUE " ".
               05  LINE 17 COLUMN 41 PIC X(27) FROM ALL "Ü".

       01  ENTR-03  BCK-CLR 03 FOR-CLR 15.
           03  LINE 10 COLUMN 41 PIC X(25) FROM LINE-TB (IND1, 1).
           03  LINE 11 COLUMN 41 PIC X(25) FROM LINE-TB (IND1, 2).
           03  LINE 12 COLUMN 41 PIC X(25) FROM LINE-TB (IND1, 3).
           03  LINE 13 COLUMN 41 PIC X(25) FROM LINE-TB (IND1, 4).
           03  LINE 14 COLUMN 41 PIC X(25) FROM LINE-TB (IND1, 5).
           03  LINE 15 COLUMN 41 PIC X(25) FROM LINE-TB (IND1, 6).

       01  TELA-04.
           03  BCK-CLR IND8 FOR-CLR 15.
               05  LINE IND4 COLUMN 13 PIC X(01) FROM SPACES.
               05  LINE IND4 COLUMN 14 PIC X(23) FROM DC-MENU
                   (IND1, IND2).
           03  BCK-CLR  07  FOR-CLR 00.
               05  LINE IND4 COLUMN 37 VALUE "Ü".
               05  LINE PLUS COLUMN 14 PIC X(24) FROM ALL "฿".

       01  SETA-01  BCK-CLR 1 FOR-CLR 15
              LINE  IND5 COLUMN 41 PIC X(25) FROM LINE-TB (IND1, IND2).
       01  SETA-02  BCK-CLR 3 FOR-CLR 15
              LINE  IND5 COLUMN 41 PIC X(25) FROM LINE-TB (IND1, IND2).

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING   PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK. DISPLAY  TELA-01
           OPEN     I-O      CADOPR  DISPLAY  TELA-02.

       ROT-OPER-00.
           MOVE     SPACES   TO  CHV-OPR
           ACCEPT   OPR-E02      DISPLAY  OPR-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-OPER-00.
           IF  CHV-OPR       =   SPACES
                                 GO       ROT-OPER-00.
           READ     CADOPR
           IF  FST-OPR   =  "23" MOVE 23  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-OPER-00.
           IF  FST-OPR   =  "9D" MOVE 20  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-OPER-00.
           MOVE    "G"       TO           FLG-SCR  (1)
           CALL    "DYTELA"      USING    PRM-SCR  (1)
           DISPLAY  TELA-03
           MOVE     1        TO  IND-MSG  IND1   IND2.

       ROT-FLAG-00.
           MOVE      FLAG-TB (IND1, IND2)  TO  IND3
           IF  IND3   =   00  MOVE   "  "  TO  FG-MENU (IND1, IND2).
           IF  IND3   =   99  MOVE  X"1A"  TO  FG-MENU (IND1, IND2).
           IF  IND3   >   00   AND  <  99
               MOVE  PRG-OPR (IND3)        TO  FG-MENU (IND1, IND2).
           ADD    1       TO  IND2
           IF  IND2   <   07  GO  ROT-FLAG-00.
           ADD    1       TO  IND1
           MOVE   1       TO  IND2
           IF  IND1   <   21  GO  ROT-FLAG-00.
           MOVE   1       TO  IND1.

       ROT-MENU-00.
           DISPLAY  ENTR-03.

       ROT-MENU-10.
           COMPUTE  IND5   =  IND2  +  09    DISPLAY    SETA-01.

       ROT-MENU-20.
           PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01   PERFORM  ROT-HELP-00
                                  GO       ROT-MENU-20.
           IF  TCL-MSG   =   05   PERFORM  ROT-MENU-30
                                  GO       ROT-MENU-10.
           IF  TCL-MSG   =   06   PERFORM  ROT-MENU-40
                                  GO       ROT-MENU-10.
           IF  TCL-MSG   >   00   GO       ROT-MENU-20.
           IF  HEX-MSG   =   27   GO       ROT-MENU-60.
           EVALUATE  IND1
               WHEN   01      MOVE  02  TO IND6
                              MOVE  04  TO IND8
                              MOVE  12  TO IND4
               EVALUATE  IND2
                   WHEN   1   MOVE  02  TO IND7
                   WHEN   2   GO    ROT-MENU-50
                   WHEN   3   MOVE  17  TO IND7
                   WHEN   4   MOVE  19  TO IND7
                   WHEN   5   MOVE  20  TO IND7
               END-EVALUATE
               WHEN   02      MOVE  03  TO IND6
                              MOVE  01  TO IND8
                              MOVE  14  TO IND4
               EVALUATE  IND2
                   WHEN   1   MOVE  03  TO IND7
                   WHEN   2   MOVE  06  TO IND7
                   WHEN   3   MOVE  09  TO IND7
                   WHEN   4   MOVE  11  TO IND7
                   WHEN   5   MOVE  13  TO IND7
               END-EVALUATE
               WHEN   03      MOVE  04  TO IND6
                              MOVE  05  TO IND8
                              MOVE  16  TO IND4
               EVALUATE  IND2
                   WHEN   2   MOVE  04  TO IND7
                   WHEN   5   MOVE  05  TO IND7
                   WHEN  OTHER  GO  ROT-MENU-50
               END-EVALUATE
               WHEN   06      MOVE  04  TO IND6
                              MOVE  05  TO IND8
                              MOVE  16  TO IND4
               EVALUATE  IND2
                   WHEN   2   MOVE  07  TO IND7
                   WHEN   5   MOVE  08  TO IND7
                   WHEN  OTHER  GO  ROT-MENU-50
               END-EVALUATE
               WHEN   09      MOVE  04  TO IND6
                              MOVE  05  TO IND8
                              MOVE  16  TO IND4
               EVALUATE  IND2
                   WHEN   5   MOVE  10  TO IND7
                   WHEN  OTHER  GO  ROT-MENU-50
               END-EVALUATE
               WHEN   11      MOVE  04  TO IND6
                              MOVE  05  TO IND8
                              MOVE  16  TO IND4
               EVALUATE  IND2
                   WHEN   5   MOVE  12  TO IND7
                   WHEN  OTHER  GO  ROT-MENU-50
               END-EVALUATE
               WHEN   13      MOVE  04  TO IND6
                              MOVE  05  TO IND8
                              MOVE  16  TO IND4
               EVALUATE  IND2
                   WHEN   1   GO    ROT-MENU-50
                   WHEN   2   MOVE  14  TO IND7
                   WHEN   3   MOVE  15  TO IND7
                   WHEN   4   GO    ROT-MENU-50
                   WHEN   5   MOVE  16  TO IND7
               END-EVALUATE
               WHEN   17      MOVE  03  TO IND6
                              MOVE  01  TO IND8
                              MOVE  14  TO IND4
               EVALUATE  IND2
                   WHEN   1   GO    ROT-MENU-50
                   WHEN   2   MOVE  18  TO IND7
               END-EVALUATE
               WHEN   OTHER   GO    ROT-MENU-50
           END-EVALUATE
           MOVE    "G"        TO    FLG-SCR (IND6)
           CALL    "DYTELA"  USING  PRM-SCR (IND6)
           DISPLAY  TELA-04
           MOVE     IND7      TO    IND1
           MOVE     1         TO    IND2    GO   ROT-MENU-00.

       ROT-MENU-30.
           DISPLAY  SETA-02
           IF  IND2  >  1  COMPUTE  IND2  =  IND2   -   1
               ELSE        MOVE     QTD-TAB (IND1)  TO  IND2.

       ROT-MENU-40.
           DISPLAY  SETA-02
           IF  IND2  =  QTD-TAB (IND1)   MOVE   01  TO  IND2
               ELSE                      ADD    01  TO  IND2.

       ROT-MENU-50.
           MOVE      FLAG-TB (IND1, IND2)  TO  IND3
           EVALUATE  FG-MENU (IND1, IND2)
               WHEN  "N"      MOVE  "S"    TO  PRG-OPR (IND3)
                                               FG-MENU (IND1, IND2)
               WHEN  "S"      MOVE  "N"    TO  PRG-OPR (IND3)
                                               FG-MENU (IND1, IND2)
           END-EVALUATE
           DISPLAY   SETA-01  GO  ROT-MENU-20.

       ROT-MENU-60.
           EVALUATE  IND1
               WHEN    01    MOVE      "R"      TO    FLG-SCR (1)
                             CALL     "DYTELA"  USING PRM-SCR (1)
                             REWRITE   REG-OPR  GO    ROT-OPER-00
               WHEN    02    MOVE  01  TO IND1  MOVE  2  TO  IND6
                                                MOVE  1  TO  IND2
               WHEN    03    MOVE  02  TO IND1  MOVE  3  TO  IND6
                                                MOVE  1  TO  IND2
               WHEN    04    MOVE  03  TO IND1  MOVE  4  TO  IND6
                                                MOVE  2  TO  IND2
               WHEN    05    MOVE  03  TO IND1  MOVE  4  TO  IND6
                                                MOVE  5  TO  IND2
               WHEN    06    MOVE  02  TO IND1  MOVE  3  TO  IND6
                                                MOVE  2  TO  IND2
               WHEN    07    MOVE  06  TO IND1  MOVE  4  TO  IND6
                                                MOVE  2  TO  IND2
               WHEN    08    MOVE  06  TO IND1  MOVE  4  TO  IND6
                                                MOVE  5  TO  IND2
               WHEN    09    MOVE  02  TO IND1  MOVE  3  TO  IND6
                                                MOVE  3  TO  IND2
               WHEN    10    MOVE  09  TO IND1  MOVE  4  TO  IND6
                                                MOVE  5  TO  IND2
               WHEN    11    MOVE  02  TO IND1  MOVE  3  TO  IND6
                                                MOVE  4  TO  IND2
               WHEN    12    MOVE  11  TO IND1  MOVE  4  TO  IND6
                                                MOVE  5  TO  IND2
               WHEN    13    MOVE  02  TO IND1  MOVE  3  TO  IND6
                                                MOVE  5  TO  IND2
               WHEN    14    MOVE  13  TO IND1  MOVE  4  TO  IND6
                                                MOVE  2  TO  IND2
               WHEN    15    MOVE  13  TO IND1  MOVE  4  TO  IND6
                                                MOVE  3  TO  IND2
               WHEN    16    MOVE  13  TO IND1  MOVE  4  TO  IND6
                                                MOVE  5  TO  IND2
               WHEN    17    MOVE  01  TO IND1  MOVE  2  TO  IND6
                                                MOVE  3  TO  IND2
               WHEN    18    MOVE  17  TO IND1  MOVE  3  TO  IND6
                                                MOVE  2  TO  IND2
               WHEN    19    MOVE  01  TO IND1  MOVE  2  TO  IND6
                                                MOVE  4  TO  IND2
               WHEN    20    MOVE  01  TO IND1  MOVE  2  TO  IND6
                                                MOVE  5  TO  IND2
           END-EVALUATE
           MOVE  "R"         TO   FLG-SCR (IND6)
           CALL  "DYTELA"  USING  PRM-SCR (IND6)  GO  ROT-MENU-00.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-EXIT-00.
           CLOSE    CADOPR  GOBACK.
