       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYHELP.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADHLP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-HLP
                       FILE          STATUS  IS  FST-HLP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADHLP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  FST-HLP          PIC  X(02).
           03  LIN-AUX          PIC  9(02).
           03  AUX1             PIC  X(02).
           03  AUX2             PIC  X(02).

           03  TAB-REG          PIC  X(35)  OCCURS 100.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  09.
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

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04).
           03  BCK-HLP          PIC  9(01).
           03  FOR-HLP          PIC  9(01).
           03  LET-HLP          PIC  9(01).

       SCREEN          SECTION.

       01  TELA-01.
           03  BCK-CLR BCK-HLP
               FOR-CLR FOR-HLP HIGHLIGHT.
               05  LINE 05 COLUMN 36 VALUE
                   " ษออออออออออออออ       ออออออออออออออป ".
               05  LINE 06 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 07 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 08 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 09 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 10 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 11 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 12 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 13 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 14 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 15 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 16 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 17 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 18 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 19 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 20 COLUMN 36 VALUE
                   " บ                                     บ ".
               05  LINE 21 COLUMN 36 VALUE
                   " ศอออออออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR BCK-HLP
               FOR-CLR LET-HLP HIGHLIGHT.
               05  LINE 05 COLUMN 54 VALUE "Ajuda" .
           03  BCK-CLR  00 FOR-CLR 8.
               05  LINE 06 COLUMN 77 VALUE "  ".
               05  LINE 07 COLUMN 77 VALUE "  ".
               05  LINE 08 COLUMN 77 VALUE "  ".
               05  LINE 09 COLUMN 77 VALUE "  ".
               05  LINE 10 COLUMN 77 VALUE "  ".
               05  LINE 11 COLUMN 77 VALUE "  ".
               05  LINE 12 COLUMN 77 VALUE "  ".
               05  LINE 13 COLUMN 77 VALUE "  ".
               05  LINE 14 COLUMN 77 VALUE "  ".
               05  LINE 15 COLUMN 77 VALUE "  ".
               05  LINE 16 COLUMN 77 VALUE "  ".
               05  LINE 17 COLUMN 77 VALUE "  ".
               05  LINE 18 COLUMN 77 VALUE "  ".
               05  LINE 19 COLUMN 77 VALUE "  ".
               05  LINE 20 COLUMN 77 VALUE "  ".
               05  LINE 21 COLUMN 77 VALUE "  ".
               05  LINE 22 COLUMN 38 PIC X(41) FROM SPACES.

       01  SETA-01 BCK-CLR BCK-HLP.
           03  FOR-CLR FOR-HLP  HIGHLIGHT.
               05  TRC-ACM LINE 06 COLUMN 75 VALUE "บ".
               05  TRC-ABX LINE 20 COLUMN 75 VALUE "บ".
           03  FOR-CLR LET-HLP  HIGHLIGHT.
               05  STA-ACM LINE 06 COLUMN 75 VALUE "".
               05  STA-ABX LINE 20 COLUMN 75 VALUE "".

       01  ENT-001  BCK-CLR BCK-HLP
                    FOR-CLR LET-HLP HIGHLIGHT.
           03  LINE 07 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 01).
           03  LINE 08 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 02).
           03  LINE 09 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 03).
           03  LINE 10 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 04).
           03  LINE 11 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 05).
           03  LINE 12 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 06).
           03  LINE 13 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 07).
           03  LINE 14 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 08).
           03  LINE 15 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 09).
           03  LINE 16 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 10).
           03  LINE 17 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 11).
           03  LINE 18 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 12).
           03  LINE 19 COLUMN 39 PIC X(35) USING TAB-REG (IND1 + 13).

       PROCEDURE       DIVISION  USING  PRM-HLP.

       ROT-0000-00.
           OPEN     INPUT        CADHLP
           MOVE     1        TO  IND1
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-01.

       ROT-0000-10.
           MOVE     SPACES   TO  TAB-REG (IND1)
           ADD      1        TO  IND1
           IF  IND1  <  101      GO       ROT-0000-10.
           MOVE     PRG-HLP  TO  IND-HLP
           MOVE     ZEROS    TO  LIN-HLP  IND1
           START    CADHLP       KEY  >   CHV-HLP.

       ROT-0000-20.
           READ     CADHLP  NEXT
           IF  FST-HLP  =   "10" OR
               IND-HLP  NOT   =  PRG-HLP
                                 GO       ROT-TELA-00.
           MOVE     LIN-HLP  TO  LIN-AUX
           MOVE     DES-HLP  TO  TAB-REG (LIN-HLP)
           GO       ROT-0000-20.

       ROT-TELA-00.
           IF  (IND1 + 13)  <  LIN-AUX  DISPLAY  STA-ABX
               IF  IND1  >  0  DISPLAY  STA-ACM
                   ELSE        DISPLAY  TRC-ACM
           ELSE    IF  IND1    >    0   DISPLAY  STA-ACM  TRC-ABX
                       ELSE    DISPLAY  TRC-ACM. DISPLAY  ENT-001.

       ROT-MOVE-00.
           PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   05  GO       ROT-MOVE-10.
           IF  TCL-MSG   =   06  GO       ROT-MOVE-20.
           IF  TCL-MSG   =   53  GO       ROT-MOVE-30.
           IF  TCL-MSG   =   54  GO       ROT-MOVE-40.
           IF  TCL-MSG   >   00  GO       ROT-MOVE-00.
           IF  HEX-MSG   =   13  GO       ROT-MOVE-00.
           CLOSE    CADHLP       PERFORM  ROT-RTLA-00  GOBACK.

       ROT-MOVE-10.
           IF  IND1  =   00         GO    ROT-MOVE-00.
           SUBTRACT  1  FROM  IND1  GO    ROT-TELA-00.

       ROT-MOVE-20.
           IF  LIN-AUX  <  (IND1 + 14)    GO   ROT-MOVE-00.
           ADD       1  TO  IND1          GO   ROT-TELA-00.

       ROT-MOVE-30.
           IF  IND1  >  13  SUBTRACT  13  FROM  IND1  GO  ROT-TELA-00.
           IF  IND1  >  00  MOVE      00   TO   IND1  GO  ROT-TELA-00.
           GO  ROT-MOVE-00.

       ROT-MOVE-40.
           IF  LIN-AUX  <  (IND1 + 14)  GO   ROT-MOVE-00.
           ADD     13   TO  IND1        GO   ROT-TELA-00.

       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.
