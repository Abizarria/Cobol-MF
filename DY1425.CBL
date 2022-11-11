       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1425.
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

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP
                       FILE          STATUS  IS  FST-LDP.

           SELECT      CADSDO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SDO
                       FILE          STATUS  IS  FST-SDO.

           SELECT      CADSRT        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-SRT.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADLBC.CPY.
       COPY  CADLDP.CPY.
       COPY  CADSDO.CPY.

       SD  CADSRT.

       01  REG-SRT.
           03  CHV-SRT.
               05  COD-SRT      PIC  9(04).
               05  DTA-SRT      PIC  9(06).
               05  LCT-SRT      PIC  9(03).
           03  BUG-SRT          PIC  9(01).
           03  HST-SRT          PIC  X(25).
           03  VAL-SRT          PIC  9(07)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-LBC          PIC  X(02).
           03  FST-LDP          PIC  X(02).
           03  FST-SDO          PIC  X(02).
           03  FST-SRT          PIC  X(02).

           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  IND3             PIC  9(03).
           03  IND4             PIC  9(02).
           03  TECLADO          PIC  9(02).

           03  DTA-TLA.
               05  ANO-TLA      PIC  9(02).
               05  MES-TLA      PIC  9(02).
               05  DIA-TLA      PIC  9(02).

           03  REG-AUX.
               05  FLG-AUX      PIC  X(01).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  DIA-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-AUX      PIC  9(02).
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  HST-AUX      PIC  X(27).
               05  VAL-AUX      PIC  Z.ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  TAB-CTA          OCCURS 800.
               05  CHV-TAB      PIC  X(13).
               05  REG-TAB      PIC  X(53).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1425".
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
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 12 COLUMN 13 PIC X(55) FROM ALL "ß".
               05  LINE 18 COLUMN 13 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 13 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 14 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 15 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 16 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 17 COLUMN 13 PIC X(55) FROM SPACES.
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 13 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 14 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 15 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 16 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 17 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 18 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 18 COLUMN 14 PIC X(54) FROM ALL "ß".

       01  MENU-01  BCK-CLR 05 FOR-CLR 15.
           03  LINE 13 COLUMN 14 PIC X(53) FROM REG-TAB (IND2).
           03  LINE 14 COLUMN 14 PIC X(53) FROM REG-TAB (IND2 + 1).
           03  LINE 15 COLUMN 14 PIC X(53) FROM REG-TAB (IND2 + 2).
           03  LINE 16 COLUMN 14 PIC X(53) FROM REG-TAB (IND2 + 3).
           03  LINE 17 COLUMN 14 PIC X(53) FROM REG-TAB (IND2 + 4).

       01  SETA-01 BCK-CLR 05.
           03  TRC-ACM LINE 13 COLUMN 66 VALUE " ".
           03  TRC-ABX LINE 17 COLUMN 66 VALUE " ".
           03  STA-ACM LINE 13 COLUMN 66 VALUE "" FOR-CLR 11.
           03  STA-ABX LINE 17 COLUMN 66 VALUE "" FOR-CLR 11.

       01  LN-SETA BCK-CLR 07 FOR-CLR 12
                   LINE IND4 COLUMN 14 PIC X(53) FROM REG-TAB (IND3).

       01  LN-MENU BCK-CLR 05 FOR-CLR 15
                   LINE IND4 COLUMN 14 PIC X(53) FROM REG-TAB (IND3).

       01  ENTR-01     AUTO.
           03  DOC-E01 LINE 13 COLUMN 60 PIC ZZZZZZ9      USING DOC-LBC.
           03  HST-E01 LINE 15 COLUMN 29 PIC X(25)        USING HST-LBC.

       01  ENTR-02  BCK-CLR 01 FOR-CLR 15.
           03  DOC-E02 LINE 13 COLUMN 60 PIC 9(07)         FROM DOC-LBC.
           03  HST-E02 LINE 15 COLUMN 29 PIC X(25)         FROM HST-LBC.
           03  VAL-E02 LINE 17 COLUMN 29 PIC Z.ZZZ.ZZ9,99  FROM VAL-LBC.

       PROCEDURE       DIVISION  USING  PRM-LCT.

       ROT-0000-00.
           OPEN     I-O          CADLBC
                                 CADLDP   CADSDO
           MOVE     SPACES   TO  CHV-LDP.

       ROT-PESQ-00.
           MOVE     31       TO  IND-MSG
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-EXIT-00.
           READ     CADLDP  NEXT
           IF  FLG-LDP  NOT   =  "P"  GO  ROT-PESQ-00.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-01.

       ROT-PESQ-10.
           MOVE     SPACES   TO  CHV-LDP
           SORT     CADSRT   ON  ASCENDING    KEY
                                              BUG-SRT DTA-SRT
                                              COD-SRT LCT-SRT
                    INPUT        PROCEDURE    IS  ROT-SORT-00
                    OUTPUT       PROCEDURE    IS  ROT-READ-00.
           MOVE     13       TO  IND4.

       ROT-EXBE-00.
           IF (IND2 + 04) < IND1 DISPLAY  STA-ABX
               IF  IND2     >  1 DISPLAY  STA-ACM
                   ELSE          DISPLAY  TRC-ACM
           ELSE    IF  IND2 >  1 DISPLAY  STA-ACM  TRC-ABX
                       ELSE      DISPLAY  TRC-ACM.
           DISPLAY     MENU-01   LN-SETA.

       ROT-EXBE-10.
           MOVE  6  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-EXBE-10.
           IF  TCL-MSG   =   05  GO       ROT-EXBE-20.
           IF  TCL-MSG   =   06  GO       ROT-EXBE-30.
           IF  TCL-MSG   =   53  GO       ROT-EXBE-40.
           IF  TCL-MSG   =   54  GO       ROT-EXBE-50.
           IF  TCL-MSG   >   00  GO       ROT-EXBE-10.
           IF  HEX-MSG   =   27  PERFORM  ROT-RTLA-00
                                 GO       ROT-ENTR-00.
           MOVE     REG-TAB     (IND3)    TO  REG-AUX
           MOVE     CHV-TAB     (IND3)    TO  CHV-LDP
           READ     CADLDP
           IF  FLG-AUX  =   " "  COMPUTE  VAL-LBC   =
                                          VAL-LBC   +  VAL-LDP
                                 MOVE     ""      TO  FLG-AUX
           ELSE
                                 COMPUTE  VAL-LBC   =
                                          VAL-LBC   -  VAL-LDP
                                 MOVE     " "      TO  FLG-AUX.
           MOVE     REG-AUX  TO  REG-TAB  (IND3)
           DISPLAY  LN-SETA      GO       ROT-EXBE-30.

       ROT-EXBE-20.
           IF  IND3  =   01      GO       ROT-EXBE-10.
           IF  IND4  =   13      COMPUTE  IND2 = IND2  -  1
                                 COMPUTE  IND3 = IND3  -  1
                                 GO       ROT-EXBE-00.
           DISPLAY  LN-MENU      COMPUTE  IND3 = IND3  -  1
                                 COMPUTE  IND4 = IND4  -  1
           DISPLAY  LN-SETA      GO       ROT-EXBE-10.

       ROT-EXBE-30.
           IF  IND3  =  IND1     GO       ROT-EXBE-10.
           IF  IND4  =   17      ADD      1  TO  IND2
                                 ADD      1  TO  IND3
                                 GO       ROT-EXBE-00.
           DISPLAY  LN-MENU      ADD      1  TO  IND3
                                 ADD      1  TO  IND4
           DISPLAY  LN-SETA      GO       ROT-EXBE-10.

       ROT-EXBE-40.
           IF  IND2  =   01      GO       ROT-EXBE-10.
           IF  IND2  >   04      COMPUTE  IND3 = IND2  -  5
                                 COMPUTE  IND2 = IND2  -  5
           ELSE
                                 MOVE     1  TO  IND2  IND3.
           MOVE    13  TO  IND4  GO       ROT-EXBE-00.

       ROT-EXBE-50.
           IF  (IND2  +  5)   >  IND1 GO  ROT-EXBE-10.
           COMPUTE  IND3      =  IND2  +  5
           COMPUTE  IND2      =  IND2  +  5
           MOVE     13  TO  IND4 GO       ROT-EXBE-00.

       ROT-SORT-00  SECTION.

       ROT-SORT-10.
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       ROT-SORT-20.
           READ     CADLDP  NEXT
           IF  FLG-LDP  NOT   =  "P"
                                 GO       ROT-SORT-10.
           MOVE     ZEROS    TO  BUG-SRT
           IF  DTA-LDP   <       100000
                                 MOVE  1  TO  BUG-SRT.
           MOVE     DSP-LDP  TO  COD-SRT
           MOVE     DTA-LDP  TO  DTA-SRT
           MOVE     LCT-LDP  TO  LCT-SRT
           MOVE     HST-LDP  TO  HST-SRT
           MOVE     VAL-LDP  TO  VAL-SRT
           RELEASE  REG-SRT      GO       ROT-SORT-10.

       ROT-SORT-20.
           MOVE     ZEROS    TO  VAL-LBC  IND1.

       ROT-SORT-30.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1      <  800  GO       ROT-SORT-30.
           MOVE     0        TO  IND1.

       ROT-READ-00  SECTION.

       ROT-READ-10.
           RETURN   CADSRT
           IF  FST-SRT   =  "10" GO       ROT-READ-20.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  FLG-AUX
           MOVE     DTA-SRT  TO  DTA-TLA
           MOVE     DIA-TLA  TO  DIA-AUX
           MOVE     MES-TLA  TO  MES-AUX
           MOVE     ANO-TLA  TO  ANO-AUX
           MOVE     HST-SRT  TO  HST-AUX
           MOVE     VAL-SRT  TO  VAL-AUX
           MOVE     CHV-SRT  TO  CHV-TAB (IND1)
           MOVE     REG-AUX  TO  REG-TAB (IND1)
           GO       ROT-READ-10.

       ROT-READ-20.
           MOVE     1        TO  IND2  IND3.

       ROT-READ-30  SECTION.

       ROT-ENTR-00.
           PERFORM  ROT-RTLA-00
           IF  VAL-LBC   =    0  GO       ROT-EXIT-00.

       ROT-ENTR-10.
           MOVE     SPACES   TO  HST-LBC
           MOVE     ZEROS    TO  LCT-LBC  DOC-LBC.

       ROT-ENTR-20.
           ACCEPT   DOC-E01      DISPLAY  DOC-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  TELA-01
                                 DISPLAY  MENU-01
                                 GO       ROT-EXBE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-20.
           IF  DOC-LBC   =    0  GO       ROT-ENTR-20.

       ROT-ENTR-30.
           ACCEPT   HST-E01      DISPLAY  HST-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-30.
           IF  HST-LBC   =       SPACES
                                 GO       ROT-ENTR-30.
           DISPLAY  VAL-E02.

       ROT-ENTR-50.
           MOVE 10  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-50.
           IF  TCL-MSG   >   00  GO       ROT-ENTR-50.
           IF  HEX-MSG   =   27  GO       ROT-ENTR-30.
           MOVE     0        TO  IND2
           MOVE     PRM-LCT  TO  BCO-LBC
           MOVE     DTA-LCT  TO  DTA-LBC
           MOVE     5        TO  COD-LBC.

       ROT-ATUA-00.
           ADD      1        TO  IND2
           IF  IND2    >    IND1 GO       ROT-ATUA-40.
           MOVE     REG-TAB     (IND2)    TO  REG-AUX
           IF  FLG-AUX   =  " "  GO       ROT-ATUA-00.

       ROT-ATUA-10.
           MOVE     CHV-TAB     (IND2)    TO  CHV-LDP
           READ     CADLDP       MOVE 20  TO  IND-MSG
           IF  FST-LDP   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-ATUA-10.
           IF  FST-LDP   =  "23" GO       ROT-ATUA-00.
           DELETE   CADLDP
           MOVE     2        TO  PRX-SDO
           MOVE     PRX-LDP  TO  SUF-SDO
           MOVE     DTA-LCT  TO  DTA-LDP
           MOVE     "L"      TO  FLG-LDP.

       ROT-ATUA-20.
           WRITE    REG-LDP      ADD   1  TO  LCT-LDP
           IF  FST-LDP   =  "22" GO       ROT-ATUA-20.

       ROT-ATUA-30.
           READ     CADSDO       MOVE 20  TO  IND-MSG
           IF  FST-SDO   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-ATUA-30.
           ADD      VAL-LDP  TO  SAI-SDO
           REWRITE  REG-SDO      GO       ROT-ATUA-00.

       ROT-ATUA-40.
           WRITE    REG-LBC      ADD   2  TO  LCT-LBC
           IF  FST-LBC   =  "22" GO       ROT-ATUA-40.
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
           CLOSE    CADLBC  CADLDP  CADSDO  GOBACK.
