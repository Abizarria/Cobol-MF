       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1217.
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

           SELECT      CADLCX        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCX
                       FILE          STATUS  IS  FST-LCX.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS
                       FILE          STATUS  IS  FST-PRS.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADBCO.CPY.
       COPY  CADLBC.CPY.
       COPY  CADLCX.CPY.
       COPY  CADPRS.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-BCO          PIC  X(02).
           03  FST-LBC          PIC  X(02).
           03  FST-LCX          PIC  X(02).
           03  FST-PRS          PIC  X(02).

           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  IND3             PIC  9(03).
           03  IND4             PIC  9(02).
           03  TECLADO          PIC  9(02).

           03  DTA-DEP.
               05  ANO-DEP      PIC  9(02).
               05  MES-DEP      PIC  9(02).
               05  DIA-DEP      PIC  9(02).

           03  REG-AUX.
               05  FLG-AUX      PIC  X(01).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  DIA-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  BCO-AUX      PIC  9(03).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  HST-AUX      PIC  X(26).
               05  VAL-AUX      PIC  ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  TAB-REG          OCCURS     900.
               05  REG-TAB      PIC  X(49).
               05  CHV-TAB      PIC  X(12).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1217".
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

       01  PRM-MVC.
           03  PRS-MVC.
               05  TUR-MVC      PIC  9(01).
               05  CXA-MVC      PIC  9(02).
               05  ATV-MVC      PIC  9(01).
               05  LIN-MVC      PIC  9(03).
           03  DTA-MVC          PIC  9(06).
           03  POS-MVC          PIC  9(02).

       SCREEN          SECTION.

       01  TELA-01.
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 08 COLUMN 40 VALUE " ÍÍ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 08 COLUMN 18 PIC X(22) FROM SPACES.
               05  LINE 08 COLUMN 21 PIC X(15) FROM DES-PRS.

       01  TELA-02.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 17 COLUMN 15 PIC X(51) FROM ALL "ß".
               05  LINE 19 COLUMN 15 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 18 COLUMN 15 PIC X(51) FROM
                   "  Total Lan‡ado ..:               R$".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 18 COLUMN 34 PIC ZZZ.ZZ9,9    FROM QTD-PRS.
               05  LINE 18 COLUMN 52 PIC Z.ZZZ.ZZ9,99 FROM VDD-PRS.
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 19 COLUMN 16 PIC X(50) FROM ALL "ß".

       01  MENU-01 BCK-CLR 02 FOR-CLR 15.
           03  LINE 10 COLUMN 16 PIC X(49) FROM REG-TAB (IND2).
           03  LINE 11 COLUMN 16 PIC X(49) FROM REG-TAB (IND2 + 1).
           03  LINE 12 COLUMN 16 PIC X(49) FROM REG-TAB (IND2 + 2).
           03  LINE 13 COLUMN 16 PIC X(49) FROM REG-TAB (IND2 + 3).
           03  LINE 14 COLUMN 16 PIC X(49) FROM REG-TAB (IND2 + 4).
           03  LINE 15 COLUMN 16 PIC X(49) FROM REG-TAB (IND2 + 5).

       01  MENU-02  BCK-CLR 02 FOR-CLR 15.
           03  LINE IND4 COLUMN 16 PIC X(49) FROM REG-TAB (IND3).

       01  MENU-03  BCK-CLR 07 FOR-CLR 12.
           03  LINE IND4 COLUMN 16 PIC X(49) FROM REG-TAB (IND3).

       01  SETA-01  BCK-CLR 02.
           03  TRC-ACM LINE 10 COLUMN 65 VALUE " ".
           03  TRC-ABX LINE 15 COLUMN 65 VALUE " ".
           03  STA-ACM LINE 10 COLUMN 65 VALUE "" FOR-CLR 14.
           03  STA-ABX LINE 15 COLUMN 65 VALUE "" FOR-CLR 14.

       01  ENTR-01 LINE POS-MVC COLUMN 52 PIC Z.ZZZ.ZZ9,99 USING VDD-PRS
                   AUTO.

       PROCEDURE       DIVISION  USING  PRM-MVC.

       ROT-0000-00.
           OPEN     INPUT        CADBCO   CADLBC
                    I-O          CADLCX   CADPRS
           MOVE     PRS-MVC  TO  CHV-PRS
           PERFORM  ROT-GTLA-00  READ     CADPRS
           IF  PRX-PRS   =  "1"  MOVE     1  TO  IND1
                                 GO       ROT-ZERA-00.
       ROT-ENTR-00.
           DISPLAY  TELA-01      ACCEPT   ENTR-01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =  "01" PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
           IF  TECLADO   =  "02" PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-00.
           PERFORM  ROT-RTLA-00  GO       ROT-EXIT-00.

       ROT-ZERA-00.
           MOVE     SPACES   TO  REG-TAB  (IND1)
                                 CHV-TAB  (IND1)
           IF  IND1    <    900  ADD      1  TO  IND1
                                 GO       ROT-ZERA-00.
           MOVE     10       TO  IND4
           MOVE     1        TO  IND2     IND3.
           MOVE     ZEROS    TO  CHV-BCO  IND1.

       ROT-PESQ-00.
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-PESQ-30.
           READ     CADBCO  NEXT
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           MOVE     DTA-MVC  TO  DTA-LBC.

       ROT-PESQ-10.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-PESQ-30.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO
                                 GO       ROT-PESQ-00.
           IF  COD-LBC  NOT   =  1
                                 GO       ROT-PESQ-10.
           MOVE     SPACES   TO  FLG-AUX
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX  FLG-AUX
           MOVE     BCO-LBC  TO  BCO-007
           MOVE     DTA-LBC  TO  DTA-007
           MOVE     LCT-LBC  TO  LCT-007
           READ     CADLCX       INVALID  KEY
                                 GO       ROT-PESQ-20.
           MOVE     ""      TO  FLG-AUX.

       ROT-PESQ-20.
           MOVE     DTA-LBC  TO  DTA-DEP
           MOVE     DIA-DEP  TO  DIA-AUX
           MOVE     MES-DEP  TO  MES-AUX
           MOVE     BCO-LBC  TO  BCO-AUX
           MOVE     HST-LBC  TO  HST-AUX
           MOVE     VAL-LBC  TO  VAL-AUX
           ADD      1        TO  IND1
           MOVE     CHV-LBC  TO  CHV-TAB  (IND1)
           MOVE     REG-AUX  TO  REG-TAB  (IND1)
           GO       ROT-PESQ-10.

       ROT-PESQ-30.
           IF  IND1   =   0      MOVE 31  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXIT-00.
           DISPLAY  TELA-01      TELA-02.

       ROT-PESQ-40.
           DISPLAY  TRC-ACM      TRC-ABX
           IF (IND2  +  05)  <   IND1
                                 DISPLAY  STA-ABX.
           IF  IND2  >  01       DISPLAY  STA-ACM.
           DISPLAY  MENU-01      MENU-03.

       ROT-MENU-00.
           MOVE  6  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-00.
           IF  TCL-MSG   =   05  GO       ROT-ST05-00.
           IF  TCL-MSG   =   06  GO       ROT-ST06-00.
           IF  TCL-MSG   =   53  GO       ROT-PGUP-00.
           IF  TCL-MSG   =   54  GO       ROT-PGDW-00.
           IF  TCL-MSG   >   00  GO       ROT-MENU-00.
           IF  HEX-MSG   =   27  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
       ROT-MUDA-00.
           MOVE     REG-TAB      (IND3)   TO  REG-AUX
           MOVE     CHV-TAB      (IND3)   TO  CHV-LBC
           READ     CADLBC
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX
           MOVE     BCO-LBC  TO  BCO-007
           MOVE     DTA-LBC  TO  DTA-007
           MOVE     LCT-LBC  TO  LCT-007
           READ     CADLCX       INVALID  KEY
                                 GO       ROT-MUDA-10.
           COMPUTE  QTD-PRS   =  QTD-PRS  -   1
           COMPUTE  VDD-PRS   =  VDD-PRS  -   VAL-LBC
           MOVE     SPACES   TO  FLG-AUX
           MOVE     REG-AUX  TO  REG-TAB  (IND3)
           DISPLAY  MENU-03      TELA-02
           DELETE   CADLCX       GO       ROT-ST06-00.

       ROT-MUDA-10.
           ADD      1        TO  QTD-PRS
           ADD      VAL-LBC  TO  VDD-PRS
           MOVE     ""      TO  FLG-AUX
           MOVE     REG-AUX  TO  REG-TAB  (IND3)
           MOVE     HST-LBC  TO  HST-LCX
           MOVE     VAL-LBC  TO  VAL-LCX
           DISPLAY  MENU-03      TELA-02
           WRITE    REG-LCX      GO       ROT-ST06-00.

       ROT-ST05-00.
           IF  IND3  =   01      GO       ROT-MENU-00.
           IF  IND4  =   10      SUBTRACT 1 FROM IND2
                                 SUBTRACT 1 FROM IND3
                                 GO       ROT-PESQ-40.
           DISPLAY  MENU-02      SUBTRACT 1 FROM IND3
                                 SUBTRACT 1 FROM IND4
           DISPLAY  MENU-03      GO       ROT-MENU-00.

       ROT-ST06-00.
           IF  IND3  =  IND1     GO       ROT-MENU-00.
           IF  IND4  =   15      ADD      1  TO  IND2
                                 ADD      1  TO  IND3
                                 GO       ROT-PESQ-40.
           DISPLAY  MENU-02      ADD      1  TO  IND3
                                 ADD      1  TO  IND4
           DISPLAY  MENU-03      GO       ROT-MENU-00.

       ROT-PGUP-00.
           IF  IND2  =   01      GO       ROT-MENU-00.
           IF  IND2  >   10      COMPUTE  IND3  =
                                          IND2  -  06
                                 COMPUTE  IND2  =
                                          IND2  -  06
           ELSE
                                 MOVE     1  TO  IND2
                                                 IND3.
           MOVE  10  TO  IND4    GO       ROT-PESQ-40.

       ROT-PGDW-00.
           IF  IND1         <   (IND2  +  06)
                                 GO       ROT-MENU-00.
           COMPUTE  IND3    =    IND2  +  06
           COMPUTE  IND2    =    IND2  +  06
           MOVE  10  TO  IND4    GO       ROT-PESQ-40.


       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-EXIT-00.
           REWRITE  REG-PRS
           CLOSE    CADBCO   CADLBC
                    CADLCX   CADPRS   GOBACK.
