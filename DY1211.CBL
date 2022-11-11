       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1211.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADECR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ECR
                       FILE          STATUS  IS  FST-ECR.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST
                       FILE          STATUS  IS  FST-EST.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS
                       FILE          STATUS  IS  FST-PRS.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADECR.CPY.
       COPY  CADEST.CPY.
       COPY  CADPRS.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-EST          PIC  X(02).
           03  FST-ECR          PIC  X(02).
           03  FST-PRS          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  IND3             PIC  9(02).
           03  IND4             PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  ECR-AUX          PIC  9(06)V9.
           03  AFR-AUX          PIC  9(04).
           03  VDA-AUX          PIC  9(06)V9.

           03  REG-AUX.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  BMB-AUX      PIC  9(02).
               05  FILLER       PIC  X(04)  VALUE  SPACES.
               05  ICL-AUX      PIC  999999,9.
               05  FILLER       PIC  X(04)  VALUE  SPACES.
               05  FCL-AUX      PIC  999999,9.
               05  FILLER       PIC  X(04)  VALUE  SPACES.
               05  AFC-AUX      PIC  ZZZ9.
               05  FILLER       PIC  X(04)  VALUE  SPACES.
               05  VDC-AUX      PIC  ZZZ.ZZ9,9.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  TAB-ECR          OCCURS  30.
               05  REG-TAB      PIC  X(49).
               05  CHV-TAB      PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1211".
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

       01  ENTR-01     AUTO.
           03  FCL-E01 LINE IND4 COLUMN 35 PIC ZZZZZ9,9 USING ECR-AUX.
           03  AFR-E01 LINE IND4 COLUMN 47 PIC ZZZ9     USING AFR-AUX.

       01  ENTR-02 BCK-CLR 07 FOR-CLR 12.
           03  FCL-T02 LINE IND4 COLUMN 35 PIC 999999,9  FROM ECR-AUX.
           03  AFR-T02 LINE IND4 COLUMN 47 PIC ZZZ9      FROM AFR-AUX.
           03  VDA-T02 LINE IND4 COLUMN 55 PIC ZZZ.ZZ9,9 FROM VDA-AUX.

       PROCEDURE       DIVISION  USING  PRM-MVC.

       ROT-0000-00.
           OPEN     INPUT        CADEST
                    I-O          CADECR   CADPRS
           MOVE     1        TO  IND1
           MOVE     TUR-MVC  TO  TUR-PRS
           MOVE     CXA-MVC  TO  CXA-PRS
           MOVE     ATV-MVC  TO  ATV-PRS
           MOVE     LIN-MVC  TO  LIN-PRS
           PERFORM  ROT-GTLA-00  READ     CADPRS
           DISPLAY  TELA-01      TELA-02.

       ROT-MENU-00.
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1  <  30       ADD      1  TO  IND1
                                 GO       ROT-MENU-00.
           MOVE     ZEROS    TO  CHV-ECR
           MOVE     10  TO  IND4 MOVE     1  TO  IND2
           MOVE     00  TO  IND1 MOVE     1  TO  IND3.

       ROT-MENU-10.
           START    CADECR       KEY  >   CHV-ECR
           IF  FST-ECR   =  "23" GO       ROT-MENU-20.
           READ     CADECR  NEXT
           IF (CX1-ECR  NOT   =  CXA-PRS  AND
               CX2-ECR  NOT   =  CXA-PRS) OR  CMB-ECR
               NOT   =  PRX-PRS  GO       ROT-MENU-10.
           MOVE     CHV-ECR  TO  BMB-AUX
           MOVE     ICL-ECR  TO  ICL-AUX
           MOVE     FCL-ECR  TO  FCL-AUX
           MOVE     AFC-ECR  TO  AFC-AUX
           MOVE     VDC-ECR  TO  VDC-AUX
           ADD      1        TO  IND1
           MOVE     REG-AUX  TO  REG-TAB (IND1)
           MOVE     CHV-ECR  TO  CHV-TAB (IND1)
           GO       ROT-MENU-10.

       ROT-MENU-20.
           DISPLAY  TRC-ACM      TRC-ABX
           IF (IND2  +  05)  <   IND1
                                 DISPLAY  STA-ABX.
           IF  IND2  >  01       DISPLAY  STA-ACM.
           DISPLAY  MENU-01      MENU-03.

       ROT-MENU-30.
           MOVE  2  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-30.
           IF  TCL-MSG   =   05  GO       ROT-ST05-00.
           IF  TCL-MSG   =   06  GO       ROT-ST06-00.
           IF  TCL-MSG   =   53  GO       ROT-PGUP-00.
           IF  TCL-MSG   =   54  GO       ROT-PGDW-00.
           IF  TCL-MSG   >   00  GO       ROT-MENU-30.
           IF  HEX-MSG   =   27  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
       ROT-ENTR-00.
           MOVE     CHV-TAB      (IND3)   TO  CHV-ECR
           READ     CADECR       MOVE 20  TO  IND-MSG
           IF  FST-ECR   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-MENU-30.
           MOVE     FCL-ECR  TO  ECR-AUX
           MOVE     AFC-ECR  TO  AFR-AUX.

       ROT-ENTR-10.
           ACCEPT   FCL-E01      DISPLAY  FCL-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  MENU-03
                                 GO       ROT-MENU-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
       ROT-ENTR-20.
           ACCEPT   AFR-E01      DISPLAY  AFR-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-20.
           COMPUTE  VDA-AUX   = (ECR-AUX  +   1000000
                              -  ICL-ECR) -   AFR-AUX
           DISPLAY  VDA-T02      MOVE 44  TO  IND-MSG
           IF  VDA-AUX  NOT   <  10000,0
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-20.
           MOVE     1        TO  PRX-EST
           MOVE     CMB-ECR  TO  SUF-EST
           READ     CADEST
           COMPUTE  QTD-PRS  =   QTD-PRS  -   VDC-ECR  +   VDA-AUX
           COMPUTE  CTD-PRS  =   CTD-PRS  -  (VDC-ECR  *   CTM-EST)
                                          +  (VDA-AUX  *   CTM-EST)
           COMPUTE  VDD-PRS  =   VDD-PRS  -  (VDC-ECR  *   VAL-ECR)
                                          +  (VDA-AUX  *   VAL-ECR)
           MOVE     CHV-ECR  TO  BMB-AUX
           MOVE     ICL-ECR  TO  ICL-AUX
           MOVE     ECR-AUX  TO  FCL-ECR  FCL-AUX
           MOVE     AFR-AUX  TO  AFC-ECR  AFC-AUX
           MOVE     VDA-AUX  TO  VDC-ECR  VDC-AUX
           MOVE     REG-AUX  TO  REG-TAB (IND3)
           REWRITE  REG-ECR
           DISPLAY  TELA-02      GO       ROT-ST06-00.

       ROT-ST05-00.
           IF  IND3      =   01  GO       ROT-MENU-30.
           IF  IND4      =   10  SUBTRACT 1 FROM IND2
                                 SUBTRACT 1 FROM IND3
                                 GO       ROT-MENU-20.
           DISPLAY  MENU-02      SUBTRACT 1 FROM IND3
           COMPUTE  IND4     =   IND4  -  1
           DISPLAY  MENU-03      GO       ROT-MENU-30.

       ROT-ST06-00.
           IF  IND3      =  IND1 GO       ROT-MENU-30.
           IF  IND4      =   15  ADD      1  TO  IND2
                                 ADD      1  TO  IND3
                                 GO       ROT-MENU-20.
           DISPLAY  MENU-02      ADD      1  TO  IND3
           COMPUTE  IND4     =   IND4  +  1
           DISPLAY  MENU-03      GO       ROT-MENU-30.

       ROT-PGUP-00.
           IF  IND2      =   01  GO       ROT-MENU-30.
           MOVE     10       TO  IND4
           IF  IND2      >   10  COMPUTE  IND3 = IND2 - 06
                                 COMPUTE  IND2 = IND2 - 06
           ELSE
                                 MOVE     1  TO  IND2   IND3.
           GO      ROT-MENU-20.

       ROT-PGDW-00.
           IF  IND1      <      (IND2  +  06)
                                 GO       ROT-MENU-30.
           MOVE     10       TO  IND4
           COMPUTE  IND3     =   IND2  +  06
           COMPUTE  IND2     =   IND2  +  06
           GO       ROT-MENU-20.

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
           REWRITE  REG-PRS
           CLOSE    CADPRS   CADECR   CADEST   GOBACK.
