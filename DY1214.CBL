       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1214.
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

           SELECT      CADCLI        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CLI
                       FILE          STATUS  IS  FST-CLI.

           SELECT      CADCRD        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CRD
                       FILE          STATUS  IS  FST-CRD.

           SELECT      CADFAT        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-FAT
                       FILE          STATUS  IS  FST-FAT.

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

       COPY  CADCDV.CPY.
       COPY  CADCLI.CPY.
       COPY  CADCRD.CPY.
       COPY  CADFAT.CPY.
       COPY  CADLCX.CPY.
       COPY  CADPRS.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CDV          PIC  X(02).
           03  FST-CLI          PIC  X(02).
           03  FST-CRD          PIC  X(02).
           03  FST-FAT          PIC  X(02).
           03  FST-LCX          PIC  X(02).
           03  FST-PRS          PIC  X(02).

           03  IND1             PIC  9(02)  VALUE  01.
           03  IND2             PIC  9(02).
           03  IND3             PIC  9(02).
           03  IND4             PIC  9(02).
           03  IND5             PIC  9(02).
           03  IND6             PIC  9(02).
           03  TECLADO          PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  REG-AX1.
               05  FLG-AX1      PIC  X(01).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  CLI-AX1      PIC  X(18).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  DIA-AX1      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AX1      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-AX1      PIC  9(02).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  NUM-AX1      PIC  9(06).
               05  VAL-AX1      PIC  ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  REG-AX2.
               05  FLG-AX2      PIC  X(01).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  DIA-AX2      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AX2      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-AX2      PIC  9(02).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  NOM-AX2      PIC  X(26).
               05  VAL-AX2      PIC  ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  REG-AX3.
               05  FLG-AX3      PIC  X(01).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  CPF-AX3      PIC  999.999.999.99.
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  DIA-AX3      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AX3      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-AX3      PIC  9(02).
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  NUM-AX3      PIC  999.999.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  VAL-AX3      PIC  ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  TAB-REG          OCCURS      06.
               05  REG-TAB      PIC  X(49).
               05  CHV-TAB      PIC  X(23).
               05  RED-FAT      REDEFINES   CHV-TAB.
                   07  FAT-TAB  PIC  X(17).
                   07  FILLER   PIC  X(06).
               05  RED-CRD      REDEFINES   CHV-TAB.
                   07  CRD-TAB  PIC  X(16).
                   07  FILLER   PIC  X(07).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1214".
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
           03  LINE 10 COLUMN 16 PIC X(49) FROM REG-TAB (1).
           03  LINE 11 COLUMN 16 PIC X(49) FROM REG-TAB (2).
           03  LINE 12 COLUMN 16 PIC X(49) FROM REG-TAB (3).
           03  LINE 13 COLUMN 16 PIC X(49) FROM REG-TAB (4).
           03  LINE 14 COLUMN 16 PIC X(49) FROM REG-TAB (5).
           03  LINE 15 COLUMN 16 PIC X(49) FROM REG-TAB (6).

       01  MENU-02  BCK-CLR 02 FOR-CLR 15.
           03  LINE IND3 COLUMN 16 PIC X(49) FROM REG-TAB (IND2).

       01  MENU-03  BCK-CLR 07 FOR-CLR 12.
           03  LINE IND3 COLUMN 16 PIC X(49) FROM REG-TAB (IND2).

       01  SETA-01  BCK-CLR 02.
           03  TRC-ACM LINE 10 COLUMN 65 VALUE " ".
           03  TRC-ABX LINE 15 COLUMN 65 VALUE " ".
           03  STA-ACM LINE 10 COLUMN 65 VALUE "" FOR-CLR 14.
           03  STA-ABX LINE 15 COLUMN 65 VALUE "" FOR-CLR 14.

       01  ENTR-01 LINE IND3 COLUMN 52 PIC Z.ZZZ.ZZ9,99 USING VDD-PRS.
       01  ENTR-02 LINE IND3 COLUMN 54 PIC   ZZZ.ZZ9,99 USING VAL-LCX.

       01  EXBE-01 BCK-CLR 07  FOR-CLR 12
                   LINE IND3 COLUMN 54 PIC ZZZ.ZZ9,99 FROM VAL-LCX.

       PROCEDURE       DIVISION  USING  PRM-MVC.

       ROT-0000-00.
           OPEN     I-O          CADPRS
           MOVE     POS-MVC  TO  IND3
           MOVE     PRS-MVC  TO  CHV-PRS
           READ     CADPRS       PERFORM  ROT-GTLA-00
           IF  PRX-PRS   <  "A"  OR   =   "S"
                                 GO       ROT-PESQ-00.
       ROT-ENTR-00.
           DISPLAY  TELA-01      ACCEPT   ENTR-01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =  "01" PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
           IF  TECLADO   =  "02" PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-00.
           PERFORM  ROT-RTLA-00  GO       ROT-EXIT-00.

       ROT-PESQ-00.
           IF  PRX-PRS   =  "1"  OPEN     INPUT   CADCLI
                                                  CADFAT.
           IF  PRX-PRS   =  "S"  OPEN     INPUT   CADCDV.
           IF  PRX-PRS   >  "1"  AND   <  "A"
                                 OPEN     INPUT   CADCRD.
           PERFORM  ROT-ZERA-00  OPEN     I-O     CADLCX
           MOVE     ZEROS    TO  IND1
           MOVE     SPACES   TO  CHV-CDV
                                 CHV-CRD  CHV-FAT.
       ROT-PESQ-10.
           IF  PRX-PRS   =  "S"  GO       ROT-PCDV-00.
           MOVE     PRX-PRS  TO  PRX-CRD
           IF  PRX-PRS   >  "1"  AND  <   "A"
                                 GO       ROT-PCRD-00.
       ROT-PFAT-00.
           START    CADFAT       KEY  >   CHV-FAT
           IF  FST-FAT   =  "23" GO       ROT-MENU-00.
           READ     CADFAT  NEXT
           PERFORM  ROT-PFAT-10  THRU     ROT-PFAT-20
           ADD      1        TO  IND1
           MOVE     CHV-FAT  TO  FAT-TAB  (IND1)
           MOVE     REG-AX1  TO  REG-TAB  (IND1)
           IF  IND1      <   06  GO       ROT-PFAT-00.
           GO       ROT-MENU-00.

       ROT-PFAT-10.
           MOVE     SPACES   TO  COD-LCX  FLG-AX1
           MOVE     VAL-FAT  TO  VAL-AX1
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     CLI-FAT  TO  CLI-002
           MOVE     DTA-FAT  TO  DTA-002
           MOVE     NUM-FAT  TO  FAT-002
           READ     CADLCX       INVALID  KEY
                                 GO       ROT-PFAT-20.
           MOVE     ""      TO  FLG-AX1
           MOVE     VAL-LCX  TO  VAL-AX1.

       ROT-PFAT-20.
           MOVE     CLI-FAT  TO  CHV-CLI
           READ     CADCLI
           MOVE     NOM-CLI  TO  CLI-AX1
           MOVE     DTA-FAT  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-AX1
           MOVE     MES-AUX  TO  MES-AX1
           MOVE     ANO-AUX  TO  ANO-AX1
           MOVE     NUM-FAT  TO  NUM-AX1.

       ROT-PCRD-00.
           START    CADCRD       KEY  >   CHV-CRD
           IF  FST-CRD   =  "23" GO       ROT-MENU-00.
           READ     CADCRD  NEXT
           IF  PRX-CRD  NOT   =  PRX-PRS
                                 GO       ROT-MENU-00.
           PERFORM  ROT-PCRD-10  THRU     ROT-PCRD-20
           ADD      1        TO  IND1
           MOVE     CHV-CRD  TO  CRD-TAB  (IND1)
           MOVE     REG-AX2  TO  REG-TAB  (IND1)
           IF  IND1      <   06  GO       ROT-PCRD-00.
           GO       ROT-MENU-00.

       ROT-PCRD-10.
           MOVE     SPACES   TO  COD-LCX  FLG-AX2
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     DTA-CRD  TO  DTA-003
           MOVE     NUM-CRD  TO  NUM-003
           MOVE     LCT-CRD  TO  LCT-003
           READ     CADLCX       INVALID  KEY
                                 GO       ROT-PCRD-20.
           MOVE     ""      TO  FLG-AX2.

       ROT-PCRD-20.
           MOVE     DTA-CRD  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-AX2
           MOVE     MES-AUX  TO  MES-AX2
           MOVE     ANO-AUX  TO  ANO-AX2
           MOVE     NOM-CRD  TO  NOM-AX2
           MOVE     VAL-CRD  TO  VAL-AX2.

       ROT-PCDV-00.
           START    CADCDV       KEY  >   CHV-CDV
           IF  FST-CDV   =  "23" GO       ROT-MENU-00.
           READ     CADCDV  NEXT
           PERFORM  ROT-PCDV-10  THRU     ROT-PCDV-20
           ADD      1        TO  IND1
           MOVE     CHV-CDV  TO  CHV-TAB  (IND1)
           MOVE     REG-AX3  TO  REG-TAB  (IND1)
           IF  IND1     <    06  GO       ROT-PCDV-00.
           GO       ROT-MENU-00.

       ROT-PCDV-10.
           MOVE     SPACES   TO  FLG-AX3
           MOVE     VAL-CDV  TO  VAL-AX3
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     CHV-CDV  TO  COD-LCX
           READ     CADLCX       INVALID  KEY
                                 GO       ROT-PCDV-20.
           MOVE     ""      TO  FLG-AX3
           MOVE     VAL-LCX  TO  VAL-AX3.

       ROT-PCDV-20.
           MOVE     CPF-CDV  TO  CPF-AX3
           MOVE     DTA-CDV  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-AX3
           MOVE     MES-AUX  TO  MES-AX3
           MOVE     ANO-AUX  TO  ANO-AX3
           MOVE     NUM-CDV  TO  NUM-AX3.

       ROT-MENU-00.
           IF  IND1   =   ZEROS  MOVE 49  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXIT-00.
           MOVE     01       TO  IND2
           MOVE     10       TO  IND3
           DISPLAY  TELA-01      TELA-02.

       ROT-MENU-10.
           DISPLAY  TRC-ACM      TRC-ABX
           IF  PRX-PRS   =  "1"  PERFORM  ROT-MENU-30.
           IF  PRX-PRS   >  "1"  AND  <   "A"
                                 PERFORM  ROT-MENU-40.
           IF  PRX-PRS   =  "S"  PERFORM  ROT-MENU-50.
           DISPLAY  MENU-01      MENU-03.

       ROT-MENU-20.
           MOVE  6  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-20.
           IF  TCL-MSG   =   05  GO       ROT-ST05-00.
           IF  TCL-MSG   =   06  GO       ROT-ST06-00.
           IF  TCL-MSG   =   53  GO       ROT-PGUP-00.
           IF  TCL-MSG   =   54  GO       ROT-PGDW-00.
           IF  TCL-MSG   >   00  GO       ROT-MENU-20.
           IF  HEX-MSG   =   27  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
           IF  PRX-PRS   =  "1"  GO       ROT-LFAT-00.
           IF  PRX-PRS   =  "S"  GO       ROT-LCDV-00.
           IF  PRX-PRS   >  "1"  AND  <   "A"
                                 GO       ROT-LCRD-00.
       ROT-MENU-30.
           MOVE     FAT-TAB      (1)  TO  CHV-FAT
           START    CADFAT       KEY  <   CHV-FAT
           IF  FST-FAT   =  "00" DISPLAY  STA-ACM.
           MOVE     FAT-TAB      (IND1)   TO  CHV-FAT
           START    CADFAT       KEY  >   CHV-FAT
           IF  FST-FAT   =  "00" DISPLAY  STA-ABX.

       ROT-MENU-40.
           MOVE     CRD-TAB      (1)  TO  CHV-CRD
           START    CADCRD       KEY  <   CHV-CRD
           READ     CADCRD       PREVIOUS
           IF  FST-CRD   =  "00" AND      PRX-CRD
               =    PRX-PRS      DISPLAY  STA-ACM.
           MOVE     CRD-TAB      (IND1)   TO  CHV-CRD
           START    CADCRD       KEY  >   CHV-CRD
           READ     CADCRD  NEXT
           IF  FST-CRD   =  "00" AND      PRX-CRD
               =    PRX-PRS      DISPLAY  STA-ABX.

       ROT-MENU-50.
           MOVE     CHV-TAB      (1)  TO  CHV-CDV
           START    CADCDV       KEY  <   CHV-CDV
           IF  FST-CDV   =  "00" DISPLAY  STA-ACM.
           MOVE     CHV-TAB      (IND1)   TO  CHV-CDV
           START    CADCDV       KEY  >   CHV-CDV
           IF  FST-CDV   =  "00" DISPLAY  STA-ABX.

       ROT-ST05-00.
           IF  IND3      =   10  GO       ROT-ST05-10.
           DISPLAY  MENU-02
           COMPUTE  IND2     =   IND2  -  1
           COMPUTE  IND3     =   IND3  -  1
           DISPLAY  MENU-03      GO       ROT-MENU-20.

       ROT-ST05-10.
           IF  PRX-PRS   =  "1"  GO       ROT-ST05-20.
           IF  PRX-PRS   =  "S"  GO       ROT-ST05-40.
           IF  PRX-PRS   >  "1"  AND  <   "A"
                                 GO       ROT-ST05-30.
       ROT-ST05-20.
           MOVE     FAT-TAB      (1)  TO  CHV-FAT
           START    CADFAT       KEY  <   CHV-FAT
           IF  FST-FAT   =  "23" GO       ROT-MENU-20.
           READ     CADFAT       PREVIOUS
           MOVE     5  TO  IND4
           MOVE     6  TO  IND5  PERFORM  ROT-ST05-50
           PERFORM  ROT-PFAT-10  THRU     ROT-PFAT-20
           MOVE     CHV-FAT  TO  FAT-TAB  (IND5)
           MOVE     REG-AX1  TO  REG-TAB  (IND5)
           GO       ROT-MENU-10.

       ROT-ST05-30.
           MOVE     CRD-TAB      (1)  TO  CHV-CRD
           START    CADCRD       KEY  <   CHV-CRD
           IF  FST-CRD   =  "23" GO       ROT-MENU-20.
           READ     CADCRD       PREVIOUS
           IF  PRX-CRD  NOT   =  PRX-PRS
                                 GO       ROT-MENU-20.
           MOVE     5  TO  IND4
           MOVE     6  TO  IND5  PERFORM  ROT-ST05-50
           PERFORM  ROT-PCRD-10  THRU     ROT-PCRD-20
           MOVE     CHV-CRD  TO  CRD-TAB  (IND5)
           MOVE     REG-AX2  TO  REG-TAB  (IND5)
           GO       ROT-MENU-10.

       ROT-ST05-40.
           MOVE     CHV-TAB      (1)  TO  CHV-CDV
           START    CADCDV       KEY  <   CHV-CDV
           IF  FST-CDV   =  "23" GO       ROT-MENU-20.
           READ     CADCDV       PREVIOUS
           MOVE     5  TO  IND4
           MOVE     6  TO  IND5  PERFORM  ROT-ST05-50
           PERFORM  ROT-PCDV-10  THRU     ROT-PCDV-20
           MOVE     CHV-CDV  TO  CHV-TAB  (IND5)
           MOVE     REG-AX3  TO  REG-TAB  (IND5)
           GO       ROT-MENU-10.

       ROT-ST05-50.
           MOVE     CHV-TAB     (IND4)    TO  CHV-TAB (IND5)
           MOVE     REG-TAB     (IND4)    TO  REG-TAB (IND5)
           COMPUTE  IND4      =  IND4     -   1
           COMPUTE  IND5      =  IND5     -   1
           IF  IND4      >    0  GO       ROT-ST05-50.

       ROT-ST06-00.
           IF  IND2    =    IND1 GO       ROT-ST06-10.
           DISPLAY  MENU-02
           COMPUTE  IND2     =   IND2  +  1
           COMPUTE  IND3     =   IND3  +  1
           DISPLAY  MENU-03      GO       ROT-MENU-20.

       ROT-ST06-10.
           IF  PRX-PRS   =  "1"  GO       ROT-ST06-20.
           IF  PRX-PRS   =  "S"  GO       ROT-ST06-40.
           IF  PRX-PRS   >  "1"  AND  <   "A"
                                 GO       ROT-ST06-30.
       ROT-ST06-20.
           MOVE     FAT-TAB      (IND1)   TO  CHV-FAT
           START    CADFAT       KEY  >   CHV-FAT
           IF  FST-FAT   =  "23" GO       ROT-MENU-20.
           READ     CADFAT  NEXT
           MOVE     2  TO  IND4
           MOVE     1  TO  IND5  PERFORM  ROT-ST06-50
           PERFORM  ROT-PFAT-10  THRU     ROT-PFAT-20
           MOVE     CHV-FAT  TO  FAT-TAB  (IND5)
           MOVE     REG-AX1  TO  REG-TAB  (IND5)
           GO       ROT-MENU-10.

       ROT-ST06-30.
           MOVE     CRD-TAB      (IND1)   TO  CHV-CRD
           START    CADCRD       KEY  >   CHV-CRD
           IF  FST-CRD   =  "23" GO       ROT-MENU-20.
           READ     CADCRD  NEXT
           IF  PRX-CRD  NOT   =  PRX-PRS
                                 GO       ROT-MENU-20.
           MOVE     2  TO  IND4
           MOVE     1  TO  IND5  PERFORM  ROT-ST06-50
           PERFORM  ROT-PCRD-10  THRU     ROT-PCRD-20
           MOVE     CHV-CRD  TO  CRD-TAB  (IND5)
           MOVE     REG-AX2  TO  REG-TAB  (IND5)
           GO       ROT-MENU-10.

       ROT-ST06-40.
           MOVE     CHV-TAB      (IND1)   TO  CHV-CDV
           START    CADCDV       KEY  >   CHV-CDV
           IF  FST-CDV   =  "23" GO       ROT-MENU-20.
           READ     CADCDV  NEXT
           MOVE     2  TO  IND4
           MOVE     1  TO  IND5  PERFORM  ROT-ST06-50
           PERFORM  ROT-PCDV-10  THRU     ROT-PCDV-20
           MOVE     CHV-CDV  TO  CHV-TAB  (IND5)
           MOVE     REG-AX3  TO  REG-TAB  (IND5)
           GO       ROT-MENU-10.

       ROT-ST06-50.
           MOVE     CHV-TAB     (IND4)    TO  CHV-TAB (IND5)
           MOVE     REG-TAB     (IND4)    TO  REG-TAB (IND5)
           COMPUTE  IND4      =  IND4     +   1
           COMPUTE  IND5      =  IND5     +   1
           IF  IND4      <    7  GO       ROT-ST06-50.

       ROT-PGUP-00.
           MOVE     1        TO  IND6
           IF  PRX-PRS   =  "1"  GO       ROT-PGUP-10.
           IF  PRX-PRS   =  "S"  GO       ROT-PGUP-50.
           IF  PRX-PRS   >  "1"  AND  <   "A"
                                 GO       ROT-PGUP-30.
       ROT-PGUP-10.
           MOVE     FAT-TAB      (1)  TO  CHV-FAT
           START    CADFAT       KEY  <   CHV-FAT
           IF  FST-FAT   =  "23" GO       ROT-MENU-20.
           MOVE     1  TO  IND2  MOVE 10  TO  IND3.

       ROT-PGUP-20.
           READ     CADFAT       PREVIOUS
           IF  FST-FAT   =  "10" GO       ROT-MENU-10.
           MOVE     5  TO  IND4
           MOVE     6  TO  IND5  PERFORM  ROT-ST05-50
           PERFORM  ROT-PFAT-10  THRU     ROT-PFAT-20
           MOVE     CHV-FAT  TO  FAT-TAB  (IND5)
           MOVE     REG-AX1  TO  REG-TAB  (IND5)
           IF  IND6     >    05  GO       ROT-MENU-10.
           ADD      1  TO  IND6  GO       ROT-PGUP-20.

       ROT-PGUP-30.
           MOVE     CRD-TAB      (1)  TO  CHV-CRD
           START    CADCRD       KEY  <   CHV-CRD
           IF  FST-CRD   =  "23" GO       ROT-MENU-20.
           MOVE     1  TO  IND2  MOVE 10  TO  IND3.

       ROT-PGUP-40.
           READ     CADCRD       PREVIOUS
           IF  FST-CRD   =  "10" GO       ROT-MENU-10.
           IF  PRX-CRD  NOT   =  PRX-PRS
                                 GO       ROT-MENU-10.
           MOVE     5  TO  IND4
           MOVE     6  TO  IND5  PERFORM  ROT-ST05-50
           PERFORM  ROT-PCRD-10  THRU     ROT-PCRD-20
           MOVE     CHV-CRD  TO  CRD-TAB  (IND5)
           MOVE     REG-AX2  TO  REG-TAB  (IND5)
           IF  IND6     >    05  GO       ROT-MENU-10.
           ADD      1  TO  IND6  GO       ROT-PGUP-40.

       ROT-PGUP-50.
           MOVE     CHV-TAB      (1)  TO  CHV-CDV
           START    CADCDV       KEY  <   CHV-CDV
           IF  FST-CDV   =  "23" GO       ROT-MENU-20.
           MOVE     1  TO  IND2  MOVE 10  TO  IND3.

       ROT-PGUP-60.
           READ     CADCDV       PREVIOUS
           IF  FST-CDV   =  "10" GO       ROT-MENU-10.
           MOVE     5  TO  IND4
           MOVE     6  TO  IND5  PERFORM  ROT-ST05-50
           PERFORM  ROT-PCDV-10  THRU     ROT-PCDV-20
           MOVE     CHV-CDV  TO  CHV-TAB  (IND5)
           MOVE     REG-AX3  TO  REG-TAB  (IND5)
           IF  IND6     >    05  GO       ROT-MENU-10.
           ADD      1  TO  IND6  GO       ROT-PGUP-60.

       ROT-PGDW-00.
           MOVE     1        TO  IND6
           IF  PRX-PRS   =  "1"  GO       ROT-PGDW-10.
           IF  PRX-PRS   =  "S"  GO       ROT-PGDW-50.
           IF  PRX-PRS   >  "1"  AND  <   "A"
                                 GO       ROT-PGDW-30.
       ROT-PGDW-10.
           MOVE     FAT-TAB      (IND1)   TO  CHV-FAT
           START    CADFAT       KEY  >   CHV-FAT
           IF  FST-FAT   =  "23" GO       ROT-MENU-20.
           MOVE     1  TO  IND2  MOVE 10  TO  IND3.

       ROT-PGDW-20.
           READ     CADFAT  NEXT
           IF  FST-FAT   =  "10" GO       ROT-MENU-10.
           MOVE     2  TO  IND4
           MOVE     1  TO  IND5  PERFORM  ROT-ST06-50
           PERFORM  ROT-PFAT-10  THRU     ROT-PFAT-20
           MOVE     CHV-FAT  TO  FAT-TAB  (IND5)
           MOVE     REG-AX1  TO  REG-TAB  (IND5)
           IF  IND6     >    05  GO       ROT-MENU-10.
           ADD      1  TO  IND6  GO       ROT-PGDW-20.

       ROT-PGDW-30.
           MOVE     CRD-TAB      (IND1)   TO  CHV-CRD
           START    CADCRD       KEY  >   CHV-CRD
           IF  FST-CRD   =  "23" GO       ROT-MENU-20.
           MOVE     1  TO  IND2  MOVE 10  TO  IND3.

       ROT-PGDW-40.
           READ     CADCRD  NEXT
           IF  FST-CRD   =  "10" GO       ROT-MENU-10.
           IF  PRX-CRD  NOT   =  PRX-PRS
                                 GO       ROT-MENU-10.
           MOVE     2  TO  IND4
           MOVE     1  TO  IND5  PERFORM  ROT-ST06-50
           PERFORM  ROT-PCRD-10  THRU     ROT-PCRD-20
           MOVE     CHV-CRD  TO  CRD-TAB  (IND5)
           MOVE     REG-AX2  TO  REG-TAB  (IND5)
           IF  IND6     >    05  GO       ROT-MENU-10.
           ADD      1  TO  IND6  GO       ROT-PGDW-40.

       ROT-PGDW-50.
           MOVE     CHV-TAB      (IND1)   TO  CHV-CDV
           START    CADCDV       KEY  >   CHV-CDV
           IF  FST-CDV   =  "23" GO       ROT-MENU-20.
           MOVE     1  TO  IND2  MOVE 10  TO  IND3.

       ROT-PGDW-60.
           READ     CADCDV  NEXT
           IF  FST-CDV   =  "10" GO       ROT-MENU-10.
           MOVE     2  TO  IND4
           MOVE     1  TO  IND5  PERFORM  ROT-ST06-50
           PERFORM  ROT-PCDV-10  THRU     ROT-PCDV-20
           MOVE     CHV-CDV  TO  CHV-TAB  (IND5)
           MOVE     REG-AX3  TO  REG-TAB  (IND5)
           IF  IND6     >    05  GO       ROT-MENU-10.
           ADD      1  TO  IND6  GO       ROT-PGDW-60.

       ROT-LFAT-00.
           MOVE     REG-TAB      (IND2)   TO  REG-AX1
           MOVE     FAT-TAB      (IND2)   TO  CHV-FAT
           READ     CADFAT
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX
           MOVE     CLI-FAT  TO  CLI-002
           MOVE     DTA-FAT  TO  DTA-002
           MOVE     NUM-FAT  TO  FAT-002
           READ     CADLCX       INVALID  KEY
                                 GO       ROT-LFAT-10.
           COMPUTE  QTD-PRS   =  QTD-PRS   -  1
           COMPUTE  VDD-PRS   =  VDD-PRS   -  VAL-LCX
           MOVE     SPACES   TO  FLG-AX1
           MOVE     VAL-FAT  TO  VAL-AX1
           MOVE     REG-AX1  TO  REG-TAB  (IND2)
           DISPLAY  MENU-03      TELA-02
           DELETE   CADLCX       GO       ROT-ST06-00.

       ROT-LFAT-10.
           MOVE     VAL-FAT  TO  VAL-LCX
           ACCEPT   ENTR-02      DISPLAY  EXBE-01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =  "01" GO       ROT-MENU-20.
           IF  TECLADO   =  "02" PERFORM  ROT-HELP-00
                                 GO       ROT-LFAT-10.
           IF  VAL-LCX   =    0  GO       ROT-LFAT-10.
           MOVE     48       TO  IND-MSG
           IF  VAL-LCX   >       VAL-FAT
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-LFAT-10.
           COMPUTE  QTD-PRS   =  QTD-PRS   +  1
           COMPUTE  VDD-PRS   =  VDD-PRS   +  VAL-LCX
           MOVE     ""      TO  FLG-AX1
           MOVE     VAL-LCX  TO  VAL-AX1
           MOVE     REG-AX1  TO  REG-TAB  (IND2)
           MOVE     CLI-FAT  TO  CHV-CLI
           READ     CADCLI
           MOVE     NOM-CLI  TO  HST-LCX
           DISPLAY  MENU-03      TELA-02
           WRITE    REG-LCX      GO       ROT-ST06-00.

       ROT-LCRD-00.
           MOVE     REG-TAB      (IND2)   TO  REG-AX2
           MOVE     CRD-TAB      (IND2)   TO  CHV-CRD
           READ     CADCRD
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX
           MOVE     DTA-CRD  TO  DTA-003
           MOVE     NUM-CRD  TO  NUM-003
           MOVE     LCT-CRD  TO  LCT-003
           READ     CADLCX       INVALID  KEY
                                 GO       ROT-LCRD-10.
           COMPUTE  QTD-PRS   =  QTD-PRS   -  1
           COMPUTE  VDD-PRS   =  VDD-PRS   -  VAL-CRD
           MOVE     SPACES   TO  FLG-AX2
           MOVE     REG-AX2  TO  REG-TAB  (IND2)
           DISPLAY  MENU-03      TELA-02
           DELETE   CADLCX       GO       ROT-ST06-00.

       ROT-LCRD-10.
           COMPUTE  QTD-PRS   =  QTD-PRS   +  1
           COMPUTE  VDD-PRS   =  VDD-PRS   +  VAL-CRD
           MOVE     ""      TO  FLG-AX2
           MOVE     REG-AX2  TO  REG-TAB  (IND2)
           MOVE     NOM-CRD  TO  HST-LCX
           MOVE     VAL-CRD  TO  VAL-LCX
           DISPLAY  MENU-03      TELA-02
           WRITE    REG-LCX      GO       ROT-ST06-00.

       ROT-LCDV-00.
           MOVE     REG-TAB      (IND2)   TO  REG-AX3
           MOVE     CHV-TAB      (IND2)   TO  CHV-CDV
           READ     CADCDV
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     CHV-CDV  TO  COD-LCX
           READ     CADLCX       INVALID  KEY
                                 GO       ROT-LCDV-10.
           COMPUTE  QTD-PRS   =  QTD-PRS   -  1
           COMPUTE  VDD-PRS   =  VDD-PRS   -  VAL-LCX
           MOVE     SPACES   TO  FLG-AX3
           MOVE     VAL-CDV  TO  VAL-AX3
           MOVE     REG-AX3  TO  REG-TAB  (IND2)
           DISPLAY  MENU-03      TELA-02
           DELETE   CADLCX       GO       ROT-ST06-00.

       ROT-LCDV-10.
           MOVE     VAL-CDV  TO  VAL-LCX
           ACCEPT   ENTR-02      DISPLAY  EXBE-01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =  "01" GO       ROT-MENU-20.
           IF  TECLADO   =  "02" PERFORM  ROT-HELP-00
                                 GO       ROT-LCDV-10.
           IF  VAL-LCX   =    0  GO       ROT-LCDV-10.
           MOVE     48       TO  IND-MSG
           IF  VAL-LCX   <       VAL-CDV
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-LCDV-10.
           COMPUTE  QTD-PRS   =  QTD-PRS   +  1
           COMPUTE  VDD-PRS   =  VDD-PRS   +  VAL-LCX
           MOVE     ""      TO  FLG-AX3
           MOVE     VAL-LCX  TO  VAL-AX3
           MOVE     REG-AX3  TO  REG-TAB  (IND2)
           MOVE     NOM-CDV  TO  HST-LCX
           DISPLAY  MENU-03      TELA-02
           WRITE    REG-LCX      GO       ROT-ST06-00.

       ROT-ZERA-00.
           MOVE     SPACES   TO  REG-TAB (IND1)
                                 CHV-TAB (IND1)
           IF  IND1    <      6  ADD      1  TO  IND1
                                 GO       ROT-ZERA-00.
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
           IF  PRX-PRS   =  "1"  CLOSE    CADCLI
                                          CADFAT   CADLCX.
           IF  PRX-PRS   >  "1"  AND   <   "A"
                                 CLOSE    CADCRD   CADLCX.
           IF  PRX-PRS   =  "S"  CLOSE    CADCDV   CADLCX.
           REWRITE  REG-PRS      CLOSE    CADPRS   GOBACK.
