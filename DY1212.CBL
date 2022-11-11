       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1212.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST
                       FILE          STATUS  IS  FST-EST.

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

       COPY  CADEST.CPY.
       COPY  CADLCX.CPY.
       COPY  CADPRS.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-EST          PIC  X(02).
           03  FST-LCX          PIC  X(02).
           03  FST-PRS          PIC  X(02).

           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  IND3             PIC  9(03).
           03  IND4             PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  ACM-PSQ          PIC  9(02).
           03  QTD-LCT          PIC  9(04)V9.

           03  REG-AUX.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  PRX-AUX      PIC  X(01).
               05  FILLER       PIC  X(01)  VALUE  "-".
               05  SUF-AUX      PIC  9(03).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  DES-AUX      PIC  X(26).
               05  QTD-AUX      PIC  ZZ9,9.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  VAL-AUX      PIC  Z.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  TAB-REG          OCCURS      100.
               05  REG-TAB      PIC  X(49).
               05  CHV-TAB      PIC  9(03).

           03  NOM-PSQ          PIC  X(25).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  25.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1212".
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

       01  TELA-03.
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 10 COLUMN 65 PIC X(01) FROM SPACES.
               05  LINE 15 COLUMN 65 PIC X(01) FROM SPACES.
               05  LINE 10 COLUMN 16 PIC X(49) FROM
                   " C¢digo Produto .:  -     -".
               05  LINE 11 COLUMN 16 PIC X(49) FROM SPACES.
               05  LINE 12 COLUMN 16 PIC X(49) FROM
                   " Descri‡Æo ......:".
               05  LINE 13 COLUMN 16 PIC X(49) FROM
                   " Pre‡o de Venda .:".
               05  LINE 14 COLUMN 16 PIC X(49) FROM SPACES.
               05  LINE 15 COLUMN 16 PIC X(49) FROM
                   " Saldo Estoque ..:               Venda :".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 10 COLUMN 35 PIC X(01) FROM PRX-PRS.
               05  LINE 10 COLUMN 45 PIC X(15) FROM DES-PRS.

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

       01  ENTR-01.
           03  SUF-I01 LINE 10 COLUMN 37 PIC ZZ9     USING SUF-EST.
           03  PSQ-I01 LINE 12 COLUMN 35 PIC X(25)   USING NOM-PSQ.
           03  QTD-I01 LINE 15 COLUMN 57 PIC Z.ZZ9,9 USING QTD-LCT.

       01  ENTR-02 BCK-CLR 02 FOR-CLR 15.
           03  SUF-I02 LINE 10 COLUMN 37 PIC 9(03)        FROM SUF-EST.
           03  PSQ-I02 LINE 12 COLUMN 35 PIC X(25)        FROM NOM-PSQ.
           03  DES-I02 LINE 12 COLUMN 35 PIC X(25)        FROM DES-EST.
           03  VDA-I02 LINE 13 COLUMN 35 PIC ZZZ.ZZ9,9999 FROM VDA-EST.
           03  SDO-I02 LINE 15 COLUMN 34 PIC ----.--9,9   FROM SDO-EST.
           03  QTD-I02 LINE 15 COLUMN 57 PIC Z.ZZ9,9      FROM QTD-LCT.

       01  ENTR-03.
           03  QTD-A01  LINE IND4 COLUMN 50 PIC ZZ9,9  USING QTD-LCT.
           03  QTD-A02  BCK-CLR 07 FOR-CLR 12
                        LINE IND4 COLUMN 50 PIC ZZ9,9   FROM QTD-LCT.

       PROCEDURE       DIVISION  USING  PRM-MVC.

       ROT-0000-00.
           OPEN     INPUT        CADEST
                    I-O          CADLCX   CADPRS
           MOVE     PRS-MVC  TO  CHV-PRS
           READ     CADPRS       PERFORM  ROT-GTLA-00
           DISPLAY  TELA-01      TELA-02
           IF  QTD-PRS   >    0  GO       ROT-MENU-00.

       ROT-INCL-00.
           DISPLAY  TELA-03      MOVE  0  TO  SUF-EST
           ACCEPT   SUF-I01      DISPLAY  SUF-I02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01
               IF   QTD-PRS   >  ZEROS
                                 GO       ROT-MENU-00
               ELSE
                                 PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-INCL-00.
           IF  SUF-EST   <   01  GO       ROT-NOME-00.
           MOVE     PRX-PRS  TO  PRX-EST
           READ     CADEST       MOVE 21  TO  IND-MSG
           IF  FST-EST   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-INCL-00.
       ROT-INCL-10.
           DISPLAY  SUF-I02      DES-I02
                    VDA-I02      SDO-I02.
       ROT-INCL-20.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-INCL-20.
           IF  TCL-MSG   =   05  GO       ROT-ST05-00.
           IF  TCL-MSG   =   06  GO       ROT-ST06-00.
           IF  TCL-MSG   >   00  GO       ROT-INCL-20.
           IF  HEX-MSG   =   27  GO       ROT-INCL-00.
           MOVE     ZEROS    TO  QTD-LCT.

       ROT-INCL-30.
           ACCEPT   QTD-I01      DISPLAY  QTD-I02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-INCL-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-INCL-30.
           IF  QTD-LCT   =    0  GO       ROT-INCL-30.
           ADD      QTD-LCT  TO  QTD-PRS
           COMPUTE  CTD-PRS   =  CTD-PRS
                              +  QTD-LCT  *   CTM-EST
           COMPUTE  VDD-PRS   =  VDD-PRS
                              +  QTD-LCT  *   VDA-EST
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX
           MOVE     SUF-EST  TO  COD-001
           READ     CADLCX
           MOVE     DES-EST  TO  HST-LCX
           MOVE     VDA-EST  TO  VAL-LCX
           IF  FST-LCX   =  "23" MOVE     QTD-LCT  TO  QTD-LCX
                                 WRITE    REG-LCX
           ELSE
                                 ADD      QTD-LCT  TO  QTD-LCX
                                 REWRITE  REG-LCX.
           DISPLAY  TELA-02      GO       ROT-INCL-00.

       ROT-MENU-00.
           MOVE     1  TO  IND1  PERFORM  ROT-ZERA-00
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX.

       ROT-MENU-10.
           START    CADLCX       KEY  >   CHV-LCX
           IF  FST-LCX   =  "23" GO       ROT-MENU-20.
           READ     CADLCX  NEXT
           IF  TUR-LCX  NOT   =  TUR-PRS  OR
               CXA-LCX  NOT   =  CXA-PRS  OR
               ATV-LCX  NOT   =  ATV-PRS  OR  PRX-LCX
               NOT   =  PRX-PRS  GO       ROT-MENU-20.
           MOVE     PRX-LCX  TO  PRX-EST  PRX-AUX
           MOVE     COD-001  TO  SUF-EST  SUF-AUX
           READ     CADEST
           MOVE     DES-EST  TO  DES-AUX
           MOVE     QTD-LCX  TO  QTD-AUX
           COMPUTE  VAL-AUX   =  QTD-LCX  *   VDA-EST
           ADD      1        TO  IND1
           MOVE     REG-AUX  TO  REG-TAB (IND1)
           MOVE     COD-001  TO  CHV-TAB (IND1)
           GO       ROT-MENU-10.

       ROT-MENU-20.
           DISPLAY  TRC-ACM      TRC-ABX
           IF (IND2  +  05)  <   IND1
                                 DISPLAY  STA-ABX.
           IF  IND2  >  01       DISPLAY  STA-ACM.
           DISPLAY  MENU-01      MENU-03.

       ROT-MENU-30.
           MOVE  5  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-30.
           IF  TCL-MSG   =   05  GO       ROT-ST05-10.
           IF  TCL-MSG   =   06  GO       ROT-ST06-10.
           IF  TCL-MSG   =   53  GO       ROT-PGUP-00.
           IF  TCL-MSG   =   54  GO       ROT-PGDW-00.
           IF  TCL-MSG   =   23  GO       ROT-INCL-00.
           IF  TCL-MSG   =   17  PERFORM  ROT-READ-00
                                 GO       ROT-EXCL-00.
           IF  TCL-MSG   >   00  GO       ROT-MENU-30.
           IF  HEX-MSG   =   27  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
       ROT-READ-00.
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX
           MOVE     REG-TAB      (IND3)   TO  REG-AUX
           MOVE     CHV-TAB      (IND3)   TO  COD-001
           READ     CADLCX
           MOVE     PRX-LCX  TO  PRX-EST
           MOVE     COD-001  TO  SUF-EST
           READ     CADEST
           MOVE     QTD-LCX  TO  QTD-LCT.

       ROT-ALTR-00.
           ACCEPT   QTD-A01      DISPLAY  QTD-A02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  MENU-03
                                 GO       ROT-MENU-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ALTR-00.
           IF  QTD-LCT   =    0  GO       ROT-ALTR-00.
           COMPUTE  QTD-PRS   =  QTD-PRS
                              -  QTD-LCX  +   QTD-LCT
           COMPUTE  CTD-PRS   =  CTD-PRS
                              - (QTD-LCX  *   CTM-EST)
                              + (QTD-LCT  *   CTM-EST)
           COMPUTE  VDD-PRS   =  VDD-PRS
                              - (QTD-LCX  *   VDA-EST)
                              + (QTD-LCT  *   VDA-EST)
           COMPUTE  VAL-AUX   = (QTD-LCT  *   VDA-EST)
           MOVE     QTD-LCT  TO  QTD-LCX      QTD-AUX
           MOVE     DES-EST  TO  HST-LCX
           MOVE     VDA-EST  TO  VAL-LCX
           MOVE     REG-AUX  TO  REG-TAB      (IND3)
           DISPLAY  MENU-03      TELA-02
           REWRITE  REG-LCX      GO       ROT-MENU-30.

       ROT-EXCL-00.
           COMPUTE  QTD-PRS   =  QTD-PRS  -   QTD-LCX
           COMPUTE  CTD-PRS   =  CTD-PRS
                              - (QTD-LCX  *   CTM-EST)
           COMPUTE  VDD-PRS   =  VDD-PRS
                              - (QTD-LCX  *   VDA-EST)
           DELETE   CADLCX       DISPLAY      TELA-02
           IF  QTD-PRS   >    0  GO       ROT-MENU-00.
           PERFORM  ROT-RTLA-00  GO       ROT-EXIT-00.

       ROT-NOME-00.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  SUF-EST
           DISPLAY  TELA-03      SUF-I02
           MOVE     26       TO  IND1.

       ROT-NOME-10.
           ACCEPT   PSQ-I01      DISPLAY  PSQ-I02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-INCL-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-NOME-10.
           IF  NOM-PSQ   =       SPACES
                                 GO       ROT-NOME-10.
       ROT-NOME-20.
           COMPUTE  IND1      =  IND1  -  1
           IF  LET-PSQ   (IND1)  =        SPACES
                                 GO       ROT-NOME-20.
           MOVE     PRX-PRS  TO  PRX-EST
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-NOME-30.
           MOVE     22       TO  IND-MSG
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           READ     CADEST  NEXT
           IF  PRX-EST  NOT  =   PRX-PRS
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           DISPLAY  SUF-I02      MOVE  0  TO  ACM-PSQ
           INSPECT  DES-EST      TALLYING     ACM-PSQ
                                 FOR  ALL     NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-NOME-30.
           PERFORM  ROT-INCL-10  GO       ROT-NOME-50.

       ROT-NOME-40.
           MOVE     22       TO  IND-MSG
           START    CADEST       KEY  <   CHV-EST
           IF  FST-EST   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           READ     CADEST       PREVIOUS
           IF  PRX-EST  NOT  =   PRX-PRS
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           DISPLAY  SUF-I02      MOVE  0  TO  ACM-PSQ
           INSPECT  DES-EST      TALLYING     ACM-PSQ
                                 FOR  ALL     NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-NOME-40.
           PERFORM  ROT-INCL-10.

       ROT-NOME-50.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-NOME-50.
           IF  TCL-MSG   =   05  GO       ROT-NOME-40.
           IF  TCL-MSG   =   06  GO       ROT-NOME-30.
           IF  TCL-MSG   >   00  GO       ROT-NOME-50.
           IF  HEX-MSG   =   27  GO       ROT-NOME-00.
           MOVE  0  TO  QTD-LCT  GO       ROT-INCL-30.

       ROT-ST05-00.
           START    CADEST       KEY  <   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-INCL-20.
           READ     CADEST       PREVIOUS
           IF  PRX-EST       =   PRX-PRS
                                 GO       ROT-INCL-10.
           READ     CADEST  NEXT GO       ROT-INCL-20.

       ROT-ST05-10.
           IF  IND3      =   01  GO       ROT-MENU-30.
           IF  IND4      =   10  SUBTRACT 1 FROM IND2
                                 SUBTRACT 1 FROM IND3
                                 GO       ROT-MENU-20.
           DISPLAY  MENU-02      SUBTRACT 1 FROM IND3
           COMPUTE  IND4     =   IND4  -  1
           DISPLAY  MENU-03      GO       ROT-MENU-30.

       ROT-ST06-00.
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-INCL-20.
           READ     CADEST  NEXT
           IF  PRX-EST       =   PRX-PRS
                                 GO       ROT-INCL-10.
           READ  CADEST PREVIOUS GO       ROT-INCL-20.

       ROT-ST06-10.
           IF  IND3      =  IND1 GO       ROT-MENU-30.
           IF  IND4      =   15  ADD      1  TO  IND2
                                 ADD      1  TO  IND3
                                 GO       ROT-MENU-20.
           DISPLAY  MENU-02      ADD      1  TO  IND3
           COMPUTE  IND4     =   IND4  +  1
           DISPLAY  MENU-03      GO       ROT-MENU-30.

       ROT-PGUP-00.
           IF  IND2      =   01  GO       ROT-MENU-30.
           IF  IND2      >   10  COMPUTE  IND3  =   IND2  -  06
                                 COMPUTE  IND2  =   IND2  -  06
           ELSE
                                 MOVE     1     TO  IND2   IND3.
           MOVE     10  TO  IND4 GO       ROT-MENU-20.

       ROT-PGDW-00.
           IF  IND1      <      (IND2  +  06)
                                 GO       ROT-MENU-30.
           COMPUTE  IND3     =   IND2  +  06
           COMPUTE  IND2     =   IND2  +  06
           MOVE     10  TO  IND4 GO       ROT-MENU-20.

       ROT-ZERA-00.
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1   <     100  ADD   1  TO     IND1
                                 GO       ROT-ZERA-00.
           MOVE     00       TO  IND1
           MOVE     10       TO  IND4
           MOVE     01       TO  IND2     IND3.

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
           CLOSE    CADEST   CADLCX   CADPRS   GOBACK.
