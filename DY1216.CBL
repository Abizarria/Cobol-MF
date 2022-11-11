       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1216.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADDSP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-DSP
                       FILE          STATUS  IS  FST-DSP.

           SELECT      CADLCX        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCX
                       FILE          STATUS  IS  FST-LCX.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP
                       FILE          STATUS  IS  FST-LDP.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS
                       FILE          STATUS  IS  FST-PRS.

           SELECT      CADSRT        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-SRT.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADDSP.CPY.
       COPY  CADLCX.CPY.
       COPY  CADLDP.CPY.
       COPY  CADPRS.CPY.

       SD  CADSRT.

       01  REG-SRT.
           03  CHV-SRT.
               05  PRX-SRT      PIC  9(01).
               05  SUF-SRT      PIC  9(03).
               05  DTA-SRT      PIC  9(06).
               05  LCT-SRT      PIC  9(03).
           03  HST-SRT          PIC  X(25).
           03  VAL-SRT          PIC  9(07)V99.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-DSP          PIC  X(02).
           03  FST-LCX          PIC  X(02).
           03  FST-LDP          PIC  X(02).
           03  FST-PRS          PIC  X(02).
           03  FST-SRT          PIC  X(02).

           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  IND3             PIC  9(03).
           03  IND4             PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  ACM-PSQ          PIC  9(02).
           03  VAL-AUX          PIC  9(06)V99.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  REG-AX1.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  PRX-AX1      PIC  X(01).
               05  FILLER       PIC  X(01)  VALUE  "-".
               05  SUF-AX1      PIC  9(03).
               05  FILLER       PIC  X(04)  VALUE  SPACES.
               05  HST-AX1      PIC  X(28).
               05  VAL-AX1      PIC  ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  REG-AX2.
               05  FLG-AX2      PIC  X(01)  VALUE  SPACES.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  DIA-AX2      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AX2      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-AX2      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  HST-AX2      PIC  X(25).
               05  VAL-AX2      PIC  Z.ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  TAB-REG          OCCURS      300.
               05  REG-TAB      PIC  X(49).
               05  CHV-TAB      PIC  X(13).
               05  RED-CHV      REDEFINES   CHV-TAB.
                   07  PRX-TAB  PIC  9(01).
                   07  SUF-TAB  PIC  9(03).
                   07  LCT-TAB  PIC  9(03).
                   07  FILLER   PIC  X(06).

           03  NOM-PSQ          PIC  X(25).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  25.

           03  TAB-ATV.
               05  FILLER       PIC  X(18)  VALUE  "OBRIG. TRABALHISTA".
               05  FILLER       PIC  X(18)  VALUE  "OBRIGA€åES FISCAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESP. OPERACIONAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESPESA DE CAPITAL".
               05  FILLER       PIC  X(18)  VALUE  "CREDITOS DE SàCIOS".
               05  FILLER       PIC  X(18)  VALUE  "COMPRA DE PRODUTOS".
           03  RED-ATV          REDEFINES   TAB-ATV.
               05  ATV-TAB      PIC  X(18)  OCCURS  6.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1216".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-PSQ.
           03  OPC-PSQ          PIC  9(01)  VALUE  2.
           03  IND-PSQ          PIC  X(01)  VALUE  "1".
           03  LIN-PSQ          PIC  9(02)  VALUE  08.
           03  COL-PSQ          PIC  9(02)  VALUE  35.
           03  SB1-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB2-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB3-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB4-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB5-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB6-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB7-PSQ          PIC  X(26)  VALUE  SPACES.

       01  PRM-DTA.
           03  DTA-DTA.
               05  ANO-DTA      PIC  9(02).
               05  MES-DTA      PIC  9(02).
               05  DIA-DTA      PIC  9(02).
           03  LIN-DTA          PIC  9(02)  VALUE  10.
           03  COL-DTA          PIC  9(02)  VALUE  35.
           03  BCK-DTA          PIC  9(01)  VALUE  2.
           03  FOR-DTA          PIC  9(01)  VALUE  7.
           03  FLG-DTA          PIC  9(01).

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

       01  TELA-03  BCK-CLR 02 FOR-CLR 14.
           03  LINE 10 COLUMN 65 PIC X(01) FROM SPACES.
           03  LINE 15 COLUMN 65 PIC X(01) FROM SPACES.
           03  LINE 10 COLUMN 16 PIC X(49) FROM
               " C¢digo da Conta :  -     -".
           03  LINE 11 COLUMN 16 PIC X(49) FROM SPACES.
           03  LINE 12 COLUMN 16 PIC X(49) FROM " Descri‡Æo ......:".
           03  LINE 13 COLUMN 16 PIC X(49) FROM " Hist¢rico ......:".
           03  LINE 14 COLUMN 16 PIC X(49) FROM SPACES.
           03  LINE 15 COLUMN 16 PIC X(49) FROM " Valor Pago .....:".

       01  MENU-01 BCK-CLR 02 FOR-CLR 15.
           03  LINE 10 COLUMN 16 PIC X(49) FROM REG-TAB (IND2).
           03  LINE 11 COLUMN 16 PIC X(49) FROM REG-TAB (IND2 + 1).
           03  LINE 12 COLUMN 16 PIC X(49) FROM REG-TAB (IND2 + 2).
           03  LINE 13 COLUMN 16 PIC X(49) FROM REG-TAB (IND2 + 3).
           03  LINE 14 COLUMN 16 PIC X(49) FROM REG-TAB (IND2 + 4).
           03  LINE 15 COLUMN 16 PIC X(49) FROM REG-TAB (IND2 + 5).

       01  MENU-02  BCK-CLR 02 FOR-CLR 15
                    LINE IND4 COLUMN 16 PIC X(49) FROM REG-TAB (IND3).

       01  MENU-03  BCK-CLR 07 FOR-CLR 12
                    LINE IND4 COLUMN 16 PIC X(49) FROM REG-TAB (IND3).

       01  SETA-01  BCK-CLR 02.
           03  TRC-ACM LINE 10 COLUMN 65 VALUE " ".
           03  TRC-ABX LINE 15 COLUMN 65 VALUE " ".
           03  STA-ACM LINE 10 COLUMN 65 VALUE "" FOR-CLR 14.
           03  STA-ABX LINE 15 COLUMN 65 VALUE "" FOR-CLR 14.

       01  ENTR-01     AUTO.
           03  SUF-I01 LINE 10   COLUMN 37 PIC ZZ9        USING SUF-DSP.
           03  PSQ-I01 LINE 12   COLUMN 35 PIC X(25)      USING NOM-PSQ.
           03  HST-I01 LINE 13   COLUMN 35 PIC X(25)      USING HST-LCX.
           03  VAL-I01 LINE 15   COLUMN 35 PIC ZZZ.ZZ9,99 USING VAL-LCX.

       01  ENTR-02  BCK-CLR 02 FOR-CLR 15.
           03  PRX-I02 LINE 10 COLUMN 35 PIC 9(01)      FROM PRX-DSP.
           03  ATV-I02 LINE 10 COLUMN 45 PIC X(18)      FROM ATV-TAB
                                                            (PRX-DSP).
           03  SUF-I02 LINE 10 COLUMN 37 PIC 9(03)      FROM SUF-DSP.
           03  PSQ-I02 LINE 12 COLUMN 35 PIC X(25)      FROM NOM-PSQ.
           03  DES-I02 LINE 12 COLUMN 35 PIC X(25)      FROM DES-DSP.
           03  HST-I02 LINE 13 COLUMN 35 PIC X(25)      FROM HST-LCX.
           03  VAL-I02 LINE 15 COLUMN 35 PIC ZZZ.ZZ9,99 FROM VAL-LCX.

       01  ENTR-03     AUTO.
           03  HST-A01 LINE IND4 COLUMN 26 PIC X(25)      USING HST-LCX.
           03  VAL-A01 LINE IND4 COLUMN 54 PIC ZZZ.ZZ9,99 USING VAL-LCX.

       01  ENTR-04  BCK-CLR 07   FOR-CLR 12.
           03  HST-A02 LINE IND4 COLUMN 26 PIC X(25)      FROM HST-LCX.
           03  VAL-A02 LINE IND4 COLUMN 54 PIC ZZZ.ZZ9,99 FROM VAL-LCX.

       01  LIMP-01  BCK-CLR 02.
           03  LINE 12 COLUMN 35 PIC X(25) FROM SPACES.
           03  LINE 13 COLUMN 35 PIC X(25) FROM SPACES.
           03  LINE 15 COLUMN 35 PIC X(10) FROM SPACES.

       PROCEDURE       DIVISION  USING  PRM-MVC.

       ROT-0000-00.
           OPEN     INPUT        CADDSP   CADLDP
                    I-O          CADLCX   CADPRS
           MOVE     PRS-MVC  TO  CHV-PRS
           READ     CADPRS       PERFORM  ROT-GTLA-00
           IF  PRX-PRS   =  "2"  GO       RT2-PESQ-00.
           DISPLAY  TELA-01      TELA-02
           IF  QTD-PRS   >   0   GO       RT1-PESQ-00.

       RT1-INCL-00.
           DISPLAY  TELA-03      PERFORM  ROT-PESQ-00
           IF  IND-PSQ   <    1
               IF   QTD-PRS   >  ZEROS
                                 GO       RT1-PESQ-00
               ELSE
                                 PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
           MOVE     IND-PSQ  TO  PRX-DSP
           DISPLAY  PRX-I02      ATV-I02.

       RT1-INCL-10.
           DISPLAY  LIMP-01      MOVE  0  TO  SUF-DSP
           ACCEPT   SUF-I01      DISPLAY      SUF-I02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-INCL-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-INCL-10.
           IF  SUF-DSP   <   01  GO       ROT-NOME-00.
           MOVE     IND-PSQ  TO  PRX-DSP
           READ     CADDSP       MOVE 21  TO  IND-MSG
           IF  FST-DSP   =  "00" GO       ROT-CODE-00.
           PERFORM  ROT-MSGM-00  GO       RT1-INCL-10.

       RT1-INCL-20.
           MOVE     SPACES   TO  HST-LCX
           MOVE     ZEROS    TO  VAL-LCX.

       RT1-INCL-30.
           ACCEPT   HST-I01      DISPLAY  HST-I02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-INCL-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-INCL-30.
           IF  HST-LCX   =       SPACES
                                 GO       RT1-INCL-30.
       RT1-INCL-40.
           ACCEPT   VAL-I01      DISPLAY  VAL-I02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-INCL-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-INCL-40.
           IF  VAL-LCX   =    0  GO       RT1-INCL-40.
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX
           MOVE     PRX-DSP  TO  PRX-005
           MOVE     SUF-DSP  TO  SUF-005
           MOVE     1        TO  LCT-005.

       RT1-INCL-50.
           WRITE    REG-LCX      ADD   1  TO  LCT-005
           IF  FST-LCX   =  "22" GO       RT1-INCL-50.
           ADD      1        TO  QTD-PRS
           ADD      VAL-LCX  TO  VDD-PRS
           DISPLAY  TELA-02      GO       RT1-INCL-10.

       RT1-PESQ-00.
           MOVE     1  TO  IND1  PERFORM  ROT-ZERA-00
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX.

       RT1-PESQ-10.
           START    CADLCX       KEY  >   CHV-LCX
           IF  FST-LCX   =  "23" GO       RT1-MENU-00.
           READ     CADLCX  NEXT
           IF  TUR-LCX  NOT   =  TUR-PRS  OR
               CXA-LCX  NOT   =  CXA-PRS  OR
               ATV-LCX  NOT   =  ATV-PRS  OR  PRX-LCX
               NOT  =   PRX-PRS  GO       RT1-MENU-00.
           MOVE     PRX-005  TO  PRX-AX1
           MOVE     SUF-005  TO  SUF-AX1
           MOVE     HST-LCX  TO  HST-AX1
           MOVE     VAL-LCX  TO  VAL-AX1
           ADD      1        TO  IND1
           MOVE     REG-AX1  TO  REG-TAB  (IND1)
           MOVE     PRX-005  TO  PRX-TAB  (IND1)
           MOVE     SUF-005  TO  SUF-TAB  (IND1)
           MOVE     LCT-005  TO  LCT-TAB  (IND1)
           GO       RT1-PESQ-10.

       RT1-MENU-00.
           DISPLAY  TRC-ACM      TRC-ABX
           IF (IND2  +  05)  <   IND1
                                 DISPLAY  STA-ABX.
           IF  IND2  >  01       DISPLAY  STA-ACM.
           DISPLAY  MENU-01      MENU-03.

       RT1-MENU-10.
           MOVE  5  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       RT1-MENU-10.
           IF  TCL-MSG   =   05  GO       ROT-ST05-00.
           IF  TCL-MSG   =   06  GO       ROT-ST06-00.
           IF  TCL-MSG   =   53  GO       ROT-PGUP-00.
           IF  TCL-MSG   =   54  GO       ROT-PGDW-00.
           IF  TCL-MSG   =   23  GO       RT1-INCL-00.
           IF  TCL-MSG   =   17  PERFORM  RT1-READ-00
                                 GO       RT1-EXCL-00.
           IF  TCL-MSG   >   00  GO       RT1-MENU-10.
           IF  HEX-MSG   =   27  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
       RT1-READ-00.
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX
           MOVE     PRX-TAB      (IND3)   TO  PRX-005
           MOVE     SUF-TAB      (IND3)   TO  SUF-005
           MOVE     LCT-TAB      (IND3)   TO  LCT-005
           MOVE     REG-TAB      (IND3)   TO  REG-AX1
           READ     CADLCX
           MOVE     VAL-LCX  TO  VAL-AUX.

       RT1-ALTR-00.
           ACCEPT   HST-A01      DISPLAY  HST-A02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  MENU-03
                                 GO       RT1-MENU-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-ALTR-00.
           IF  HST-AX1   =       SPACES
                                 GO       RT1-ALTR-00.
       RT1-ALTR-10.
           ACCEPT   VAL-A01      DISPLAY  VAL-A02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-ALTR-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-ALTR-10.
           IF  VAL-LCX   =    0  GO       RT1-ALTR-10.
           MOVE     HST-LCX  TO  HST-AX1
           MOVE     VAL-LCX  TO  VAL-AX1
           MOVE     REG-AX1  TO  REG-TAB  (IND3)
           COMPUTE  VDD-PRS   =  VDD-PRS
                              -  VAL-AUX  +   VAL-LCX
           REWRITE  REG-LCX
           DISPLAY  TELA-02      GO       RT1-MENU-10.

       RT1-EXCL-00.
           COMPUTE  QTD-PRS   =  QTD-PRS  -   1
           COMPUTE  VDD-PRS   =  VDD-PRS  -   VAL-LCX
           DELETE   CADLCX       DISPLAY      TELA-02
           IF  QTD-PRS   >    0  GO       RT1-PESQ-00.
           PERFORM  ROT-RTLA-00  GO       ROT-EXIT-00.

       RT2-PESQ-00.
           MOVE     SPACES   TO  CHV-LDP
           SORT     CADSRT   ON  ASCENDING  KEY  DTA-SRT
                                                 PRX-SRT
                                                 SUF-SRT
                    INPUT        PROCEDURE  IS   RT2-SORT-00
                    OUTPUT       PROCEDURE  IS   RT2-SORT-30
           IF  IND1      <    1  PERFORM  ROT-MSGM-00
                                 GO       ROT-EXIT-00.
           DISPLAY  TELA-01      TELA-02.

       RT2-MENU-00.
           DISPLAY  TRC-ACM      TRC-ABX
           IF (IND2  +  05)  <   IND1
                                 DISPLAY  STA-ABX.
           IF  IND2  >  01       DISPLAY  STA-ACM.
           DISPLAY  MENU-01      MENU-03.

       RT2-MENU-10.
           MOVE  6  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       RT2-MENU-10.
           IF  TCL-MSG   =   05  GO       ROT-ST05-00.
           IF  TCL-MSG   =   06  GO       ROT-ST06-00.
           IF  TCL-MSG   =   53  GO       ROT-PGUP-00.
           IF  TCL-MSG   =   54  GO       ROT-PGDW-00.
           IF  TCL-MSG   >   00  GO       RT2-MENU-10.
           IF  HEX-MSG   =   27  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
       RT2-ALTR-00.
           MOVE     REG-TAB      (IND3)   TO  REG-AX2
           MOVE     CHV-TAB      (IND3)   TO  CHV-LDP
           READ     CADLDP
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX
           MOVE     PRX-LDP  TO  PRX-006
           MOVE     SUF-LDP  TO  SUF-006
           MOVE     DTA-LDP  TO  DTA-006
           MOVE     LCT-LDP  TO  LCT-006
           READ     CADLCX       INVALID  KEY
                                 GO       RT2-ALTR-10.
           COMPUTE  QTD-PRS   =  QTD-PRS   -  1
           COMPUTE  VDD-PRS   =  VDD-PRS   -  VAL-LDP
           MOVE     SPACES   TO  FLG-AX2
           MOVE     REG-AX2  TO  REG-TAB  (IND3)
           DISPLAY  MENU-03      TELA-02
           DELETE   CADLCX       GO       ROT-ST06-00.

       RT2-ALTR-10.
           COMPUTE  QTD-PRS   =  QTD-PRS   +  1
           COMPUTE  VDD-PRS   =  VDD-PRS   +  VAL-LDP
           MOVE     ""      TO  FLG-AX2
           MOVE     REG-AX2  TO  REG-TAB  (IND3)
           MOVE     HST-LDP  TO  HST-LCX
           MOVE     VAL-LDP  TO  VAL-LCX
           DISPLAY  MENU-03      TELA-02
           WRITE    REG-LCX      GO       ROT-ST06-00.

       RT2-SORT-00  SECTION.

       RT2-SORT-10.
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       RT2-SORT-20.
           READ     CADLDP  NEXT
           IF  FLG-LDP  NOT   =  "P"
                                 GO       RT2-SORT-10.
           MOVE     CHV-LDP  TO  CHV-SRT
           MOVE     HST-LDP  TO  HST-SRT
           MOVE     VAL-LDP  TO  VAL-SRT
           RELEASE  REG-SRT      GO       RT2-SORT-10.

       RT2-SORT-20.
           MOVE     1  TO  IND1  PERFORM  ROT-ZERA-00.

       RT2-SORT-30  SECTION.

       RT2-SORT-40.
           RETURN   CADSRT
           IF  FST-SRT   =  "10" GO       RT2-SORT-60.
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX  FLG-AX2
           MOVE     PRX-SRT  TO  PRX-006
           MOVE     SUF-SRT  TO  SUF-006
           MOVE     DTA-SRT  TO  DTA-006
           MOVE     LCT-SRT  TO  LCT-006
           READ     CADLCX       INVALID  KEY
                                 GO       RT2-SORT-50.
           MOVE     ""      TO  FLG-AX2.

       RT2-SORT-50.
           MOVE     DTA-SRT  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-AX2
           MOVE     MES-AUX  TO  MES-AX2
           MOVE     ANO-AUX  TO  ANO-AX2
           MOVE     HST-SRT  TO  HST-AX2
           MOVE     VAL-SRT  TO  VAL-AX2
           ADD      1        TO  IND1
           MOVE     CHV-SRT  TO  CHV-TAB (IND1)
           MOVE     REG-AX2  TO  REG-TAB (IND1)
           GO       RT2-SORT-40.

       RT2-SORT-60.
           MOVE     31       TO  IND-MSG.

       RT2-SORT-70  SECTION.

       ROT-CODE-00.
           DISPLAY  SUF-I02      DES-I02.

       ROT-CODE-10.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           IF  TCL-MSG   =   05  GO       ROT-CODE-20.
           IF  TCL-MSG   =   06  GO       ROT-CODE-30.
           IF  TCL-MSG   >   00  GO       ROT-CODE-30.
           IF  HEX-MSG   =   27  GO       RT1-INCL-10.
           GO       RT1-INCL-20.

       ROT-CODE-20.
           START    CADDSP       KEY  <   CHV-DSP
           IF  FST-DSP   =  "23" GO       ROT-CODE-10.
           READ     CADDSP       PREVIOUS
           IF  PRX-DSP       =   IND-PSQ
                                 GO       ROT-CODE-00.
           READ     CADDSP  NEXT GO       ROT-CODE-10.

       ROT-CODE-30.
           START    CADDSP       KEY  >   CHV-DSP
           IF  FST-DSP   =  "23" GO       ROT-CODE-10.
           READ     CADDSP  NEXT
           IF  PRX-DSP       =   IND-PSQ
                                 GO       ROT-CODE-00.
           READ CADDSP PREVIOUS  GO       ROT-CODE-10.

       ROT-NOME-00.
           MOVE     ZEROS    TO  SUF-DSP
           MOVE     SPACES   TO  NOM-PSQ
           DISPLAY  SUF-I02      MOVE     26  TO IND1.

       ROT-NOME-10.
           ACCEPT   PSQ-I01      DISPLAY  PSQ-I02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-INCL-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-NOME-10.
           IF  NOM-PSQ   =       SPACES
                                 GO       ROT-NOME-10.
       ROT-NOME-20.
           COMPUTE  IND1      =  IND1  -  1
           IF  LET-PSQ (IND1)    =        SPACES
                                 GO       ROT-NOME-20.
           MOVE     IND-PSQ  TO  PRX-DSP
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-NOME-30.
           MOVE     22       TO  IND-MSG
           START    CADDSP       KEY  >   CHV-DSP
           IF  FST-DSP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           READ     CADDSP  NEXT
           IF  PRX-DSP  NOT  =   IND-PSQ
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           DISPLAY  SUF-I02      MOVE  0  TO  ACM-PSQ
           INSPECT  DES-DSP      TALLYING     ACM-PSQ
                                 FOR  ALL     NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-NOME-30.
           PERFORM  ROT-CODE-00  GO       ROT-NOME-50.

       ROT-NOME-40.
           MOVE     22       TO  IND-MSG
           START    CADDSP       KEY  <   CHV-DSP
           IF  FST-DSP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           READ     CADDSP       PREVIOUS
           IF  PRX-DSP  NOT  =   IND-PSQ
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           DISPLAY  SUF-I02      MOVE  0  TO  ACM-PSQ
           INSPECT  DES-DSP      TALLYING     ACM-PSQ
                                 FOR  ALL     NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-NOME-40.
           DISPLAY  SUF-I02      DES-I02.

       ROT-NOME-50.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-NOME-50.
           IF  TCL-MSG   =   05  GO       ROT-NOME-40.
           IF  TCL-MSG   =   06  GO       ROT-NOME-30.
           IF  TCL-MSG   >   00  GO       ROT-NOME-50.
           IF  HEX-MSG   =   27  GO       ROT-NOME-00.
           GO       RT1-INCL-20.

       ROT-ZERA-00.
           MOVE     SPACES   TO  REG-TAB  (IND1)
                                 CHV-TAB  (IND1)
           IF  IND1    <    300  ADD      1  TO  IND1
                                 GO       ROT-ZERA-00.
           MOVE     00       TO  IND1
           MOVE     10       TO  IND4
           MOVE     01       TO  IND2     IND3.

       ROT-ST05-00.
           IF  IND3  =   01      GO       ROT-MOVE-10.
           IF  IND4  =   10      SUBTRACT 1 FROM IND2
                                 SUBTRACT 1 FROM IND3
                                 GO       ROT-MOVE-00.
           DISPLAY  MENU-02      SUBTRACT 1 FROM IND3
                                 SUBTRACT 1 FROM IND4
           DISPLAY  MENU-03      GO       ROT-MOVE-10.

       ROT-ST06-00.
           IF  IND3  =  IND1     GO       ROT-MOVE-10.
           IF  IND4  =   15      ADD      1  TO  IND2
                                 ADD      1  TO  IND3
                                 GO       ROT-MOVE-00.
           DISPLAY  MENU-02      ADD      1  TO  IND3
                                 ADD      1  TO  IND4
           DISPLAY  MENU-03      GO       ROT-MOVE-10.

       ROT-PGUP-00.
           IF  IND2  =   01      GO       ROT-MOVE-10.
           IF  IND2  >   10      COMPUTE  IND3  =
                                          IND2  -  06
                                 COMPUTE  IND2  =
                                          IND2  -  06
           ELSE
                                 MOVE     1  TO  IND2
                                                 IND3.
           MOVE  10  TO  IND4    GO       ROT-MOVE-00.

       ROT-PGDW-00.
           IF  IND1         <            (IND2  +  06)
                                 GO       ROT-MOVE-10.
           COMPUTE  IND3    =    IND2  +  06
           COMPUTE  IND2    =    IND2  +  06
           MOVE  10  TO  IND4    GO       ROT-MOVE-00.

       ROT-MOVE-00.
           IF  PRX-PRS   =  "1"  GO       RT1-MENU-00.
           IF  PRX-PRS   =  "2"  GO       RT2-MENU-00.

       ROT-MOVE-10.
           IF  PRX-PRS   =  "1"  GO       RT1-MENU-10.
           IF  PRX-PRS   =  "2"  GO       RT2-MENU-10.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-PESQ-00.
           CALL    "DYPESQ"  USING  PRM-PSQ
                                    PRM-HLP.
       ROT-DATA-00.
           CALL    "DYDATA"  USING  PRM-DTA
                                    PRM-HLP.
       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-EXIT-00.
           REWRITE  REG-PRS
           CLOSE    CADDSP   CADLDP
                    CADLCX   CADPRS  GOBACK.
