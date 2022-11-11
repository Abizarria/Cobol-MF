       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1253.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCMB        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CMB
                       FILE          STATUS  IS  FST-CMB.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADCXA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CXA
                       FILE          STATUS  IS  FST-CXA.

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

           SELECT      CADLCP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCP
                       FILE          STATUS  IS  FST-LCP.

           SELECT      CADLCX        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCX
                       FILE          STATUS  IS  FST-LCX.

           SELECT      CADLES        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LES
                       FILE          STATUS  IS  FST-LES.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS
                       FILE          STATUS  IS  FST-PRS.

           SELECT      CADTNQ        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TNQ
                       FILE          STATUS  IS  FST-TNQ.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCMB.CPY.
       COPY  CADCTR.CPY.
       COPY  CADCXA.CPY.
       COPY  CADECR.CPY.
       COPY  CADEST.CPY.
       COPY  CADLCP.CPY.
       COPY  CADLCX.CPY.
       COPY  CADLES.CPY.
       COPY  CADPRS.CPY.
       COPY  CADTNQ.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CMB          PIC  X(02).
           03  FST-CTR          PIC  X(02).
           03  FST-CXA          PIC  X(02).
           03  FST-ECR          PIC  X(02).
           03  FST-EST          PIC  X(02).
           03  FST-LCP          PIC  X(02).
           03  FST-LCX          PIC  X(02).
           03  FST-LES          PIC  X(02).
           03  FST-PRS          PIC  X(02).
           03  FST-TNQ          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  IND3             PIC  9(02).
           03  IND4             PIC  9(02).
           03  IND5             PIC  9(02).
           03  IND6             PIC  9(02).
           03  IND7             PIC  9(02).
           03  IND8             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  SDO-AUX          PIC  9(05)V9.
           03  ACM-CNT          PIC S9(06)V9  VALUE  ZEROS.
           03  ACM-FIS          PIC S9(06)V9  VALUE  ZEROS.
           03  ACM-DIF          PIC S9(06)V9  VALUE  ZEROS.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  REG-AUX.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  NUM-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  QTD-AUX      PIC  ---.--9,9.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  TAB-CMB          OCCURS  15.
               05  CMB-TBC.
                   07  FILLER   PIC  X(01).
                   07  DES-TBC  PIC  X(16).
               05  SDI-TBC      PIC  9(06)V9.
               05  CMP-TBC      PIC S9(06)V9.
               05  VDA-TBC      PIC  9(06)V9.
               05  SDO-TBC      PIC S9(06)V9.
               05  SDF-TBC      PIC  9(06)V9.
               05  DIF-TBC      PIC S9(06)V9.

           03  TAB-TNQ          PIC  X(14)  OCCURS  20.

           03  TAB-REL.
               05  FILLER       PIC  X(15)  VALUE  " DIµRIO VENDAS ".
               05  FILLER       PIC  X(15)  VALUE  " FOLHAS L.M.C. ".
               05  FILLER       PIC  X(15)  VALUE  " ANALÖTICO DIA ".
           03  RED-REL          REDEFINES   TAB-REL.
               05  REL-TAB      PIC  X(15)  OCCURS  03.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1253".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  19.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-NOM.
           03  COD-NOM          PIC  9(06).
           03  CLI-NOM          PIC  X(40).
           03  CGC-NOM          PIC  9(14).
           03  INS-NOM          PIC  X(15).
           03  END-NOM          PIC  X(40).
           03  CEP-NOM          PIC  9(08).
           03  CID-NOM          PIC  X(25).
           03  EST-NOM          PIC  X(02).

       01  PRM-CAB.
           03  SEM-CAB          PIC  X(04).
           03  DIA-CAB          PIC  9(02).
           03  B01-CAB          PIC  X(01).
           03  MES-CAB          PIC  9(02).
           03  B02-CAB          PIC  X(01).
           03  ANO-CAB          PIC  9(02).

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
           03  LINE 04 COLUMN 03 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 06 COLUMN 13 VALUE " Administra‡Æo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 07 COLUMN 15 VALUE " Controle de Receitas ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 08 COLUMN 17 VALUE " EmissÆo de Relat¢rio ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 06 COLUMN 35 VALUE "ß".
               05  LINE 07 COLUMN 37 VALUE "ß".
               05  LINE 08 COLUMN 39 VALUE "ß".
               05  LINE 07 COLUMN 14 VALUE "Ü".
               05  LINE 08 COLUMN 16 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 09 COLUMN 13 VALUE " ÉÍÍ
      -            " ÍÍÍ                ÍÍÍ» ".
               05  LINE 10 COLUMN 13 VALUE " º
      -            "                          º ".
               05  LINE 11 COLUMN 13 VALUE " º
      -            "                          º ".
               05  LINE 12 COLUMN 13 VALUE " º
      -            "                          º ".
               05  LINE 13 COLUMN 13 VALUE " º
      -            "                          º ".
               05  LINE 14 COLUMN 13 VALUE " º
      -            "                          º ".
               05  LINE 15 COLUMN 13 VALUE " º
      -            "                          º ".
               05  LINE 16 COLUMN 13 VALUE " º
      -            "                          º ".
               05  LINE 17 COLUMN 13 VALUE " º
      -            "                          º ".
               05  LINE 18 COLUMN 13 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -            "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 09 COLUMN 19 PIC X(22) FROM
                   " Di rio de Vendas/LMC".
               05  LINE 09 COLUMN 48 PIC X(01) FROM SPACES.
               05  LINE 09 COLUMN 49 PIC X(04) FROM SEM-CAB.
               05  LINE 09 COLUMN 53 PIC 9(02) FROM DIA-AUX.
               05  LINE 09 COLUMN 55 PIC X(01) FROM "/".
               05  LINE 09 COLUMN 56 PIC 9(02) FROM MES-AUX.
               05  LINE 09 COLUMN 58 PIC X(01) FROM "/".
               05  LINE 09 COLUMN 59 PIC 9(02) FROM ANO-AUX.
               05  LINE 09 COLUMN 61 PIC X(01) FROM SPACES.
           03  BCK-CLR  00.
               05  LINE 10 COLUMN 69 VALUE "  ".
               05  LINE 11 COLUMN 69 VALUE "  ".
               05  LINE 12 COLUMN 69 VALUE "  ".
               05  LINE 13 COLUMN 69 VALUE "  ".
               05  LINE 14 COLUMN 69 VALUE "  ".
               05  LINE 15 COLUMN 69 VALUE "  ".
               05  LINE 16 COLUMN 69 VALUE "  ".
               05  LINE 17 COLUMN 69 VALUE "  ".
               05  LINE 18 COLUMN 69 VALUE "  ".
               05  LINE 19 COLUMN 15 PIC X(56) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 10 COLUMN 17 PIC X(19) FROM ALL "ß".
               05  LINE 17 COLUMN 17 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 11 COLUMN 17 PIC X(19) FROM SPACES.
               05  LINE 11 COLUMN 18 PIC X(17) FROM CMB-TBC (IND2).
               05  LINE 12 COLUMN 17 PIC X(19) FROM SPACES.
               05  LINE 12 COLUMN 18 PIC X(17) FROM CMB-TBC (IND2 + 1).
               05  LINE 13 COLUMN 17 PIC X(19) FROM SPACES.
               05  LINE 13 COLUMN 18 PIC X(17) FROM CMB-TBC (IND2 + 2).
               05  LINE 14 COLUMN 17 PIC X(19) FROM SPACES.
               05  LINE 14 COLUMN 18 PIC X(17) FROM CMB-TBC (IND2 + 3).
               05  LINE 15 COLUMN 17 PIC X(19) FROM SPACES.
               05  LINE 15 COLUMN 18 PIC X(17) FROM CMB-TBC (IND2 + 4).
               05  LINE 16 COLUMN 17 PIC X(19) FROM SPACES.
               05  LINE 16 COLUMN 18 PIC X(17) FROM CMB-TBC (IND2 + 5).
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 11 COLUMN 36 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 36 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 36 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 36 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 36 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 36 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 36 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 18 PIC X(18) FROM ALL "ß".

       01  TELA-03.
           03  BCK-CLR 03 FOR-CLR 07.
               05  LINE 10 COLUMN 39 PIC X(25) FROM ALL "ß".
               05  LINE 17 COLUMN 39 PIC X(01) FROM     "Ü".
           03  BCK-CLR 03 FOR-CLR 15.
               05  LINE 11 COLUMN 39 PIC X(25) FROM "  INICIAL ÄÄ".
               05  LINE 11 COLUMN 53 PIC ZZZ.ZZ9,9  FROM SDI-TBC (IND3).
               05  LINE 12 COLUMN 39 PIC X(25) FROM "  COMPRAS ÄÄ".
               05  LINE 12 COLUMN 52 PIC ----.--9,9 FROM CMP-TBC (IND3).
               05  LINE 13 COLUMN 39 PIC X(25) FROM "  VENDAS ÄÄÄ".
               05  LINE 13 COLUMN 53 PIC ZZZ.ZZ9,9  FROM VDA-TBC (IND3).
               05  LINE 14 COLUMN 39 PIC X(25) FROM "  CONTµBIL Ä".
               05  LINE 14 COLUMN 52 PIC ----.--9,9 FROM SDO-TBC (IND3).
               05  LINE 15 COLUMN 39 PIC X(25) FROM "  FÖSICO ÄÄÄ".
               05  LINE 15 COLUMN 53 PIC ZZZ.ZZ9,9  FROM SDF-TBC (IND3).
           03  BCK-CLR 03 FOR-CLR 14.
               05  LINE 16 COLUMN 39 PIC X(25) FROM "  DIFERÒN€A ".
               05  LINE 16 COLUMN 52 PIC ----.--9,9 FROM DIF-TBC (IND3).
           03  BCK-CLR 00 FOR-CLR 03.
               05  LINE 11 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 40 PIC X(24) FROM ALL "ß".

       01  MENU-01  BCK-CLR 02 FOR-CLR 15.
           03  LINE IND4 COLUMN 18 PIC X(17) FROM CMB-TBC (IND3).

       01  SETA-01  BCK-CLR 07 FOR-CLR 12.
           03  LINE IND4 COLUMN 18 PIC X(17) FROM CMB-TBC (IND3).

       01  PREV-01  BCK-CLR 02.
           03  TR1-ACM LINE 11 COLUMN 35 VALUE " ".
           03  TR1-ABX LINE 16 COLUMN 35 VALUE " ".
           03  ST1-ACM LINE 11 COLUMN 35 VALUE "" FOR-CLR 14.
           03  ST1-ABX LINE 16 COLUMN 35 VALUE "" FOR-CLR 14.

       01  TELA-04  BCK-CLR 03 FOR-CLR 15.
           03  LINE 11 COLUMN 40 PIC X(09) FROM " TANQUES".
           03  LINE 11 COLUMN 49 PIC X(14) FROM TAB-TNQ (IND6).
           03  LINE 12 COLUMN 40 PIC X(09) FROM SPACES.
           03  LINE 12 COLUMN 49 PIC X(14) FROM TAB-TNQ (IND6 + 1).
           03  LINE 13 COLUMN 40 PIC X(09) FROM SPACES.
           03  LINE 13 COLUMN 49 PIC X(14) FROM TAB-TNQ (IND6 + 2).
           03  LINE 14 COLUMN 40 PIC X(09) FROM SPACES.
           03  LINE 14 COLUMN 49 PIC X(14) FROM TAB-TNQ (IND6 + 3).
           03  LINE 15 COLUMN 40 PIC X(09) FROM SPACES.
           03  LINE 15 COLUMN 49 PIC X(14) FROM TAB-TNQ (IND6 + 4).
           03  LINE 16 COLUMN 40 PIC X(09) FROM SPACES.
           03  LINE 16 COLUMN 49 PIC X(14) FROM TAB-TNQ (IND6 + 5).

       01  MENU-02  BCK-CLR 03 FOR-CLR 15.
           03  LINE IND8 COLUMN 49 PIC X(14) FROM TAB-TNQ (IND7).

       01  PREV-02  BCK-CLR 03.
           03  TR2-ACM LINE 11 COLUMN 63 VALUE " ".
           03  TR2-ABX LINE 16 COLUMN 63 VALUE " ".
           03  ST2-ACM LINE 11 COLUMN 63 VALUE "" FOR-CLR 14.
           03  ST2-ABX LINE 16 COLUMN 63 VALUE "" FOR-CLR 14.

       01  ENTR-01 LINE IND8 COLUMN 54 PIC ZZ.ZZ9,9 USING SDO-AUX.

       01  TELA-05.
           03  BCK-CLR 03 FOR-CLR 07.
               05  LINE 10 COLUMN 47 PIC X(17) FROM ALL "ß".
               05  LINE 14 COLUMN 47 PIC X(01) FROM     "Ü".
           03  BCK-CLR 03 FOR-CLR 15.
               05  LINE 11 COLUMN 47 VALUE "  DIµRIO VENDAS  ".
               05  LINE 12 COLUMN 47 VALUE "  FOLHAS L.M.C.  ".
               05  LINE 13 COLUMN 47 VALUE "  ANALÖTICO DIA  ".
           03  BCK-CLR 00 FOR-CLR 03.
               05  LINE 11 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 48 PIC X(16) FROM ALL "ß".
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 10 COLUMN 17 PIC X(27) FROM ALL "ß".
               05  LINE 14 COLUMN 17 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 11 COLUMN 17 PIC X(27) FROM "  CONTµBIL ÄÄ".
               05  LINE 11 COLUMN 32 PIC ----.--9,9 FROM ACM-CNT.
               05  LINE 12 COLUMN 17 PIC X(27) FROM "  FÖSICO ÄÄÄÄ".
               05  LINE 12 COLUMN 32 PIC ----.--9,9 FROM ACM-FIS.
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 13 COLUMN 17 PIC X(27) FROM "  DIFERÒN€A Ä".
               05  LINE 13 COLUMN 32 PIC ----.--9,9 FROM ACM-DIF.
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 11 COLUMN 44 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 44 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 44 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 44 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 18 PIC X(26) FROM ALL "ß".

       01  MENU-03  BCK-CLR 03 FOR-CLR 15.
           03  LINE IND5 COLUMN 48 PIC X(15) FROM REL-TAB (IND6).

       01  SETA-03  BCK-CLR 07 FOR-CLR 12.
           03  LINE IND5 COLUMN 48 PIC X(15) FROM REL-TAB (IND6).

       01  TELA-06.
           03  BCK-CLR 04 FOR-CLR 07.
               05  LINE 15 COLUMN 17 PIC X(47) FROM ALL "ß".
               05  LINE 17 COLUMN 17 PIC X(01) FROM     "Ü".
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 16 COLUMN 17 PIC X(47) FROM "   DESEJA CONCLUIR
      -            "O MOVIMENTO DIµRIO ?  [ ]".
           03  BCK-CLR 00 FOR-CLR 04.
               05  LINE 16 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 18 PIC X(46) FROM ALL "ß".

       01  ENTR-02  BCK-CLR 04 FOR-CLR 15
                    LINE 16 COLUMN 59 PIC X(01) USING RSP-OPC AUTO.

       01  TELA-07.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 15 COLUMN 17 PIC X(47) FROM ALL "ß".
               05  LINE 17 COLUMN 17 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 16 COLUMN 17 PIC X(47) FROM
                   "    AGUARDE, CONCLUINDO MOVIMENTO DIµRIO ... ".
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 16 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 18 PIC X(46) FROM ALL "ß".

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"      USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL    =   1  GOBACK.
           OPEN     INPUT        CADCXA   CADLCP   CADPRS.

       ROT-PESQ-00.
           MOVE     47       TO  IND-MSG
           MOVE     SPACES   TO  CHV-CXA.

       ROT-PESQ-10.
           START    CADCXA       KEY  >   CHV-CXA
           IF  FST-CXA   =  "23" GO       ROT-PESQ-30.
           READ     CADCXA  NEXT
           IF  FLG-CXA   =    2  GO       ROT-PESQ-10.
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-CXA  TO  TUR-PRS
           MOVE     NUM-CXA  TO  CXA-PRS.

       ROT-PESQ-20.
           START    CADPRS       KEY  >   CHV-PRS
           IF  FST-PRS   =  "23" GO       ROT-PESQ-30.
           READ     CADPRS  NEXT
           IF  TUR-PRS  NOT   =  TUR-CXA  OR
               CXA-PRS  NOT   =  NUM-CXA
                                 GO       ROT-PESQ-10.
           IF  VDD-PRS   =    0  GO       ROT-PESQ-20.
           PERFORM  ROT-MSGM-00  GO       ROT-EXIT-00.

       ROT-PESQ-30.
           MOVE     38       TO  IND-MSG
           MOVE     SPACES   TO  CHV-CXA.

       ROT-PESQ-40.
           START    CADCXA       KEY  >   CHV-CXA
           IF  FST-CXA   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-EXIT-00.
           READ     CADCXA  NEXT
           IF  FLG-CXA   =    1  GO       ROT-PESQ-40.
           MOVE     DTA-CXA  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-CAB
           MOVE     MES-AUX  TO  MES-CAB
           MOVE     ANO-AUX  TO  ANO-CAB
           MOVE     SPACES   TO  CHV-LCP
           MOVE     0  TO  IND1  PERFORM  ROT-NOME-00
           DISPLAY  TELA-01      PERFORM  ROT-ZERA-00.

       ROT-PESQ-50.
           START    CADLCP       KEY  >   CHV-LCP
           IF  FST-LCP   =  "23" GO       ROT-PESQ-60.
           READ     CADLCP  NEXT
           IF  PRX-LCP   >   "1" GO       ROT-PESQ-60.
           IF  DTA-LCP  NOT   =  DTA-AUX
                                 GO       ROT-PESQ-50.
           ADD      QTD-LCP  TO  CMP-TBC (SUF-LCP)
                                 SDO-TBC (SUF-LCP)
           GO       ROT-PESQ-50.

       ROT-PESQ-60.
           CLOSE    CADCXA       CADPRS   CADLCP
           OPEN     I-O          CADTNQ
                    INPUT        CADCMB   CADECR
           MOVE     11       TO  IND4
           MOVE     01       TO  IND2     IND3
           MOVE     ZEROS    TO  CHV-CMB  IND1
                                 CHV-ECR  CHV-TNQ.
       ROT-PESQ-70.
           START    CADECR       KEY  >   CHV-ECR
           IF  FST-ECR   =  "23" GO       ROT-PESQ-80.
           READ     CADECR  NEXT
           ADD      VDD-ECR  TO  VDA-TBC (CMB-ECR)
           SUBTRACT VDD-ECR FROM SDO-TBC (CMB-ECR)
           GO       ROT-PESQ-70.

       ROT-PESQ-80.
           START    CADTNQ       KEY  >   CHV-TNQ
           IF  FST-TNQ   =  "23" GO       ROT-PESQ-90.
           READ     CADTNQ  NEXT
           ADD      SDI-TNQ  TO  SDI-TBC (CMB-TNQ)
                                 SDO-TBC (CMB-TNQ)
           ADD      SDF-TNQ  TO  SDF-TBC (CMB-TNQ)
           GO       ROT-PESQ-80.

       ROT-PESQ-90.
           START    CADCMB       KEY  >   CHV-CMB
           IF  FST-CMB   =  "23" GO       ROT-MENU-00.
           READ     CADCMB  NEXT
           MOVE     DES-CMB  TO  DES-TBC (CHV-CMB)
           COMPUTE  DIF-TBC              (CHV-CMB)
                              =  SDF-TBC (CHV-CMB)
                              -  SDO-TBC (CHV-CMB)
           ADD      1   TO  IND1 GO       ROT-PESQ-90.

       ROT-MENU-00.
           DISPLAY     TELA-02   SETA-01  TELA-03
           IF (IND2 + 05) < IND1 DISPLAY  ST1-ABX
               IF  IND2     >  1 DISPLAY  ST1-ACM
                   ELSE          DISPLAY  TR1-ACM
           ELSE    IF  IND2 >  1 DISPLAY  ST1-ACM  TR1-ABX
                       ELSE      DISPLAY  TR1-ACM.
       ROT-MENU-10.
           MOVE  2  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-10.
           IF  TCL-MSG   =   05  GO       ROT-MENU-20.
           IF  TCL-MSG   =   06  GO       ROT-MENU-30.
           IF  TCL-MSG   =   53  GO       ROT-MENU-40.
           IF  TCL-MSG   =   54  GO       ROT-MENU-50.
           IF  TCL-MSG   >   00  GO       ROT-MENU-10.
           IF  HEX-MSG   =   27  MOVE  0  TO  CHV-CMB
                                 GO       ROT-IMPR-00.
           PERFORM  ROT-GTLA-00  GO       ROT-TANQ-00.

       ROT-MENU-20.
           IF  IND3      =   01  GO       ROT-MENU-10.
           IF  IND4      =   11  COMPUTE  IND2 = IND2  -  1
                                 COMPUTE  IND3 = IND3  -  1
                                 GO       ROT-MENU-00.
           DISPLAY  MENU-01      COMPUTE  IND3 = IND3  -  1
           DISPLAY  TELA-03      COMPUTE  IND4 = IND4  -  1
           DISPLAY  SETA-01      GO       ROT-MENU-10.

       ROT-MENU-30.
           IF  IND3      =  IND1 GO       ROT-MENU-10.
           IF  IND4      =   16  COMPUTE  IND2 = IND2  +  1
                                 COMPUTE  IND3 = IND3  +  1
                                 GO       ROT-MENU-00.
           DISPLAY  MENU-01      COMPUTE  IND3 = IND3  +  1
           DISPLAY  TELA-03      COMPUTE  IND4 = IND4  +  1
           DISPLAY  SETA-01      GO       ROT-MENU-10.

       ROT-MENU-40.
           IF  IND2      =   01  GO       ROT-MENU-10.
           MOVE     11       TO  IND4
           IF  IND2      >    6  COMPUTE  IND3 = IND2 - 6
                                 COMPUTE  IND2 = IND2 - 6
           ELSE
                                 MOVE  1  TO   IND2  IND3.
           GO      ROT-MENU-00.

       ROT-MENU-50.
           IF  IND1      <      (IND2  +  6)
                                 GO       ROT-MENU-10.
           COMPUTE  IND3     =   IND2  +  6
           COMPUTE  IND2     =   IND2  +  6
           MOVE     11  TO  IND4 GO       ROT-MENU-00.

       ROT-TANQ-00.
           MOVE     ZEROS    TO  CHV-TNQ  IND5.

       ROT-TANQ-10.
           ADD      1        TO  IND5
           MOVE     SPACES   TO  TAB-TNQ  (IND5)
           IF  IND5    <     20  GO       ROT-TANQ-10.
           MOVE      0  TO  IND5 MOVE     1  TO  IND6
           MOVE     11  TO  IND8 MOVE     1  TO  IND7.

       ROT-TANQ-20.
           START    CADTNQ       KEY  >   CHV-TNQ
           IF  FST-TNQ   =  "23" GO       ROT-TANQ-30.
           READ     CADTNQ  NEXT
           IF  CMB-TNQ  NOT   =  IND3 GO  ROT-TANQ-20.
           ADD      1        TO  IND5
           MOVE     CHV-TNQ  TO  NUM-AUX
           MOVE     SDF-TNQ  TO  QTD-AUX
           MOVE     REG-AUX  TO  TAB-TNQ (IND5)
           GO       ROT-TANQ-20.

       ROT-TANQ-30.
           DISPLAY  TELA-04
           IF (IND6 + 05) < IND5 DISPLAY  ST2-ABX
               IF  IND6     >  1 DISPLAY  ST2-ACM
                   ELSE          DISPLAY  TR2-ACM
           ELSE    IF  IND6 >  1 DISPLAY  ST2-ACM  TR2-ABX
                       ELSE      DISPLAY  TR2-ACM.
       ROT-TANQ-40.
           MOVE     TAB-TNQ      (IND7)   TO  REG-AUX
           MOVE     NUM-AUX  TO  CHV-TNQ
           READ     CADTNQ       MOVE 20  TO  IND-MSG
           IF  FST-TNQ   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-TANQ-40.
           MOVE     SDF-TNQ  TO  SDO-AUX.

       ROT-TANQ-50.
           ACCEPT   ENTR-01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  MENU-02
                                 GO       ROT-MENU-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-TANQ-50.
           COMPUTE  SDF-TBC      (IND3)   =
                    SDF-TBC      (IND3)   -   SDF-TNQ
                                          +   SDO-AUX
           COMPUTE  DIF-TBC      (IND3)   =
                    DIF-TBC      (IND3)   -   SDF-TNQ
                                          +   SDO-AUX
           MOVE     SDO-AUX  TO  SDF-TNQ  QTD-AUX
           MOVE     REG-AUX  TO  TAB-TNQ  (IND7)
           REWRITE  REG-TNQ      DISPLAY  MENU-02
           IF  IND7  =  IND5     GO       ROT-MENU-00.
           IF  IND8  =   16      ADD      1  TO  IND6
                                 ADD      1  TO  IND7
                                 PERFORM  ROT-TANQ-30
           ELSE
                                 ADD      1  TO  IND7
                                 ADD      1  TO  IND8.
           GO       ROT-TANQ-40.

       ROT-IMPR-00.
           START    CADCMB       KEY  >   CHV-CMB
           IF  FST-CMB   =  "23" GO       ROT-IMPR-10.
           READ     CADCMB  NEXT
           ADD      SDO-TBC     (CHV-CMB) TO  ACM-CNT
           ADD      SDF-TBC     (CHV-CMB) TO  ACM-FIS
           ADD      DIF-TBC     (CHV-CMB) TO  ACM-DIF
           GO       ROT-IMPR-00.

       ROT-IMPR-10.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-01
           MOVE     11  TO IND5  DISPLAY  TELA-05
           MOVE     01  TO IND6  DISPLAY  SETA-03.

       ROT-IMPR-20.
           MOVE  1  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-IMPR-20.
           IF  TCL-MSG   =   05  GO       ROT-IMPR-40.
           IF  TCL-MSG   =   06  GO       ROT-IMPR-50.
           IF  TCL-MSG   >   00  GO       ROT-IMPR-20.
           IF  HEX-MSG   =   13  PERFORM  ROT-IMPR-30
                                 GO       ROT-IMPR-20.
           DISPLAY  TELA-06      GO       ROT-CONF-00.

       ROT-IMPR-30.
           CLOSE    CADCMB       CADECR   CADTNQ
           IF  IND6   =   1      CALL    "DYRDIA".
           IF  IND6   =   2      CALL    "DYRLMC".
           IF  IND6   =   3      CALL    "DYRANA".
           OPEN     I-O          CADTNQ
                    INPUT        CADCMB   CADECR.
       ROT-IMPR-40.
           DISPLAY  MENU-03
           IF  IND6   =  01      MOVE     03  TO IND6
                                 MOVE     13  TO IND5
           ELSE
                                 COMPUTE  IND5 = IND5  -  01
                                 COMPUTE  IND6 = IND6  -  01.
           DISPLAY  SETA-03      GO       ROT-IMPR-20.

       ROT-IMPR-50.
           DISPLAY  MENU-03
           IF  IND6   =  03      MOVE     01  TO IND6
                                 MOVE     11  TO IND5
           ELSE
                                 COMPUTE  IND5 = IND5  +  01
                                 COMPUTE  IND6 = IND6  +  01.
           DISPLAY  SETA-03      GO       ROT-IMPR-20.

       ROT-CONF-00.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   ENTR-02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CONF-00.
           IF  RSP-OPC  NOT   =  "S"
                                 GO       ROT-CONF-00.
           MOVE     20       TO  IND-MSG
           DISPLAY  TELA-07      MOVE  0  TO  CHV-TNQ.

       ROT-CONF-10.
           START    CADTNQ       KEY  >   CHV-TNQ
           IF  FST-TNQ   =  "23" GO       ROT-CONF-20.

       ROT-CONF-15.
           READ     CADTNQ  NEXT MOVE 20  TO  IND-MSG
           IF  FST-TNQ   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-CONF-15.
           MOVE     SDF-TNQ  TO  SDI-TNQ
           MOVE     ZEROS    TO  SDF-TNQ
           REWRITE  REG-TNQ      GO       ROT-CONF-10.

       ROT-CONF-20.
           CLOSE    CADCMB       CADECR   CADTNQ
           OPEN     I-O          CADCTR   CADCXA   CADECR.

       ROT-CONF-25.
           READ     CADCTR
           IF  FST-CTR   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-CONF-25.
           ADD      1        TO  DIA-CTR
           ADD      1        TO  LMC-CTR
           MOVE     DTA-AUX  TO  DTD-CTR
           REWRITE  REG-CTR
           MOVE     ZEROS    TO  CHV-CXA  CHV-ECR.

       ROT-CONF-30.
           START    CADCXA       KEY  >   CHV-CXA
           IF  FST-CXA   =  "23" GO       ROT-CONF-40.

       ROT-CONF-35.
           READ     CADCXA  NEXT
           IF  FST-CXA   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-CONF-35.
           MOVE     ZEROS    TO  DTA-CXA
           MOVE     1        TO  FLG-CXA
           REWRITE  REG-CXA      GO       ROT-CONF-30.

       ROT-CONF-40.
           START    CADECR       KEY  >   CHV-ECR
           IF  FST-ECR   =  "23" GO       ROT-CONF-50.

       ROT-CONF-45.
           READ     CADECR  NEXT
           IF  FST-ECR   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-CONF-45.
           MOVE     FCL-ECR  TO  IDL-ECR  ICL-ECR
           MOVE     ZEROS    TO  AFC-ECR  AFD-ECR
                                 VDC-ECR  VDD-ECR
           REWRITE  REG-ECR      GO       ROT-CONF-40.

       ROT-CONF-50.
           CLOSE    CADCTR       CADCXA   CADECR
           OPEN     I-O          CADEST   CADLES   CADPRS
           MOVE     ZEROS    TO  CHV-EST  CHV-PRS.

       ROT-CONF-55.
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-CONF-70.

       ROT-CONF-60.
           READ     CADEST  NEXT
           IF  FST-EST   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-CONF-60.
           IF  PRX-EST   >  "1"  GO       ROT-CONF-70.
           MOVE     PRX-EST  TO  PRX-LES
           MOVE     SUF-EST  TO  SUF-LES
           MOVE     DTA-AUX  TO  DTA-LES
           MOVE     PRM-OPR  TO  OPR-LES
           MOVE     ZEROS    TO  LCT-LES
           MOVE     SDO-TBC     (SUF-EST) TO  SDA-LES
           MOVE     SDF-TBC     (SUF-EST) TO  SDC-LES
                                              SDO-EST
           ADD      VDA-TBC     (SUF-EST) TO  QVM-EST
                                              QVA-EST.
       ROT-CONF-65.
           WRITE    REG-LES      ADD   1  TO  LCT-LES
           IF  FST-LES   =  "22" GO       ROT-CONF-65.
           REWRITE  REG-EST      GO       ROT-CONF-55.

       ROT-CONF-70.
           START    CADPRS       KEY  >   CHV-PRS
           IF  FST-PRS   =  "23" GO       ROT-CONF-80.

       ROT-CONF-75.
           READ     CADPRS  NEXT
           IF  FST-PRS   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-CONF-75.
           MOVE     ZEROS    TO  QTD-PRS
                                 CTD-PRS  VDD-PRS
           REWRITE  REG-PRS      GO       ROT-CONF-70.

       ROT-CONF-80.
           CLOSE    CADEST       CADLES   CADPRS
           OPEN     OUTPUT       CADLCX
           CLOSE    CADLCX       GOBACK.

       ROT-ZERA-00.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  CMB-TBC  (IND1)
           MOVE     ZEROS    TO  SDI-TBC  (IND1)
                                 CMP-TBC  (IND1)
                                 VDA-TBC  (IND1)
                                 SDO-TBC  (IND1)
                                 SDF-TBC  (IND1)
                                 DIF-TBC  (IND1)
           IF  IND1     <    15  GO       ROT-ZERA-00.

       ROT-HELP-00.
           CALL   "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING  PRM-MSG.

       ROT-NOME-00.
           CALL   "DYNOME"  USING  PRM-NOM
                                   PRM-CAB.
       ROT-GTLA-00.
           MOVE   "G"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE   "R"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

       ROT-EXIT-00.
           CLOSE   CADCXA  CADLCP  CADPRS  GOBACK.

       ROT-EXIT-10.
           CLOSE   CADCMB  CADECR  CADTNQ  GOBACK.
