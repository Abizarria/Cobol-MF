       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1121.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADATV        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ATV
                       FILE          STATUS  IS  FST-ATV.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

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

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADATV.CPY.
       COPY  CADCTR.CPY.
       COPY  CADEST.CPY.
       COPY  CADLCP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-ATV          PIC  X(02).
           03  FST-CTR          PIC  X(02).
           03  FST-EST          PIC  X(02).
           03  FST-LCP          PIC  X(02).

           03  IND1             PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  ACM-PSQ          PIC  9(02).

           03  DTA-LIM.
               05  ANO-LIM      PIC  9(02).
               05  MES-LIM      PIC  9(02).
               05  DIA-LIM      PIC  9(02).

           03  NOM-PSQ          PIC  X(25).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  25.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  02.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1121".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-PSQ.
           03  OPC-PSQ          PIC  9(01)  VALUE  1.
           03  IND-PSQ          PIC  X(01)  VALUE  "1".
           03  LIN-PSQ          PIC  9(02)  VALUE  09.
           03  COL-PSQ          PIC  9(02)  VALUE  37.
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
           03  LIN-DTA          PIC  9(02)  VALUE  15.
           03  COL-DTA          PIC  9(02)  VALUE  56.
           03  BCK-DTA          PIC  9(01)  VALUE  6.
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

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 05 COLUMN 07 VALUE " Administraฦo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 06 COLUMN 09 VALUE " Controle de Estoques ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 11 VALUE " Movimentos e Acertos ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 05 COLUMN 29 VALUE "฿".
               05  LINE 06 COLUMN 31 VALUE "฿".
               05  LINE 07 COLUMN 33 VALUE "฿".
               05  LINE 06 COLUMN 08 VALUE "Ü".
               05  LINE 07 COLUMN 10 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 08 COLUMN 07 VALUE " ษออ
      -            " ออออออออออออออออออออออออออออออออออออป ".
               05  LINE 09 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 10 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 11 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 12 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 13 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 14 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 15 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 16 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 17 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 18 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 19 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 20 COLUMN 07 VALUE " ศออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 08 COLUMN 13 VALUE "  Compra de Produtos  ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 09 COLUMN 75 VALUE "  ".
               05  LINE 10 COLUMN 75 VALUE "  ".
               05  LINE 11 COLUMN 75 VALUE "  ".
               05  LINE 12 COLUMN 75 VALUE "  ".
               05  LINE 13 COLUMN 75 VALUE "  ".
               05  LINE 14 COLUMN 75 VALUE "  ".
               05  LINE 15 COLUMN 75 VALUE "  ".
               05  LINE 16 COLUMN 75 VALUE "  ".
               05  LINE 17 COLUMN 75 VALUE "  ".
               05  LINE 18 COLUMN 75 VALUE "  ".
               05  LINE 19 COLUMN 75 VALUE "  ".
               05  LINE 20 COLUMN 75 VALUE "  ".
               05  LINE 21 COLUMN 09 PIC X(68) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 18 PIC X(46) FROM ALL "฿".
               05  LINE 13 COLUMN 18 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 00.
               05  LINE 13 COLUMN 19 PIC X(45) FROM ALL "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 10 COLUMN 18 PIC X(46) FROM
                   "  Cขdigo Produto :  -     -                   ".
               05  LINE 11 COLUMN 18 PIC X(46) FROM SPACES.
               05  LINE 12 COLUMN 18 PIC X(46) FROM
                   "  Descriฦo .....:                            ".
           03  BCK-CLR 00 FOR-CLR 07.
               05  LINE 10 COLUMN 64 PIC X(01) FROM " ".
               05  LINE 11 COLUMN 64 PIC X(01) FROM " ".
               05  LINE 12 COLUMN 64 PIC X(01) FROM " ".
               05  LINE 13 COLUMN 64 PIC X(01) FROM " ".

       01  TELA-03.
           03  BCK-CLR 06 FOR-CLR 07.
               05  LINE 14 COLUMN 11 PIC X(59) FROM ALL "฿".
               05  LINE 19 COLUMN 11 PIC X(01) FROM     "Ü".
           03  BCK-CLR 06 FOR-CLR 14.
               05  LINE 15 COLUMN 11 PIC X(59) FROM "  Nota Fiscal .:
      -            "            Data Compra :               ".
               05  LINE 16 COLUMN 11 PIC X(59) FROM SPACES.
               05  LINE 17 COLUMN 11 PIC X(59) FROM "  Preo Custo .:
      -            "            Quantidade .:               ".
               05  LINE 18 COLUMN 11 PIC X(59) FROM "  Preo Venda .:
      -            "            Valor Total :               ".
           03  BCK-CLR 00 FOR-CLR 06.
               05  LINE 15 COLUMN 70 PIC X(01) FROM " ".
               05  LINE 16 COLUMN 70 PIC X(01) FROM " ".
               05  LINE 17 COLUMN 70 PIC X(01) FROM " ".
               05  LINE 18 COLUMN 70 PIC X(01) FROM " ".
               05  LINE 19 COLUMN 70 PIC X(01) FROM " ".
               05  LINE 19 COLUMN 12 PIC X(58) FROM ALL "฿".

       01  ENTR-01    AUTO.
           03 SUF-E02 LINE 10 COLUMN 39 PIC ZZ9           USING SUF-EST.
           03 NTF-E02 LINE 15 COLUMN 28 PIC ZZZ.ZZ9       USING NTF-LCP.
           03 CST-E02 LINE 17 COLUMN 28 PIC ZZZ.ZZ9,9999  USING CST-EST.
           03 VDA-E02 LINE 18 COLUMN 28 PIC ZZZ.ZZ9,9999  USING VDA-EST.
           03 QTD-E02 LINE 17 COLUMN 55 PIC ---.--9       USING QTD-LCP.
           03 VAL-E02 LINE 18 COLUMN 55 PIC --.---.--9,99 USING VAL-LCP.
           03 PSQ-E02 LINE 12 COLUMN 37 PIC X(25)         USING NOM-PSQ.

       01  ENTR-02 BCK-CLR 02 FOR-CLR 15.
           03  PRX-T02 LINE 10 COLUMN 37 PIC X(01)        FROM CHV-ATV.
           03  ATV-T02 LINE 10 COLUMN 47 PIC X(15)        FROM DES-ATV.
           03  SUF-T02 LINE 10 COLUMN 39 PIC 9(03)        FROM SUF-EST.
           03  DES-T02 LINE 12 COLUMN 37 PIC X(25)        FROM DES-EST.
           03  PSQ-T02 LINE 12 COLUMN 37 PIC X(25)        FROM NOM-PSQ.

       01  ENTR-02 BCK-CLR 06 FOR-CLR 15.
           03  NTF-T03 LINE 15 COLUMN 28 PIC 999.999       FROM NTF-LCP.
           03  CST-T03 LINE 17 COLUMN 28 PIC ZZZ.ZZ9,9999  FROM CST-EST.
           03  VDA-T03 LINE 18 COLUMN 28 PIC ZZZ.ZZ9,9999  FROM VDA-EST.
           03  QTD-T03 LINE 17 COLUMN 55 PIC ---.--9       FROM QTD-LCP.
           03  VAL-T03 LINE 18 COLUMN 55 PIC --.---.--9,99 FROM VAL-LCP.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"      USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL  =  1     GOBACK.  DISPLAY  TELA-01
           ACCEPT   DTA-DTA FROM DATE
           OPEN     INPUT        CADATV   CADCTR
                    I-O          CADEST   CADLCP
           READ     CADCTR
           MOVE     DTI-CTR  TO  DTA-LIM
           MOVE     31       TO  DIA-LIM.

       ROT-CODE-00.
           DISPLAY  TELA-02      PERFORM  ROT-PESQ-00
           IF  IND-PSQ   <    1  GO       ROT-EXIT-00.
           MOVE     IND-PSQ  TO  CHV-ATV
           READ     CADATV
           DISPLAY  PRX-T02      ATV-T02.

       ROT-CODE-10.
           MOVE     ZEROS    TO  SUF-EST
           ACCEPT   SUF-E02      DISPLAY  SUF-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           PERFORM  ROT-GTLA-00
           IF  SUF-EST   <   1   GO       ROT-PESQ-10.
           MOVE     CHV-ATV  TO  PRX-EST
           READ     CADEST       MOVE 21  TO  IND-MSG
           IF  FST-EST   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-10.
       ROT-EXBE-00.
           DISPLAY  SUF-T02      DES-T02.

       ROT-EXBE-10.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-EXBE-10.
           IF  TCL-MSG   =   05  GO       ROT-RTRN-00.
           IF  TCL-MSG   =   06  GO       ROT-PROX-00.
           IF  TCL-MSG   >   00  GO       ROT-EXBE-10.
           IF  HEX-MSG   =   13  GO       ROT-TEST-00.
           PERFORM  ROT-RTLA-00  GO       ROT-CODE-10.

       ROT-RTRN-00.
           START    CADEST       KEY  <   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-EXBE-10.
           READ     CADEST       PREVIOUS
           IF  PRX-EST       =   CHV-ATV
                                 GO       ROT-EXBE-00.
           READ     CADEST  NEXT GO       ROT-EXBE-10.

       ROT-PROX-00.
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-EXBE-10.
           READ     CADEST  NEXT
           IF  PRX-EST       =   CHV-ATV
                                 GO       ROT-EXBE-00.
           READ     CADEST       PREVIOUS
           GO       ROT-EXBE-10.

       ROT-TEST-00.
           READ     CADEST       MOVE 20  TO  IND-MSG
           IF  FST-EST   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-10.
           IF  FST-EST   =  "23" MOVE 36  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-10.
       ROT-ENTR-00.
           DISPLAY  TELA-03
           MOVE     ZEROS    TO  NTF-LCP  QTD-LCP.

       ROT-ENTR-10.
           ACCEPT   NTF-E02      DISPLAY  NTF-T03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-CODE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
           IF  NTF-LCP   =   0   GO       ROT-ENTR-10.

       ROT-ENTR-20.
           PERFORM  ROT-DATA-00  MOVE 18  TO  IND-MSG
           IF  FLG-DTA   =    0  GO       ROT-ENTR-00.
           IF (DTA-DTA   <       DTI-CTR  OR  >
               DTA-LIM)  OR     (PRX-EST  =   1   AND
               DTA-DTA  NOT   >  DTD-CTR)
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-20.
       ROT-ENTR-30.
           ACCEPT   CST-E02      DISPLAY  CST-T03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-30.
       ROT-ENTR-40.
           ACCEPT   VDA-E02      DISPLAY  VDA-T03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-40.
           MOVE     46       TO  IND-MSG
           IF  VDA-EST   <       CST-EST
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-40.
       ROT-ENTR-50.
           ACCEPT   QTD-E02      DISPLAY  QTD-T03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-40.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-50.
           IF  QTD-LCP   =   0   GO       ROT-ENTR-50.
           COMPUTE  VAL-LCP  =   QTD-LCP  *   CST-EST.

       ROT-ENTR-60.
           ACCEPT   VAL-E02      DISPLAY  VAL-T03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-50.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-60.
           IF  VAL-LCP   =   0   GO       ROT-ENTR-60.
           COMPUTE  CST-EST  =   VAL-LCP  /   QTD-LCP
           DISPLAY  CST-T03      MOVE 46  TO  IND-MSG
           IF  CST-EST   >       VDA-EST
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-60.
       ROT-ENTR-70.
           MOVE 10  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-70.
           IF  TCL-MSG   >   00  GO       ROT-ENTR-70.
           IF  HEX-MSG   =   27  GO       ROT-ENTR-60.
           MOVE     PRX-EST  TO  PRX-LCP
           MOVE     SUF-EST  TO  SUF-LCP
           MOVE     DTA-DTA  TO  DTA-LCP
           MOVE     ZEROS    TO  LCT-LCP.

       ROT-ENTR-80.
           WRITE    REG-LCP      ADD   1  TO  LCT-LCP
           IF  FST-LCP   =  "22" GO       ROT-ENTR-80.
           COMPUTE  CST-EST  =   VAL-LCP  /   QTD-LCP
           COMPUTE  CTM-EST  =  (CTM-EST  *   SDO-EST
                 +  VAL-LCP) /  (SDO-EST  +   QTD-LCP)
           IF  SDO-EST   <   1   COMPUTE      CTM-EST
                             =   VAL-LCP  /   QTD-LCP.
           COMPUTE  SDO-EST  =   SDO-EST  +   QTD-LCP
           COMPUTE  PCT-EST  =  (VDA-EST  -   CTM-EST)
                             *   100      /   VDA-EST
           REWRITE  REG-EST
           PERFORM  ROT-RTLA-00  GO       ROT-CODE-10.

       ROT-PESQ-00.
           CALL    "DYPESQ"      USING    PRM-PSQ
                                          PRM-HLP.
       ROT-PESQ-10.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  SUF-EST
           DISPLAY  SUF-T02.

       ROT-PESQ-20.
           ACCEPT   PSQ-E02      DISPLAY  PSQ-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-CODE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-20.
           IF  NOM-PSQ   =       SPACES
                                 GO       ROT-PESQ-20.
           MOVE     26  TO  IND1 PERFORM  ROT-PESQ-60.

       ROT-PESQ-30.
           SUBTRACT  1  FROM     IND1
           IF  LET-PSQ (IND1) =  " "  GO  ROT-PESQ-30.
           MOVE     CHV-ATV  TO  PRX-EST
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-PESQ-40.
           MOVE     22       TO  IND-MSG
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           READ     CADEST  NEXT
           IF  PRX-EST  NOT  =   CHV-ATV
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           DISPLAY  SUF-T02
           INSPECT  DES-EST      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-40.
           PERFORM  ROT-EXBE-00  GO       ROT-PESQ-70.

       ROT-PESQ-50.
           MOVE     22       TO  IND-MSG
           START    CADEST       KEY  <   CHV-EST
           IF  FST-EST   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           READ     CADEST       PREVIOUS
           IF  PRX-EST  NOT  =   CHV-ATV
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           DISPLAY  SUF-T02
           INSPECT  DES-EST      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-50.
           PERFORM  ROT-EXBE-00  GO       ROT-PESQ-70.

       ROT-PESQ-60.
           PERFORM  ROT-RTLA-00
           MOVE     0        TO  ACM-PSQ
           DISPLAY  SUF-T02      PSQ-T02.

       ROT-PESQ-70.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-70.
           IF  TCL-MSG   =   05  PERFORM  ROT-PESQ-60
                                 GO       ROT-PESQ-50.
           IF  TCL-MSG   =   06  PERFORM  ROT-PESQ-60
                                 GO       ROT-PESQ-40.
           IF  TCL-MSG   >   00  GO       ROT-PESQ-70.
           IF  HEX-MSG   =   13  GO       ROT-TEST-00.
           PERFORM  ROT-RTLA-00  GO       ROT-PESQ-10.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

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
           CLOSE    CADATV  CADCTR
                    CADEST  CADLCP  GOBACK.
