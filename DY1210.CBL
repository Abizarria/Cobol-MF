       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1210.
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

           SELECT      CADCRD        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CRD
                       FILE          STATUS  IS  FST-CRD.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADCXA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CXA
                       LOCK          MODE    IS  EXCLUSIVE
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

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS
                       FILE          STATUS  IS  FST-PRS.

           SELECT      CADSDO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SDO
                       FILE          STATUS  IS  FST-SDO.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCDV.CPY.
       COPY  CADCRD.CPY.
       COPY  CADCTR.CPY.
       COPY  CADCXA.CPY.
       COPY  CADECR.CPY.
       COPY  CADEST.CPY.
       COPY  CADFAT.CPY.
       COPY  CADLCX.CPY.
       COPY  CADLBC.CPY.
       COPY  CADLDP.CPY.
       COPY  CADPRS.CPY.
       COPY  CADSDO.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CDV          PIC  X(02).
           03  FST-CRD          PIC  X(02).
           03  FST-CTR          PIC  X(02).
           03  FST-CXA          PIC  X(02).
           03  FST-ECR          PIC  X(02).
           03  FST-EST          PIC  X(02).
           03  FST-FAT          PIC  X(02).
           03  FST-LCX          PIC  X(02).
           03  FST-LBC          PIC  X(02).
           03  FST-LDP          PIC  X(02).
           03  FST-PRS          PIC  X(02).
           03  FST-SDO          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  IND3             PIC  9(02).
           03  IND4             PIC  9(02).
           03  IND5             PIC  9(02).
           03  IND6             PIC  9(02).
           03  IND7             PIC  9(02).
           03  IND8             PIC  9(02).

           03  TECLADO          PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  FLG-AUX          PIC  X(01).
           03  ATV-AUX          PIC  9(01).
           03  ACM-DIF          PIC S9(07)V99.

           03  DTA-LIM.
               05  ANO-LIM      PIC  9(02).
               05  MES-LIM      PIC  9(02).
               05  DIA-LIM      PIC  9(02).

           03  DTA-AUX.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  SEM-AUX      PIC  X(04).
               05  DIA-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  REG-AX1.
               05  FLG-AX1      PIC  X(01).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  DES-AX1      PIC  X(18).
               05  TUR-AX1      PIC  X(06).

           03  REG-AX2.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  DES-AX2      PIC  X(21).
               05  QTD-AX2      PIC  ZZZ.ZZ9,9.
               05  FILLER       PIC  X(05)  VALUE  SPACES.
               05  VAL-AX2      PIC  Z.ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  TAB-TUR.
               05  FILLER       PIC  X(05)  VALUE  "MANHA".
               05  FILLER       PIC  X(05)  VALUE  "TARDE".
               05  FILLER       PIC  X(05)  VALUE  "NOITE".
           03  RED-TUR          REDEFINES   TAB-TUR.
               05  TUR-TAB      PIC  X(05)  OCCURS  03.

           03  TAB-SEM.
               05  FILLER       PIC  X(03)  VALUE  "DOM".
               05  FILLER       PIC  X(03)  VALUE  "SEG".
               05  FILLER       PIC  X(03)  VALUE  "TER".
               05  FILLER       PIC  X(03)  VALUE  "QUA".
               05  FILLER       PIC  X(03)  VALUE  "QUI".
               05  FILLER       PIC  X(03)  VALUE  "SEX".
               05  FILLER       PIC  X(03)  VALUE  "SAB".
           03  RED-SEM          REDEFINES   TAB-SEM.
               05  SEM-TAB      PIC  X(03)  OCCURS  07.

           03  TAB-001          OCCURS  20.
               05  REG-001      PIC  X(26).
               05  CHV-001      PIC  9(03).

           03  TAB-002          OCCURS  60.
               05  REG-002      PIC  X(49).
               05  CHV-002      PIC  9(07).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1210".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  12.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-DTA.
           03  DTA-DTA.
               05  ANO-DTA      PIC  9(02).
               05  MES-DTA      PIC  9(02).
               05  DIA-DTA      PIC  9(02).
           03  LIN-DTA          PIC  9(02)  VALUE  10.
           03  COL-DTA          PIC  9(02)  VALUE  24.
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

       01  PRM-MVC.
           03  PRS-MVC.
               05  TUR-MVC      PIC  9(01).
               05  CXA-MVC      PIC  9(02).
               05  ATV-MVC      PIC  9(01).
               05  LIN-MVC      PIC  9(03).
           03  DTA-MVC          PIC  9(06).
           03  POS-MVC          PIC  9(02).

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 04 COLUMN 03 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 05 COLUMN 12 VALUE " Administraฦo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 06 COLUMN 14 VALUE " Controle de Receitas ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 16 VALUE " Movimento dos Caixas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 05 COLUMN 34 VALUE "฿".
               05  LINE 06 COLUMN 36 VALUE "฿".
               05  LINE 07 COLUMN 38 VALUE "฿".
               05  LINE 06 COLUMN 13 VALUE "Ü".
               05  LINE 07 COLUMN 15 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 08 COLUMN 12 VALUE " ษออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออป ".
               05  LINE 09 COLUMN 12 VALUE " บ
      -            "                            บ ".
               05  LINE 10 COLUMN 12 VALUE " บ
      -            "                            บ ".
               05  LINE 11 COLUMN 12 VALUE " บ
      -            "                            บ ".
               05  LINE 12 COLUMN 12 VALUE " บ
      -            "                            บ ".
               05  LINE 13 COLUMN 12 VALUE " บ
      -            "                            บ ".
               05  LINE 14 COLUMN 12 VALUE " บ
      -            "                            บ ".
               05  LINE 15 COLUMN 12 VALUE " บ
      -            "                            บ ".
               05  LINE 16 COLUMN 12 VALUE " บ
      -            "                            บ ".
               05  LINE 17 COLUMN 12 VALUE " บ
      -            "                            บ ".
               05  LINE 18 COLUMN 12 VALUE " บ
      -            "                            บ ".
               05  LINE 19 COLUMN 12 VALUE " บ
      -            "                            บ ".
               05  LINE 20 COLUMN 12 VALUE " ศออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 00.
               05  LINE 09 COLUMN 70 VALUE "  ".
               05  LINE 10 COLUMN 70 VALUE "  ".
               05  LINE 11 COLUMN 70 VALUE "  ".
               05  LINE 12 COLUMN 70 VALUE "  ".
               05  LINE 13 COLUMN 70 VALUE "  ".
               05  LINE 14 COLUMN 70 VALUE "  ".
               05  LINE 15 COLUMN 70 VALUE "  ".
               05  LINE 16 COLUMN 70 VALUE "  ".
               05  LINE 17 COLUMN 70 VALUE "  ".
               05  LINE 18 COLUMN 70 VALUE "  ".
               05  LINE 19 COLUMN 70 VALUE "  ".
               05  LINE 20 COLUMN 70 VALUE "  ".
               05  LINE 21 COLUMN 14 PIC X(58) FROM SPACES.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 15 PIC X(19) FROM ALL "฿".
               05  LINE 11 COLUMN 15 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 10 COLUMN 15 PIC X(19) FROM
                   "  Data :".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 10 COLUMN 34 PIC X(01) FROM     " ".
               05  LINE 11 COLUMN 34 PIC X(01) FROM     " ".
               05  LINE 11 COLUMN 16 PIC X(18) FROM ALL "฿".

       01  TELA-02.
           03  BCK-CLR 03 FOR-CLR 07.
               05  LINE 09 COLUMN 38 PIC X(28) FROM ALL "฿".
               05  LINE 16 COLUMN 38 PIC X(01) FROM     "Ü".
           03  BCK-CLR 03 FOR-CLR 15.
               05  LINE 10 COLUMN 38 PIC X(28) FROM SPACES.
               05  LINE 11 COLUMN 38 PIC X(28) FROM SPACES.
               05  LINE 12 COLUMN 38 PIC X(28) FROM SPACES.
               05  LINE 13 COLUMN 38 PIC X(28) FROM SPACES.
               05  LINE 14 COLUMN 38 PIC X(28) FROM SPACES.
               05  LINE 15 COLUMN 38 PIC X(28) FROM SPACES.
           03  BCK-CLR 00 FOR-CLR 03.
               05  LINE 10 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 11 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 39 PIC X(27) FROM ALL "฿".

       01  TELA-03.
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 08 COLUMN 15 VALUE " ".
               05  LINE 08 COLUMN 42 VALUE " ออออ ".
               05  LINE 08 COLUMN 64 VALUE " ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 08 COLUMN 17 VALUE "                 -       ".
               05  LINE 08 COLUMN 18 PIC X(15) FROM DES-CXA.
               05  LINE 08 COLUMN 36 PIC X(05) FROM TUR-TAB (TUR-CXA).
               05  LINE 08 COLUMN 50 PIC X(14) FROM DTA-AUX.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 15 PIC X(51) FROM ALL "฿".
               05  LINE 16 COLUMN 15 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 10 COLUMN 15 PIC X(51) FROM SPACES.
               05  LINE 11 COLUMN 15 PIC X(51) FROM SPACES.
               05  LINE 12 COLUMN 15 PIC X(51) FROM SPACES.
               05  LINE 13 COLUMN 15 PIC X(51) FROM SPACES.
               05  LINE 14 COLUMN 15 PIC X(51) FROM SPACES.
               05  LINE 15 COLUMN 15 PIC X(51) FROM SPACES.
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 10 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 11 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 16 PIC X(50) FROM ALL "฿".

       01  TELA-04.
           03  BCK-CLR 04 FOR-CLR 07.
               05  LINE 17 COLUMN 15 PIC X(51) FROM ALL "฿".
               05  LINE 19 COLUMN 15 PIC X(01) FROM     "Ü".
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 18 COLUMN 15 PIC X(51) FROM
                   "  DIFERาN€A DO CAIXA ........ R$".
               05  LINE 18 COLUMN 50 PIC ---.---.--9,99 FROM ACM-DIF.
           03  BCK-CLR 00 FOR-CLR 04.
               05  LINE 18 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 19 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 19 COLUMN 16 PIC X(50) FROM ALL "฿".

       01  TELA-05.
           03  BCK-CLR 04 FOR-CLR 07.
               05  LINE 17 COLUMN 15 PIC X(51) FROM ALL "฿".
               05  LINE 19 COLUMN 15 PIC X(01) FROM     "Ü".
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 18 COLUMN 15 PIC X(51) FROM
                   "  DESEJA CONFIRMAR OS LAN€AMENTOS DO CAIXA ?  [ ]".
           03  BCK-CLR 00 FOR-CLR 04.
               05  LINE 18 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 19 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 19 COLUMN 16 PIC X(50) FROM ALL "฿".

       01  TELA-06.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 17 COLUMN 15 PIC X(51) FROM ALL "฿".
               05  LINE 19 COLUMN 15 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 18 COLUMN 15 PIC X(51) FROM
                   "  AGUARDE, CONCLUINDO OS LAN€AMENTOS DO CAIXA ...".
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 18 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 19 COLUMN 66 PIC X(01) FROM     " ".
               05  LINE 19 COLUMN 16 PIC X(50) FROM ALL "฿".

       01  MENU-01  BCK-CLR 03 FOR-CLR 15.
           03  LINE 10 COLUMN 39 PIC X(26) FROM REG-001 (IND2).
           03  LINE 11 COLUMN 39 PIC X(26) FROM REG-001 (IND2 + 1).
           03  LINE 12 COLUMN 39 PIC X(26) FROM REG-001 (IND2 + 2).
           03  LINE 13 COLUMN 39 PIC X(26) FROM REG-001 (IND2 + 3).
           03  LINE 14 COLUMN 39 PIC X(26) FROM REG-001 (IND2 + 4).
           03  LINE 15 COLUMN 39 PIC X(26) FROM REG-001 (IND2 + 5).

       01  SETA-01  BCK-CLR 03.
           03  TR1-ACM LINE 10 COLUMN 65 VALUE " ".
           03  TR1-ABX LINE 15 COLUMN 65 VALUE " ".
           03  ST1-ACM LINE 10 COLUMN 65 VALUE "" FOR-CLR 14.
           03  ST1-ABX LINE 15 COLUMN 65 VALUE "" FOR-CLR 14.

       01  MENU-02  BCK-CLR 02 FOR-CLR 15.
           03  LINE 10 COLUMN 16 PIC X(49) FROM REG-002 (IND6).
           03  LINE 11 COLUMN 16 PIC X(49) FROM REG-002 (IND6 + 1).
           03  LINE 12 COLUMN 16 PIC X(49) FROM REG-002 (IND6 + 2).
           03  LINE 13 COLUMN 16 PIC X(49) FROM REG-002 (IND6 + 3).
           03  LINE 14 COLUMN 16 PIC X(49) FROM REG-002 (IND6 + 4).
           03  LINE 15 COLUMN 16 PIC X(49) FROM REG-002 (IND6 + 5).

       01  SETA-02  BCK-CLR 02.
           03  TR2-ACM LINE 10 COLUMN 65 VALUE " ".
           03  TR2-ABX LINE 15 COLUMN 65 VALUE " ".
           03  ST2-ACM LINE 10 COLUMN 65 VALUE "" FOR-CLR 14.
           03  ST2-ABX LINE 15 COLUMN 65 VALUE "" FOR-CLR 14.

       01  MEN-1-1  BCK-CLR 03 FOR-CLR 15.
           03  LINE IND4 COLUMN 39 PIC X(26) FROM REG-001 (IND3).

       01  MEN-1-2  BCK-CLR 01 FOR-CLR 15.
           03  LINE IND4 COLUMN 39 PIC X(26) FROM REG-001 (IND3).

       01  MEN-2-1  BCK-CLR 02 FOR-CLR 15.
           03  LINE IND8 COLUMN 16 PIC X(49) FROM REG-002 (IND7).

       01  MEN-2-2  BCK-CLR 07 FOR-CLR 12.
           03  LINE IND8 COLUMN 16 PIC X(49) FROM REG-002 (IND7).

       01  ENTR-01  BCK-CLR 04 FOR-CLR 15
                    LINE 18 COLUMN 62 PIC X(01) USING RSP-OPC AUTO.

       PROCEDURE       DIVISION  USING    PRM-OPR.

       ROT-0000-00.
           CALL     "DYSPOL"     USING    PRM-OPR
                                          PRM-SPL
           IF  LCK-SPL   =    1  GOBACK.
           OPEN     INPUT        CADCTR
                    I-O          CADCXA   CADPRS
           READ     CADCTR       CLOSE    CADCTR
           MOVE     DTD-CTR  TO  DTA-DTA  DTA-LIM
           MOVE     01       TO  IND1
           ADD      01       TO  DIA-DTA
           MOVE     31       TO  DIA-LIM
           MOVE     35       TO  IND-MSG
           IF  FST-CXA   =  "9A" PERFORM  ROT-MSGM-00
                                 GO       ROT-EXIT-00.
       ROT-PESQ-00.
           MOVE     SPACES   TO  REG-001 (IND1)
           IF  IND1    <     20  ADD      1  TO  IND1
                                 GO       ROT-PESQ-00.
           MOVE     00       TO  IND1
           MOVE     10       TO  IND4
           MOVE     01       TO  IND2     IND3
           MOVE     SPACES   TO  CHV-CXA  FLG-AUX.

       ROT-PESQ-10.
           START    CADCXA       KEY  >   CHV-CXA
           IF  FST-CXA   =  "23" GO       ROT-PESQ-40.
           READ     CADCXA  NEXT
           IF  FLG-CXA   =    2  GO       ROT-PESQ-10.
           MOVE     SPACES   TO  CHV-PRS  FLG-AX1
           MOVE     TUR-CXA  TO  TUR-PRS
           MOVE     NUM-CXA  TO  CXA-PRS.

       ROT-PESQ-20.
           START    CADPRS       KEY  >   CHV-PRS
           IF  FST-PRS   =  "23" GO       ROT-PESQ-30.
           READ     CADPRS  NEXT
           IF  TUR-PRS  NOT   =  TUR-CXA  OR  CXA-PRS
               NOT  =   NUM-CXA  PERFORM  ROT-PESQ-30
                                 GO       ROT-PESQ-10.
           IF  VDD-PRS   =    0  GO       ROT-PESQ-20.
           MOVE     ""      TO  FLG-AX1  FLG-AUX.
           PERFORM  ROT-PESQ-30  GO       ROT-PESQ-10.

       ROT-PESQ-30.
           MOVE     TUR-TAB     (TUR-CXA) TO  TUR-AX1
           MOVE     DES-CXA  TO  DES-AX1
           ADD      1        TO  IND1
           MOVE     REG-AX1  TO  REG-001 (IND1)
           MOVE     CHV-CXA  TO  CHV-001 (IND1).

       ROT-PESQ-40.
           MOVE     34       TO  IND-MSG
           IF  IND1      =    0  PERFORM  ROT-MSGM-00
                                 GO       ROT-EXIT-00.
       ROT-PESQ-50.
           DISPLAY  TELA-01      PERFORM  ROT-DATA-00
           IF  FLG-DTA   =    0  GO       ROT-EXIT-00.
           MOVE     18       TO  IND-MSG
           IF  DTA-DTA   <       DTI-CTR
               OR    >  DTA-LIM  PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-50.
           DISPLAY  TELA-02
           MOVE     SEM-TAB     (FLG-DTA) TO  SEM-AUX
           MOVE     DIA-DTA  TO  DIA-AUX
           MOVE     MES-DTA  TO  MES-AUX
           MOVE     ANO-DTA  TO  ANO-AUX.

       ROT-MENU-00.
           DISPLAY  TR1-ACM      TR1-ABX
           IF (IND2  +  05)  <   IND1
                                 DISPLAY  ST1-ABX.
           IF  IND2  >  01       DISPLAY  ST1-ACM.
           DISPLAY  MENU-01      MEN-1-2.

       ROT-MENU-10.
           MOVE  1  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-10.
           IF  TCL-MSG   =   05  GO       ROT-MENU-20.
           IF  TCL-MSG   =   06  GO       ROT-MENU-30.
           IF  TCL-MSG   =   53  GO       ROT-MENU-40.
           IF  TCL-MSG   =   54  GO       ROT-MENU-50.
           IF  TCL-MSG   >   00  GO       ROT-MENU-10.
           IF  HEX-MSG   =   27  GO       ROT-PESQ-50.
           MOVE     CHV-001      (IND3)   TO  CHV-CXA
           MOVE     REG-001      (IND3)   TO  REG-AX1
           IF  FLG-AX1   =  " "  AND
               FLG-AUX   =  ""  MOVE 47  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-MENU-10.
           READ     CADCXA       MOVE     1  TO  IND5
           MOVE     TUR-CXA  TO  TUR-MVC
           MOVE     NUM-CXA  TO  CXA-MVC
           MOVE     DTA-DTA  TO  DTA-CXA  DTA-MVC
           REWRITE  REG-CXA      GO       ROT-SOMA-00.

       ROT-MENU-20.
           IF  IND3      =   01  GO       ROT-MENU-10.
           IF  IND4      =   10  SUBTRACT 1 FROM IND2
                                 SUBTRACT 1 FROM IND3
                                 GO       ROT-MENU-00.
           DISPLAY  MEN-1-1      SUBTRACT 1 FROM IND3
           COMPUTE  IND4     =   IND4  -  1
           DISPLAY  MEN-1-2      GO       ROT-MENU-10.

       ROT-MENU-30.
           IF  IND3      =  IND1 GO       ROT-MENU-10.
           IF  IND4      =   15  ADD      1  TO  IND2
                                 ADD      1  TO  IND3
                                 GO       ROT-MENU-00.
           DISPLAY  MEN-1-1      ADD      1  TO  IND3
           COMPUTE  IND4     =   IND4  +  1
           DISPLAY  MEN-1-2      GO       ROT-MENU-10.

       ROT-MENU-40.
           IF  IND2      =   01  GO       ROT-MENU-10.
           MOVE     10       TO  IND4
           IF  IND2      >    6  COMPUTE  IND3 = IND2 - 6
                                 COMPUTE  IND2 = IND2 - 6
           ELSE
                                 MOVE  1  TO   IND2  IND3.
           GO      ROT-MENU-00.

       ROT-MENU-50.
           IF  IND1 < (IND2 + 6) GO       ROT-MENU-10.
           COMPUTE  IND3     =   IND2  +  6
           COMPUTE  IND2     =   IND2  +  6
           MOVE     10  TO  IND4 GO       ROT-MENU-00.

       ROT-SOMA-00.
           MOVE     SPACES   TO  REG-002  (IND5)
           IF  IND5    <     60  ADD      1  TO  IND5
                                 GO       ROT-SOMA-00.
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-CXA  TO  TUR-PRS
           MOVE     NUM-CXA  TO  CXA-PRS
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-03
           START    CADPRS       KEY  >   CHV-PRS
           READ     CADPRS  NEXT
           MOVE     ATV-PRS  TO  ATV-AUX
           MOVE     10       TO  IND8
           MOVE     01       TO  IND6     IND7
           MOVE     ZEROS    TO  ACM-DIF  IND5.

       ROT-SOMA-05.
           IF  ATV-PRS   =    1  OPEN   INPUT  CADECR
                                               CADEST.
           IF  ATV-PRS   =    2  OPEN   INPUT  CADEST
                                               CADLCX.
           IF  ATV-PRS   =    3  OPEN   INPUT  CADLCX.
           IF  ATV-PRS   =    4  OPEN   I-O    CADLCX
                                        INPUT  CADCRD.
           IF  ATV-PRS   =    6  OPEN   I-O    CADLCX
                                        INPUT  CADLDP.
           IF  ATV-PRS   =    7  OPEN   I-O    CADLCX
                                        INPUT  CADLBC.
       ROT-SOMA-10.
           IF (ATV-PRS   =    4  AND     (PRX-PRS   >  "9"
                                 AND          NOT   =  "S"))  OR
              (ATV-PRS   =    7  AND      PRX-PRS   >  "1")   OR
               ATV-PRS   =    5  GO       ROT-SOMA-70.
           MOVE     ZEROS    TO  CHV-ECR  QTD-PRS
                                 CTD-PRS  VDD-PRS
           IF  ATV-PRS   =    1  GO       ROT-SOMA-20.
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX.

       ROT-SOMA-15.
           START    CADLCX       KEY  >   CHV-LCX
           IF  FST-LCX   =  "23" GO       ROT-SOMA-70.
           READ     CADLCX  NEXT
           IF  TUR-LCX  NOT   =  TUR-PRS  OR
               CXA-LCX  NOT   =  CXA-PRS  OR
               ATV-LCX  NOT   =  ATV-PRS  OR  PRX-LCX
               NOT  =   PRX-PRS  GO       ROT-SOMA-70.
           IF  ATV-PRS   =    2  PERFORM  ROT-SOMA-25.
           IF  ATV-PRS   =    3  PERFORM  ROT-SOMA-30.
           IF  ATV-PRS   =    4  GO       ROT-SOMA-35.
           IF  ATV-PRS   =    6  GO       ROT-SOMA-50.
           IF  ATV-PRS   =    7  GO       ROT-SOMA-60.
           GO       ROT-SOMA-15.

       ROT-SOMA-20.
           START    CADECR       KEY  >   CHV-ECR
           IF  FST-ECR   =  "23" GO       ROT-SOMA-70.
           READ     CADECR  NEXT
           IF (CX1-ECR  NOT   =  CXA-PRS  AND
               CX2-ECR  NOT   =  CXA-PRS) OR  CMB-ECR
               NOT   =  PRX-PRS  GO       ROT-SOMA-20.
           MOVE     ATV-PRS  TO  PRX-EST
           MOVE     CMB-ECR  TO  SUF-EST
           READ     CADEST
           ADD      VDC-ECR  TO  QTD-PRS
           COMPUTE  CTD-PRS   =  CTD-PRS  +   VDC-ECR  *   CTM-EST
           COMPUTE  VDD-PRS   =  VDD-PRS  +   VDC-ECR  *   VAL-ECR
           GO       ROT-SOMA-20.

       ROT-SOMA-25.
           MOVE     PRX-LCX  TO  PRX-EST
           MOVE     COD-001  TO  SUF-EST
           READ     CADEST
           ADD      QTD-LCX  TO  QTD-PRS
           COMPUTE  CTD-PRS   =  CTD-PRS  +   QTD-LCX  *   CTM-EST
           COMPUTE  VDD-PRS   =  VDD-PRS  +   QTD-LCX  *   VDA-EST.

       ROT-SOMA-30.
           ADD      QTD-LCX  TO  QTD-PRS
           ADD      VAL-LCX  TO  VDD-PRS.

       ROT-SOMA-35.
           IF  PRX-PRS   =  "1"  OR  "S"
                                 PERFORM  ROT-SOMA-45
                                 GO       ROT-SOMA-15.
           MOVE     PRX-LCX  TO  PRX-CRD
           MOVE     DTA-003  TO  DTA-CRD
           MOVE     NUM-003  TO  NUM-CRD
           MOVE     LCT-003  TO  LCT-CRD
           READ     CADCRD
           IF  FST-CRD   =  "00" PERFORM  ROT-SOMA-40
                                 GO       ROT-SOMA-15.
           DELETE   CADLCX       GO       ROT-SOMA-15.

       ROT-SOMA-40.
           ADD      1        TO  QTD-PRS
           ADD      VAL-CRD  TO  VDD-PRS.

       ROT-SOMA-45.
           ADD      1        TO  QTD-PRS
           ADD      VAL-LCX  TO  VDD-PRS.

       ROT-SOMA-50.
           IF  PRX-PRS   =  "1"  PERFORM  ROT-SOMA-45
                                 GO       ROT-SOMA-15.
           MOVE     PRX-006  TO  PRX-LDP
           MOVE     SUF-006  TO  SUF-LDP
           MOVE     DTA-006  TO  DTA-LDP
           MOVE     LCT-006  TO  LCT-LDP
           READ     CADLDP
           IF  FLG-LDP   =  "P"  PERFORM  ROT-SOMA-55
                                 GO       ROT-SOMA-15.
           DELETE   CADLCX       GO       ROT-SOMA-15.

       ROT-SOMA-55.
           ADD      1        TO  QTD-PRS
           ADD      VAL-LDP  TO  VDD-PRS.

       ROT-SOMA-60.
           MOVE     BCO-007  TO  CHV-LBC
           MOVE     DTA-007  TO  DTA-LBC
           MOVE     LCT-007  TO  LCT-LBC
           READ     CADLBC
           IF  COD-LBC   =   1   PERFORM  ROT-SOMA-65
                                 GO       ROT-SOMA-15.
           DELETE   CADLCX       GO       ROT-SOMA-15.

       ROT-SOMA-65.
           ADD      1        TO  QTD-PRS
           ADD      VAL-LBC  TO  VDD-PRS.

       ROT-SOMA-70.
           MOVE     DES-PRS  TO  DES-AX2
           MOVE     QTD-PRS  TO  QTD-AX2
           MOVE     VDD-PRS  TO  VAL-AX2
           ADD      1        TO  IND5
           MOVE     REG-AX2  TO  REG-002  (IND5)
           MOVE     CHV-PRS  TO  CHV-002  (IND5)
           IF  ATV-PRS   >    4  COMPUTE  ACM-DIF   =
                                          ACM-DIF   +  VDD-PRS
           ELSE                  COMPUTE  ACM-DIF   =
                                          ACM-DIF   -  VDD-PRS.
           REWRITE  REG-PRS      DISPLAY  TELA-04
           START    CADPRS       KEY  >   CHV-PRS
           IF  FST-PRS   =  "23" GO       ROT-SOMA-75.
           READ     CADPRS  NEXT
           IF  ATV-PRS   >    7  GO       ROT-SOMA-75.
           IF  ATV-PRS   =       ATV-AUX
                                 GO       ROT-SOMA-10.
           PERFORM  ROT-SOMA-75  GO       ROT-SOMA-05.

       ROT-SOMA-75.
           IF  ATV-AUX   =    1  CLOSE    CADECR
                                          CADEST.
           IF  ATV-AUX   =    2  CLOSE    CADEST
                                          CADLCX.
           IF  ATV-AUX   =    3  CLOSE    CADLCX.
           IF  ATV-AUX   =    4  CLOSE    CADCRD
                                          CADLCX.
           IF  ATV-AUX   =    6  CLOSE    CADLCX
                                          CADLDP.
           IF  ATV-AUX   =    7  CLOSE    CADLBC
                                          CADLCX.
           MOVE     ATV-PRS  TO  ATV-AUX.

       ROT-MCXA-00.
           DISPLAY  TR2-ACM      TR2-ABX
           IF (IND6  +  05)  <   IND5
                                 DISPLAY  ST2-ABX.
           IF  IND6  >  01       DISPLAY  ST2-ACM.
           DISPLAY  MENU-02      MEN-2-2.

       ROT-MCXA-10.
           MOVE  1  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MCXA-10.
           IF  TCL-MSG   =   05  GO       ROT-MCXA-20.
           IF  TCL-MSG   =   06  GO       ROT-MCXA-30.
           IF  TCL-MSG   =   53  GO       ROT-MCXA-40.
           IF  TCL-MSG   =   54  GO       ROT-MCXA-50.
           IF  TCL-MSG   >   00  GO       ROT-MCXA-10.
           IF  HEX-MSG   =   27  GO       ROT-CONF-00.
           MOVE     CHV-002      (IND7)   TO  CHV-PRS
           MOVE     CHV-PRS  TO  PRS-MVC
           MOVE     IND8     TO  POS-MVC
           READ     CADPRS
           IF  ATV-PRS   <    5  COMPUTE  ACM-DIF   =
                                          ACM-DIF   +   VDD-PRS
           ELSE                  COMPUTE  ACM-DIF   =
                                          ACM-DIF   -   VDD-PRS.
           IF  ATV-PRS   =    1  CALL    "DY1211" USING PRM-MVC
                                 CANCEL  "DY1211".
           IF  ATV-PRS   =    2  CALL    "DY1212" USING PRM-MVC
                                 CANCEL  "DY1212".
           IF  ATV-PRS   =    3  CALL    "DY1213" USING PRM-MVC
                                 CANCEL  "DY1213".
           IF  ATV-PRS   =    4  CALL    "DY1214" USING PRM-MVC
                                 CANCEL  "DY1214".
           IF  ATV-PRS   =    5  CALL    "DY1215" USING PRM-MVC
                                 CANCEL  "DY1215".
           IF  ATV-PRS   =    6  CALL    "DY1216" USING PRM-MVC
                                 CANCEL  "DY1216".
           IF  ATV-PRS   =    7  CALL    "DY1217" USING PRM-MVC
                                 CANCEL  "DY1217".
           MOVE     PRS-MVC  TO  CHV-PRS
           READ     CADPRS
           IF  ATV-PRS   <    5  COMPUTE  ACM-DIF   =
                                          ACM-DIF   -   VDD-PRS
           ELSE                  COMPUTE  ACM-DIF   =
                                          ACM-DIF   +   VDD-PRS.
           MOVE     DES-PRS  TO  DES-AX2
           MOVE     QTD-PRS  TO  QTD-AX2
           MOVE     VDD-PRS  TO  VAL-AX2
           MOVE     REG-AX2  TO  REG-002  (IND7)
           DISPLAY  MEN-2-2      TELA-04  GO        ROT-MCXA-10.

       ROT-MCXA-20.
           IF  IND7      =   01  GO       ROT-MCXA-10.
           IF  IND8      =   10  SUBTRACT 1 FROM IND6
                                 SUBTRACT 1 FROM IND7
                                 GO       ROT-MCXA-00.
           DISPLAY  MEN-2-1      SUBTRACT 1 FROM IND7
           COMPUTE  IND8     =   IND8  -  1
           DISPLAY  MEN-2-2      GO       ROT-MCXA-10.

       ROT-MCXA-30.
           IF  IND7      =  IND5 GO       ROT-MCXA-10.
           IF  IND8      =   15  ADD      1  TO  IND6
                                 ADD      1  TO  IND7
                                 GO       ROT-MCXA-00.
           DISPLAY  MEN-2-1      ADD      1  TO  IND7
           COMPUTE  IND8     =   IND8  +  1
           DISPLAY  MEN-2-2      GO       ROT-MCXA-10.

       ROT-MCXA-40.
           IF  IND6      =   01  GO       ROT-MCXA-10.
           IF  IND6      >   06  COMPUTE  IND7 = IND6 - 6
                                 COMPUTE  IND6 = IND6 - 6
           ELSE
                                 MOVE  1  TO  IND6   IND7.
           MOVE     10  TO  IND8 GO       ROT-MCXA-00.

       ROT-MCXA-50.
           IF  IND5 < (IND6 + 6) GO       ROT-MCXA-10.
           COMPUTE  IND7      =  IND6  +  6
           COMPUTE  IND6      =  IND6  +  6
           MOVE     10  TO  IND8 GO       ROT-MCXA-00.

       ROT-CONF-00.
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-MVC  TO  TUR-PRS
           MOVE     CXA-MVC  TO  CXA-PRS
           MOVE     8        TO  ATV-PRS
           START    CADPRS       KEY   >  CHV-PRS
           READ     CADPRS  NEXT
           MOVE     ACM-DIF  TO  VDD-PRS
           REWRITE  REG-PRS
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-MVC  TO  TUR-PRS
           MOVE     CXA-MVC  TO  CXA-PRS.

       ROT-CONF-10.
           START    CADPRS       KEY  >   CHV-PRS
           IF  FST-PRS   =  "23" GO       ROT-CONF-60.
           READ     CADPRS  NEXT
           IF  TUR-PRS  NOT   =  TUR-MVC  OR  CXA-PRS
               NOT  =   CXA-MVC  GO       ROT-CONF-60.
           IF  VDD-PRS   =    0  GO       ROT-CONF-10.
           MOVE     REG-001      (IND3)   TO  REG-AX1
           MOVE     ""      TO  FLG-AX1  FLG-AUX
           MOVE     REG-AX1  TO  REG-001  (IND3)
           PERFORM  ROT-RTLA-00  DISPLAY  MEN-1-2.

       ROT-CONF-20.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  TELA-05      ACCEPT   ENTR-01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =  "01" GO       ROT-EXIT-00.
           IF  TECLADO   =  "02" PERFORM  ROT-HELP-00
                                 GO       ROT-CONF-20.
           IF  RSP-OPC  NOT   =  "S"
                                 GO       ROT-CONF-20.
           MOVE     0        TO  ATV-AUX
           MOVE     2        TO  FLG-CXA
           DISPLAY  TELA-06      REWRITE  REG-CXA.

       ROT-CONF-30.
           ADD      1        TO  ATV-AUX
           IF  ATV-AUX   >    8  GO       ROT-EXIT-00.
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-MVC  TO  TUR-PRS
           MOVE     CXA-MVC  TO  CXA-PRS
           MOVE     ATV-AUX  TO  ATV-PRS
           IF  ATV-AUX   =    1  CLOSE    CADCXA
                                 OPEN     I-O      CADECR
                                                   CADEST.
           IF  ATV-AUX   =    2  CLOSE    CADECR
                                 OPEN     INPUT    CADLCX.
           IF  ATV-AUX   =    3  CLOSE    CADEST.
           IF  ATV-AUX   =    4  OPEN     I-O
                                          CADFAT   CADCDV
                                          CADCRD   CADSDO.
           IF  ATV-AUX   =    5  CLOSE    CADFAT
                                          CADCDV   CADCRD.
           IF  ATV-AUX   =    6  OPEN     I-O      CADLDP.
           IF  ATV-AUX   =    7  CLOSE    CADLDP   CADSDO
                                 OPEN     I-O      CADLBC.
           IF  ATV-AUX   =    8  CLOSE    CADLBC   CADLCX.

       ROT-CONF-40.
           START    CADPRS       KEY  >   CHV-PRS
           IF  FST-PRS   =  "23" GO       ROT-EXIT-00.
           READ     CADPRS  NEXT
           IF  ATV-PRS  NOT   =  ATV-AUX
                                 GO       ROT-CONF-30.
           IF (ATV-PRS   =    4  AND     (PRX-PRS   >  "I"
                                 AND      NOT       =  "S"))  OR
              (ATV-PRS   =    5  AND     (PRX-PRS   <  "A"
                                 OR       PRX-PRS   >  "I"))  OR
              (ATV-PRS   =    7  AND      PRX-PRS   >  "1")
                                 GO       ROT-CONF-50.
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SPACES   TO  COD-LCX
           MOVE     ZEROS    TO  CHV-ECR
           IF  ATV-PRS   =    1  GO       ROT-BXA1-00.
           IF  ATV-PRS   =    2  GO       ROT-BXA2-00.
           IF  ATV-PRS   =    4  GO       ROT-BXA4-00.
           IF  ATV-PRS   =    5  GO       ROT-BXA5-00.
           IF  ATV-PRS   =    6  GO       ROT-BXA6-00.
           IF  ATV-PRS   =    7  GO       ROT-BXA7-00.

       ROT-CONF-50.
           ADD      QTD-PRS  TO  QTM-PRS
           ADD      CTD-PRS  TO  CTM-PRS
           ADD      VDD-PRS  TO  VDM-PRS
           REWRITE  REG-PRS      GO       ROT-CONF-40.

       ROT-CONF-60.
           MOVE     SPACES   TO  FLG-AX1  FLG-AUX
           MOVE     REG-AX1  TO  REG-001  (IND3)
           PERFORM  ROT-RTLA-00  DISPLAY  MEN-1-2
           GO       ROT-MENU-10.

       ROT-BXA1-00.
           START    CADECR       KEY  >   CHV-ECR
           IF  FST-ECR   =  "23" GO       ROT-CONF-50.
           READ     CADECR  NEXT
           IF (CX1-ECR  NOT   =  CXA-PRS  AND
               CX2-ECR  NOT   =  CXA-PRS) OR  CMB-ECR
               NOT   =  PRX-PRS  GO       ROT-BXA1-00.

       ROT-BXA1-10.
           READ     CADECR       MOVE 20  TO  IND-MSG
           IF  FST-ECR   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-BXA1-10.
           MOVE     FCL-ECR  TO  ICL-ECR
           ADD      AFC-ECR  TO  AFD-ECR
           ADD      VDC-ECR  TO  VDD-ECR
           MOVE     ZEROS    TO  AFC-ECR  VDC-ECR
           REWRITE  REG-ECR      GO       ROT-BXA1-00.

       ROT-BXA2-00.
           START    CADLCX       KEY  >   CHV-LCX
           IF  FST-LCX   =  "23" GO       ROT-CONF-50.
           READ     CADLCX  NEXT
           IF  TUR-LCX  NOT   =  TUR-PRS  OR
               CXA-LCX  NOT   =  CXA-PRS  OR
               ATV-LCX  NOT   =  ATV-PRS  OR  PRX-LCX
               NOT  =  PRX-PRS   GO       ROT-CONF-50.
           MOVE     PRX-LCX  TO  PRX-EST
           MOVE     COD-001  TO  SUF-EST.

       ROT-BXA2-10.
           READ     CADEST       MOVE 20  TO  IND-MSG
           IF  FST-EST   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-BXA2-10.
           ADD      QTD-LCX  TO  QVM-EST      QVA-EST
           COMPUTE  SDO-EST   =  SDO-EST  -   QTD-LCX
           REWRITE  REG-EST      GO       ROT-BXA2-00.

       ROT-BXA4-00.
           IF  PRX-PRS   >  "9"  AND  <   "J"
                                 GO       ROT-BXA4-40.
           START    CADLCX       KEY  >   CHV-LCX
           IF  FST-LCX   =  "23" GO       ROT-BXA4-40.
           READ     CADLCX  NEXT
           IF  TUR-LCX  NOT   =  TUR-PRS  OR
               CXA-LCX  NOT   =  CXA-PRS  OR
               ATV-LCX  NOT   =  ATV-PRS  OR  PRX-LCX
               NOT  =  PRX-PRS   GO       ROT-BXA4-40.
           IF  PRX-PRS   =  "S"  GO       ROT-BXA4-30.
           IF  PRX-PRS   >  "1"  AND  <   "A"
                                 GO       ROT-BXA4-20.
       ROT-BXA4-10.
           MOVE     CLI-002  TO  CLI-FAT
           MOVE     DTA-002  TO  DTA-FAT
           MOVE     FAT-002  TO  NUM-FAT
           READ     CADFAT       MOVE 20  TO  IND-MSG
           IF  FST-FAT   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-BXA4-10.
           COMPUTE  VAL-FAT   =  VAL-FAT  -   VAL-LCX
           IF  VAL-FAT   >    0  REWRITE  REG-FAT
                                 GO       ROT-BXA4-00.
           DELETE   CADFAT       GO       ROT-BXA4-00.

       ROT-BXA4-20.
           MOVE     PRX-LCX  TO  PRX-CRD
           MOVE     DTA-003  TO  DTA-CRD
           MOVE     NUM-003  TO  NUM-CRD
           MOVE     LCT-003  TO  LCT-CRD
           READ     CADCRD       MOVE 20  TO  IND-MSG
           IF  FST-CRD   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-BXA4-20.
           DELETE   CADCRD       GO       ROT-BXA4-00.

       ROT-BXA4-30.
           MOVE     COD-LCX  TO  CHV-CDV
           READ     CADCDV       MOVE 20  TO  IND-MSG
           IF  FST-CDV   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-BXA4-30.
           DELETE   CADCDV       GO       ROT-BXA4-00.

       ROT-BXA4-40.
           MOVE     1        TO  PRX-SDO
           MOVE     PRX-PRS  TO  SUF-SDO
           READ     CADSDO       MOVE 20  TO  IND-MSG
           IF  FST-SDO   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-BXA4-40.
           ADD      VDD-PRS  TO  SAI-SDO
           REWRITE  REG-SDO      GO       ROT-CONF-50.

       ROT-BXA5-00.
           MOVE     1        TO  PRX-SDO
           MOVE     PRX-PRS  TO  SUF-SDO
           READ     CADSDO       MOVE 20  TO  IND-MSG
           IF  FST-SDO   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-BXA5-00.
           ADD      VDD-PRS  TO  ENT-SDO
           REWRITE  REG-SDO      GO       ROT-CONF-50.

       ROT-BXA6-00.
           START    CADLCX       KEY  >   CHV-LCX
           IF  FST-LCX   =  "23" GO       ROT-CONF-50.
           READ     CADLCX  NEXT
           IF  TUR-LCX  NOT   =  TUR-PRS  OR
               CXA-LCX  NOT   =  CXA-PRS  OR
               ATV-LCX  NOT   =  ATV-PRS  OR  PRX-LCX
               NOT  =  PRX-PRS   GO       ROT-CONF-50.
           IF  PRX-PRS   >  "1"  GO       ROT-BXA6-10.
           MOVE     PRX-005  TO  PRX-LDP
           MOVE     SUF-005  TO  SUF-LDP
           MOVE     DTA-MVC  TO  DTA-LDP
           MOVE     LCT-005  TO  LCT-LDP
           MOVE     "L"      TO  FLG-LDP
           MOVE     HST-LCX  TO  HST-LDP
           MOVE     VAL-LCX  TO  VAL-LDP
           PERFORM  ROT-BXA6-20  GO       ROT-BXA6-00.

       ROT-BXA6-10.
           MOVE     PRX-006  TO  PRX-LDP
           MOVE     SUF-006  TO  SUF-LDP
           MOVE     DTA-006  TO  DTA-LDP
           MOVE     LCT-006  TO  LCT-LDP
           READ     CADLDP       MOVE 20  TO  IND-MSG
           IF  FST-LDP   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-BXA6-10.
           DELETE   CADLDP
           MOVE     DTA-MVC  TO  DTA-LDP
           MOVE     "L"      TO  FLG-LDP.

       ROT-BXA6-20.
           WRITE    REG-LDP      ADD   2  TO  LCT-LDP
           IF  FST-LDP   =  "22" GO       ROT-BXA6-20.

       ROT-BXA6-30.
           MOVE     2        TO  PRX-SDO
           MOVE     PRX-LDP  TO  SUF-SDO
           READ     CADSDO       MOVE 20  TO  IND-MSG
           IF  FST-SDO   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-BXA6-30.
           ADD      VAL-LDP  TO  SAI-SDO
           REWRITE  REG-SDO      GO       ROT-BXA6-00.

       ROT-BXA7-00.
           START    CADLCX       KEY  >   CHV-LCX
           IF  FST-LCX   =  "23" GO       ROT-CONF-50.
           READ     CADLCX  NEXT
           IF  TUR-LCX  NOT   =  TUR-PRS  OR
               CXA-LCX  NOT   =  CXA-PRS  OR
               ATV-LCX  NOT   =  ATV-PRS  OR  PRX-LCX
               NOT  =  PRX-PRS   GO       ROT-CONF-50.

       ROT-BXA7-10.
           MOVE     BCO-007  TO  BCO-LBC
           MOVE     DTA-007  TO  DTA-LBC
           MOVE     LCT-007  TO  LCT-LBC
           READ     CADLBC       MOVE 20  TO  IND-MSG
           IF  FST-LBC   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-BXA7-10.
           MOVE     2        TO  COD-LBC
           REWRITE  REG-LBC      GO       ROT-BXA7-00.

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
           CLOSE    CADCXA   CADPRS   GOBACK.
