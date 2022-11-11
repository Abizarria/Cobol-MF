       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1155.
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

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST
                       FILE          STATUS  IS  FST-EST.

           SELECT      CADLES        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LES
                       FILE          STATUS  IS  FST-LES.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADATV.CPY.
       COPY  CADEST.CPY.
       COPY  CADLES.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-ATV          PIC  X(02).
           03  FST-EST          PIC  X(02).
           03  FST-LES          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  NUM-OPC          PIC  9(01).
           03  ACM-QTD          PIC S9(07)V9.
           03  ACM-VAL          PIC S9(08)V99.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  TAB-OPC.
               05  FILLER       PIC  X(18)  VALUE " Selecionar Grupo ".
               05  FILLER       PIC  X(18)  VALUE " Rela‡Æo Completa ".
           03  RED-OPC   REDEFINES   TAB-OPC.
               05  OPC-TAB      PIC  X(18)  OCCURS  02.

       01  CAB-001.
           03  FILLER           PIC  X(47)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(38)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC X(125)  VALUE
               "RELACAO DOS ACERTOS DE SALDOS NO ESTOQUE".
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC X(137)  VALUE  ALL  "-".

       01  CAB-004.
           03  FILLER           PIC  X(64) VALUE  "CODIDO     DATA
      -        "OPERADOR      DESCRICAO DO PRODUTO".
           03  FILLER           PIC  X(73) VALUE  "SD ANTERIOR     CORRI
      -        "GIDO    DIF. / DIA      DIF. / MES   VALOR DIFEREN€A".

       01  CAB-005              PIC X(137)  VALUE  ALL  "- ".

       01  DET-001.
           03  PRX-DT1          PIC  X(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DT1          PIC  9(03).
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  DIA-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DT1          PIC  9(02).
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  OPR-DT1          PIC  X(14).
           03  DES-DT1          PIC  X(30).
           03  SDA-DT1          PIC  ----.--9,9.
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  SDC-DT1          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  DFD-DT1          PIC  ----.--9,9.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  DFM-DT1          PIC  --.---.--9,9   BLANK WHEN ZEROS.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  VAL-DT1          PIC  ---.---.--9,99 BLANK WHEN ZEROS.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  10.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1150".
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

       01  PRM-PSQ.
           03  OPC-PSQ          PIC  9(01)  VALUE  1.
           03  IND-PSQ          PIC  X(01)  VALUE  "1".
           03  LIN-PSQ          PIC  9(02)  VALUE  12.
           03  COL-PSQ          PIC  9(02)  VALUE  40.
           03  SB1-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB2-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB3-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB4-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB5-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB6-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB7-PSQ          PIC  X(26)  VALUE  SPACES.

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

       01  PRM-IMP.
           03  ARQ-IMP          PIC  9(06).
           03  PAG-IMP          PIC  9(03).
           03  FLG-IMP          PIC  9(01)  VALUE  2.
           03  LIN-IMP          PIC  9(02)  VALUE  16.
           03  CL1-IMP          PIC  9(02)  VALUE  40.
           03  CL2-IMP          PIC  9(02)  VALUE  46.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 31 VALUE " ÉÍÍÍÍ          ÍÍÍÍ» ".
               05  LINE 10 COLUMN 31 VALUE " º                    º ".
               05  LINE 11 COLUMN 31 VALUE " º                    º ".
               05  LINE 12 COLUMN 31 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 09 COLUMN 38 VALUE " Corre‡Æo ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 10 COLUMN 55 VALUE "  ".
               05  LINE 11 COLUMN 55 VALUE "  ".
               05  LINE 12 COLUMN 55 VALUE "  ".
               05  LINE 13 COLUMN 33 PIC X(24) FROM "‡Æo de Saldos  º".

       01  SETA-01.
           03  BCK-CLR 07 FOR-CLR 12
               LINE 10 COLUMN 34 PIC X(18) FROM OPC-TAB (1).
           03  BCK-CLR 02 FOR-CLR 07
               LINE 11 COLUMN 34 PIC X(18) FROM OPC-TAB (2).

       01  SETA-02.
           03  BCK-CLR 02 FOR-CLR 07
               LINE 10 COLUMN 34 PIC X(18) FROM OPC-TAB (1).
           03  BCK-CLR 07 FOR-CLR 12
               LINE 11 COLUMN 34 PIC X(18) FROM OPC-TAB (2).

       01  TELA-02.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 07 COLUMN 21 VALUE " Administra‡Æo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 08 COLUMN 23 VALUE " Controle de Estoques ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 09 COLUMN 25 VALUE " EmissÆo de Relat¢rio ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 07 COLUMN 43 VALUE "ß".
               05  LINE 08 COLUMN 45 VALUE "ß".
               05  LINE 09 COLUMN 47 VALUE "ß".
               05  LINE 08 COLUMN 22 VALUE "Ü".
               05  LINE 09 COLUMN 24 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 10 COLUMN 21 VALUE
                   " ÉÍÍ                        ÍÍÍÍÍÍÍ» ".
               05  LINE 11 COLUMN 21 VALUE
                   " º                                   º ".
               05  LINE 12 COLUMN 21 VALUE
                   " º                                   º ".
               05  LINE 13 COLUMN 21 VALUE
                   " º                                   º ".
               05  LINE 14 COLUMN 21 VALUE
                   " º                                   º ".
               05  LINE 15 COLUMN 21 VALUE
                   " º                                   º ".
               05  LINE 16 COLUMN 21 VALUE
                   " º                                   º ".
               05  LINE 17 COLUMN 21 VALUE
                   " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 10 COLUMN 27 VALUE "  Corre‡Æo de Saldos  ".
           03  BCK-CLR  00.
               05  LINE 11 COLUMN 60 VALUE "  ".
               05  LINE 12 COLUMN 60 VALUE "  ".
               05  LINE 13 COLUMN 60 VALUE "  ".
               05  LINE 14 COLUMN 60 VALUE "  ".
               05  LINE 15 COLUMN 60 VALUE "  ".
               05  LINE 16 COLUMN 60 VALUE "  ".
               05  LINE 17 COLUMN 60 VALUE "  ".
               05  LINE 18 COLUMN 23 PIC X(39) FROM SPACES.

       01  TELA-03 BCK-CLR 02.
           03  FOR-CLR  07.
               05  LINE 11 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 13 COLUMN 25 PIC X(01) FROM     "Ü".
           03  FOR-CLR  14.
               05  LINE 12 COLUMN 25 PIC X(30) FROM "   Grupo .:".
           03  FOR-CLR  00.
               05  LINE 12 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  ENTR-03  BCK-CLR 02 FOR-CLR 15.
           03  LINE 12 COLUMN 37 PIC X(15) FROM DES-ATV.

       01  TELA-04 BCK-CLR 04.
           03  FOR-CLR  07.
               05  LINE 11 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 13 COLUMN 25 PIC X(01) FROM     "Ü".
           03  FOR-CLR  15.
               05  LINE 12 COLUMN 25 PIC X(30) FROM
                   "       RELA€ÇO COMPLETA       ".
           03  FOR-CLR  00.
               05  LINE 12 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  TELA-05 BCK-CLR 01.
           03  FOR-CLR  07.
               05  LINE 14 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 16 COLUMN 25 PIC X(01) FROM     "Ü".
           03  FOR-CLR  15.
               05  LINE 15 COLUMN 25 PIC X(30) FROM
                   "   P ginas Montadas Ä    ".
               05  PAG-T05 LINE 15 COLUMN 49 PIC ZZ9 FROM PAG-CB1.
           03  FOR-CLR  00.
               05  LINE 15 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 26 PIC X(29) FROM ALL "Ü".

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           ACCEPT   ARQ-IMP   FROM    TIME
           OPEN     OUTPUT   CADTMP
                    INPUT    CADATV   CADEST   CADLES
           MOVE     1    TO  NUM-OPC  DISPLAY  SETA-01.

       ROT-MENU-00.
           MOVE  1  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-00.
           IF  TCL-MSG = 5 OR 6  PERFORM  ROT-MENU-10
                                 GO       ROT-MENU-00.
           IF  TCL-MSG   >   00  GO       ROT-MENU-00.
           IF  HEX-MSG   =   27  GO       ROT-EXIT-00.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-02
           GO       ROT-GRUP-00,          ROT-TOTA-00,
                    DEPENDING    ON       NUM-OPC.
       ROT-MENU-10.
           IF  NUM-OPC   =   1   MOVE  2  TO  NUM-OPC
                                 DISPLAY      SETA-02
           ELSE
                                 MOVE  1  TO  NUM-OPC
                                 DISPLAY      SETA-01.
       ROT-GRUP-00.
           DISPLAY  TELA-03      PERFORM  ROT-PESQ-00.
           IF  IND-PSQ   <    1  PERFORM  ROT-RTLA-00
                                 GO       ROT-MENU-00.
           MOVE     IND-PSQ  TO  CHV-ATV
           READ     CADATV       DISPLAY  ENTR-03
           GO       ROT-IMPR-00.

       ROT-TOTA-00.
           DISPLAY  TELA-04      MOVE " " TO  CHV-ATV.

       ROT-IMPR-00.
           MOVE     31       TO  IND-MSG
           MOVE     SPACES   TO  CHV-LES
           MOVE     CHV-ATV  TO  PRX-LES
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
                                 DFM-DT1  VAL-DT1
                                 PAG-CB1  CHV-TMP
           START    CADLES       KEY  >   CHV-LES
           IF  FST-LES   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-IMPR-60.
           READ     CADLES  NEXT
           IF  NUM-OPC  =     1  AND
               PRX-LES  NOT   =  CHV-ATV
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-IMPR-60.
           DISPLAY  TELA-05      PERFORM  ROT-NOME-00.

       ROT-IMPR-10.
           DISPLAY  PAG-T05
           ADD      1        TO  PAG-CB1  CHV-TMP
           MOVE     PAG-CB1  TO  PAG-TMP
           MOVE     1        TO  LIN-TMP
           MOVE     CAB-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     2        TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     3        TO  LIN-TMP
           MOVE     CAB-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     4        TO  LIN-TMP
           MOVE     CAB-003  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     5        TO  LIN-TMP
           MOVE     CAB-004  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     6        TO  LIN-TMP
           MOVE     CAB-003  TO  DET-TMP
           WRITE    REG-TMP.

       ROT-IMPR-20.
           IF  LIN-TMP   <   61  GO       ROT-IMPR-30.
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     92       TO  LIN-TMP
           MOVE     CAB-005  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     93       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP      PERFORM  ROT-IMPR-10.

       ROT-IMPR-30.
           MOVE     PRX-LES  TO  PRX-DT1  PRX-EST
           MOVE     SUF-LES  TO  SUF-DT1  SUF-EST
           MOVE     DTA-LES  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     ANO-AUX  TO  ANO-DT1
           MOVE     OPR-LES  TO  OPR-DT1
           READ     CADEST
           MOVE     DES-EST  TO  DES-DT1
           MOVE     SDA-LES  TO  SDA-DT1
           MOVE     SDC-LES  TO  SDC-DT1
           COMPUTE  DFD-DT1  =  (SDC-LES  -   SDA-LES)
           COMPUTE  ACM-QTD  =   ACM-QTD  +  (SDC-LES  -  SDA-LES)
           COMPUTE  ACM-VAL  =   ACM-VAL  +   CST-EST  *
                                             (SDC-LES  -  SDA-LES)
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           READ     CADLES  NEXT
           IF (FST-LES   =  "10")    OR
              (PRX-LES  NOT  =   PRX-EST  AND
               NUM-OPC   =   1)  GO       ROT-IMPR-40.
           IF (PRX-LES  NOT  =   PRX-EST) OR
              (SUF-LES  NOT  =   SUF-EST)
                                 PERFORM  ROT-IMPR-40
                                 GO       ROT-IMPR-20.
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-20.

       ROT-IMPR-40.
           MOVE     ACM-QTD  TO  DFM-DT1
           MOVE     ACM-VAL  TO  VAL-DT1
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           ADD      1        TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
                                 DFM-DT1  VAL-DT1.
       ROT-IMPR-50.
           MOVE     PAG-CB1  TO  PAG-IMP
           DISPLAY  PAG-T05      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE     FILE       CADTMP
           OPEN      OUTPUT      CADTMP.

       ROT-IMPR-60.
           IF  NUM-OPC   =    1  DISPLAY  TELA-02
                                 GO       ROT-GRUP-00.
           PERFORM  ROT-RTLA-00  GO       ROT-MENU-00.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING  PRM-MSG.

       ROT-HELP-00.
           CALL   "DYHELP"  USING  PRM-HLP.

       ROT-GTLA-00.
           MOVE   "G"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE   "R"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

       ROT-PESQ-00.
           CALL   "DYPESQ"  USING  PRM-PSQ
                                   PRM-HLP.
       ROT-NOME-00.
           ACCEPT  DTA-SYS  FROM   DATE
           MOVE    DIA-SYS   TO    DIA-CAB
           MOVE    MES-SYS   TO    MES-CAB
           MOVE    ANO-SYS   TO    ANO-CAB
           CALL   "DYNOME"  USING  PRM-NOM
                                   PRM-CAB
           MOVE    CLI-NOM   TO    CLI-CB1
           MOVE    PRM-CAB   TO    DTA-CB2.

       ROT-EXIT-00.
           CLOSE   CADATV  CADEST  CADLES  CADTMP
           DELETE   FILE   CADTMP  GOBACK.
