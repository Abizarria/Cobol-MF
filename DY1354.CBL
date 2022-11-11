       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1354.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP
                       FILE          STATUS  IS  FST-LDP.

           SELECT      CADSRT        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-SRT.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCTR.CPY.
       COPY  CADLDP.CPY.
       COPY  CADTMP.CPY.

       SD  CADSRT.

       01  REG-SRT.
           03  DTA-SRT          PIC  9(06).
           03  PRX-SRT          PIC  9(01).
           03  SUF-SRT          PIC  9(03).
           03  HST-SRT          PIC  X(25).
           03  VAL-SRT          PIC S9(07)V99.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CTR          PIC  X(02).
           03  FST-LDP          PIC  X(02).
           03  FST-SRT          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  NUM-OPC          PIC  9(01).
           03  PRX-OPC          PIC  9(01).
           03  ACM-DIA          PIC S9(08)V99.
           03  ACM-TOT          PIC S9(08)V99.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-LCT.
               05  ANO-LCT      PIC  9(02).
               05  MES-LCT      PIC  9(02).
               05  DIA-LCT      PIC  9(02).

           03  DTA-LIM.
               05  ANO-LIM      PIC  9(02).
               05  MES-LIM      PIC  9(02).
               05  DIA-LIM      PIC  9(02).

           03  REG-AUX.
               05  DIA-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-AUX      PIC  9(02).
               05  FILLER       PIC  X(04)  VALUE  SPACES.

           03  TAB-OPC.
               05  FILLER       PIC  X(18)  VALUE " Selecionar Grupo ".
               05  FILLER       PIC  X(18)  VALUE " Rela‡Æo Completa ".
           03  RED-OPC   REDEFINES   TAB-OPC.
               05  OPC-TAB      PIC  X(18)  OCCURS  02.

           03  TAB-ATV.
               05  FILLER       PIC  X(18)  VALUE  "OBRIG. TRABALHISTA".
               05  FILLER       PIC  X(18)  VALUE  "OBRIGACOES FISCAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESP. OPERACIONAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESPESA DE CAPITAL".
               05  FILLER       PIC  X(18)  VALUE  "CREDITOS DE SOCIOS".
               05  FILLER       PIC  X(18)  VALUE  "COMPRA DE PRODUTOS".
           03  RED-ATV          REDEFINES   TAB-ATV.
               05  ATV-TAB      PIC  X(18)  OCCURS  6.

       01  CAB-001.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(68)  VALUE
               "RELACAO DAS CONTAS PAGAS NO PERIODO".
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "  DATA      CODIGO   HISTORICO DO MOVIMENTO          VALOR P
      -    "AGO     TOTAL DO DIA".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  REG-DT1          PIC  X(12).
           03  PRX-DT1          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DT1          PIC  9(03).
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  HST-DT1          PIC  X(29).
           03  VAL-DT1          PIC  --.---.--9,99.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  TOT-DT1          PIC  --.---.--9,99 BLANK WHEN ZEROS.

       01  TOT-001.
           03  FILLER           PIC  X(66)  VALUE  "
      -        " TOTAL DAS CONTAS PAGAS NO PERIODO ..... R$".
           03  VAL-TT1          PIC  ---.---.--9,99.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  29.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1350".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

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
           03  OPC-PSQ          PIC  9(01)  VALUE  2.
           03  IND-PSQ          PIC  X(01)  VALUE  "1".
           03  LIN-PSQ          PIC  9(02)  VALUE  12.
           03  COL-PSQ          PIC  9(02)  VALUE  37.
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
           03  FLG-IMP          PIC  9(01)  VALUE  1.
           03  LIN-IMP          PIC  9(02)  VALUE  16.
           03  CL1-IMP          PIC  9(02)  VALUE  40.
           03  CL2-IMP          PIC  9(02)  VALUE  46.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 31 VALUE " ÉÍÍÍ            ÍÍÍ» ".
               05  LINE 10 COLUMN 31 VALUE " º                    º ".
               05  LINE 11 COLUMN 31 VALUE " º                    º ".
               05  LINE 12 COLUMN 31 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 09 COLUMN 37 VALUE " Pagamentos ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 10 COLUMN 55 VALUE "  ".
               05  LINE 11 COLUMN 55 VALUE "  ".
               05  LINE 12 COLUMN 55 VALUE "  ".
               05  LINE 13 COLUMN 33 PIC X(24) FROM "entos … Pagar  º".

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
               05  LINE 08 COLUMN 23 VALUE " Controle de Despesas ".
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
               05  LINE 10 COLUMN 27 VALUE "  Pagamentos Per¡odo  ".
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
               05  LINE 12 COLUMN 25 PIC X(30) FROM "  Grupo :".
           03  FOR-CLR  00.
               05  LINE 12 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  ENTR-03  BCK-CLR 02 FOR-CLR 15
                    LINE 12 COLUMN 35 PIC X(18) FROM ATV-TAB (PRX-OPC).

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
                   "   Aguarde, Processando ...".
           03  FOR-CLR  00.
               05  LINE 15 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  TELA-06 BCK-CLR 01.
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
           OPEN     INPUT    CADCTR   CADLDP
                    OUTPUT   CADTMP   READ     CADCTR
           MOVE     1   TO   NUM-OPC  DISPLAY  SETA-01.

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
           DISPLAY  TELA-03      PERFORM  ROT-PESQ-00
           IF  IND-PSQ   <    1  PERFORM  ROT-RTLA-00
                                 GO       ROT-MENU-00.
           MOVE     IND-PSQ  TO  PRX-OPC
           DISPLAY  ENTR-03      GO       ROT-TEST-00.

       ROT-TOTA-00.
           DISPLAY  TELA-04      MOVE  0  TO  PRX-OPC.

       ROT-TEST-00.
           MOVE     SPACES   TO  CHV-LDP
           MOVE     PRX-OPC  TO  PRX-LDP
           MOVE     DTI-CTR  TO  DTA-LIM
           MOVE     31       TO  DIA-LIM  IND-MSG.

       ROT-TEST-10.
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-TEST-20.
           READ     CADLDP  NEXT
           IF  PRX-LDP  NOT   =  PRX-OPC  AND
               NUM-OPC   =    1  PERFORM  ROT-MSGM-00
                                 GO       ROT-TEST-20.
           IF  FLG-LDP   =  "E"  OR  "P"  OR
               DTA-LDP   >       DTA-LIM
                                 GO       ROT-TEST-10.
           DISPLAY  TELA-05
           SORT     CADSRT   ON  ASCENDING    KEY
                                 DTA-SRT  PRX-SRT  SUF-SRT
                    INPUT        PROCEDURE    IS   ROT-SORT-00
                    OUTPUT       PROCEDURE    IS   ROT-IMPR-00
           MOVE     PAG-CB1  TO  PAG-IMP
           DISPLAY  PAG-T05      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE     FILE       CADTMP
           OPEN      OUTPUT      CADTMP.

       ROT-TEST-20.
           IF  NUM-OPC   =    1  DISPLAY  TELA-02
                                 GO       ROT-GRUP-00.
           PERFORM  ROT-RTLA-00  GO       ROT-MENU-00.

       ROT-SORT-00  SECTION.

       ROT-SORT-10.
           MOVE     DTA-LDP  TO  DTA-SRT
           MOVE     PRX-LDP  TO  PRX-SRT
           MOVE     SUF-LDP  TO  SUF-SRT
           MOVE     HST-LDP  TO  HST-SRT
           EVALUATE FLG-LDP
               WHEN   "L"
                      MOVE       VAL-LDP  TO  VAL-SRT
               WHEN   "R"
                      COMPUTE    VAL-SRT  =   VAL-LDP  -  2  *  VAL-LDP
           END-EVALUATE
           RELEASE  REG-SRT.

       ROT-SORT-20.
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       ROT-SORT-30.
           READ     CADLDP  NEXT
           IF  PRX-LDP  NOT  =   PRX-OPC  AND
               NUM-OPC   =   1   GO       ROT-SORT-30.
           IF  FLG-LDP   =  "E"  OR  "P"  OR  DTA-LDP
               >    DTA-LIM      GO       ROT-SORT-20.
           GO       ROT-SORT-10.

       ROT-SORT-30.
           PERFORM  ROT-NOME-00  DISPLAY  TELA-06
           MOVE     ZEROS    TO  CHV-TMP  PAG-CB1
                                 ACM-DIA  ACM-TOT.
       ROT-IMPR-00  SECTION.

       ROT-IMPR-10.
           RETURN   CADSRT
           MOVE     DTA-SRT  TO  DTA-LCT.

       ROT-IMPR-20.
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
           WRITE    REG-TMP      PERFORM  ROT-IMPR-50.

       ROT-IMPR-30.
           MOVE     PRX-SRT  TO  PRX-DT1
           MOVE     SUF-SRT  TO  SUF-DT1
           MOVE     HST-SRT  TO  HST-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      VAL-SRT  TO  ACM-DIA
           RETURN   CADSRT
           IF  FST-SRT   =  "10" GO       ROT-IMPR-60.
           IF  LIN-TMP   <   60  GO       ROT-IMPR-40.
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
           WRITE    REG-TMP      PERFORM  ROT-IMPR-20.

       ROT-IMPR-40.
           IF  DTA-SRT   >       DTA-LCT
                                 PERFORM  ROT-IMPR-60
                                 GO       ROT-IMPR-30.
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     SPACES   TO  REG-DT1
           GO       ROT-IMPR-30.

       ROT-IMPR-50.
           MOVE     DIA-LCT  TO  DIA-AUX
           MOVE     MES-LCT  TO  MES-AUX
           MOVE     ANO-LCT  TO  ANO-AUX
           MOVE     REG-AUX  TO  REG-DT1
           MOVE     ZEROS    TO  TOT-DT1.

       ROT-IMPR-60.
           MOVE     ACM-DIA  TO  TOT-DT1
           ADD      ACM-DIA  TO  ACM-TOT
           MOVE     ZEROS    TO  ACM-DIA
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     SPACES   TO  DET-TMP
           MOVE     DTA-SRT  TO  DTA-LCT
           WRITE    REG-TMP      PERFORM  ROT-IMPR-50.

       ROT-IMPR-70.
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     ACM-TOT  TO  VAL-TT1
           MOVE     TOT-001  TO  DET-TMP
           WRITE    REG-TMP.

       ROT-IMPR-80  SECTION.

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
           CLOSE   CADCTR  CADLDP  CADTMP
           DELETE   FILE   CADTMP  GOBACK.
