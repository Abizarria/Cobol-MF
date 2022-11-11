       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1151.
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

           SELECT      CADSRT        ASSIGN  TO  DISK.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADATV.CPY.
       COPY  CADEST.CPY.
       COPY  CADTMP.CPY.

       SD  CADSRT.

       01  REG-SRT.
           03  PRX-SRT          PIC  X(01).
           03  SUF-SRT          PIC  9(03).
           03  DES-SRT          PIC  X(25).
           03  VOL-SRT          PIC  9(02)V99.
           03  SDO-SRT          PIC S9(06)V9.
           03  MIN-SRT          PIC  9(06).
           03  CST-SRT          PIC  9(06)V9999.
           03  CTM-SRT          PIC  9(06)V9999.
           03  VDA-SRT          PIC  9(06)V9999.
           03  PCT-SRT          PIC S9(03)V99.
           03  QVM-SRT          PIC  9(07)V9.
           03  QVA-SRT          PIC  9(08)V9.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-ATV          PIC  X(02).
           03  FST-EST          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  IND1             PIC  9(01).
           03  OP1-AUX          PIC  9(01).
           03  OP2-AUX          PIC  9(01).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  TB1-OPC.
               05  FILLER       PIC  X(18)  VALUE " Selecionar Grupo ".
               05  FILLER       PIC  X(18)  VALUE " Rela‡Æo Completa ".
           03  RED-OP1   REDEFINES   TB1-OPC.
               05  OPC-TB1      PIC  X(18)  OCCURS  02.

           03  TB2-OPC.
               05  FILLER       PIC  X(18)  VALUE  " Ordem de C¢digos ".
               05  FILLER       PIC  X(18)  VALUE  " Ordem Alfab‚tica ".
           03  RED-OP2   REDEFINES   TB2-OPC.
               05  OPC-TB2      PIC  X(18)  OCCURS  02.

       01  CAB-001.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(68)  VALUE
               "LISTA DE PRECOS DE PRODUTOS".
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "CODIGO   DESCRICAO DO PRODUTO       VOLUME
      -    "      VALOR UNITARIO".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  PRX-DT1          PIC  X(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DT1          PIC  9(03).
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  DES-DT1          PIC  X(28).
           03  VOL-DT1          PIC  Z9,99.
           03  FILLER           PIC  X(26)  VALUE
               "  --------------------->".
           03  VDA-DT1          PIC  ZZZ.ZZ9,9999.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  06.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1150".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-SCR              OCCURS  02.
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
           03  IND-PSQ          PIC  X(01).
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
           03  FLG-IMP          PIC  9(01)  VALUE  1.
           03  LIN-IMP          PIC  9(02)  VALUE  16.
           03  CL1-IMP          PIC  9(02)  VALUE  40.
           03  CL2-IMP          PIC  9(02)  VALUE  46.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 31 VALUE " ÉÍÍÍÍÍ        ÍÍÍÍÍ» ".
               05  LINE 10 COLUMN 31 VALUE " º  Selecionar Grupo  º ".
               05  LINE 11 COLUMN 31 VALUE " º  Rela‡Æo Completa  º ".
               05  LINE 12 COLUMN 31 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 09 COLUMN 39 VALUE " Pre‡os ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 10 COLUMN 55 VALUE "  ".
               05  LINE 11 COLUMN 55 VALUE "  ".
               05  LINE 12 COLUMN 55 VALUE "  ".
               05  LINE 13 COLUMN 33 PIC X(24) FROM "‡Æo de Saldos  º".

       01  ST1-TL1.
           03  BCK-CLR 07 FOR-CLR 12
               LINE 10 COLUMN 34 PIC X(18) FROM OPC-TB1 (1).
           03  BCK-CLR 02 FOR-CLR 07
               LINE 11 COLUMN 34 PIC X(18) FROM OPC-TB1 (2).

       01  ST2-TL1.
           03  BCK-CLR 02 FOR-CLR 07
               LINE 10 COLUMN 34 PIC X(18) FROM OPC-TB1 (1).
           03  BCK-CLR 07 FOR-CLR 12
               LINE 11 COLUMN 34 PIC X(18) FROM OPC-TB1 (2).

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
               05  LINE 10 COLUMN 27 VALUE "  Pre‡os de Produtos  ".
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

       01  TELA-05.
           03  BCK-CLR 03 FOR-CLR 11.
               05  LINE 15 COLUMN 39 VALUE " ÉÍÍÍÍÍ        ÍÍÍÍÍ» ".
               05  LINE 16 COLUMN 39 VALUE " º                    º ".
               05  LINE 17 COLUMN 39 VALUE " º                    º ".
               05  LINE 18 COLUMN 39 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 03 FOR-CLR 15.
               05  LINE 15 COLUMN 47 VALUE " Op‡äes ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 16 COLUMN 63 VALUE "  ".
               05  LINE 17 COLUMN 63 VALUE "  ".
               05  LINE 18 COLUMN 63 VALUE "  ".
               05  LINE 19 COLUMN 41 PIC X(24) FROM SPACES.

       01  ST1-TL5.
           03  BCK-CLR 07 FOR-CLR 12
               LINE 16 COLUMN 42 PIC X(18) FROM OPC-TB2 (1).
           03  BCK-CLR 03 FOR-CLR 15
               LINE 17 COLUMN 42 PIC X(18) FROM OPC-TB2 (2).

       01  ST2-TL5.
           03  BCK-CLR 03 FOR-CLR 15
               LINE 16 COLUMN 42 PIC X(18) FROM OPC-TB2 (1).
           03  BCK-CLR 07 FOR-CLR 12
               LINE 17 COLUMN 42 PIC X(18) FROM OPC-TB2 (2).

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
           IF  LCK-SPL = 1   GOBACK.
           ACCEPT   ARQ-IMP   FROM    TIME
           OPEN     OUTPUT   CADTMP
                    INPUT    CADATV   CADEST
           MOVE     2   TO   OP1-AUX  DISPLAY  TELA-01.

       ROT-MENU-00.
           IF  OP1-AUX   =   1   MOVE  2  TO  OP1-AUX
                                 DISPLAY      ST2-TL1
           ELSE
                                 MOVE  1  TO  OP1-AUX
                                 DISPLAY      ST1-TL1.
       ROT-MENU-10.
           MOVE  1  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-10.
           IF  TCL-MSG = 5 OR 6  GO       ROT-MENU-00.
           IF  TCL-MSG   >   00  GO       ROT-MENU-10.
           IF  HEX-MSG   =   27  GO       ROT-EXIT-00.
           MOVE     SPACES   TO  IND-PSQ
           MOVE     1        TO  IND1
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-02
           IF  OP1-AUX   =    2  DISPLAY  TELA-04
                                 GO       ROT-MENU-30.
       ROT-MENU-20.
           DISPLAY  TELA-03      PERFORM  ROT-PESQ-00
           IF  IND-PSQ   <    1  GO       ROT-LIMP-00.
           MOVE     IND-PSQ  TO  CHV-ATV
           READ     CADATV       DISPLAY  ENTR-03.

       ROT-MENU-30.
           MOVE     28       TO  IND-MSG
           MOVE     IND-PSQ  TO  PRX-EST
           MOVE     ZEROS    TO  SUF-EST
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-LIMP-00.
           READ     CADEST  NEXT
           IF  OP1-AUX  =     1  AND      PRX-EST
                                 NOT   =  IND-PSQ
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-LIMP-00.
           MOVE     2        TO  OP2-AUX  IND1
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-05.

       ROT-MENU-40.
           IF  OP2-AUX   =   1   MOVE  2  TO  OP2-AUX
                                 DISPLAY      ST2-TL5
           ELSE
                                 MOVE  1  TO  OP2-AUX
                                 DISPLAY      ST1-TL5.
       ROT-MENU-50.
           MOVE  1  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-50.
           IF  TCL-MSG = 5 OR 6  GO       ROT-MENU-40.
           IF  TCL-MSG   >   00  GO       ROT-MENU-50.
           IF  HEX-MSG   =   27  GO       ROT-LIMP-00.
           MOVE     2  TO  IND1  PERFORM  ROT-RTLA-00
           IF  OP2-AUX   =    1  GO       ROT-IMPR-00.

       ROT-SORT-00.
           SORT     CADSRT   ON  ASCENDING    KEY  DES-SRT
                    INPUT        PROCEDURE    IS   ROT-SORT-10
                    OUTPUT       PROCEDURE    IS   ROT-SORT-40
           GO       ROT-LIMP-00.

       ROT-SORT-10  SECTION.

       ROT-SORT-20.
           RELEASE  REG-SRT FROM REG-EST
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-SORT-30.
           READ     CADEST  NEXT
           IF  OP1-AUX   =   1   AND      PRX-EST
                                 NOT  =   IND-PSQ
                                 GO       ROT-SORT-30.
           GO       ROT-SORT-20.

       ROT-SORT-30.
           PERFORM  ROT-IMPR-00  THRU     ROT-IMPR-10.

       ROT-SORT-40  SECTION.

       ROT-SORT-50.
           RETURN   CADSRT   AT  END  GO  ROT-SORT-70.
           IF  LIN-TMP   >   61  PERFORM  ROT-IMPR-30.

       ROT-SORT-60.
           MOVE     PRX-SRT  TO  PRX-DT1
           MOVE     SUF-SRT  TO  SUF-DT1
           MOVE     DES-SRT  TO  DES-DT1
           MOVE     VOL-SRT  TO  VOL-DT1
           MOVE     VDA-SRT  TO  VDA-DT1
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-SORT-50.

       ROT-SORT-70.
           PERFORM  ROT-IMPR-50.

       ROT-SORT-80  SECTION.

       ROT-IMPR-00.
           MOVE     ZEROS    TO  PAG-CB1  CHV-TMP
           PERFORM  ROT-NOME-00  DISPLAY  TELA-06
           MOVE     IND-PSQ  TO  PRX-EST
           MOVE     ZEROS    TO  SUF-EST.

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
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-IMPR-50.
           READ     CADEST  NEXT
           IF  OP1-AUX   =   1   AND
               PRX-EST  NOT  =   IND-PSQ
                                 GO       ROT-IMPR-50.
           IF  LIN-TMP   <   62  GO       ROT-IMPR-40.

       ROT-IMPR-30.
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

       ROT-IMPR-40.
           MOVE     PRX-EST  TO  PRX-DT1
           MOVE     SUF-EST  TO  SUF-DT1
           MOVE     DES-EST  TO  DES-DT1
           MOVE     VOL-EST  TO  VOL-DT1
           MOVE     VDA-EST  TO  VDA-DT1
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO      ROT-IMPR-20.

       ROT-IMPR-50.
           MOVE     PAG-CB1  TO  PAG-IMP
           DISPLAY  PAG-T05      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE     FILE       CADTMP
           OPEN      OUTPUT      CADTMP.

       ROT-LIMP-00.
           MOVE     1        TO  IND1
           PERFORM  ROT-RTLA-00  GO       ROT-MENU-10.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING  PRM-MSG.

       ROT-HELP-00.
           CALL   "DYHELP"  USING  PRM-HLP.

       ROT-GTLA-00.
           MOVE   "G"        TO    FLG-SCR (IND1)
           CALL   "DYTELA"  USING  PRM-SCR (IND1).

       ROT-RTLA-00.
           MOVE   "R"        TO    FLG-SCR (IND1)
           CALL   "DYTELA"  USING  PRM-SCR (IND1).

       ROT-PESQ-00.
           CALL   "DYPESQ"  USING  PRM-PSQ  PRM-HLP.

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
           CLOSE   CADATV  CADEST  CADTMP
           DELETE  FILE    CADTMP  GOBACK.
