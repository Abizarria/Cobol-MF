       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1451.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADBCO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-BCO
                       FILE          STATUS  IS  FST-BCO.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC
                       FILE          STATUS  IS  FST-LBC.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADBCO.CPY.
       COPY  CADCTR.CPY.
       COPY  CADLBC.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CTR          PIC  X(02).
           03  FST-BCO          PIC  X(02).
           03  FST-LBC          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  IND1             PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  DTA-INI          PIC  9(06).
           03  DTA-FIN          PIC  9(06).
           03  ACM-PSQ          PIC  9(02).
           03  ACM-PAG          PIC  9(02).
           03  ACM-SDO          PIC S9(08)V99.

           03  SD1-AUX          PIC  X(25)  VALUE  "SALDO INICIAL ".
           03  SD2-AUX          PIC  X(25)  VALUE  "SALDO ANTERIOR".
           03  SD3-AUX          PIC  X(25)  VALUE  "SALDO ATUAL   ".

           03  NOM-PSQ          PIC  X(20).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  20.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

       01  CAB-001.
           03  FILLER           PIC  X(47)  VALUE  "-DYNAMIC-".
           03  CLI-C01          PIC  X(40).
           03  FILLER           PIC  X(38)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-C01          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(32)  VALUE  "EXTRATO BANCARIO".
           03  FILLER           PIC  X(08)  VALUE  "CONTA :".
           03  COD-C02          PIC  9(03).
           03  FILLER           PIC  X(03)  VALUE  " -".
           03  BCO-C02          PIC  X(34).
           03  FILLER           PIC  X(10)  VALUE  "PERIODO :".
           03  DDI-C02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MMI-C02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AAI-C02          PIC  9(02).
           03  FILLER           PIC  X(03)  VALUE  " A".
           03  DDF-C02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MMF-C02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AAF-C02          PIC  9(02).
           03  FILLER           PIC  X(16)  VALUE  SPACES.
           03  DTA-C02          PIC  X(12).

       01  CAB-003              PIC X(137)  VALUE  "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".

       01  CAB-004              PIC X(137)  VALUE  "³     DATA     ³   H
      -    "ISTORICO DO MOVIMENTO      ³  DOCUMENTO  ³  COD  ³    D E B
      -    "I T O S    ³   C R E D I T O S   ³      S A L D O S     ³".

       01  CAB-005              PIC X(137)  VALUE  "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-006              PIC X(137)  VALUE  "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  CAB-007              PIC X(137)  VALUE  ALL  "- ".

       01  CAB-011.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-C11          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-C11          PIC  9(03).

       01  CAB-012.
           03  FILLER           PIC  X(25)  VALUE  "EXTRATO BANCARIO".
           03  FILLER           PIC  X(08)  VALUE  "CONTA :".
           03  COD-C12          PIC  9(03).
           03  FILLER           PIC  X(03)  VALUE  " -".
           03  BCO-C12          PIC  X(29).
           03  DTA-C12          PIC  X(12).

       01  CAB-013              PIC  X(80)  VALUE
           "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".

       01  CAB-014              PIC  X(80)  VALUE
           "³ HISTORICO DO MOVIMENTO    ³ NR. DOC.³ COD ³   D E B I T O
      -    "  ³  C R E D I T O ³".

       01  CAB-015              PIC  X(80)  VALUE
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-016              PIC  X(80)  VALUE
           "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  CAB-017              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  FILLER           PIC  X(04)  VALUE  "³".
           03  DIA-D01          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-D01          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-D01          PIC  9(02).
           03  FILLER           PIC  X(07)  VALUE  "   ³".
           03  HST-D01          PIC  X(28).
           03  FILLER           PIC  X(04)  VALUE  "³".
           03  DOC-D01          PIC  9(07).
           03  FILLER           PIC  X(07)  VALUE  "   ³".
           03  COD-D01          PIC  9(01).
           03  FILLER           PIC  X(09)  VALUE  "   ³".
           03  DEB-D01          PIC  ZZ.ZZZ.ZZ9,99  BLANK WHEN ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "   ³".
           03  CRD-D01          PIC  ZZ.ZZZ.ZZ9,99  BLANK WHEN ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "   ³".
           03  SDO-D01          PIC  ---.---.--9,99 BLANK WHEN ZEROS.
           03  FILLER           PIC  X(04)  VALUE  "   ³".

       01  DET-002              PIC X(137)  VALUE  "³              ³
      -    "                           ³             ³       ³
      -    "           ³                     ³                      ³".

       01  DET-011.
           03  FILLER           PIC  X(02)  VALUE  "³".
           03  HST-D11          PIC  X(26).
           03  FILLER           PIC  X(02)  VALUE  "³".
           03  DOC-D11          PIC  9(07).
           03  FILLER           PIC  X(04)  VALUE  " ³".
           03  COD-D11          PIC  9(01).
           03  FILLER           PIC  X(05)  VALUE  "  ³".
           03  DEB-D11          PIC  ZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.
           03  FILLER           PIC  X(04)  VALUE  "  ³".
           03  CRD-D11          PIC ---.---.--9,99 BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " ³".

       01  DET-012              PIC  X(80)  VALUE
           "³                           ³         ³     ³
      -    "  ³                ³".

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  36.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1451".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-DTA.
           03  DTA-DTA.
               05  ANO-DTA      PIC  9(02).
               05  MES-DTA      PIC  9(02).
               05  DIA-DTA      PIC  9(02).
           03  LIN-DTA          PIC  9(02)  VALUE  13.
           03  COL-DTA          PIC  9(02).
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
           03  LIN-IMP          PIC  9(02)  VALUE  17.
           03  CL1-IMP          PIC  9(02)  VALUE  45.
           03  CL2-IMP          PIC  9(02)  VALUE  46.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 06 COLUMN 16 VALUE " Administra‡Æo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 07 COLUMN 18 VALUE " Controle C. Banc ria ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 08 COLUMN 20 VALUE " EmissÆo de Relat¢rio ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 06 COLUMN 38 VALUE "ß".
               05  LINE 07 COLUMN 40 VALUE "ß".
               05  LINE 08 COLUMN 42 VALUE "ß".
               05  LINE 07 COLUMN 17 VALUE "Ü".
               05  LINE 08 COLUMN 19 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 09 COLUMN 16 VALUE
                   " ÉÍÍ                        ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ» ".
               05  LINE 10 COLUMN 16 VALUE
                   " º                                             º ".
               05  LINE 11 COLUMN 16 VALUE
                   " º                                             º ".
               05  LINE 12 COLUMN 16 VALUE
                   " º                                             º ".
               05  LINE 13 COLUMN 16 VALUE
                   " º                                             º ".
               05  LINE 14 COLUMN 16 VALUE
                   " º                                             º ".
               05  LINE 15 COLUMN 16 VALUE
                   " º                                             º ".
               05  LINE 16 COLUMN 16 VALUE
                   " º                                             º ".
               05  LINE 17 COLUMN 16 VALUE
                   " º                                             º ".
               05  LINE 18 COLUMN 16 VALUE
                   " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 09 COLUMN 22 VALUE "  Extrato do Per¡odo  ".
           03  BCK-CLR 00.
               05  LINE 10 COLUMN 65 VALUE "  ".
               05  LINE 11 COLUMN 65 VALUE "  ".
               05  LINE 12 COLUMN 65 VALUE "  ".
               05  LINE 13 COLUMN 65 VALUE "  ".
               05  LINE 14 COLUMN 65 VALUE "  ".
               05  LINE 15 COLUMN 65 VALUE "  ".
               05  LINE 16 COLUMN 65 VALUE "  ".
               05  LINE 17 COLUMN 65 VALUE "  ".
               05  LINE 18 COLUMN 65 VALUE "  ".
               05  LINE 19 COLUMN 18 PIC X(49) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 10 COLUMN 20 PIC X(40) FROM ALL "ß".
               05  LINE 14 COLUMN 20 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 11 COLUMN 20 PIC X(40) FROM
                   "  Banco ..:     -".
               05  LINE 12 COLUMN 20 PIC X(40) FROM SPACES.
               05  LINE 13 COLUMN 20 PIC X(40) FROM
                   "  Per¡odo :           …".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 11 COLUMN 60 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 60 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 60 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 60 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 21 PIC X(39) FROM ALL "ß".

       01  ENTR-01     AUTO.
           03  CHV-E02 LINE 11 COLUMN 32 PIC ZZ9   USING CHV-BCO.
           03  PSQ-E02 LINE 11 COLUMN 38 PIC X(20) USING NOM-PSQ.

       01  ENTR-02  BCK-CLR 02 FOR-CLR 15.
           03  CHV-T02 LINE 11 COLUMN 32 PIC 9(03) FROM  CHV-BCO.
           03  BCO-T02 LINE 11 COLUMN 38 PIC X(20) FROM  DES-BCO.
           03  PSQ-T02 LINE 11 COLUMN 38 PIC X(20) FROM  NOM-PSQ.

       01  TELA-03 BCK-CLR 01.
           03  FOR-CLR  07.
               05  LINE 15 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 17 COLUMN 25 PIC X(01) FROM     "Ü".
           03  FOR-CLR  15.
               05  LINE 16 COLUMN 25 PIC X(30) FROM
                   "   P ginas Montadas Ä    ".
               05  PAG-T04 LINE 16 COLUMN 49 PIC ZZ9 FROM ACM-PAG.
           03  FOR-CLR  00.
               05  LINE 16 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 17 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 17 COLUMN 26 PIC X(29) FROM ALL "Ü".

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING   PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK. DISPLAY  TELA-01
           ACCEPT   ARQ-IMP   FROM   TIME
           OPEN     INPUT    CADBCO  CADCTR   CADLBC
           OPEN     OUTPUT   CADTMP  READ     CADCTR
           ACCEPT   DTA-INI   FROM   DATE.

       ROT-CODE-00.
           DISPLAY  TELA-02      MOVE  0  TO  CHV-BCO.

       ROT-CODE-10.
           ACCEPT   CHV-E02      DISPLAY  CHV-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           IF  CHV-BCO   =    0  GO       ROT-PESQ-00.
           READ     CADBCO       MOVE 21  TO  IND-MSG
           IF  FST-BCO   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-00.
       ROT-EXBE-00.
           DISPLAY  CHV-T02      BCO-T02.

       ROT-EXBE-10.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-EXBE-10.
           IF  TCL-MSG   =   05  GO       ROT-MOVE-05.
           IF  TCL-MSG   =   06  GO       ROT-MOVE-06.
           IF  TCL-MSG   >   00  GO       ROT-EXBE-10.
           IF  HEX-MSG   =   27  GO       ROT-CODE-00.

       ROT-ENTR-00.
           MOVE     DTA-INI  TO  DTA-DTA
           MOVE 32  TO  COL-DTA  PERFORM  ROT-DATA-00
           IF  FLG-DTA   =    0  GO       ROT-CODE-00.
           MOVE     18       TO  IND-MSG
           IF  DTA-DTA   <       DTI-CTR
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-00.
           MOVE     DTA-DTA  TO  DTA-INI  DTA-FIN.

       ROT-ENTR-10.
           MOVE     DTA-FIN  TO  DTA-DTA
           MOVE 45  TO  COL-DTA  PERFORM  ROT-DATA-00
           IF  FLG-DTA   =    0  GO       ROT-ENTR-00.
           MOVE     18       TO  IND-MSG
           IF  DTA-DTA   <       DTI-CTR
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-10.
           MOVE     19       TO  IND-MSG
           IF  DTA-DTA   <       DTA-INI
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-10.
           MOVE     DTA-DTA  TO  DTA-FIN
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           MOVE     DTA-INI  TO  DTA-LBC
           MOVE     31       TO  IND-MSG.

       ROT-TEST-00.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-10.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO  OR  DTA-LBC
               >    DTA-FIN      PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-10.
           PERFORM  ROT-NOME-00
           MOVE     SPACES   TO  CHV-LBC
           MOVE     SDO-BCO  TO  ACM-SDO
           MOVE     CHV-BCO  TO  BCO-LBC
                                 COD-C02  COD-C12
           MOVE     DES-BCO  TO  BCO-C02  BCO-C12
           MOVE     DTA-INI  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DDI-C02
           MOVE     MES-AUX  TO  MMI-C02
           MOVE     ANO-AUX  TO  AAI-C02
           MOVE     DTA-FIN  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DDF-C02
           MOVE     MES-AUX  TO  MMF-C02
           MOVE     ANO-AUX  TO  AAF-C02
           MOVE     ZEROS    TO  CHV-TMP  ACM-PAG
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-03
           IF  DTA-INI  NOT   <  DTA-FIN
                                 GO       ROT-EXT2-00.
       ROT-EXT1-00.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-EXT1-60.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO  OR  DTA-LBC
               >    DTA-FIN      GO       ROT-EXT1-60.
           IF  DTA-LBC   <       DTA-INI
                                 PERFORM  ROT-CALC-00
                                 GO       ROT-EXT1-00.
           MOVE     DTA-INI  TO  DTA-AUX.

       ROT-EXT1-10.
           DISPLAY  PAG-T04
           ADD      1        TO  ACM-PAG  CHV-TMP
           MOVE     ACM-PAG  TO  PAG-C01  PAG-TMP
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
           MOVE     CAB-005  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     DIA-AUX  TO  DIA-D01
           MOVE     MES-AUX  TO  MES-D01
           MOVE     ANO-AUX  TO  ANO-D01
           IF  ACM-PAG   =    1
               MOVE SD1-AUX  TO  HST-D01
           ELSE
               MOVE SD2-AUX  TO  HST-D01.
           MOVE     ZEROS    TO  DOC-D01  COD-D01
                                 DEB-D01  CRD-D01
           MOVE     ACM-SDO  TO  SDO-D01
           ADD      1        TO  CHV-TMP
           MOVE     7        TO  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     8        TO  LIN-TMP
           MOVE     DET-002  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     ZEROS    TO  SDO-D01.

       ROT-EXT1-20.
           PERFORM  ROT-CALC-00
           MOVE     DTA-LBC  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-D01
           MOVE     MES-AUX  TO  MES-D01
           MOVE     ANO-AUX  TO  ANO-D01
           MOVE     HST-LBC  TO  HST-D01
           MOVE     DOC-LBC  TO  DOC-D01
           MOVE     COD-LBC  TO  COD-D01.

       ROT-EXT1-30.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-EXT1-60.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO  OR  DTA-LBC
               >    DTA-FIN      GO       ROT-EXT1-60.
           IF  DTA-LBC  NOT   =  DTA-AUX
                                 GO       ROT-EXT1-50.
           ADD      1        TO  CHV-TMP  LIN-TMP
           IF  LIN-TMP   <   60  MOVE     DET-001  TO  DET-TMP
                                 WRITE    REG-TMP
                                 GO       ROT-EXT1-20.
           MOVE     ACM-SDO  TO  SDO-D01
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP.

       ROT-EXT1-40.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-006  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     92       TO  LIN-TMP
           MOVE     CAB-007  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     93       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-EXT1-10.

       ROT-EXT1-50.
           MOVE     ACM-SDO  TO  SDO-D01
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     ZEROS    TO  SDO-D01
           IF  LIN-TMP   >   58  GO       ROT-EXT1-40.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-002  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-EXT1-20.

       ROT-EXT1-60.
           MOVE     ACM-SDO  TO  SDO-D01
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-006  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-00.

       ROT-EXT2-00.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-EXT2-50.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO  OR  DTA-LBC
               >    DTA-FIN      GO       ROT-EXT2-50.
           IF  DTA-LBC   <       DTA-INI
                                 PERFORM  ROT-CALC-00
                                 GO       ROT-EXT2-00.
       ROT-EXT2-10.
           DISPLAY  PAG-T04
           ADD      1        TO  ACM-PAG  CHV-TMP
           MOVE     ACM-PAG  TO  PAG-C11  PAG-TMP
           MOVE     1        TO  LIN-TMP
           MOVE     CAB-011  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     2        TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     3        TO  LIN-TMP
           MOVE     CAB-012  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     4        TO  LIN-TMP
           MOVE     CAB-013  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     5        TO  LIN-TMP
           MOVE     CAB-014  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     6        TO  LIN-TMP
           MOVE     CAB-015  TO  DET-TMP
           WRITE    REG-TMP
           IF  ACM-PAG   =    1
               MOVE SD1-AUX  TO  HST-D11
           ELSE
               MOVE SD2-AUX  TO  HST-D11.
           MOVE     ZEROS    TO  DOC-D11
           COD-D11  DEB-D11
           MOVE     ACM-SDO  TO  CRD-D11
           ADD      1        TO  CHV-TMP
           MOVE     7        TO  LIN-TMP
           MOVE     DET-011  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     8        TO  LIN-TMP
           MOVE     DET-012  TO  DET-TMP
           WRITE    REG-TMP.

       ROT-EXT2-20.
           PERFORM  ROT-CALC-00
           MOVE     HST-LBC  TO  HST-D11
           MOVE     DOC-LBC  TO  DOC-D11
           MOVE     COD-LBC  TO  COD-D11.

       ROT-EXT2-30.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-EXT2-50.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO  OR  DTA-LBC
               >    DTA-FIN      GO       ROT-EXT2-50.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-011  TO  DET-TMP
           WRITE    REG-TMP
           IF  LIN-TMP   <   60  GO       ROT-EXT2-20.

       ROT-EXT2-40.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-016  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     92       TO  LIN-TMP
           MOVE     CAB-017  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     93       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-EXT2-10.

       ROT-EXT2-50.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-011  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-012  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     SD3-AUX  TO  HST-D11
           MOVE     ZEROS    TO  DEB-D11
                                 DOC-D11  COD-D11
           MOVE     ACM-SDO  TO  CRD-D11
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-011  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-016  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-00.

       ROT-CALC-00.
           IF  COD-LBC   <    4
               MOVE     VAL-LBC  TO  CRD-D01  CRD-D11
               MOVE     ZEROS    TO  DEB-D01  DEB-D11
               COMPUTE  ACM-SDO  =   ACM-SDO  +   VAL-LBC
           ELSE
               MOVE     VAL-LBC  TO  DEB-D01  DEB-D11
               MOVE     ZEROS    TO  CRD-D01  CRD-D11
               COMPUTE  ACM-SDO  =   ACM-SDO  -   VAL-LBC.

       ROT-MOVE-05.
           START    CADBCO       KEY  <   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-EXBE-10.
           READ     CADBCO       PREVIOUS
           GO       ROT-EXBE-00.

       ROT-MOVE-06.
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-EXBE-10.
           READ     CADBCO  NEXT GO       ROT-EXBE-00.

       ROT-PESQ-00.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  CHV-BCO
           DISPLAY  CHV-T02.

       ROT-PESQ-10.
           ACCEPT   PSQ-E02      DISPLAY  PSQ-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-10.
           IF  NOM-PSQ   =       SPACES
                                 GO       ROT-PESQ-10.
           MOVE     21  TO  IND1 PERFORM  ROT-PESQ-50.

       ROT-PESQ-20.
           SUBTRACT  1  FROM     IND1
           IF  LET-PSQ (IND1) =  " "  GO  ROT-PESQ-20.
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-PESQ-30.
           MOVE     22       TO  IND-MSG
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           READ     CADBCO  NEXT DISPLAY  CHV-T02
           INSPECT  DES-BCO      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-30.
           DISPLAY  BCO-T02      GO       ROT-PESQ-60.

       ROT-PESQ-40.
           MOVE     22       TO  IND-MSG
           START    CADBCO       KEY  <   CHV-BCO
           IF  FST-BCO   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           READ     CADBCO       PREVIOUS
           DISPLAY  CHV-T02
           INSPECT  DES-BCO      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-40.
           DISPLAY  BCO-T02      GO       ROT-PESQ-60.

       ROT-PESQ-50.
           MOVE     ZEROS    TO  ACM-PSQ
           DISPLAY  CHV-T02      PSQ-T02.

       ROT-PESQ-60.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-60.
           IF  TCL-MSG   =   05  PERFORM  ROT-PESQ-50
                                 GO       ROT-PESQ-40.
           IF  TCL-MSG   =   06  PERFORM  ROT-PESQ-50
                                 GO       ROT-PESQ-30.
           IF  TCL-MSG   >   00  GO       ROT-PESQ-60.
           IF  HEX-MSG   =   13  GO       ROT-ENTR-00.
           DISPLAY  TELA-02      GO       ROT-PESQ-00.

       ROT-IMPR-00.
           MOVE     ACM-PAG  TO  PAG-IMP
           DISPLAY  PAG-T04      CLOSE    CADTMP
           CALL    "DYIMPR"      USING    PRM-IMP
           DELETE    FILE        CADTMP
           OPEN     OUTPUT       CADTMP
           PERFORM  ROT-RTLA-00  GO       ROT-CODE-00.

       ROT-DATA-00.
           CALL   "DYDATA"  USING  PRM-DTA
                                   PRM-HLP.
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

       ROT-NOME-00.
           ACCEPT  DTA-SYS  FROM   DATE
           IF  DTA-INI  NOT   <    DTA-FIN
               MOVE     DTA-INI    TO  DTA-SYS.
           MOVE    DIA-SYS   TO    DIA-CAB
           MOVE    MES-SYS   TO    MES-CAB
           MOVE    ANO-SYS   TO    ANO-CAB
           CALL   "DYNOME"  USING  PRM-NOM
                                   PRM-CAB
           MOVE    CLI-NOM   TO    CLI-C01  CLI-C11
           MOVE    PRM-CAB   TO    DTA-C02  DTA-C12.

       ROT-EXIT-00.
           CLOSE   CADBCO  CADCTR
                   CADLBC  CADTMP
           DELETE   FILE   CADTMP  GOBACK.
