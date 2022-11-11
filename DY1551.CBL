       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1551.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCLI        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CLI
                       FILE          STATUS  IS  FST-CLI.

           SELECT      CADFAT        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-FAT
                       FILE          STATUS  IS  FST-FAT.

           SELECT      CADNTA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTA
                       FILE          STATUS  IS  FST-NTA.

           SELECT      CADSRT        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-SRT.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCLI.CPY.
       COPY  CADFAT.CPY.
       COPY  CADNTA.CPY.
       COPY  CADTMP.CPY.

       SD  CADSRT.

       01  REG-SRT.
           03  CHV-SRT          PIC  9(05).
           03  NOM-SRT          PIC  X(40).
           03  PGT-SRT          PIC  9(01).
           03  DDD-SRT          PIC  9(03).
           03  TEL-SRT          PIC  9(08).
           03  CNT-SRT          PIC  X(15).

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CLI          PIC  X(02).
           03  FST-FAT          PIC  X(02).
           03  FST-NTA          PIC  X(02).
           03  FST-SRT          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  NUM-OPC          PIC  9(01).
           03  ACM-PAG          PIC  9(03).
           03  ACM-QNT          PIC  9(03).
           03  ACM-QFT          PIC  9(03).
           03  ACM-VNT          PIC  9(06)V99.
           03  ACM-VFT          PIC  9(06)V99.

           03  TOT-QNT          PIC  9(04).
           03  TOT-QFT          PIC  9(04).
           03  TOT-VNT          PIC  9(07)V99.
           03  TOT-VFT          PIC  9(07)V99.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  TAB-PGT          PIC  X(05)  VALUE  "SDQME".
           03  RED-PGT          REDEFINES   TAB-PGT.
               05  PGT-TAB      PIC  X(01)  OCCURS  05.

           03  TAB-CTG.
               05  FILLER       PIC  X(09)  VALUE  "SEMANAL  ".
               05  FILLER       PIC  X(09)  VALUE  "DEZENAL  ".
               05  FILLER       PIC  X(09)  VALUE  "QUINZENAL".
               05  FILLER       PIC  X(09)  VALUE  "MENSAL   ".
               05  FILLER       PIC  X(09)  VALUE  "EVENTUAL ".
           03  RED-CTG          REDEFINES   TAB-CTG.
               05  CTG-TAB      PIC  X(09)  OCCURS  05.

           03  TAB-OPC.
               05  FILLER       PIC  X(18)  VALUE " Rela‡Æo Resumida ".
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
               "RELACAO DE EMPRESAS".
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC X(137)  VALUE  ALL  "-".

       01  CAB-004.
           03  FILLER           PIC  X(68)  VALUE  "CODIGO  NOME / RAZAO
      -        " SOCIAL                      CTG  NR. TELEFONE".
           03  FILLER           PIC  X(69)  VALUE  "NOME DO CONTATO
      -        "NOTAS NO PERIODO   FATURAS PEDENTES   SALDO TOTAL".

       01  CAB-005              PIC X(137)  VALUE  ALL  "- ".

       01  CAB-006.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-CB6          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB6          PIC  9(03).

       01  CAB-007.
           03  FILLER           PIC  X(68)  VALUE
               "RELACAO COMPLETA D EMPRESAS".
           03  DTA-CB7          PIC  X(12).

       01  CAB-008              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-009              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  CHV-DT1          PIC  9(05).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  NOM-DT1          PIC  X(42).
           03  PGT-DT1          PIC  X(04).
           03  DDD-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  TEL-DT1          PIC  ZZZ9.9999.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  CNT-DT1          PIC  X(21).
           03  QNT-DT1          PIC  ZZ9.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  VNT-DT1          PIC  ZZZ.ZZ9,99.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  QFT-DT1          PIC  ZZ9.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  VFT-DT1          PIC  ZZZ.ZZ9,99.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  TOT-DT1          PIC  ZZZ.ZZ9,99.

       01  DET-002.
           03  COD-DT2          PIC  9(05).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  NOM-DT2          PIC  X(40).
           03  FILLER           PIC  X(03)  VALUE  "  (".
           03  DDD-DT2          PIC  9(02).
           03  FILLER           PIC  X(02)  VALUE  ") ".
           03  TEL-DT2          PIC  ZZZ9.9999.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  CNT-DT2          PIC  X(15).

       01  DET-003.
           03  FILLER           PIC  X(07)  VALUE  SPACES.
           03  END-DT3          PIC  X(42).
           03  CID-DT3          PIC  X(29).
           03  EST-DT3          PIC  X(02).

       01  DET-004.
           03  FILLER           PIC  X(12)  VALUE  "       CEP:".
           03  CEP-DT4          PIC  99999.999.
           03  FILLER           PIC  X(15)  VALUE  "     C.N.P.J.:".
           03  CGC-DT4          PIC  99.999.999/9999.99.
           03  FILLER           PIC  X(16)  VALUE  "     PAGAMENTO:".
           03  CTG-DT4          PIC  X(10).

       01  TOT-001.
           03  FILLER           PIC  X(68)  VALUE  SPACES.
           03  FILLER           PIC  X(19)  VALUE  "- TOTAL GERAL ....".
           03  QNT-TT1          PIC  Z.ZZ9.
           03  VNT-TT1          PIC  Z.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  QFT-TT1          PIC  Z.ZZ9.
           03  VFT-TT1          PIC  Z.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  TOT-TT1          PIC  Z.ZZZ.ZZ9,99.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  48.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  28.
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1150".
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
               05  LINE 09 COLUMN 38 VALUE " Empresas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 10 COLUMN 55 VALUE "  ".
               05  LINE 11 COLUMN 55 VALUE "  ".
               05  LINE 12 COLUMN 55 VALUE "  ".
               05  LINE 13 COLUMN 33 PIC X(24) FROM "os em Cobran‡a  º".

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

       01  TELA-02.                                                                03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 07 COLUMN 21 VALUE " Administra‡Æo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 08 COLUMN 23 VALUE " Controle de Clientes ".
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
               05  LINE 10 COLUMN 27 VALUE "   Rela‡Æo Empresas   ".
           03  BCK-CLR  00.
               05  LINE 11 COLUMN 60 VALUE "  ".
               05  LINE 12 COLUMN 60 VALUE "  ".
               05  LINE 13 COLUMN 60 VALUE "  ".
               05  LINE 14 COLUMN 60 VALUE "  ".
               05  LINE 15 COLUMN 60 VALUE "  ".
               05  LINE 16 COLUMN 60 VALUE "  ".
               05  LINE 17 COLUMN 60 VALUE "  ".
               05  LINE 18 COLUMN 23 PIC X(39) FROM SPACES.
           03  BCK-CLR 04 FOR-CLR 07.
               05  LINE 11 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 13 COLUMN 25 PIC X(01) FROM     "Ü".
           03  BCK-CLR 04 FOR-CLR 00.
               05  LINE 12 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  ENTR-01.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 12 COLUMN 25 PIC X(30) FROM
                   "       RELA€ÇO RESUMIDA       ".
       01  ENTR-02.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 12 COLUMN 25 PIC X(30) FROM
                   "       RELA€ÇO COMPLETA       ".
       01  TELA-03.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 14 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 16 COLUMN 25 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 15 COLUMN 25 PIC X(30) FROM
                   "    Ordenando Cadastro ...".
           03  BCK-CLR 05 FOR-CLR 00.
               05  LINE 15 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  TELA-04 BCK-CLR 01.
           03  FOR-CLR  07.
               05  LINE 14 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 16 COLUMN 25 PIC X(01) FROM     "Ü".
           03  FOR-CLR  15.
               05  LINE 15 COLUMN 25 PIC X(30) FROM
                   "   P ginas Montadas Ä    ".
               05  PAG-T04 LINE 15 COLUMN 49 PIC ZZ9 FROM ACM-PAG.
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
                    INPUT    CADCLI   CADFAT   CADNTA.
           MOVE     1    TO  NUM-OPC  DISPLAY  SETA-01.

       ROT-MENU-00.
           MOVE  1  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-00.
           IF  TCL-MSG = 5 OR 6  PERFORM  ROT-MENU-10
                                 GO       ROT-MENU-00.
           IF  TCL-MSG   >   00  GO       ROT-MENU-00.
           IF  HEX-MSG   =   27  GO       ROT-EXIT-00.
           MOVE     ZEROS    TO  CHV-CLI
           START    CADCLI       KEY  >   CHV-CLI
           MOVE     28       TO  IND-MSG
           IF  FST-CLI   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-EXIT-00.
           MOVE     ZEROS    TO  CHV-CLI
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-02
           GO       ROT-SORT-00,          ROT-TOTA-00,
                    DEPENDING    ON       NUM-OPC.
       ROT-MENU-10.
           IF  NUM-OPC   =   1   MOVE  2  TO  NUM-OPC
                                 DISPLAY      SETA-02
           ELSE
                                 MOVE  1  TO  NUM-OPC
                                 DISPLAY      SETA-01.
       ROT-SORT-00.
           DISPLAY  ENTR-01      TELA-03
           MOVE     ZEROS    TO  TOT-QNT  TOT-QFT
                                 TOT-VNT  TOT-VFT
           SORT     CADSRT   ON  ASCENDING    KEY  NOM-SRT
                    INPUT        PROCEDURE    IS   ROT-SORT-10
                    OUTPUT       PROCEDURE    IS   ROT-RSMD-00
           GO       ROT-MENU-00.

       ROT-SORT-10  SECTION.

       ROT-SORT-20.
           START    CADCLI       KEY  >   CHV-CLI
           IF  FST-CLI   =  "23" GO       ROT-SORT-30.
           READ     CADCLI  NEXT
           MOVE     CHV-CLI  TO  CHV-SRT
           MOVE     NOM-CLI  TO  NOM-SRT
           MOVE     PGT-CLI  TO  PGT-SRT
           MOVE     DDD-CLI  TO  DDD-SRT
           MOVE     TEL-CLI  TO  TEL-SRT
           MOVE     CNT-CLI  TO  CNT-SRT
           RELEASE  REG-SRT      GO       ROT-SORT-20.

       ROT-SORT-30.
           PERFORM  ROT-IMPR-00.

       ROT-RSMD-00  SECTION.

       ROT-RSMD-10.
           DISPLAY  PAG-T04
           MOVE     1        TO  LIN-TMP
           ADD      1        TO  ACM-PAG  CHV-TMP
           MOVE     ACM-PAG  TO  PAG-CB1  PAG-TMP
           MOVE     CAB-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-003  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-004  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-003  TO  DET-TMP
           WRITE    REG-TMP.

       ROT-RSMD-20.
           RETURN   CADSRT
           IF  FST-SRT   =  "10" GO       ROT-RSMD-70.
           MOVE     CHV-SRT  TO  CHV-DT1
           MOVE     NOM-SRT  TO  NOM-DT1
           MOVE     PGT-TAB     (PGT-SRT) TO  PGT-DT1
           MOVE     DDD-SRT  TO  DDD-DT1
           MOVE     TEL-SRT  TO  TEL-DT1
           MOVE     CNT-SRT  TO  CNT-DT1
           MOVE     SPACES   TO  CHV-NTA  CHV-FAT
           MOVE     CHV-SRT  TO  CLI-NTA  CLI-FAT
           MOVE     ZEROS    TO  ACM-QNT  ACM-QFT
                                 ACM-VNT  ACM-VFT.
       ROT-RSMD-30.
           START    CADNTA       KEY  >   CHV-NTA
           IF  FST-NTA   =  "23" GO       ROT-RSMD-40.
           READ     CADNTA  NEXT
           IF  CLI-NTA  NOT   =  CHV-SRT
                                 GO       ROT-RSMD-40.
           ADD      1        TO  ACM-QNT
           ADD      VAL-NTA  TO  ACM-VNT
           GO       ROT-RSMD-30.

       ROT-RSMD-40.
           START    CADFAT       KEY  >   CHV-FAT
           IF  FST-FAT   =  "23" GO       ROT-RSMD-50.
           READ     CADFAT  NEXT
           IF  CLI-FAT  NOT   =  CHV-SRT
                                 GO       ROT-RSMD-50.
           ADD      1        TO  ACM-QFT
           ADD      VAL-FAT  TO  ACM-VFT
           GO       ROT-RSMD-40.

       ROT-RSMD-50.
           MOVE     ACM-QNT  TO  QNT-DT1
           MOVE     ACM-VNT  TO  VNT-DT1
           MOVE     ACM-QFT  TO  QFT-DT1
           MOVE     ACM-VFT  TO  VFT-DT1
           ADD      ACM-QNT  TO  TOT-QNT
           ADD      ACM-VNT  TO  TOT-VNT
           ADD      ACM-QFT  TO  TOT-QFT
           ADD      ACM-VFT  TO  TOT-VFT
           COMPUTE  TOT-DT1   =  ACM-VNT  +   ACM-VFT
           IF  LIN-TMP   <   58  GO       ROT-RSMD-60.
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
           WRITE    REG-TMP      PERFORM  ROT-RSMD-10.

       ROT-RSMD-60.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-RSMD-20.

       ROT-RSMD-70.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           MOVE     TOT-QNT  TO  QNT-TT1
           MOVE     TOT-VNT  TO  VNT-TT1
           MOVE     TOT-QFT  TO  QFT-TT1
           MOVE     TOT-VFT  TO  VFT-TT1
           COMPUTE  TOT-TT1   =  TOT-VNT  +   TOT-VFT
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     TOT-001  TO  DET-TMP
           MOVE     PAG-CB1  TO  PAG-IMP
           WRITE    REG-TMP      PERFORM  ROT-IMPR-10.

       ROT-RSMD-80  SECTION.

       ROT-TOTA-00.
           DISPLAY  ENTR-02      PERFORM  ROT-IMPR-00.

       ROT-TOTA-10.
           DISPLAY  PAG-T04
           MOVE     1        TO  LIN-TMP
           ADD      1        TO  ACM-PAG  CHV-TMP
           MOVE     ACM-PAG  TO  PAG-CB6  PAG-TMP
           MOVE     CAB-006  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-007  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-008  TO  DET-TMP
           WRITE    REG-TMP.

       ROT-TOTA-20.
           START    CADCLI       KEY  >   CHV-CLI
           IF  FST-CLI  =   "23" GO       ROT-TOTA-30.
           READ     CADCLI  NEXT
           MOVE     CHV-CLI  TO  COD-DT2
           MOVE     NOM-CLI  TO  NOM-DT2
           MOVE     DDD-CLI  TO  DDD-DT2
           MOVE     TEL-CLI  TO  TEL-DT2
           MOVE     CNT-CLI  TO  CNT-DT2
           MOVE     END-CLI  TO  END-DT3
           MOVE     CID-CLI  TO  CID-DT3
           MOVE     EST-CLI  TO  EST-DT3
           MOVE     CEP-CLI  TO  CEP-DT4
           MOVE     CGC-CLI  TO  CGC-DT4
           MOVE     CTG-TAB(PGT-CLI)  TO  CTG-DT4
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-003  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-004  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           IF  LIN-TMP   <   58  GO       ROT-TOTA-20.
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     CAB-009  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     92       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-TOTA-10.

       ROT-TOTA-30.
           MOVE     PAG-CB6  TO  PAG-IMP
           PERFORM  ROT-IMPR-10  GO       ROT-MENU-00.

       ROT-IMPR-00.
           MOVE     ZEROS    TO  CHV-TMP  ACM-PAG
           MOVE     ACM-PAG  TO  PAG-CB1  PAG-CB6
           PERFORM  ROT-NOME-00  DISPLAY  TELA-04.

       ROT-IMPR-10.
           DISPLAY  PAG-T04      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE    FILE        CADTMP
           OPEN     OUTPUT       CADTMP
           PERFORM  ROT-RTLA-00.

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
           MOVE    DIA-SYS   TO    DIA-CAB
           MOVE    MES-SYS   TO    MES-CAB
           MOVE    ANO-SYS   TO    ANO-CAB
           CALL   "DYNOME"  USING  PRM-NOM
                                   PRM-CAB
           MOVE    CLI-NOM   TO    CLI-CB1  CLI-CB6
           MOVE    PRM-CAB   TO    DTA-CB2  DTA-CB7.

       ROT-EXIT-00.
           CLOSE   CADCLI  CADFAT  CADNTA  CADTMP
           DELETE   FILE   CADTMP  GOBACK.
