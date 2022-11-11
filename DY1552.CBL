       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1552.
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

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCLI.CPY.
       COPY  CADFAT.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CLI          PIC  X(02).
           03  FST-FAT          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  ACM-VAL          PIC  9(07)V99.
           03  ACM-TOT          PIC  9(07)V99.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

       01  CAB-001.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(68)  VALUE
               "RELACAO DE FATURAS EM COBRANCA".
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "NOME / RAZAO SOCIAL    NR. TELEFONE   FATURA     DATA   VAL.
      -    " FATURA    SUB-TOTAL".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  RG1-DT1          PIC  X(38).
           03  RG2-DT1          PIC  X(30).
           03  RG3-DT1          PIC  X(12).

       01  REG-001.
           03  NOM-RG1          PIC  X(20).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  DDD-RG1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  TEL-RG1          PIC  ZZZ9.9999.
           03  FILLER           PIC  X(02)  VALUE  SPACES.

       01  REG-002.
           03  FAT-RG2          PIC  9(06).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  DIA-RG2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-RG2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-RG2          PIC  9(02).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  VAL-RG2          PIC  ZZZ.ZZ9,99.
           03  FILLER           PIC  X(01)  VALUE  SPACES.

       01  REG-003.
           03  TOT-RG3          PIC  Z.ZZZ.ZZ9,99.

       01  TOT-001.
           03  FILLER           PIC  X(38)  VALUE  SPACES.
           03  FILLER           PIC  X(30)  VALUE
               "- VALOR TOTAL EM COBRANCA  R$".
           03  VAL-TT1          PIC  Z.ZZZ.ZZ9,99.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  49.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  31.
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
               05  LINE 10 COLUMN 27 VALUE "   Faturas Cobran‡a   ".
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
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 12 COLUMN 25 PIC X(30) FROM
                   "       RELA€ÇO COMPLETA       ".
           03  BCK-CLR 04 FOR-CLR 00.
               05  LINE 12 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  TELA-02 BCK-CLR 01.
           03  FOR-CLR  07.
               05  LINE 14 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 16 COLUMN 25 PIC X(01) FROM     "Ü".
           03  FOR-CLR  15.
               05  LINE 15 COLUMN 25 PIC X(30) FROM
                   "   P ginas Montadas Ä    ".
               05  PAG-T03 LINE 15 COLUMN 49 PIC ZZ9 FROM PAG-CB1.
           03  FOR-CLR  00.
               05  LINE 15 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 26 PIC X(29) FROM ALL "Ü".

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           ACCEPT   ARQ-IMP   FROM    TIME
           OPEN     I-O      CADTMP
                    INPUT    CADCLI   CADFAT.

       ROT-IMPR-00.
           MOVE     SPACES   TO  CHV-FAT
           START    CADFAT       KEY  >   CHV-FAT
           IF  FST-FAT   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-EXIT-00.
           MOVE     ZEROS    TO  CHV-TMP  PAG-CB1
                                 ACM-VAL  ACM-TOT
           READ     CADFAT  NEXT DISPLAY  TELA-02
           MOVE     CLI-FAT  TO  CHV-CLI
           READ     CADCLI       PERFORM  ROT-NOME-00.

       ROT-IMPR-10.
           DISPLAY  PAG-T03
           MOVE     1        TO  LIN-TMP
           ADD      1        TO  PAG-CB1  CHV-TMP
           MOVE     PAG-CB1  TO  PAG-TMP
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
           WRITE    REG-TMP
           MOVE     NOM-CLI  TO  NOM-RG1
           MOVE     DDD-CLI  TO  DDD-RG1
           MOVE     TEL-CLI  TO  TEL-RG1
           MOVE     REG-001  TO  RG1-DT1.

       ROT-IMPR-20.
           IF  LIN-TMP   <   60  GO       ROT-IMPR-30.
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
           MOVE     NUM-FAT  TO  FAT-RG2
           MOVE     DTA-FAT  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-RG2
           MOVE     MES-AUX  TO  MES-RG2
           MOVE     ANO-AUX  TO  ANO-RG2
           MOVE     VAL-FAT  TO  VAL-RG2
           ADD      VAL-FAT  TO  ACM-VAL
           START    CADFAT       KEY  >   CHV-FAT
           IF  FST-FAT   =  "23" GO       ROT-IMPR-40.
           READ     CADFAT  NEXT
           IF  CLI-FAT  NOT   =  CHV-CLI
                                 PERFORM  ROT-IMPR-40
                                 GO       ROT-IMPR-20.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     REG-002  TO  RG2-DT1
           MOVE     DET-001  TO  DET-TMP
           MOVE     SPACES   TO  RG1-DT1
           WRITE    REG-TMP      GO       ROT-IMPR-20.

       ROT-IMPR-40.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ACM-VAL  TO  TOT-RG3
           MOVE     REG-002  TO  RG2-DT1
           MOVE     REG-003  TO  RG3-DT1
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           MOVE     SPACES   TO  RG3-DT1
           MOVE     CLI-FAT  TO  CHV-CLI
           READ     CADCLI
           MOVE     NOM-CLI  TO  NOM-RG1
           MOVE     DDD-CLI  TO  DDD-RG1
           MOVE     TEL-CLI  TO  TEL-RG1
           MOVE     REG-001  TO  RG1-DT1
           ADD      ACM-VAL  TO  ACM-TOT
           MOVE     ZEROS    TO  ACM-VAL.

       ROT-IMPR-50.
           MOVE     ACM-TOT  TO  VAL-TT1
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     TOT-001  TO  DET-TMP
           WRITE    REG-TMP.

       ROT-IMPR-60.
           MOVE     PAG-CB1  TO  PAG-IMP
           DISPLAY  PAG-T03      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           CLOSE    CADCLI       CADFAT
           DELETE   FILE         CADTMP   GOBACK.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING  PRM-MSG.

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
           CLOSE   CADCLI  CADFAT  CADTMP
           DELETE   FILE   CADTMP  GOBACK.
