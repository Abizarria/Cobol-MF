       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY3210.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADMDR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-MDR
                       FILE          STATUS  IS  FST-MDR.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADMDR.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-MDR          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  TECLADO          PIC  9(02).
           03  CHV-FIM          PIC  9(05).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

       01  CAB-001.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(68)  VALUE
               "RELACAO DOS CLIENTES DE MALA-DIRETA".
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "CODIGO   NOME DO CLIENTE                            NR. TELE
      -    "FONE    NUMERO DO RG".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  COD-DT1          PIC  9(05).
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  NOM-DT1          PIC  X(43).
           03  DDD-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  TEL-DT1          PIC  ZZZ9.9999.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  DOC-DT1          PIC  99.999.999.9.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  56.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "3210".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

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
               05  LINE 08 COLUMN 21 VALUE " Mala-Direta Clientes ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 09 COLUMN 23 VALUE " EmissÆo de Relat¢rio ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 08 COLUMN 43 VALUE "ß".
               05  LINE 09 COLUMN 45 VALUE "ß".
               05  LINE 09 COLUMN 22 VALUE "Ü".

       01  TELA-02.
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 10 COLUMN 21 VALUE
                   " É                        ÍÍÍÍÍÍÍÍÍ» ".
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
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 10 COLUMN 25 VALUE " Rela‡Æo dos Clientes ".
           03  BCK-CLR  00.
               05  LINE 11 COLUMN 60 VALUE "  ".
               05  LINE 12 COLUMN 60 VALUE "  ".
               05  LINE 13 COLUMN 60 VALUE "  ".
               05  LINE 14 COLUMN 60 VALUE "  ".
               05  LINE 15 COLUMN 60 VALUE "  ".
               05  LINE 16 COLUMN 60 VALUE "  ".
               05  LINE 17 COLUMN 60 VALUE "  ".
               05  LINE 18 COLUMN 23 PIC X(39) FROM SPACES.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 11 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 13 COLUMN 25 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 12 COLUMN 25 PIC X(30) FROM
                   "  C¢digos .:        …".
           03  BCK-CLR 02 FOR-CLR 00.
               05  LINE 12 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  TELA-03 BCK-CLR 01.
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

       01  ENTR-01     AUTO.
           03  CHI-E02 LINE 12 COLUMN 38 PIC ZZZZ9 USING CHV-MDR.
           03  CHF-E02 LINE 12 COLUMN 48 PIC ZZZZ9 USING CHV-FIM.

       01  ENTR-02  BCK-CLR 02 FOR-CLR 15.
           03  CHI-T02 LINE 12 COLUMN 38 PIC 9(05) USING CHV-MDR.
           03  CHF-T02 LINE 12 COLUMN 48 PIC 9(05) USING CHV-FIM.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING   PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK. DISPLAY  TELA-01
           ACCEPT   ARQ-IMP   FROM   TIME
           OPEN     INPUT    CADMDR  OUTPUT   CADTMP.

       ROT-CODE-00.
           DISPLAY  TELA-02
           MOVE     ZEROS    TO  CHV-MDR  CHV-FIM.

       ROT-CODE-10.
           ACCEPT   CHI-E02      DISPLAY  CHI-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           IF  CHV-MDR   =    0  GO       ROT-CODE-10.

       ROT-CODE-20.
           ACCEPT   CHF-E02      DISPLAY  CHF-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  TELA-02
                                 GO       ROT-CODE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-20.
           MOVE     41       TO  IND-MSG
           IF  CHV-FIM   <       CHV-MDR
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-20.
       ROT-IMPR-00.
           MOVE     32       TO  IND-MSG
           MOVE     ZEROS    TO  PAG-CB1  CHV-TMP
           START    CADMDR       KEY  =   CHV-MDR
           IF  FST-MDR   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-20.
           READ     CADMDR  NEXT
           IF  CHV-MDR       >   CHV-FIM
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-20.
           COMPUTE  CHV-MDR  =   CHV-MDR  -   1
           DISPLAY  TELA-03      PERFORM  ROT-NOME-00.

       ROT-IMPR-10.
           DISPLAY  PAG-T03
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
           START    CADMDR       KEY  >   CHV-MDR
           IF  FST-MDR   =  "23" GO       ROT-IMPR-40.
           READ     CADMDR  NEXT
           IF  CHV-MDR       >   CHV-FIM
                                 GO       ROT-IMPR-40.
           IF  LIN-TMP   <   62  GO       ROT-IMPR-30.
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
           MOVE     CHV-MDR  TO  COD-DT1
           MOVE     NOM-MDR  TO  NOM-DT1
           MOVE     DOC-MDR  TO  DOC-DT1
           MOVE     DDD-MDR  TO  DDD-DT1
           MOVE     TEL-MDR  TO  TEL-DT1
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-20.

       ROT-IMPR-40.
           MOVE     PAG-CB1  TO  PAG-IMP
           DISPLAY  PAG-T03      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE   FILE         CADTMP
           OPEN     OUTPUT       CADTMP
           DISPLAY  TELA-01      GO       ROT-CODE-00.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING  PRM-MSG.

       ROT-HELP-00.
           CALL   "DYHELP"  USING  PRM-HLP.

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
           CLOSE   CADMDR  CADTMP
           DELETE   FILE   CADTMP  GOBACK.
