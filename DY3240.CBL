       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY3240.
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

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  PER-AUX      PIC  9(04).

           03  DTA-INI.
               05  MES-INI      PIC  9(02).
               05  DIA-INI      PIC  9(02).

           03  DTA-FIM.
               05  MES-FIM      PIC  9(02).
               05  DIA-FIM      PIC  9(02).

           03  DTA-TLA.
               05  DIA-TLA      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-TLA      PIC  9(02).

       01  ETQ-001.
           03  FILLER           PIC  X(35)  VALUE  SPACES.
           03  COD-ET1          PIC  9(05).

       01  ETQ-002              PIC  X(40).

       01  ETQ-003              PIC  X(40).

       01  ETQ-004.
           03  CEP-ET4          PIC  99999.999.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  CID-ET4          PIC  X(27).
           03  EST-ET4          PIC  X(02).

       01  CAB-001              PIC  X(41)  VALUE  ALL  "- ".

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  59.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "3240".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

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
               05  LINE 10 COLUMN 25 VALUE " Etiq. Aniversariante ".
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
                   "  Per¡odo .:        …".
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
               05  PAG-T03 LINE 15 COLUMN 49 PIC ZZ9 FROM PAG-TMP.
           03  FOR-CLR  00.
               05  LINE 15 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  ENTR-01     AUTO.
           03  DTI-E02 LINE 12 COLUMN 38 PIC 99/99 USING DTA-TLA.
           03  DTF-E02 LINE 12 COLUMN 48 PIC 99/99 USING DTA-TLA.

       01  ENTR-02  BCK-CLR 02 FOR-CLR 15.
           03  DTI-T02 LINE 12 COLUMN 38 PIC 99/99 USING DTA-TLA.
           03  DTF-T02 LINE 12 COLUMN 48 PIC 99/99 USING DTA-TLA.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING   PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK. DISPLAY  TELA-01
           ACCEPT   ARQ-IMP   FROM   TIME
           OPEN     INPUT    CADMDR  OUTPUT   CADTMP.

       ROT-0000-10.
           DISPLAY  TELA-02
           MOVE     ZEROS    TO  DTA-INI.

       ROT-CODE-00.
           MOVE     DIA-INI  TO  DIA-TLA
           MOVE     MES-INI  TO  MES-TLA.

       ROT-CODE-10.
           ACCEPT   DTI-E02      DISPLAY  DTI-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           MOVE     16       TO  IND-MSG
           IF (DIA-TLA   <   01  OR   >   31)   OR
              (MES-TLA   <   01  OR   >   12)
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-10.
           MOVE     DIA-TLA  TO  DIA-INI
           MOVE     MES-TLA  TO  MES-INI
           MOVE     ZEROS    TO  DIA-TLA  MES-TLA.

       ROT-CODE-20.
           ACCEPT   DTF-E02      DISPLAY  DTF-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  TELA-02
                                 GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-20.
           MOVE     16       TO  IND-MSG
           IF (DIA-TLA   <   01  OR   >   31)   OR
              (MES-TLA   <   01  OR   >   12)
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-20.
           MOVE     19       TO  IND-MSG
           MOVE     DIA-TLA  TO  DIA-FIM
           MOVE     MES-TLA  TO  MES-FIM
           IF  DTA-FIM   <       DTA-INI
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-20.
       ROT-IMPR-00.
           MOVE     31       TO  IND-MSG
           MOVE     ZEROS    TO  CHV-MDR
                                 CHV-TMP  PAG-TMP
           START    CADMDR       KEY  >   CHV-MDR
           IF  FST-MDR   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-20.
       ROT-IMPR-10.
           READ     CADMDR  NEXT
           IF  FST-MDR   =  "10" PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-20.
           MOVE     NSC-MDR  TO  DTA-AUX
           IF  PER-AUX       <   DTA-INI
                         OR  >   DTA-FIM
                                 GO       ROT-IMPR-10.
           DISPLAY  TELA-03
           MOVE     ZEROS    TO  CHV-MDR.

       ROT-IMPR-20.
           MOVE     ZEROS    TO  LIN-TMP
           ADD      1        TO  PAG-TMP
           DISPLAY  PAG-T03.

       ROT-IMPR-30.
           START    CADMDR       KEY  >   CHV-MDR
           IF  FST-MDR   =  "23" GO       ROT-IMPR-50.
           READ     CADMDR  NEXT
           MOVE     NSC-MDR  TO  DTA-AUX
           IF  PER-AUX       <   DTA-INI
                         OR  >   DTA-FIM
                                 GO       ROT-IMPR-30.
           IF  LIN-TMP   <   66  GO       ROT-IMPR-40.
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     CAB-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     92       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP      PERFORM  ROT-IMPR-20.

       ROT-IMPR-40.
           MOVE     CHV-MDR  TO  COD-ET1
           MOVE     NOM-MDR  TO  ETQ-002
           MOVE     END-MDR  TO  ETQ-003
           MOVE     CEP-MDR  TO  CEP-ET4
           MOVE     CID-MDR  TO  CID-ET4
           MOVE     EST-MDR  TO  EST-ET4
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     ETQ-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     ETQ-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     ETQ-003  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     ETQ-004  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-30.

       ROT-IMPR-50.
           MOVE     PAG-TMP  TO  PAG-IMP
           DISPLAY  PAG-T03      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE   FILE         CADTMP
           OPEN     OUTPUT       CADTMP
           DISPLAY  TELA-01      GO       ROT-0000-10.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING   PRM-MSG.

       ROT-HELP-00.
           CALL   "DYHELP"  USING   PRM-HLP.

       ROT-EXIT-00.
           CLOSE   CADMDR   CADTMP
           DELETE   FILE    CADTMP  GOBACK.
