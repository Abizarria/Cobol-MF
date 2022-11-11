       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY3220.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-TMP          PIC  X(02).

           03  TECLADO          PIC  9(02).
           03  QTD-ETQ          PIC  9(04).
           03  ACM-ETQ          PIC  9(04).

       01  ETQ-001              PIC  X(40).

       01  ETQ-002              PIC  X(40).

       01  ETQ-003.
           03  CEP-ET3          PIC  99999.999.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  CID-ET3          PIC  X(27).
           03  EST-ET3          PIC  X(02).

       01  CAB-001              PIC  X(41)  VALUE  ALL  "- ".

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  57.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "3220".
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
               05  LINE 10 COLUMN 25 VALUE " Etiq. Endere‡o Posto ".
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
                   "      Quantidade .:".
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

       01  ENTR-01 LINE 12 COLUMN 45 PIC ZZZ9 USING QTD-ETQ AUTO.

       01  ENTR-02 BCK-CLR 2 FOR-CLR 15
                   LINE 12 COLUMN 45 PIC ZZZ9 USING QTD-ETQ.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING   PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK. DISPLAY  TELA-01
           ACCEPT   ARQ-IMP   FROM   TIME
           OPEN     OUTPUT   CADTMP.

       ROT-QTDE-00.
           DISPLAY  TELA-02
           MOVE     ZEROS    TO  QTD-ETQ  ACM-ETQ.

       ROT-QTDE-10.
           ACCEPT   ENTR-01      DISPLAY  ENTR-02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-QTDE-10.
           IF  QTD-ETQ   =    0  GO       ROT-QTDE-10.
           MOVE     ZEROS    TO  PAG-TMP  CHV-TMP
           DISPLAY  TELA-03      PERFORM  ROT-NOME-00.

       ROT-IMPR-00.
           MOVE     ZEROS    TO  LIN-TMP
           ADD      1        TO  PAG-TMP
           DISPLAY  PAG-T03.

       ROT-IMPR-10.
           IF  ACM-ETQ  NOT  <   QTD-ETQ
                                 GO       ROT-IMPR-30.
           IF  LIN-TMP   <   66  GO       ROT-IMPR-20.
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     CAB-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     92       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP      PERFORM  ROT-IMPR-00.

       ROT-IMPR-20.
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     ETQ-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     SPACES   TO  DET-TMP
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
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     SPACES   TO  DET-TMP
           ADD      1        TO  ACM-ETQ
           WRITE    REG-TMP      GO       ROT-IMPR-10.

       ROT-IMPR-30.
           MOVE     PAG-TMP  TO  PAG-IMP
           DISPLAY  PAG-T03      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE   FILE         CADTMP
           OPEN     OUTPUT       CADTMP
           DISPLAY  TELA-01      GO       ROT-QTDE-00.

       ROT-HELP-00.
           CALL   "DYHELP"  USING   PRM-HLP.

       ROT-NOME-00.
           CALL   "DYNOME"  USING   PRM-NOM
                                    PRM-CAB
           MOVE    CLI-NOM   TO     ETQ-001
           MOVE    END-NOM   TO     ETQ-002
           MOVE    CEP-NOM   TO     CEP-ET3
           MOVE    CID-NOM   TO     CID-ET3
           MOVE    EST-NOM   TO     EST-ET3.

       ROT-EXIT-00.
           CLOSE   CADTMP
           DELETE   FILE    CADTMP  GOBACK.
