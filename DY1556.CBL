       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1556.
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

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCLI.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CLI          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  IND-PGT          PIC  9(01).
           03  TECLADO          PIC  9(02).

           03  TAB-PGT.
               05  FILLER       PIC  X(09)  VALUE  "SEMANAL  ".
               05  FILLER       PIC  X(09)  VALUE  "DEZENAL  ".
               05  FILLER       PIC  X(09)  VALUE  "QUINZENAL".
               05  FILLER       PIC  X(09)  VALUE  "MENSAL   ".
               05  FILLER       PIC  X(09)  VALUE  "EVENTUAL ".
           03  RED-PGT          REDEFINES   TAB-PGT.
               05  PGT-TAB      PIC  X(09)  OCCURS  5.

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
           03  PRG-SPL          PIC  9(02)  VALUE  53.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  28.
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1556".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-PSQ.
           03  OPC-PSQ          PIC  9(01)  VALUE  5.
           03  IND-PSQ          PIC  X(01)  VALUE  "1".
           03  LIN-PSQ          PIC  9(02)  VALUE  12.
           03  COL-PSQ          PIC  9(02)  VALUE  39.
           03  SB1-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB2-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB3-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB4-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB5-PSQ          PIC  X(02)  VALUE  "ÕÕ".
           03  SB6-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB7-PSQ          PIC  X(26)  VALUE  SPACES.

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
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "€" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 07 COLUMN 21 VALUE " Administraá∆o Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 08 COLUMN 23 VALUE " Controle de Clientes ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 09 COLUMN 25 VALUE " Emiss∆o de Relat¢rio ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 07 COLUMN 43 VALUE "ﬂ".
               05  LINE 08 COLUMN 45 VALUE "ﬂ".
               05  LINE 09 COLUMN 47 VALUE "ﬂ".
               05  LINE 08 COLUMN 22 VALUE "‹".
               05  LINE 09 COLUMN 24 VALUE "‹".

       01  TELA-02.
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 10 COLUMN 21 VALUE
                   " …ÕÕ                        ÕÕÕÕÕÕÕª ".
               05  LINE 11 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 12 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 13 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 14 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 15 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 16 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 17 COLUMN 21 VALUE
                   " »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 10 COLUMN 27 VALUE " Etiquetas p/ Empresa ".
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
               05  LINE 11 COLUMN 25 PIC X(30) FROM ALL "ﬂ".
               05  LINE 13 COLUMN 25 PIC X(01) FROM     "‹".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 12 COLUMN 25 PIC X(30) FROM  "  Categoria .:".
           03  BCK-CLR 02 FOR-CLR 00.
               05  LINE 12 COLUMN 55 PIC X(01) FROM     "€".
               05  LINE 13 COLUMN 55 PIC X(01) FROM     "€".
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "‹".

       01  TELA-03 BCK-CLR 01.
           03  FOR-CLR  07.
               05  LINE 14 COLUMN 25 PIC X(30) FROM ALL "ﬂ".
               05  LINE 16 COLUMN 25 PIC X(01) FROM     "‹".
           03  FOR-CLR  15.
               05  LINE 15 COLUMN 25 PIC X(30) FROM
                   "   P†ginas Montadas ƒ    ".
               05  PAG-T03 LINE 15 COLUMN 49 PIC ZZ9 FROM PAG-TMP.
           03  FOR-CLR  00.
               05  LINE 15 COLUMN 55 PIC X(01) FROM     "€".
               05  LINE 16 COLUMN 55 PIC X(01) FROM     "€".
               05  LINE 16 COLUMN 26 PIC X(29) FROM ALL "‹".

       01  ENTR-01  BCK-CLR 02 FOR-CLR 15
                    LINE 12 COLUMN 40 PIC X(09) FROM PGT-TAB (IND-PGT).

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING   PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK. DISPLAY  TELA-01
           ACCEPT   ARQ-IMP   FROM   TIME
           OPEN     INPUT    CADCLI  OUTPUT   CADTMP.

       ROT-PESQ-00.
           DISPLAY  TELA-02      PERFORM  ROT-CATG-00
           IF  IND-PSQ   <    1  GO       ROT-EXIT-00.
           MOVE     IND-PSQ  TO  IND-PGT
           DISPLAY  ENTR-01      MOVE  0  TO  CHV-CLI.

       ROT-PESQ-10.
           START    CADCLI       KEY  >   CHV-CLI
           IF  FST-CLI   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           READ     CADCLI  NEXT
           IF  PGT-CLI  NOT   =  IND-PGT
                                 GO       ROT-PESQ-10.
           MOVE     ZEROS    TO  CHV-TMP
                                 PAG-TMP  LIN-TMP
           DISPLAY  TELA-03
           ADD      1        TO  PAG-TMP.

       ROT-IMPR-00.
           MOVE     CHV-CLI  TO  COD-ET1
           MOVE     NOM-CLI  TO  ETQ-002
           MOVE     END-CLI  TO  ETQ-003
           MOVE     CEP-CLI  TO  CEP-ET4
           MOVE     CID-CLI  TO  CID-ET4
           MOVE     EST-CLI  TO  EST-ET4
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ETQ-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ETQ-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ETQ-003  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ETQ-004  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP.

       ROT-IMPR-10.
           START    CADCLI       KEY  >   CHV-CLI
           IF  FST-CLI   =  "23" GO       ROT-IMPR-20.
           READ     CADCLI  NEXT
           IF  PGT-CLI  NOT   =  IND-PGT
                                 GO       ROT-IMPR-10.
           IF  LIN-TMP   <   62  GO       ROT-IMPR-00.
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     CAB-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     92       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  PAG-TMP
           MOVE     ZEROS    TO  LIN-TMP
           DISPLAY  PAG-T03      GO       ROT-IMPR-00.

       ROT-IMPR-20.
           MOVE     PAG-TMP  TO  PAG-IMP
           DISPLAY  PAG-T03      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE   FILE         CADTMP
           OPEN     OUTPUT       CADTMP
           DISPLAY  TELA-01      GO       ROT-PESQ-00.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING   PRM-MSG.

       ROT-CATG-00.
           CALL   "DYPESQ"  USING   PRM-PSQ
                                    PRM-HLP.
       ROT-EXIT-00.
           CLOSE   CADCLI   CADTMP
           DELETE   FILE    CADTMP  GOBACK.
