       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1554.
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
       COPY  CADTMP.CPY.

       SD  CADSRT.

       01  REG-SRT.
           03  FAT-SRT          PIC  9(06).
           03  CLI-SRT          PIC  9(05).
           03  VAL-SRT          PIC  9(06)V99.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CLI          PIC  X(02).
           03  FST-FAT          PIC  X(02).
           03  FST-SRT          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  TECLADO          PIC  9(02).
           03  ACM-PAG          PIC  9(03).
           03  FLG-AUX          PIC  X(01).
           03  NRI-AUX          PIC  9(06).
           03  NRF-AUX          PIC  9(06).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

       01  CAB-001              PIC  X(80)  VALUE
           "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".

       01  CAB-002.
           03  FILLER           PIC  X(19)  VALUE  "³ -DYNAMIC-".
           03  NOM-CB2          PIC  X(47).
           03  DTA-CB2          PIC  X(12).
           03  FILLER           PIC  X(02)  VALUE  " ³".

       01  CAB-003.
           03  FILLER           PIC  X(79)  VALUE  "³".
           03  FILLER           PIC  X(01)  VALUE  "³".

       01  CAB-004.
           03  FILLER           PIC  X(02)  VALUE  "³".
           03  END-CB4          PIC  X(40).
           03  FILLER           PIC  X(05)  VALUE  "  -".
           03  CID-CB4          PIC  X(25).
           03  FILLER           PIC  X(04)  VALUE  " -".
           03  EST-CB4          PIC  X(02).
           03  FILLER           PIC  X(02)  VALUE  " ³".

       01  CAB-005              PIC  X(80)  VALUE
           "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  REC-001              PIC  X(80)  VALUE
           "                             R   E   C   I   B   O".

       01  REC-002.
           03  FILLER          PIC  X(19)  VALUE  "      RECEBEMOS DE".
           03  CLI-RC2         PIC  X(40).
           03  FILLER          PIC  X(15)  VALUE  ", A IMPORTANCIA".

       01  REC-003.
           03  FILLER          PIC  X(12)  VALUE  "      DE R$".
           03  VAL-RC3         PIC  ZZZ.ZZ9,99.
           03  FILLER          PIC  X(52)  VALUE
               ", REFERENTE AO FORNECIMENTO DE COMBUSTIVEIS E OUTROS".

       01  REC-004.
           03  FILLER          PIC  X(40)  VALUE
               "      SERVICOS CONFORME A FATURA NUMERO".
           03  FAT-RC4         PIC  999.999.
           03  FILLER          PIC  X(01)  VALUE  ".".

       01  ROD-001.
           03  FILLER          PIC  X(30)  VALUE  SPACES.
           03  CID-RD1         PIC  X(25).
           03  FILLER          PIC  X(19)  VALUE
               ", ____ / ____ /____".

       01  ROD-002.
           03  FILLER          PIC  X(30)  VALUE  SPACES.
           03  FILLER          PIC  X(44)  VALUE  ALL  "_".

       01  ROD-003.
           03  FILLER          PIC  X(38)  VALUE  SPACES.
           03  FILLER          PIC  X(29)  VALUE
               "CARIMBO E ASSINATURA DO POSTO".

       01  ROD-004.
           03  FILLER          PIC  X(10)  VALUE  "³ C.G.C.:".
           03  CGC-RD4         PIC  99.999.999/9999.99.
           03  FILLER          PIC  X(35)  VALUE
               "              INSCRICAO ESTADUAL :".
           03  INS-RD4         PIC  X(15).
           03  FILLER          PIC  X(02)  VALUE  " ³".

       01  ROD-005             PIC  X(80)  VALUE  ALL  "- ".

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  51.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1554".
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
               05  LINE 10 COLUMN 27 VALUE "   Recibos Empresas   ".
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
                   "  Fatura :         …".
           03  BCK-CLR 02 FOR-CLR 00.
               05  LINE 12 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  TELA-02 BCK-CLR 01.
           03  FOR-CLR  07.
               05  LINE 14 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 16 COLUMN 25 PIC X(01) FROM     "Ü".
           03  FOR-CLR  15.
               05  LINE 15 COLUMN 25 PIC X(30) FROM
                   "   Aguarde, Processando ...   ".
           03  FOR-CLR  00.
               05  LINE 15 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  TELA-03  BCK-CLR 01  FOR-CLR 15.
           03  LINE 15 COLUMN 25 VALUE "   P ginas Montadas Ä        ".
           03  PAG-T03 LINE 15 COLUMN 49 PIC ZZ9 FROM ACM-PAG.

       01  ENTR-01     AUTO.
           03  NRI-E01 LINE 12 COLUMN 36 PIC ZZZZZ9 USING NRI-AUX.
           03  NRF-E01 LINE 12 COLUMN 47 PIC ZZZZZ9 USING NRF-AUX.

       01  ENTR-02  BCK-CLR 02 FOR-CLR 15.
           03  NRI-E02 LINE 12 COLUMN 36 PIC 9(06) USING NRI-AUX.
           03  NRF-E02 LINE 12 COLUMN 47 PIC 9(06) USING NRF-AUX.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"      USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL    =   1  GOBACK.
           ACCEPT   ARQ-IMP      FROM     TIME.

       ROT-ENTR-00.
           DISPLAY  TELA-01      MOVE  0  TO  NRI-AUX.

       ROT-ENTR-10.
           ACCEPT   NRI-E01      DISPLAY  NRI-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GOBACK.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
           IF  NRI-AUX   =    0  GO       ROT-ENTR-10.
           MOVE     ZEROS    TO  NRF-AUX.

       ROT-ENTR-20.
           ACCEPT   NRF-E01      DISPLAY  NRF-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-20.
           IF  NRF-AUX   <       NRI-AUX
                                 MOVE 41  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-20.
       ROT-PESQ-00.
           DISPLAY  TELA-02      OPEN   INPUT  CADFAT
           MOVE     SPACES   TO  CHV-FAT  FLG-AUX
           SORT     CADSRT   ON  ASCENDING    KEY    FAT-SRT
                    INPUT        PROCEDURE    IS     ROT-SORT-00
                    OUTPUT       PROCEDURE    IS     ROT-SORT-30
           GO       ROT-ENTR-00.

       ROT-SORT-00  SECTION.

       ROT-SORT-10.
           START    CADFAT       KEY  >   CHV-FAT
           IF  FST-FAT   =  "23" GO       ROT-SORT-20.
           READ     CADFAT  NEXT
           IF  NUM-FAT   <       NRI-AUX
               OR   >   NRF-AUX  GO       ROT-SORT-10.
           MOVE     "S"      TO  FLG-AUX
           MOVE     NUM-FAT  TO  FAT-SRT
           MOVE     CLI-FAT  TO  CLI-SRT
           MOVE     VAL-FAT  TO  VAL-SRT
           RELEASE  REG-SRT      GO       ROT-SORT-10.

       ROT-SORT-20.
           CLOSE    CADFAT.

       ROT-SORT-30  SECTION.

       ROT-SORT-40.
           MOVE     32       TO  IND-MSG
           IF  FLG-AUX   =  " "  PERFORM  ROT-MSGM-00
                                 GO       ROT-SORT-80.
           OPEN     INPUT        CADCLI
                    OUTPUT       CADTMP
           MOVE     1        TO  PAG-TMP
           MOVE     ZEROS    TO  ACM-PAG
                                 CHV-TMP  LIN-TMP
           DISPLAY  TELA-03      PERFORM  ROT-NOME-00.

       ROT-SORT-50.
           RETURN   CADSRT
           IF  FST-SRT   =  "10" GO       ROT-SORT-70.
           MOVE     CLI-SRT  TO  CHV-CLI
           READ     CADCLI
           MOVE     NOM-CLI  TO  CLI-RC2
           MOVE     VAL-SRT  TO  VAL-RC3
           MOVE     FAT-SRT  TO  FAT-RC4
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-001  TO  DET-TMP
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
           MOVE     CAB-005  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     REC-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     REC-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     REC-003  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     REC-004  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ROD-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ROD-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ROD-003  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ROD-004  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-005  TO  DET-TMP
           WRITE    REG-TMP
           IF  LIN-TMP   <   40  GO       ROT-SORT-60.
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     92       TO  LIN-TMP
           MOVE     ROD-005  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     93       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  PAG-TMP
           MOVE     ZEROS    TO  LIN-TMP
           DISPLAY  PAG-T03      GO       ROT-SORT-50.

       ROT-SORT-60.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ROD-005  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           MOVE     PAG-TMP  TO  ACM-PAG
           WRITE    REG-TMP      GO       ROT-SORT-50.

       ROT-SORT-70.
           CLOSE    CADCLI       CADTMP
           MOVE     ACM-PAG  TO  PAG-IMP
           DISPLAY  PAG-T03      PERFORM  ROT-IMPR-00.

       ROT-SORT-80.
           EXIT.

       ROT-SORT-90  SECTION.

       ROT-IMPR-00.
           CALL   "DYIMPR"  USING  PRM-IMP
           DELETE           FILE   CADTMP.

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
           MOVE    CLI-NOM   TO    NOM-CB2
           MOVE    PRM-CAB   TO    DTA-CB2
           MOVE    END-NOM   TO    END-CB4
           MOVE    CID-NOM   TO    CID-CB4  CID-RD1
           MOVE    EST-NOM   TO    EST-CB4
           MOVE    CGC-NOM   TO    CGC-RD4
           MOVE    INS-NOM   TO    INS-RD4.
