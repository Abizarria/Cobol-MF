       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1555.
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
           03  FLG-AUX          PIC  X(01).
           03  NRI-AUX          PIC  9(06).
           03  NRF-AUX          PIC  9(06).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

       01  DET-001.
           03  LOC-DT1          PIC  X(54).
           03  DIA-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DT1          PIC  9(02).

       01  DET-002.
           03  DD1-DT2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MM1-DT2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AA1-DT2          PIC  9(02).
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  FAT-DT2          PIC  999.999.
           03  FILLER           PIC  X(20)  VALUE  SPACES.
           03  DD2-DT2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MM2-DT2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AA2-DT2          PIC  9(02).

       01  DET-003.
           03  FILLER           PIC  X(52)  VALUE  SPACES.
           03  VAL-DT3          PIC  ZZZ.ZZ9,99.

       01  DET-004.
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  DES-DT4          PIC  X(40).

       01  DET-005.
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  DES-DT5          PIC  X(40).

       01  DET-006.
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  NOM-DT6          PIC  X(43).
           03  CGC-DT6          PIC  99.999.999/9999.99.

       01  DET-007.
           03  FILLER          PIC  X(03)  VALUE  SPACES.
           03  END-DT7         PIC  X(40).

       01  DET-008.
           03  FILLER          PIC  X(03)  VALUE  SPACES.
           03  CEP-DT8         PIC  99999.999.
           03  FILLER          PIC  X(02)  VALUE  SPACES.
           03  CID-DT8         PIC  X(27).
           03  EST-DT8         PIC  X(02).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  52.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1555".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-DTA.
           03  DTA-DTA.
               05  ANO-DTA      PIC  9(02).
               05  MES-DTA      PIC  9(02).
               05  DIA-DTA      PIC  9(02).
           03  LIN-DTA          PIC  9(02)  VALUE  10.
           03  COL-DTA          PIC  9(02)  VALUE  27.
           03  BCK-DTA          PIC  9(01)  VALUE  2.
           03  FOR-DTA          PIC  9(01)  VALUE  7.
           03  FLG-DTA          PIC  9(01).

       01  PRM-IMP.
           03  ARQ-IMP          PIC  9(06).
           03  PAG-IMP          PIC  9(03).
           03  FLG-IMP          PIC  9(01)  VALUE  3.
           03  LIN-IMP          PIC  9(02)  VALUE  18.
           03  CL1-IMP          PIC  9(02)  VALUE  53.
           03  CL2-IMP          PIC  9(02)  VALUE  46.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 05 COLUMN 07 VALUE " Administraฦo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 06 COLUMN 09 VALUE " Controle de Clientes ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 11 VALUE " Emissฦo de Relatขrio ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 05 COLUMN 29 VALUE "฿".
               05  LINE 06 COLUMN 31 VALUE "฿".
               05  LINE 07 COLUMN 33 VALUE "฿".
               05  LINE 06 COLUMN 08 VALUE "Ü".
               05  LINE 07 COLUMN 10 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 08 COLUMN 07 VALUE " ษออ
      -            " อออออออออออออออออออออออออออออออออออป ".
               05  LINE 09 COLUMN 07 VALUE " บ
      -            "                                     บ ".
               05  LINE 10 COLUMN 07 VALUE " บ
      -            "                                     บ ".
               05  LINE 11 COLUMN 07 VALUE " บ
      -            "                                     บ ".
               05  LINE 12 COLUMN 07 VALUE " บ
      -            "                                     บ ".
               05  LINE 13 COLUMN 07 VALUE " บ
      -            "                                     บ ".
               05  LINE 14 COLUMN 07 VALUE " บ
      -            "                                     บ ".
               05  LINE 15 COLUMN 07 VALUE " บ
      -            "                                     บ ".
               05  LINE 16 COLUMN 07 VALUE " บ
      -            "                                     บ ".
               05  LINE 17 COLUMN 07 VALUE " บ
      -            "                                     บ ".
               05  LINE 18 COLUMN 07 VALUE " บ
      -            "                                     บ ".
               05  LINE 19 COLUMN 07 VALUE " ศออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 08 COLUMN 13 VALUE "   Boletos Cobrana   ".
           03  BCK-CLR  00.
               05  LINE 09 COLUMN 74 VALUE "  ".
               05  LINE 10 COLUMN 74 VALUE "  ".
               05  LINE 11 COLUMN 74 VALUE "  ".
               05  LINE 12 COLUMN 74 VALUE "  ".
               05  LINE 13 COLUMN 74 VALUE "  ".
               05  LINE 14 COLUMN 74 VALUE "  ".
               05  LINE 15 COLUMN 74 VALUE "  ".
               05  LINE 16 COLUMN 74 VALUE "  ".
               05  LINE 17 COLUMN 74 VALUE "  ".
               05  LINE 18 COLUMN 74 VALUE "  ".
               05  LINE 19 COLUMN 74 VALUE "  ".
               05  LINE 20 COLUMN 09 PIC X(67) FROM SPACES.

           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 11 PIC X(58) FROM ALL "฿".
               05  LINE 15 COLUMN 11 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 10 COLUMN 11 PIC X(58) FROM "  Vencimento .:
      -            "           Fatura :         …".
               05  LINE 11 COLUMN 11 PIC X(58) FROM SPACES.
               05  LINE 12 COLUMN 11 PIC X(58) FROM "  Local Pagto :".
               05  LINE 13 COLUMN 11 PIC X(58) FROM "  Instruไes .:".
               05  LINE 14 COLUMN 11 PIC X(58) FROM SPACES.
           03  BCK-CLR 02 FOR-CLR 00.
               05  LINE 10 COLUMN 69 PIC X(01) FROM     "Û".
               05  LINE 11 COLUMN 69 PIC X(01) FROM     "Û".
               05  LINE 12 COLUMN 69 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 69 PIC X(01) FROM     "Û".
               05  LINE 14 COLUMN 69 PIC X(01) FROM     "Û".
               05  LINE 15 COLUMN 69 PIC X(01) FROM     "Û".
               05  LINE 15 COLUMN 12 PIC X(57) FROM ALL "Ü".

       01  TELA-02 BCK-CLR 01.
           03  FOR-CLR  07.
               05  LINE 16 COLUMN 25 PIC X(30) FROM ALL "฿".
               05  LINE 18 COLUMN 25 PIC X(01) FROM     "Ü".
           03  FOR-CLR  15.
               05  LINE 17 COLUMN 25 PIC X(30) FROM
                   "   Aguarde, Processando ...   ".
           03  FOR-CLR  00.
               05  LINE 17 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 18 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 18 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  TELA-03  BCK-CLR 01  FOR-CLR 15.
           03  LINE 17 COLUMN 25 VALUE "   Boletos Montados ฤ        ".
           03  PAG-T03 LINE 17 COLUMN 49 PIC ZZ9 FROM PAG-TMP.

       01  ENTR-01     AUTO.
           03  NRI-E01 LINE 10 COLUMN 50 PIC ZZZZZ9 USING NRI-AUX.
           03  NRF-E01 LINE 10 COLUMN 61 PIC ZZZZZ9 USING NRF-AUX.
           03  DT1-E01 LINE 12 COLUMN 27 PIC X(40)  USING LOC-DT1.
           03  DT4-E01 LINE 13 COLUMN 27 PIC X(40)  USING DES-DT4.
           03  DT5-E01 LINE 14 COLUMN 27 PIC X(40)  USING DES-DT5.

       01  ENTR-02  BCK-CLR 02 FOR-CLR 15.
           03  NRI-T01 LINE 10 COLUMN 50 PIC 9(06) USING NRI-AUX.
           03  NRF-T01 LINE 10 COLUMN 61 PIC 9(06) USING NRF-AUX.
           03  DT1-T01 LINE 12 COLUMN 27 PIC X(40) USING LOC-DT1.
           03  DT4-T01 LINE 13 COLUMN 27 PIC X(40) USING DES-DT4.
           03  DT5-T01 LINE 14 COLUMN 27 PIC X(40) USING DES-DT5.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING   PRM-OPR  PRM-SPL
           IF  LCK-SPL    =    1     GOBACK.
           ACCEPT   DTA-SYS   FROM   DATE
           ACCEPT   ARQ-IMP   FROM   TIME.

       ROT-ENTR-00.
           DISPLAY  TELA-01      MOVE  0  TO  DTA-DTA.

       ROT-ENTR-10.
           PERFORM  ROT-DATA-00  MOVE 18  TO  IND-MSG
           IF  FLG-DTA   =    0  GOBACK.
           IF  DTA-DTA   <       DTA-SYS
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-10.
           MOVE     ZEROS    TO  NRI-AUX  NRF-AUX
           MOVE     SPACES   TO  LOC-DT1
                                 DES-DT4  DES-DT5.
       ROT-ENTR-20.
           ACCEPT   NRI-E01      DISPLAY  NRI-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-20.
           IF  NRI-AUX   =    0  GO       ROT-ENTR-20.

       ROT-ENTR-30.
           ACCEPT   NRF-E01      DISPLAY  NRF-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-30.
           MOVE     41       TO  IND-MSG
           IF  NRF-AUX   <       NRI-AUX
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-30.
       ROT-ENTR-40.
           ACCEPT   DT1-E01      DISPLAY  DT1-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-40.
       ROT-ENTR-50.
           ACCEPT   DT4-E01      DISPLAY  DT4-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-40.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-50.
       ROT-ENTR-60.
           ACCEPT   DT5-E01      DISPLAY  DT5-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-50.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-60.
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
           MOVE     ZEROS    TO  PAG-TMP
                                 CHV-TMP  LIN-TMP
           DISPLAY  TELA-03.

       ROT-SORT-50.
           RETURN   CADSRT
           IF  FST-SRT   =  "10" GO       ROT-SORT-70.
           MOVE     CLI-SRT  TO  CHV-CLI
           READ     CADCLI
           MOVE     DIA-DTA  TO  DIA-DT1
           MOVE     MES-DTA  TO  MES-DT1
           MOVE     ANO-DTA  TO  ANO-DT1
           MOVE     DIA-SYS  TO  DD1-DT2  DD2-DT2
           MOVE     MES-SYS  TO  MM1-DT2  MM2-DT2
           MOVE     ANO-SYS  TO  AA1-DT2  AA2-DT2
           MOVE     FAT-SRT  TO  FAT-DT2
           MOVE     VAL-SRT  TO  VAL-DT3
           MOVE     NOM-CLI  TO  NOM-DT6
           MOVE     CGC-CLI  TO  CGC-DT6
           MOVE     END-CLI  TO  END-DT7
           MOVE     CEP-CLI  TO  CEP-DT8
           MOVE     CID-CLI  TO  CID-DT8
           MOVE     EST-CLI  TO  EST-DT8
           PERFORM  ROT-SORT-60  1 TIMES
           ADD      1        TO  CHV-TMP  PAG-TMP
           MOVE     2        TO  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           PERFORM  ROT-SORT-60  4 TIMES
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-002  TO  DET-TMP
           WRITE    REG-TMP
           PERFORM  ROT-SORT-60  1 TIMES
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-003  TO  DET-TMP
           WRITE    REG-TMP
           PERFORM  ROT-SORT-60  2 TIMES
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-004  TO  DET-TMP
           WRITE    REG-TMP
           PERFORM  ROT-SORT-60  1 TIMES
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-005  TO  DET-TMP
           WRITE    REG-TMP
           PERFORM  ROT-SORT-60  6 TIMES
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-006  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-007  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-008  TO  DET-TMP
           WRITE    REG-TMP
           PERFORM  ROT-SORT-60  9 TIMES
           DISPLAY  PAG-T03      GO       ROT-SORT-50.

       ROT-SORT-60.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP.

       ROT-SORT-70.
           CLOSE    CADCLI       CADTMP
           MOVE     PAG-TMP  TO  PAG-IMP
           DISPLAY  PAG-T03      PERFORM  ROT-IMPR-00.

       ROT-SORT-80.
           EXIT.

       ROT-SORT-90  SECTION.

       ROT-IMPR-00.
           CALL   "DYIMPR"  USING   PRM-IMP
           DELETE           FILE    CADTMP.

       ROT-DATA-00.
           CALL   "DYDATA"  USING   PRM-DTA   PRM-HLP.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING   PRM-MSG.

       ROT-HELP-00.
           CALL   "DYHELP"  USING   PRM-HLP.
