       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYRANA.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCXA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CXA
                       FILE          STATUS  IS  FST-CXA.

           SELECT      CADLCX        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCX
                       FILE          STATUS  IS  FST-LCX.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS
                       FILE          STATUS  IS  FST-PRS.

           SELECT      CADSRT        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-SRT.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCXA.CPY.
       COPY  CADLCX.CPY.
       COPY  CADPRS.CPY.
       COPY  CADTMP.CPY.

       SD  CADSRT.

       01  REG-SRT.
           03  CHV-SRT.
               05  ATV-SRT      PIC  9(01).
               05  PRX-SRT      PIC  X(01).
               05  COD-SRT      PIC  X(23).
           03  DES-SRT          PIC  X(15).
           03  HST-SRT          PIC  X(25).
           03  QTD-SRT          PIC  9(04)V9.
           03  VAL-SRT          PIC  9(06)V99.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CXA          PIC  X(02).
           03  FST-LCX          PIC  X(02).
           03  FST-PRS          PIC  X(02).
           03  FST-SRT          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  ATV-ANT          PIC  9(01).
           03  PRX-ANT          PIC  X(01).
           03  ACM-QTD          PIC  9(04)V9.
           03  ACM-VAL          PIC  9(06)V99.
           03  ACM-TOT          PIC  9(07)V99.

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
           03  FILLER           PIC  X(68)  VALUE  "ANALITICO DO DIA".
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "ATIVIDADE         CODIGO QT/DT DESCRICAO / HISTORICO       V
      -    "ALOR R$     TOTAL R$".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  ATV-DT1          PIC  X(18).
           03  CP1-DT1          PIC  X(06).
           03  RD1-CP1          REDEFINES   CP1-DT1.
               05  PRX-DT1      PIC  X(01).
               05  TRC-DT1      PIC  X(01).
               05  SUF-DT1      PIC  9(03).
               05  FILLER       PIC  X(01).
           03  RD2-CP1          REDEFINES   CP1-DT1.
               05  NUM-DT1      PIC  9(06).
           03  CP2-DT1          PIC  X(07).
           03  RD1-CP2          REDEFINES   CP2-DT1.
               05  QTD-DT1      PIC  ZZZ9,9.
               05  FILLER       PIC  X(01).
           03  RD2-CP2          REDEFINES   CP2-DT1.
               05  FILLER       PIC  X(01).
               05  DIA-DT1      PIC  9(02).
               05  BAR-DT1      PIC  X(01).
               05  MES-DT1      PIC  9(02).
               05  FILLER       PIC  X(01).
           03  DES-DT1          PIC  X(26).
           03  VAL-DT1          PIC  ZZZ.ZZ9,99.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  TOT-DT1          PIC  Z.ZZZ.ZZ9,99  BLANK WHEN ZEROS.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  31.
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

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
           03  FLG-IMP          PIC  9(01)  VALUE  1.
           03  LIN-IMP          PIC  9(02)  VALUE  17.
           03  CL1-IMP          PIC  9(02)  VALUE  48.
           03  CL2-IMP          PIC  9(02)  VALUE  47.

       SCREEN          SECTION.

       01  TELA-01.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 15 COLUMN 17 PIC X(47) FROM ALL "ß".
               05  LINE 17 COLUMN 17 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 16 COLUMN 17 PIC X(47) FROM "      AGUARDE, PESQ
      -            "UISANDO LAN€AMENTOS ...".
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 16 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 18 PIC X(46) FROM ALL "ß".

       01  TELA-02.
           03  BCK-CLR 01 FOR-CLR 07.
               05  LINE 15 COLUMN 26 PIC X(30) FROM ALL "ß".
               05  LINE 17 COLUMN 26 PIC X(01) FROM     "Ü".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 16 COLUMN 26 PIC X(30) FROM
                   "   P ginas Montadas Ä        ".
               05  PAG-T02 LINE 16 COLUMN 50 PIC ZZ9 FROM PAG-CB1.
           03  BCK-CLR 00 FOR-CLR 01.
               05  LINE 16 COLUMN 56 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 56 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 27 PIC X(29) FROM ALL "ß".

       PROCEDURE       DIVISION.

       ROT-PESQ-00.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-01
           ACCEPT   ARQ-IMP      FROM     TIME
           OPEN     INPUT        CADCXA
                                 CADLCX   CADPRS
           MOVE     SPACES   TO  CHV-CXA  CHV-LCX
           START    CADLCX       KEY  >   CHV-LCX
           IF  FST-LCX   =  "23" PERFORM  ROT-RTLA-00
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXIT-00.
       ROT-PESQ-10.
           START    CADCXA       KEY  >   CHV-CXA
           READ     CADCXA  NEXT
           IF  FLG-CXA   =    1  GO       ROT-PESQ-10.
           MOVE     DTA-CXA  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-CAB
           MOVE     MES-AUX  TO  MES-CAB
           MOVE     ANO-AUX  TO  ANO-CAB
           MOVE     SPACES   TO  CHV-LCX
           SORT     CADSRT   ON  ASCENDING  KEY  CHV-SRT
                    INPUT        PROCEDURE  IS   ROT-SORT-00
                    OUTPUT       PROCEDURE  IS   ROT-SORT-40
           PERFORM  ROT-RTLA-00  GOBACK.

       ROT-SORT-00  SECTION.

       ROT-SORT-10.
           START    CADLCX       KEY  >   CHV-LCX
           IF  FST-LCX   =  "23" GO       ROT-SORT-30.
           READ     CADLCX  NEXT
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-LCX  TO  TUR-PRS
           MOVE     CXA-LCX  TO  CXA-PRS
           MOVE     ATV-LCX  TO  ATV-PRS  ATV-SRT
           MOVE     PRX-LCX  TO  PRX-SRT
           MOVE     COD-LCX  TO  COD-SRT.

       ROT-SORT-20.
           START    CADPRS       KEY  >   CHV-PRS
           IF  FST-PRS   =  "23" GO       ROT-SORT-10.
           READ     CADPRS  NEXT
           IF  PRX-PRS   NOT  =  PRX-LCX
                                 GO       ROT-SORT-20.
           MOVE     DES-PRS  TO  DES-SRT
           MOVE     HST-LCX  TO  HST-SRT
           MOVE     QTD-LCX  TO  QTD-SRT
           MOVE     VAL-LCX  TO  VAL-SRT
           RELEASE  REG-SRT      GO       ROT-SORT-10.

       ROT-SORT-30.
           CLOSE    CADCXA       CADLCX   CADPRS
           OPEN     I-O          CADTMP
           MOVE     ZEROS    TO  PAG-CB1
                                 ACM-QTD  ACM-VAL
           PERFORM  ROT-RTLA-00  DISPLAY  TELA-02.

       ROT-SORT-40  SECTION.

       ROT-SORT-50.
           RETURN   CADSRT
           PERFORM  ROT-NOME-00  PERFORM  ROT-IMPR-30.

       ROT-SORT-60.
           MOVE     SPACES   TO  CP1-DT1  CP2-DT1
           MOVE     "-"      TO  TRC-DT1
           MOVE     "/"      TO  BAR-DT1
           MOVE     DES-SRT  TO  ATV-DT1
           MOVE     ATV-SRT  TO  ATV-ANT
           MOVE     PRX-SRT  TO  PRX-ANT
           MOVE     ZEROS    TO  ACM-TOT  TOT-DT1.

       ROT-SORT-70.
           IF  ATV-SRT   =    2  PERFORM  ROT-IMP2-00.
           IF  ATV-SRT   =    3  PERFORM  ROT-IMP3-00.
           IF  ATV-SRT   =    4  PERFORM  ROT-IMP4-00.
           IF  ATV-SRT   =    6  PERFORM  ROT-IMP6-00.
           IF  ATV-SRT   =    7  PERFORM  ROT-IMP7-00.
           RETURN   CADSRT
           IF  FST-SRT   =  "10" PERFORM  ROT-IMPR-10
                                 GO       ROT-SORT-80.
           IF  ATV-SRT   =       ATV-ANT  AND
               PRX-SRT   =       PRX-ANT  AND
               COD-SRT   =       COD-LCX  AND
               ATV-SRT   =    2  GO       ROT-SORT-70.
           IF  ATV-SRT  NOT   =  ATV-ANT  OR  PRX-SRT
               NOT  =   PRX-ANT  PERFORM  ROT-IMPR-10
                                 GO       ROT-SORT-60.
           PERFORM  ROT-IMPR-00  GO       ROT-SORT-70.

       ROT-SORT-80.
           MOVE     PAG-CB1  TO  PAG-IMP
           DISPLAY  PAG-T02
           CLOSE    CADTMP       PERFORM  ROT-IMPR-40.

       ROT-SORT-90  SECTION.

       ROT-IMP2-00.
           MOVE     COD-SRT  TO  COD-LCX
           MOVE     PRX-SRT  TO  PRX-DT1
           MOVE     COD-001  TO  SUF-DT1
           MOVE     HST-SRT  TO  DES-DT1
           ADD      QTD-SRT  TO  ACM-QTD
           COMPUTE  ACM-VAL   =  ACM-VAL
                              +  QTD-SRT  *   VAL-SRT
           MOVE     ACM-QTD  TO  QTD-DT1
           MOVE     ACM-VAL  TO  VAL-DT1
           COMPUTE  ACM-TOT   =  ACM-TOT
                              +  QTD-SRT  *   VAL-SRT.
       ROT-IMP3-00.
           MOVE     COD-SRT  TO  COD-LCX
           MOVE     PRX-SRT  TO  PRX-DT1
           MOVE     COD-001  TO  SUF-DT1
           MOVE     QTD-SRT  TO  QTD-DT1
           MOVE     HST-SRT  TO  DES-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      VAL-SRT  TO  ACM-TOT.

       ROT-IMP4-00.
           IF  PRX-SRT   =  "1"  PERFORM  ROT-IMP4-10.
           IF  PRX-SRT   =  "S"  PERFORM  ROT-IMP4-30.
           IF  PRX-SRT   >  "1"  AND   <  "A"
                                 PERFORM  ROT-IMP4-20.
       ROT-IMP4-10.
           MOVE     COD-SRT  TO  COD-LCX
           MOVE     DTA-002  TO  DTA-AUX
           MOVE     FAT-002  TO  NUM-DT1
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     HST-SRT  TO  DES-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      VAL-SRT  TO  ACM-TOT.

       ROT-IMP4-20.
           MOVE     COD-SRT  TO  COD-LCX
           MOVE     DTA-003  TO  DTA-AUX
           MOVE     NUM-003  TO  NUM-DT1
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     HST-SRT  TO  DES-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      VAL-SRT  TO  ACM-TOT.

       ROT-IMP4-30.
           MOVE     COD-SRT  TO  COD-LCX
           MOVE     DTA-004  TO  DTA-AUX
           MOVE     NUM-004  TO  NUM-DT1
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     HST-SRT  TO  DES-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      VAL-SRT  TO  ACM-TOT.

       ROT-IMP6-00.
           IF  PRX-SRT   =  "1"  PERFORM  ROT-IMP6-10.
           IF  PRX-SRT   =  "2"  PERFORM  ROT-IMP6-20.

       ROT-IMP6-10.
           MOVE     COD-SRT  TO  COD-LCX
           MOVE     PRX-005  TO  PRX-DT1
           MOVE     SUF-005  TO  SUF-DT1
           MOVE     DIA-CAB  TO  DIA-DT1
           MOVE     MES-CAB  TO  MES-DT1
           MOVE     HST-SRT  TO  DES-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      VAL-SRT  TO  ACM-TOT.

       ROT-IMP6-20.
           MOVE     COD-SRT  TO  COD-LCX
           MOVE     DTA-006  TO  DTA-AUX
           MOVE     PRX-006  TO  PRX-DT1
           MOVE     SUF-006  TO  SUF-DT1
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     HST-SRT  TO  DES-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      VAL-SRT  TO  ACM-TOT.

       ROT-IMP7-00.
           MOVE     COD-SRT  TO  COD-LCX
           MOVE     "B"      TO  PRX-DT1
           MOVE     BCO-007  TO  SUF-DT1
           MOVE     DTA-007  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     HST-SRT  TO  DES-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      VAL-SRT  TO  ACM-TOT.

       ROT-IMPR-00.
           IF  LIN-TMP   >   60  PERFORM  ROT-IMPR-20.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     SPACES   TO  ATV-DT1
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL.

       ROT-IMPR-10.
           MOVE     ACM-TOT  TO  TOT-DT1
           PERFORM  ROT-IMPR-00
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP.

       ROT-IMPR-20.
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
           WRITE    REG-TMP      PERFORM  ROT-IMPR-30.

       ROT-IMPR-30.
           DISPLAY  PAG-T02
           ADD      1        TO  PAG-CB1  CHV-TMP
           MOVE     PAG-CB1  TO  PAG-TMP
           MOVE     1        TO  LIN-TMP
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
           MOVE     DES-SRT  TO  ATV-DT1.

       ROT-IMPR-40.
           CALL    "DYIMPR"  USING  PRM-IMP
           DELETE            FILE   CADTMP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-NOME-00.
           CALL    "DYNOME"  USING  PRM-NOM  PRM-CAB
           MOVE     CLI-NOM   TO    CLI-CB1
           MOVE     PRM-CAB   TO    DTA-CB2.

       ROT-EXIT-00.
           CLOSE    CADCXA   CADLCX  CADPRS   GOBACK.
