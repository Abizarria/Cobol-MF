       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYRDIA.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADCXA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CXA
                       FILE          STATUS  IS  FST-CXA.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS
                       FILE          STATUS  IS  FST-PRS.

           SELECT      CADSDO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SDO
                       FILE          STATUS  IS  FST-SDO.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCTR.CPY.
       COPY  CADCXA.CPY.
       COPY  CADPRS.CPY.
       COPY  CADSDO.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CTR          PIC  X(02).
           03  FST-CXA          PIC  X(02).
           03  FST-PRS          PIC  X(02).
           03  FST-SDO          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  IND-SDO          PIC  9(02).
           03  ACM-LN1          PIC  9(02).
           03  ACM-LN2          PIC  9(02).
           03  ACM-PAG          PIC  9(01).
           03  ACM-QTD          PIC  9(06)V9.
           03  ACM-QTM          PIC  9(07)V9.
           03  ACM-MED          PIC  9(06)V9.
           03  ACM-DIF          PIC S9(07)V99.
           03  TOT-DIF          PIC S9(08)V99.
           03  TOT-CRD          PIC S9(08)V99.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  ATV-TAB       OCCURS    08.
               05  PRX-TAB   OCCURS    35.
                   07  DES-TAB  PIC  X(15).
                   07  QTD-TAB  PIC  9(06)V9.
                   07  QTM-TAB  PIC  9(07)V9.
                   07  VDD-TAB  PIC S9(07)V9999.
                   07  VDM-TAB  PIC S9(08)V9999.
                   07  CRD-TAB  PIC S9(08)V9999.

           03  TAB-IND   VALUE  "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".
               05  IND-TAB      PIC  X(01)  OCCURS  35  INDEXED BY I.

       01  CAB-001.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(68)  VALUE  "DIARIO DE VENDAS".
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC  X(80)  VALUE
           "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ ÚÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿".

       01  CAB-004              PIC  X(80)  VALUE
           "³ +/-   DESCRICAO        VALOR APURADO ³ ³ VENDA/DIA ³  VEND
      -    "A/MES  ³ MEDIA/DIA ³".

       01  CAB-005              PIC  X(80)  VALUE
           "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´".

       01  DET-001.
           03  RG1-DT1          PIC  X(39).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  RG2-DT1          PIC  X(39).

       01  REG-001.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  FILLER           PIC  X(01)  VALUE  "[".
           03  SNL-RG1          PIC  X(01).
           03  FILLER           PIC  X(04)  VALUE  "]".
           03  DES-RG1          PIC  X(17).
           03  VAL-RG1          PIC  --.---.--9,99.
           03  FILLER           PIC  X(01)  VALUE  SPACES.

       01  REG-002.
           03  FILLER           PIC  X(02)  VALUE  "³".
           03  QTD-RG2          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  QTM-RG2          PIC  Z.ZZZ.ZZ9,9.
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  MED-RG2          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(02)  VALUE  " ³".

       01  REG-003              PIC  X(39)  VALUE
           "ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´".

       01  REG-004              PIC  X(39)  VALUE
           "ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  REG-005              PIC  X(39)  VALUE
           " ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".

       01  REG-006.
           03  FILLER           PIC  X(25)  VALUE
               "  [=]   TOTAL DAS VENDAS".
           03  VAL-RG6          PIC  ZZ.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(01)  VALUE  SPACES.

       01  REG-007.
           03  FILLER           PIC  X(25)  VALUE
               "  [=]   DIF. DIA / MES".
           03  VAL-RG7          PIC  --.---.--9,99.
           03  FILLER           PIC  X(01)  VALUE  SPACES.

       01  REG-008.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  RG1-RG8          PIC  X(18).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  RG2-RG8          PIC  X(18).

       01  REG-009.
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  VAL-RG9          PIC  ---.---.--9,99.
           03  FILLER           PIC  X(01)  VALUE  SPACES.

       01  REG-010              PIC  X(18)  VALUE  "  ACUMULADO NO MES".

       01  REG-011              PIC  X(18)  VALUE  "  CONTAS A RECEBER".

       01  REG-012              PIC  X(18)  VALUE  "  ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".

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
               05  PAG-T02 LINE 16 COLUMN 50 PIC ZZ9 FROM ACM-PAG.
           03  BCK-CLR 00 FOR-CLR 01.
               05  LINE 16 COLUMN 56 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 56 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 27 PIC X(29) FROM ALL "ß".

       PROCEDURE       DIVISION.

       ROT-0000-00.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-01
           ACCEPT   ARQ-IMP       FROM     TIME
           OPEN     I-O          CADTMP
                    INPUT        CADCXA   CADCTR
                                 CADPRS   CADSDO
           MOVE     1        TO  IND1      IND2.

       ROT-PESQ-00.
           MOVE     ZEROS    TO  QTD-TAB (IND1, IND2)
                                 QTM-TAB (IND1, IND2)
                                 VDD-TAB (IND1, IND2)
                                 VDM-TAB (IND1, IND2)
                                 CRD-TAB (IND1, IND2)
           MOVE     SPACES   TO  DES-TAB (IND1, IND2)
           ADD      1   TO  IND2
           IF  IND2     <    36  GO       ROT-PESQ-00.
           MOVE     1   TO  IND2 ADD      1  TO  IND1
           IF  IND1     <    09  GO       ROT-PESQ-00.
           MOVE     ZEROS    TO  TOT-CRD  TOT-DIF
           MOVE     SPACES   TO  CHV-CXA  CHV-SDO.

       ROT-PESQ-10.
           START    CADCXA       KEY  >   CHV-CXA
           IF  FST-CXA   =  "23" GO       ROT-IMPR-00.
           READ     CADCXA  NEXT
           IF  FLG-CXA   =    2  MOVE     DTA-CXA  TO  DTA-AUX.
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-CXA  TO  TUR-PRS
           MOVE     NUM-CXA  TO  CXA-PRS.

       ROT-PESQ-20.
           START    CADPRS       KEY  >   CHV-PRS
           IF  FST-PRS   =  "23" GO       ROT-PESQ-30.
           READ     CADPRS  NEXT
           IF  TUR-PRS  NOT   =  TUR-CXA  OR  CXA-PRS
               NOT  =   NUM-CXA  GO       ROT-PESQ-10.
           SET      I   TO    1
           SEARCH   IND-TAB      WHEN     PRX-PRS  =
                                          IND-TAB (I)
           MOVE     ATV-PRS  TO  IND1
           SET      IND2     TO  I
           MOVE     DES-PRS  TO  DES-TAB (IND1, IND2)
           ADD      QTM-PRS  TO  QTM-TAB (IND1, IND2)
           ADD      VDM-PRS  TO  VDM-TAB (IND1, IND2)
           IF  FLG-CXA   =    1  AND
               ATV-PRS   <    8  GO       ROT-PESQ-20.
           ADD      QTD-PRS  TO  QTD-TAB (IND1, IND2)
           ADD      VDD-PRS  TO  VDD-TAB (IND1, IND2)
           IF  IND1      <    8  GO       ROT-PESQ-20.
           ADD      VDM-PRS  TO  TOT-DIF
           GO       ROT-PESQ-20.

       ROT-PESQ-30.
           START    CADSDO       KEY  >   CHV-SDO
           IF  FST-SDO   =  "23" GO       ROT-IMPR-00.
           READ     CADSDO  NEXT
           IF  PRX-SDO   >    1  GO       ROT-IMPR-00.
           SET      I   TO    1
           SEARCH   IND-TAB      WHEN     SUF-SDO  =
                                          IND-TAB (I)
           SET      IND-SDO  TO  I
           COMPUTE  CRD-TAB  (5, IND-SDO)  =  SDI-SDO
                 +  ENT-SDO   -  SAI-SDO   +  COR-SDO
           ADD      CRD-TAB  (5, IND-SDO)  TO TOT-CRD
           GO       ROT-PESQ-30.

       ROT-IMPR-00.
           MOVE     DIA-AUX  TO  DIA-CAB
           MOVE     MES-AUX  TO  MES-CAB
           MOVE     ANO-AUX  TO  ANO-CAB
           PERFORM  ROT-NOME-00  READ     CADCTR
           COMPUTE  PAG-CB1   =  DIA-CTR  +  1
           MOVE     ZEROS    TO  CHV-TMP  ACM-PAG
           PERFORM  ROT-RTLA-00  DISPLAY  TELA-02
           MOVE     1        TO  CHV-TMP
                                 PAG-TMP  LIN-TMP
           MOVE     CAB-001  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     2        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           MOVE     3        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-002  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     4        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-003  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     5        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-004  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     6        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-005  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     1        TO  IND1     IND2
           MOVE     ZEROS    TO  ACM-LN1  ACM-LN2
                                 ACM-QTD  ACM-QTM
                                 ACM-MED  ACM-DIF.
       ROT-IMPR-10.
           MOVE     SPACES   TO  RG1-DT1  RG2-DT1
           IF  IND1   =  1       GO       ROT-COMB-00.
           IF  IND1   =  2   OR  3
                                 GO       ROT-VEND-00.
           IF  IND1   =  4       GO       ROT-RECB-00.
           IF  IND1   >  4  AND  <   8
                                 GO       ROT-CRED-00.
           IF  IND1   =  8       GO       ROT-DIFR-00.

       ROT-IMPR-20.
           ADD      1        TO  IND2
           IF  IND2    <     36
               IF  DES-TAB   (IND1, IND2)  =   SPACES
                                 GO       ROT-IMPR-20
               ELSE
                                 GO       ROT-IMPR-10
           ELSE
               IF  IND1   =   3  GO       ROT-VEND-10.

       ROT-IMPR-30.
           ADD     1         TO  IND1
           MOVE    0         TO  IND2
           IF  IND1    <     09  GO       ROT-IMPR-20.

       ROT-IMPR-40.
           MOVE     1        TO  PAG-IMP  ACM-PAG
           DISPLAY  PAG-T02      CLOSE    CADTMP
           CALL    "DYIMPR"      USING    PRM-IMP
           PERFORM  ROT-RTLA-00  GO       ROT-EXIT-00.

       ROT-COMB-00.
           MOVE     DES-TAB (IND1, IND2)  TO  DES-RG1
           MOVE     VDD-TAB (IND1, IND2)  TO  VAL-RG1
           MOVE     QTD-TAB (IND1, IND2)  TO  QTD-RG2
           MOVE     QTM-TAB (IND1, IND2)  TO  QTM-RG2
           COMPUTE  MED-RG2   =  QTM-TAB (IND1, IND2)
                              /  PAG-CB1
           ADD      QTD-TAB (IND1, IND2)  TO  ACM-QTD
           ADD      QTM-TAB (IND1, IND2)  TO  ACM-QTM
           COMPUTE  ACM-MED   =  ACM-MED   +
                    QTM-TAB (IND1, IND2)   /  PAG-CB1
           MOVE     "-"      TO  SNL-RG1
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-002  TO  RG2-DT1
           COMPUTE  ACM-DIF   =  ACM-DIF
                              -  VDD-TAB (IND1, IND2)
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-20.

       ROT-VEND-00.
           MOVE     DES-TAB (IND1, IND2)  TO  DES-RG1
           MOVE     VDD-TAB (IND1, IND2)  TO  VAL-RG1
           MOVE     "-"      TO  SNL-RG1
           MOVE     REG-001  TO  RG1-DT1
           ADD      1        TO  ACM-LN1
           IF  ACM-LN1   =   01  MOVE     REG-003  TO  RG2-DT1.
           IF  ACM-LN1   =   02  MOVE     ACM-QTD  TO  QTD-RG2
                                 MOVE     ACM-QTM  TO  QTM-RG2
                                 MOVE     ACM-MED  TO  MED-RG2
                                 MOVE     REG-002  TO  RG2-DT1.
           IF  ACM-LN1   =   03  MOVE     REG-004  TO  RG2-DT1.
           COMPUTE  ACM-DIF   =  ACM-DIF
                              -  VDD-TAB (IND1, IND2)
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-20.

       ROT-VEND-10.
           MOVE     REG-005  TO  RG1-DT1
           MOVE     SPACES   TO  RG2-DT1
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     ACM-DIF  TO  VAL-RG6
           MOVE     REG-006  TO  RG1-DT1
           MOVE     REG-010  TO  RG1-RG8
           MOVE     SPACES   TO  RG2-RG8
           MOVE     REG-008  TO  RG2-DT1
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     SPACES   TO  RG1-DT1
           MOVE     REG-012  TO  RG1-RG8
           MOVE     REG-008  TO  RG2-DT1
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-30.

       ROT-RECB-00.
           MOVE     DES-TAB (IND1, IND2)  TO  DES-RG1
           MOVE     VDD-TAB (IND1, IND2)  TO  VAL-RG1
           MOVE     VDM-TAB (IND1, IND2)  TO  VAL-RG9
           MOVE     "-"      TO  SNL-RG1
           MOVE     REG-009  TO  RG1-RG8
           MOVE     SPACES   TO  RG2-RG8
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-008  TO  RG2-DT1
           COMPUTE  ACM-DIF   =  ACM-DIF
                              -  VDD-TAB (IND1, IND2)
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-20.

       ROT-CRED-00.
           IF  ACM-LN2   =    2  GO       ROT-CRED-10.
           MOVE     REG-009  TO  RG1-RG8
           MOVE     REG-011  TO  RG2-RG8
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-008  TO  RG2-DT1
           MOVE     DET-001  TO  DET-TMP
           REWRITE  REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  RG1-DT1  RG1-RG8
           MOVE     REG-012  TO  RG2-RG8
           MOVE     REG-008  TO  RG2-DT1
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     2        TO  ACM-LN2.

       ROT-CRED-10.
           MOVE     DES-TAB (IND1, IND2)  TO  DES-RG1
           MOVE     VDD-TAB (IND1, IND2)  TO  VAL-RG1
           MOVE     VDM-TAB (IND1, IND2)  TO  VAL-RG9
           MOVE     "+"      TO  SNL-RG1
           MOVE     REG-009  TO  RG1-RG8
           MOVE     SPACES   TO  RG2-RG8
           COMPUTE  ACM-DIF   =  ACM-DIF
                              +  VDD-TAB (IND1, IND2)
           IF  IND1      >   05  OR
               IND2      >   18  GO       ROT-CRED-20.
           MOVE     CRD-TAB (IND1, IND2)  TO  VAL-RG9
           MOVE     REG-009  TO  RG2-RG8.

       ROT-CRED-20.
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-008  TO  RG2-DT1
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-20.

       ROT-DIFR-00.
           IF  ACM-LN2   =    3  PERFORM  ROT-DIFR-10.
           MOVE     REG-005  TO  RG1-DT1
           MOVE     SPACES   TO  RG2-DT1
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ACM-DIF  TO  VAL-RG7
           MOVE     TOT-DIF  TO  VAL-RG9
           MOVE     REG-009  TO  RG1-RG8
           MOVE     TOT-CRD  TO  VAL-RG9
           MOVE     REG-009  TO  RG2-RG8
           MOVE     REG-007  TO  RG1-DT1
           MOVE     REG-008  TO  RG2-DT1
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           MOVE     3        TO  ACM-LN2.

       ROT-DIFR-10.
           MOVE     DES-TAB (IND1, IND2)  TO  DES-RG1
           MOVE     VDD-TAB (IND1, IND2)  TO  VAL-RG1
           MOVE     VDM-TAB (IND1, IND2)  TO  VAL-RG9
           MOVE     "*"      TO  SNL-RG1
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-009  TO  RG1-RG8
           MOVE     SPACES   TO  RG2-RG8
           MOVE     REG-008  TO  RG2-DT1
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-20.

       ROT-GTLA-00.
           MOVE   "G"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE   "R"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

       ROT-NOME-00.
           CALL   "DYNOME"  USING  PRM-NOM
                                   PRM-CAB
           MOVE    CLI-NOM   TO    CLI-CB1
           MOVE    PRM-CAB   TO    DTA-CB2.

       ROT-EXIT-00.
           CLOSE   CADCTR  CADCXA
                   CADPRS  CADSDO
           DELETE   FILE   CADTMP  GOBACK.
