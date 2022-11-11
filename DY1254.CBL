       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1254.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS
                       FILE          STATUS  IS  FST-PRS.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCTR.CPY.
       COPY  CADPRS.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CTR          PIC  X(02).
           03  FST-PRS          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  IND1             PIC  9(01).
           03  IND2             PIC  9(02).
           03  IND3             PIC  9(01).
           03  MED-AUX          PIC  9(06)V9.
           03  PRV-AUX          PIC  9(07)V9.
           03  ACM-VDA          PIC  9(07)V9.
           03  DES-AUX          PIC  X(15)  VALUE  "SUB-TOTAL".

           03  VAL-AUX          PIC  9(02)V99.
           03  RED-VAL          REDEFINES   VAL-AUX.
               05  INT-AUX      PIC  9(02).
               05  DEC-AUX      PIC  9(02).

           03  DTA-SYS.                             
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  ATV-TAB       OCCURS    03.
               05  PRX-TAB   OCCURS    35.
                   07  DES-TAB  PIC  X(15).
                   07  QVM-TAB  PIC  9(07)V9.

           03  TAB-IND   VALUE  "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".
               05  IND-TAB      PIC  X(01)  OCCURS  35  INDEXED BY I.

           03  FILLER           VALUE      "310031303130313130313031".
               05  DIA-TAB      PIC  9(02)  OCCURS  12.

       01  CAB-001.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(34)  VALUE
               "PREVISAO DE MOVIMENTOS  -  BASE :".
           03  DDI-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MMI-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AAI-CB2          PIC  9(02).
           03  FILLER           PIC  X(03)  VALUE  " ·".
           03  DDF-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MMF-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AAF-CB2          PIC  9(02).
           03  FILLER           PIC  X(15)  VALUE  SPACES.
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC  X(80)  VALUE
           "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".

       01  CAB-004              PIC  X(80)  VALUE
           "³  DESCRICAO        ³   CONCLUIDO  ³ MEDIA / DIA ³   PREVIST
      -    "O   ³ TOTAL NO MES ³".

       01  CAB-005              PIC  X(80)  VALUE
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-006              PIC  X(80)  VALUE
           "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  DET-001.
           03  FILLER           PIC  X(03)  VALUE  "³".
           03  DES-DT1          PIC  X(15).
           03  FILLER           PIC  X(04)  VALUE  "  ³".
           03  CON-DT1          PIC  Z.ZZZ.ZZ9,9.
           03  FILLER           PIC  X(05)  VALUE  "  ³".
           03  MED-DT1          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(04)  VALUE  "  ³".
           03  PRV-DT1          PIC  Z.ZZZ.ZZ9,9.
           03  FILLER           PIC  X(04)  VALUE  "  ³".
           03  TOT-DT1          PIC  Z.ZZZ.ZZ9,9.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  20.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  39.
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
               05  LINE 08 COLUMN 23 VALUE " Controle de Receitas ".
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
               05  LINE 10 COLUMN 27 VALUE " PrevisÆo  Movimentos ".
           03  BCK-CLR 00.
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
           03  BCK-CLR 01 FOR-CLR 07.
               05  LINE 14 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 16 COLUMN 25 PIC X(01) FROM     "Ü".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 15 COLUMN 25 PIC X(30) FROM
                   "   P ginas Montadas Ä    ".
               05  PAG-T01 LINE 15 COLUMN 49 PIC ZZ9 FROM PAG-CB1.
           03  BCK-CLR 01 FOR-CLR 00.
               05  LINE 15 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 26 PIC X(29) FROM ALL "Ü".

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"      USING    PRM-OPR
                                          PRM-SPL
           IF  LCK-SPL   =    1  GOBACK.
           OPEN     INPUT        CADCTR
           READ     CADCTR
           IF  DIA-CTR   =    0  PERFORM  ROT-MSGM-00
                                 GO       ROT-EXIT-00.
           ACCEPT   ARQ-IMP FROM TIME
           MOVE     DTI-CTR  TO  DTA-SYS
           MOVE     DIA-SYS  TO  DDI-CB2
           MOVE     MES-SYS  TO  MMI-CB2
           MOVE     ANO-SYS  TO  AAI-CB2
           MOVE     DTD-CTR  TO  DTA-SYS
           MOVE     DIA-SYS  TO  DDF-CB2
           MOVE     MES-SYS  TO  MMF-CB2
           MOVE     ANO-SYS  TO  AAF-CB2
           OPEN     INPUT        CADPRS
                    I-O          CADTMP
           COMPUTE  VAL-AUX  =   ANO-SYS  /  4
           IF  DEC-AUX   =   0
               MOVE     29   TO  DIA-TAB  (02)
           ELSE
               MOVE     28   TO  DIA-TAB  (02).
           MOVE     1        TO  IND1     IND2
           MOVE     ZEROS    TO  PAG-CB1  ACM-VDA
           PERFORM  ROT-NOME-00  DISPLAY  TELA-01.

       ROT-PESQ-00.
           MOVE     SPACES   TO  DES-TAB (IND1, IND2)
           MOVE     ZEROS    TO  QVM-TAB (IND1, IND2)
           ADD      1   TO  IND2
           IF  IND2     <    36  GO       ROT-PESQ-00.
           MOVE     1   TO  IND2 ADD      1  TO  IND1
           IF  IND1     <     4  GO       ROT-PESQ-00.
           MOVE     SPACES   TO  CHV-PRS.

       ROT-PESQ-10.
           START    CADPRS       KEY  >   CHV-PRS
           IF  FST-PRS   =  "23" GO       ROT-IMPR-00.
           READ     CADPRS  NEXT
           IF  ATV-PRS   >    3  GO       ROT-PESQ-10.
           SET      I   TO    1
           SEARCH   IND-TAB      WHEN     PRX-PRS  =
                                          IND-TAB (I)
           MOVE     ATV-PRS  TO  IND1
           SET      IND2     TO  I
           MOVE     DES-PRS  TO  DES-TAB (IND1, IND2)
           ADD      QTM-PRS  TO  QVM-TAB (IND1, IND2)
           GO       ROT-PESQ-10.

       ROT-IMPR-00.
           MOVE     1        TO  CHV-TMP  PAG-TMP
                                 LIN-TMP  PAG-CB1
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
           MOVE     CAB-005  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     1        TO  IND1  IND2  IND3.

       ROT-IMPR-10.
           IF  IND1  >  IND3     PERFORM  ROT-IMPR-30.
           MOVE     DES-TAB (IND1, IND2)  TO  DES-DT1
           MOVE     QVM-TAB (IND1, IND2)  TO  CON-DT1
           COMPUTE  MED-AUX   =  QVM-TAB  (IND1, IND2)
                                           /  DIA-CTR
           COMPUTE  PRV-AUX   = (DIA-CTR   /  DDF-CB2)
                              * (DIA-TAB     (MMF-CB2)
                              -  DDF-CB2)  *  MED-AUX
           COMPUTE  TOT-DT1   =  QVM-TAB  (IND1, IND2)
                                           +  PRV-AUX
           ADD      QVM-TAB (IND1, IND2)  TO  ACM-VDA
           MOVE     MED-AUX  TO  MED-DT1
           MOVE     PRV-AUX  TO  PRV-DT1
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP.

       ROT-IMPR-20.
           ADD      1        TO  IND2
           IF  IND2    <     36
               IF  DES-TAB      (IND1, IND2)  =   SPACES
                                 GO           ROT-IMPR-20
               ELSE
                                 GO           ROT-IMPR-10
           ELSE
               ADD     1     TO  IND1
               MOVE    0     TO  IND2
               IF  IND1  <   04  GO           ROT-IMPR-20.

       ROT-IMPR-30.
           MOVE     DES-AUX  TO  DES-DT1
           MOVE     ACM-VDA  TO  CON-DT1
           COMPUTE  MED-AUX   =  ACM-VDA   /  DIA-CTR
           COMPUTE  PRV-AUX   = (DIA-CTR   /  DDF-CB2)
                              * (DIA-TAB     (MMF-CB2)
                              -  DDF-CB2)  *  MED-AUX
           COMPUTE  TOT-DT1   =  ACM-VDA   +  PRV-AUX
           MOVE     MED-AUX  TO  MED-DT1
           MOVE     PRV-AUX  TO  PRV-DT1
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-005  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-005  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     IND1     TO  IND3
           MOVE     ZEROS    TO  ACM-VDA.

       ROT-IMPR-40.
           MOVE     CAB-006  TO  DET-TMP
           REWRITE  REG-TMP      DISPLAY  PAG-T01
           MOVE     PAG-CB1  TO  PAG-IMP
           CLOSE    CADPRS       CADTMP
           CALL    "DYIMPR"      USING    PRM-IMP
           GO       ROT-EXIT-00.

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
           DELETE   FILE    CADTMP
           CLOSE   CADCTR   GOBACK.
