       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1251.
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

       COPY  CADCXA.CPY.
       COPY  CADPRS.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CXA          PIC  X(02).
           03  FST-PRS          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  IND3             PIC  9(02).
           03  IND4             PIC  9(02).
           03  ATV-AUX          PIC  9(01).
           03  ACM-ATV          PIC  9(08)V99.
           03  VAL-AUX          PIC  ZZ.ZZZ.ZZ9,99.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  REG-AUX.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  DES-AUX      PIC  X(16).
               05  FILLER       PIC  X(02)  VALUE  "-".
               05  TUR-AUX      PIC  X(06).

           03  TAB-REG          OCCURS  20.
               05  REG-TAB      PIC  X(25).
               05  CHV-TAB      PIC  9(03).

           03  TAB-TUR          PIC  X(15)  VALUE  "MANHATARDENOITE".
           03  RED-TUR          REDEFINES   TAB-TUR.
               05  TUR-TAB      PIC  X(05)  OCCURS  03.

       01  CAB-001.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(18)  VALUE  "RESUMO MOVIMENTO :".
           03  CXA-CB2          PIC  X(25).
           03  FILLER           PIC  X(25)  VALUE  SPACES.
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "       DESCRICAO              QUANTIDADE      VALOR LANCADO
      -    "           SUB-TOTAL".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  FILLER           PIC  X(01)  VALUE  "[".
           03  SNL-DT1          PIC  X(01).
           03  FILLER           PIC  X(05)  VALUE  "]".
           03  DES-DT1          PIC  X(23).
           03  QTD-DT1          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(07)  VALUE  SPACES.
           03  VAL-DT1          PIC  Z.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  TOT-DT1          PIC  X(13).

       01  TOT-001.
           03  FILLER           PIC  X(67)  VALUE  "[=]    DIFERENCA DO
      -        "CAIXA ................................".
           03  VAL-TT1          PIC  --.---.--9,99.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  17.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1251".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
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
           03  LIN-IMP          PIC  9(02)  VALUE  16.
           03  CL1-IMP          PIC  9(02)  VALUE  41.
           03  CL2-IMP          PIC  9(02)  VALUE  47.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "?" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 07 COLUMN 19 VALUE " Administra??o Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 08 COLUMN 21 VALUE " Controle de Receitas ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 09 COLUMN 23 VALUE " Emiss?o de Relat?rio ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 07 COLUMN 41 VALUE "?".
               05  LINE 08 COLUMN 43 VALUE "?".
               05  LINE 09 COLUMN 45 VALUE "?".
               05  LINE 08 COLUMN 20 VALUE "?".
               05  LINE 09 COLUMN 22 VALUE "?".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 10 COLUMN 19 VALUE
                   " ???                        ????????????? ".
               05  LINE 11 COLUMN 19 VALUE
                   " ?                                        ? ".
               05  LINE 12 COLUMN 19 VALUE
                   " ?                                        ? ".
               05  LINE 13 COLUMN 19 VALUE
                   " ?                                        ? ".
               05  LINE 14 COLUMN 19 VALUE
                   " ?                                        ? ".
               05  LINE 15 COLUMN 19 VALUE
                   " ?                                        ? ".
               05  LINE 16 COLUMN 19 VALUE
                   " ?                                        ? ".
               05  LINE 17 COLUMN 19 VALUE
                   " ?????????????????????????????????????????? ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 10 COLUMN 25 VALUE "   Resumo Movimento   ".
           03  BCK-CLR  00.
               05  LINE 11 COLUMN 63 VALUE "  ".
               05  LINE 12 COLUMN 63 VALUE "  ".
               05  LINE 13 COLUMN 63 VALUE "  ".
               05  LINE 14 COLUMN 63 VALUE "  ".
               05  LINE 15 COLUMN 63 VALUE "  ".
               05  LINE 16 COLUMN 63 VALUE "  ".
               05  LINE 17 COLUMN 63 VALUE "  ".
               05  LINE 18 COLUMN 21 PIC X(44) FROM SPACES.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 11 COLUMN 23 PIC X(35) FROM ALL "?".
               05  LINE 13 COLUMN 23 PIC X(01) FROM     "?".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 12 COLUMN 23 PIC X(35) FROM "  Caixa :".
           03  BCK-CLR 02 FOR-CLR 00.
               05  LINE 12 COLUMN 58 PIC X(01) FROM     "?".
               05  LINE 13 COLUMN 58 PIC X(01) FROM     "?".
               05  LINE 13 COLUMN 24 PIC X(34) FROM ALL "?".

       01  TELA-02 BCK-CLR 05 FOR-CLR 11.
           03  LINE 12 COLUMN 37 VALUE " ????????        ????????? ".
           03  LINE 13 COLUMN 37 VALUE " ?                         ? ".
           03  LINE 14 COLUMN 37 VALUE " ?                         ? ".
           03  LINE 15 COLUMN 37 VALUE " ?                         ? ".
           03  LINE 16 COLUMN 37 VALUE " ?                         ? ".
           03  LINE 17 COLUMN 37 VALUE " ?                         ? ".
           03  LINE 18 COLUMN 37 VALUE " ?                         ? ".
           03  LINE 19 COLUMN 37 VALUE " ??????????????????????????? ".
           03  LINE 12 COLUMN 48 VALUE "Caixas" FOR-CLR 14.
           03  BCK-CLR 00.
               05  LINE 13 COLUMN 66 VALUE "  ".
               05  LINE 14 COLUMN 66 VALUE "  ".
               05  LINE 15 COLUMN 66 VALUE "  ".
               05  LINE 16 COLUMN 66 VALUE "  ".
               05  LINE 17 COLUMN 66 VALUE "  ".
               05  LINE 18 COLUMN 66 VALUE "  ".
               05  LINE 19 COLUMN 66 VALUE "  ".
               05  LINE 20 COLUMN 39 PIC X(29) FROM SPACES.

       01  TELA-03 BCK-CLR 01.
           03  FOR-CLR  07.
               05  LINE 14 COLUMN 26 PIC X(30) FROM ALL "?".
               05  LINE 16 COLUMN 26 PIC X(01) FROM     "?".
           03  FOR-CLR  15.
               05  LINE 15 COLUMN 26 PIC X(30) FROM
                   "   P?ginas Montadas ?        ".
               05  PAG-T03 LINE 15 COLUMN 50 PIC ZZ9 FROM PAG-CB1.
           03  FOR-CLR  00.
               05  LINE 15 COLUMN 56 PIC X(01) FROM     "?".
               05  LINE 16 COLUMN 56 PIC X(01) FROM     "?".
               05  LINE 16 COLUMN 27 PIC X(29) FROM ALL "?".

       01  MENU-01  BCK-CLR 05 FOR-CLR 15.
           03  LINE 13 COLUMN 39 PIC X(25) FROM REG-TAB (IND2).
           03  LINE 14 COLUMN 39 PIC X(25) FROM REG-TAB (IND2 + 1).
           03  LINE 15 COLUMN 39 PIC X(25) FROM REG-TAB (IND2 + 2).
           03  LINE 16 COLUMN 39 PIC X(25) FROM REG-TAB (IND2 + 3).
           03  LINE 17 COLUMN 39 PIC X(25) FROM REG-TAB (IND2 + 4).
           03  LINE 18 COLUMN 39 PIC X(25) FROM REG-TAB (IND2 + 5).

       01  MENU-02  BCK-CLR 05 FOR-CLR 15.
           03  LINE IND4 COLUMN 39 PIC X(25) FROM REG-TAB (IND3).

       01  MENU-03  BCK-CLR 07 FOR-CLR 12.
           03  LINE IND4 COLUMN 39 PIC X(25) FROM REG-TAB (IND3).

       01  SETA-01  BCK-CLR 05.
           03  TRC-ACM LINE 13 COLUMN 64 VALUE "?" FOR-CLR 11.
           03  TRC-ABX LINE 18 COLUMN 64 VALUE "?" FOR-CLR 11.
           03  STA-ACM LINE 13 COLUMN 64 VALUE "" FOR-CLR 15.
           03  STA-ABX LINE 18 COLUMN 64 VALUE "" FOR-CLR 15.

       01  ENTR-01  BCK-CLR 02 FOR-CLR 15
                    LINE 12 COLUMN 32 PIC X(25) FROM REG-AUX.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           ACCEPT   ARQ-IMP   FROM    TIME
           OPEN     INPUT    CADCXA   CADPRS
                    OUTPUT   CADTMP   MOVE  1  TO IND1.

       ROT-MENU-00.
           MOVE     SPACES   TO  REG-TAB  (IND1)
           IF  IND1    <     20  ADD      1  TO  IND1
                                 GO       ROT-MENU-00.
           MOVE     ZEROS    TO  CHV-CXA
           MOVE     13       TO  IND4
           MOVE     00       TO  IND1
           MOVE     01       TO  IND2     IND3.

       ROT-MENU-10.
           START    CADCXA       KEY  >   CHV-CXA
           IF  FST-CXA   =  "23" GO       ROT-MENU-20.
           READ     CADCXA  NEXT
           MOVE     DES-CXA  TO  DES-AUX
           MOVE     TUR-TAB     (TUR-CXA) TO  TUR-AUX
           ADD      1        TO  IND1
           MOVE     REG-AUX  TO  REG-TAB       (IND1)
           MOVE     CHV-CXA  TO  CHV-TAB       (IND1)
           GO       ROT-MENU-10.

       ROT-MENU-20.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-02.

       ROT-MENU-30.
           DISPLAY  TRC-ACM      TRC-ABX
           IF (IND2  +  05)  <   IND1
                                 DISPLAY  STA-ABX.
           IF  IND2  >  01       DISPLAY  STA-ACM.
           DISPLAY     MENU-01   MENU-03.

       ROT-MENU-40.
           MOVE  1  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-40.
           IF  TCL-MSG   =   05  GO       ROT-ST05-00.
           IF  TCL-MSG   =   06  GO       ROT-ST06-00.
           IF  TCL-MSG   =   53  GO       ROT-PGUP-00.
           IF  TCL-MSG   =   54  GO       ROT-PGDW-00.
           IF  TCL-MSG   >   00  GO       ROT-MENU-40.
           IF  HEX-MSG   =   27  GO       ROT-EXIT-00.

       ROT-IMPR-00.
           MOVE     CHV-TAB      (IND3)   TO  CHV-CXA
           READ     CADCXA
           MOVE     DTA-CXA  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-CAB
           MOVE     MES-AUX  TO  MES-CAB
           MOVE     ANO-AUX  TO  ANO-CAB
           MOVE     REG-TAB      (IND3)   TO  CXA-CB2
           PERFORM  ROT-NOME-00  PERFORM  ROT-RTLA-00
           MOVE     ZEROS    TO  ACM-ATV
                                 PAG-CB1  CHV-TMP
           DISPLAY  ENTR-01      TELA-03
           MOVE     SPACES   TO  CHV-PRS  TOT-DT1
           MOVE     TUR-CXA  TO  TUR-PRS
           MOVE     NUM-CXA  TO  CXA-PRS
           START    CADPRS       KEY  >   CHV-PRS
           READ     CADPRS  NEXT
           MOVE     ATV-PRS  TO  ATV-AUX.

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
           IF  ATV-PRS   >    4  MOVE "+" TO  SNL-DT1.
           IF  ATV-PRS   <    5  MOVE "-" TO  SNL-DT1.
           MOVE     DES-PRS  TO  DES-DT1
           MOVE     QTD-PRS  TO  QTD-DT1
           MOVE     VDD-PRS  TO  VAL-DT1
           ADD      VDD-PRS  TO  ACM-ATV
           START    CADPRS       KEY  >   CHV-PRS
           READ     CADPRS  NEXT
           IF  ATV-PRS   >    7  PERFORM  ROT-IMPR-30
                                 GO       ROT-IMPR-50.
           IF  ATV-PRS  NOT   =  ATV-AUX
                                 GO       ROT-IMPR-40.
           IF  LIN-TMP   <   61  PERFORM  ROT-IMPR-30
                                 GO       ROT-IMPR-20.
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
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP.

       ROT-IMPR-40.
           MOVE     ACM-ATV  TO  VAL-AUX
           MOVE     VAL-AUX  TO  TOT-DT1
           PERFORM  ROT-IMPR-30
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     SPACES   TO  DET-TMP  TOT-DT1
           MOVE     ZEROS    TO  ACM-ATV
           MOVE     ATV-PRS  TO  ATV-AUX
           WRITE    REG-TMP      GO       ROT-IMPR-20.

       ROT-IMPR-50.
           MOVE     VDD-PRS  TO  VAL-TT1
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     TOT-001  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     PAG-CB1  TO  PAG-IMP
           DISPLAY  PAG-T03      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE     FILE       CADTMP
           OPEN      OUTPUT      CADTMP
           PERFORM  ROT-RTLA-00  GO       ROT-MENU-20.

       ROT-ST05-00.
           IF  IND3      =   01  GO       ROT-MENU-40.
           IF  IND4      =   13  SUBTRACT 1 FROM IND2
                                 SUBTRACT 1 FROM IND3
                                 GO       ROT-MENU-30.
           DISPLAY  MENU-02      SUBTRACT 1 FROM IND3
           COMPUTE  IND4     =   IND4  -  1
           DISPLAY  MENU-03      GO       ROT-MENU-40.

       ROT-ST06-00.
           IF  IND3      =  IND1 GO       ROT-MENU-40.
           IF  IND4      =   18  ADD      1  TO  IND2
                                 ADD      1  TO  IND3
                                 GO       ROT-MENU-30.
           DISPLAY  MENU-02      ADD      1  TO  IND3
           COMPUTE  IND4     =   IND4  +  1
           DISPLAY  MENU-03      GO       ROT-MENU-40.

       ROT-PGUP-00.
           IF  IND2      =   01  GO       ROT-MENU-40.
           MOVE     13       TO  IND4
           IF  IND2      >    6  COMPUTE  IND3 = IND2 - 6
                                 COMPUTE  IND2 = IND2 - 6
           ELSE
                                 MOVE  1  TO   IND2  IND3.
           GO      ROT-MENU-30.

       ROT-PGDW-00.
           IF  IND1 < (IND2 + 6) GO       ROT-MENU-40.
           COMPUTE  IND3     =   IND2  +  6
           COMPUTE  IND2     =   IND2  +  6
           MOVE     13  TO  IND4 GO       ROT-MENU-30.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING  PRM-MSG.

       ROT-HELP-00.
           CALL   "DYHELP"  USING  PRM-HLP.

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
           CLOSE   CADCXA  CADPRS  CADTMP
           DELETE   FILE   CADTMP  GOBACK.
