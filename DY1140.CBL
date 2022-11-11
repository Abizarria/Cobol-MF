       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1140.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADATV        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ATV
                       FILE          STATUS  IS  FST-ATV.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST
                       FILE          STATUS  IS  FST-EST.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADATV.CPY.
       COPY  CADEST.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-ATV          PIC  X(02).
           03  FST-EST          PIC  X(02).

           03  IND1             PIC  9(02).
           03  LIN-AUX          PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  ACM-QTD          PIC  9(06)V9.
           03  TOT-QTD          PIC  9(07)V9.
           03  ACM-CST          PIC S9(07)V9999.
           03  ACM-VDA          PIC S9(07)V9999.
           03  TOT-CST          PIC S9(08)V9999.
           03  TOT-VDA          PIC S9(08)V9999.

           03  REG-TAB          PIC  X(63)  OCCURS  40.

           03  REG-AUX.
               05  DES-AUX      PIC  X(16).
               05  QTD-AUX      PIC  Z.ZZZ.ZZ9,9.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  CST-AUX      PIC  ---.---.--9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  VDA-AUX      PIC  ---.---.--9,99.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  05.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1140".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  09.
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 05 COLUMN 06 VALUE " Administraฦo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 06 COLUMN 08 VALUE " Controle de Estoques ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 05 COLUMN 28 VALUE "฿".
               05  LINE 06 COLUMN 30 VALUE "฿".
               05  LINE 06 COLUMN 07 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 07 COLUMN 06 VALUE " ษ                        
      -            "ออออออออออออออออออออออออออออออออออออออออป ".
               05  LINE 08 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 09 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 10 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 11 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 12 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 13 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 14 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 15 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 16 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 17 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 18 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 19 COLUMN 06 VALUE " บ
      -            "                                        บ ".
               05  LINE 20 COLUMN 06 VALUE " ศออออออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 10 VALUE " Posiฦo do Movimento ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 08 COLUMN 76 VALUE  "  ".
               05  LINE 09 COLUMN 76 VALUE  "  ".
               05  LINE 10 COLUMN 76 VALUE  "  ".
               05  LINE 11 COLUMN 76 VALUE  "  ".
               05  LINE 12 COLUMN 76 VALUE  "  ".
               05  LINE 13 COLUMN 76 VALUE  "  ".
               05  LINE 14 COLUMN 76 VALUE  "  ".
               05  LINE 15 COLUMN 76 VALUE  "  ".
               05  LINE 16 COLUMN 76 VALUE  "  ".
               05  LINE 17 COLUMN 76 VALUE  "  ".
               05  LINE 18 COLUMN 76 VALUE  "  ".
               05  LINE 19 COLUMN 76 VALUE  "  ".
               05  LINE 20 COLUMN 76 VALUE  "  ".
               05  LINE 21 COLUMN 08 PIC X(70) FROM SPACES.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 08 COLUMN 10 PIC X(61) FROM ALL "฿".
               05  LINE 16 COLUMN 10 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 09 COLUMN 10 PIC X(61) FROM SPACES.
               05  LINE 10 COLUMN 10 PIC X(61) FROM SPACES.
               05  LINE 11 COLUMN 10 PIC X(61) FROM SPACES.
               05  LINE 12 COLUMN 10 PIC X(61) FROM SPACES.
               05  LINE 13 COLUMN 10 PIC X(61) FROM SPACES.
               05  LINE 14 COLUMN 10 PIC X(61) FROM SPACES.
               05  LINE 15 COLUMN 10 PIC X(61) FROM SPACES.
           03  LINE 09 COLUMN 11 VALUE " DESCRI€วO        QUANTIDADE
      -        "TOTAL CUSTO    TOTAL VENDA " BCK-CLR 07 FOR-CLR 12.
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 09 COLUMN 71 PIC X(01) FROM     " ".
               05  LINE 10 COLUMN 71 PIC X(01) FROM     " ".
               05  LINE 11 COLUMN 71 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 71 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 71 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 71 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 71 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 71 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 11 PIC X(60) FROM ALL "฿".
           03  BCK-CLR 01 FOR-CLR 07.
               05  LINE 17 COLUMN 10 PIC X(61) FROM ALL "฿".
               05  LINE 19 COLUMN 10 PIC X(01) FROM     "Ü".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 18 COLUMN 10 PIC X(61) FROM "  TOTAL GERAL ...".
           03  BCK-CLR 00 FOR-CLR 01.
               05  LINE 18 COLUMN 71 PIC X(01) FROM     " ".
               05  LINE 19 COLUMN 71 PIC X(01) FROM     " ".
               05  LINE 19 COLUMN 11 PIC X(60) FROM ALL "฿".

       01  ENTR-01  BCK-CLR 02 FOR-CLR 15.
           03  LINE 10 COLUMN 12 PIC X(57) FROM REG-TAB (IND1).
           03  LINE 11 COLUMN 12 PIC X(57) FROM REG-TAB (IND1 + 1).
           03  LINE 12 COLUMN 12 PIC X(57) FROM REG-TAB (IND1 + 2).
           03  LINE 13 COLUMN 12 PIC X(57) FROM REG-TAB (IND1 + 3).
           03  LINE 14 COLUMN 12 PIC X(57) FROM REG-TAB (IND1 + 4).
           03  LINE 15 COLUMN 12 PIC X(57) FROM REG-TAB (IND1 + 5).

       01  ENTR-02  BCK-CLR 01 FOR-CLR 15.
           03  LINE 18 COLUMN 28 PIC Z.ZZZ.ZZ9,9    FROM TOT-QTD.
           03  LINE 18 COLUMN 40 PIC ---.---.--9,99 FROM TOT-CST.
           03  LINE 18 COLUMN 55 PIC ---.---.--9,99 FROM TOT-VDA.

       01  SETA-01  BCK-CLR 02.
           03  TRC-ACM LINE 10 COLUMN 70 VALUE " ".
           03  TRC-ABX LINE 15 COLUMN 70 VALUE " ".
           03  STA-ACM LINE 10 COLUMN 70 VALUE "" FOR-CLR 14.
           03  STA-ABX LINE 15 COLUMN 70 VALUE "" FOR-CLR 14.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           MOVE     0          TO     IND1
           OPEN     INPUT    CADATV   CADEST.

       ROT-0000-10.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1     <    40  GO       ROT-0000-10.
           MOVE     0        TO  IND1.

       ROT-CALC-00.
           MOVE     SPACES   TO  CHV-ATV
           MOVE     ZEROS    TO  TOT-QTD
                                 TOT-CST  TOT-VDA
           START    CADATV       KEY  >   CHV-ATV.

       ROT-CALC-10.
           READ     CADATV  NEXT
           IF  FST-ATV  =   "10" GO       ROT-EXBE-00.
           MOVE     CHV-ATV  TO  PRX-EST
           MOVE     ZEROS    TO  SUF-EST  ACM-QTD
                                 ACM-CST  ACM-VDA.
       ROT-CALC-20.
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-CALC-30.
           READ     CADEST  NEXT
           IF  PRX-EST  NOT   =  CHV-ATV
                                 GO       ROT-CALC-30.
           COMPUTE  ACM-QTD   =  ACM-QTD  +   SDO-EST
           COMPUTE  ACM-CST   =  ACM-CST  +   SDO-EST  *   CST-EST
           COMPUTE  ACM-VDA   =  ACM-VDA  +   SDO-EST  *   VDA-EST.
           GO       ROT-CALC-20.

       ROT-CALC-30.
           ADD      1        TO  IND1
           MOVE     DES-ATV  TO  DES-AUX
           MOVE     ACM-QTD  TO  QTD-AUX
           MOVE     ACM-CST  TO  CST-AUX
           MOVE     ACM-VDA  TO  VDA-AUX
           MOVE     REG-AUX  TO  REG-TAB (IND1)
           ADD      ACM-QTD  TO  TOT-QTD
           ADD      ACM-CST  TO  TOT-CST
           ADD      ACM-VDA  TO  TOT-VDA
           DISPLAY  ENTR-02      GO      ROT-CALC-10.

       ROT-EXBE-00.
           MOVE     IND1     TO  LIN-AUX
           MOVE     1        TO  IND1.

       ROT-EXBE-10.
           IF  (IND1 + 05)  <  LIN-AUX  DISPLAY  STA-ABX
               IF  IND1  >  1  DISPLAY  STA-ACM
                   ELSE        DISPLAY  TRC-ACM
           ELSE    IF  IND1    >    1   DISPLAY  STA-ACM  TRC-ABX
                       ELSE    DISPLAY  TRC-ACM. DISPLAY  ENTR-01.

       ROT-EXBE-20.
           PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-EXBE-20.
           IF  TCL-MSG   =   05  GO       ROT-MOVE-05.
           IF  TCL-MSG   =   06  GO       ROT-MOVE-06.
           IF  TCL-MSG   =   53  GO       ROT-MOVE-53.
           IF  TCL-MSG   =   54  GO       ROT-MOVE-54.
           IF  TCL-MSG   >   00  GO       ROT-EXBE-20.
           IF  HEX-MSG   =   13  GO       ROT-EXBE-20.
           CLOSE    CADATV       CADEST   GOBACK.

       ROT-MOVE-05.
           IF  IND1  =   01         GO    ROT-EXBE-20.
           SUBTRACT  1  FROM  IND1  GO    ROT-EXBE-10.

       ROT-MOVE-06.
           IF  LIN-AUX  <  (IND1 + 06)    GO   ROT-EXBE-20.
           ADD       1  TO  IND1          GO   ROT-EXBE-10.

       ROT-MOVE-53.
           IF  IND1  >  06  SUBTRACT  06  FROM  IND1  GO  ROT-EXBE-10.
           IF  IND1  >  01  MOVE      01   TO   IND1  GO  ROT-EXBE-10.
           GO  ROT-EXBE-20.

       ROT-MOVE-54.
           IF  LIN-AUX  <  (IND1 + 06)  GO   ROT-EXBE-20.
           ADD     06   TO  IND1        GO   ROT-EXBE-10.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.
