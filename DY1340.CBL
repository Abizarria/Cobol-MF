       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1340.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP
                       FILE          STATUS  IS  FST-LDP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCTR.CPY.
       COPY  CADLDP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-CTR          PIC  X(02).
           03  FST-LDP          PIC  X(02).

           03  IND1             PIC  9(02).
           03  TECLADO          PIC  X(02).
           03  PRX-AUX          PIC  9(01).
           03  ACM-LIQ          PIC S9(08)V99.
           03  TOT-LIQ          PIC S9(08)V99.
           03  ACM-PDT          PIC  9(08)V99.
           03  TOT-PDT          PIC  9(08)V99.

           03  DTA-LIM.
               05  ANO-LIM      PIC  9(02).
               05  MES-LIM      PIC  9(02).
               05  DIA-LIM      PIC  9(02).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  25.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1340".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR  04 FOR-CLR   15.
               05  LINE 05 COLUMN 10 VALUE " Administra‡Æo Global ".
           03  BCK-CLR  01 FOR-CLR   15.
               05  LINE 06 COLUMN 12 VALUE " Controle de Despesas ".
           03  BCK-CLR  00 FOR-CLR   08.
               05  LINE 05 COLUMN 32 VALUE "ß".
               05  LINE 06 COLUMN 34 VALUE "ß".
               05  LINE 06 COLUMN 11 VALUE "Ü".
           03  BCK-CLR 7 FOR-CLR 14.
               05 LINE 07 COLUMN 10 VALUE " É                        Í
      -           "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ» ".
               05 LINE 08 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 09 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 10 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 11 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 12 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 13 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 14 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 15 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 16 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 17 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 18 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 19 COLUMN 10 VALUE " º
      -           "                               º ".
               05 LINE 20 COLUMN 10 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -           "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 14 VALUE " Posi‡Æo do Movimento ".
           03  BCK-CLR 00.
               05  LINE 08 COLUMN 72 VALUE "  ".
               05  LINE 09 COLUMN 72 VALUE "  ".
               05  LINE 10 COLUMN 72 VALUE "  ".
               05  LINE 11 COLUMN 72 VALUE "  ".
               05  LINE 12 COLUMN 72 VALUE "  ".
               05  LINE 13 COLUMN 72 VALUE "  ".
               05  LINE 14 COLUMN 72 VALUE "  ".
               05  LINE 15 COLUMN 72 VALUE "  ".
               05  LINE 16 COLUMN 72 VALUE "  ".
               05  LINE 17 COLUMN 72 VALUE "  ".
               05  LINE 18 COLUMN 72 VALUE "  ".
               05  LINE 19 COLUMN 72 VALUE "  ".
               05  LINE 20 COLUMN 72 VALUE "  ".
               05  LINE 21 COLUMN 12 PIC X(62) FROM SPACES.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 08 COLUMN 14 PIC X(53) FROM ALL "ß".
               05  LINE 15 COLUMN 14 VALUE "Ü".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 09 COLUMN 67 VALUE " ".
               05  LINE 10 COLUMN 67 VALUE " ".
               05  LINE 11 COLUMN 67 VALUE " ".
               05  LINE 12 COLUMN 67 VALUE " ".
               05  LINE 13 COLUMN 67 VALUE " ".
               05  LINE 14 COLUMN 67 VALUE " ".
               05  LINE 15 COLUMN 67 VALUE " ".
               05  LINE 15 COLUMN 15 PIC X(52) FROM ALL "ß".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 09 COLUMN 14 PIC X(53) FROM SPACES.
               05  LINE 10 COLUMN 14 PIC X(53) FROM
                   "  OBRIG. TRABALHISTA".
               05  LINE 11 COLUMN 14 PIC X(53) FROM
                   "  OBRIGA€åES FISCAIS".
               05  LINE 12 COLUMN 14 PIC X(53) FROM
                   "  DESP. OPERACIONAIS".
               05  LINE 13 COLUMN 14 PIC X(53) FROM
                   "  DESPESA DE CAPITAL".
               05  LINE 14 COLUMN 14 PIC X(53) FROM
                   "  CRDITOS DE SàCIOS".
           03  BCK-CLR 07 FOR-CLR 12.
               05  LINE 09 COLUMN 15 VALUE " DESCRI€ÇO DO GRUPO     VALO
      -            "R PAGO   VALOR · PAGAR ".
           03  BCK-CLR 01 FOR-CLR 07.
               05  LINE 16 COLUMN 14 PIC X(53) FROM ALL "ß".
               05  LINE 19 COLUMN 14 VALUE "Ü".
           03  BCK-CLR 00 FOR-CLR 01.
               05  LINE 17 COLUMN 67 VALUE " ".
               05  LINE 18 COLUMN 67 VALUE " ".
               05  LINE 19 COLUMN 67 VALUE " ".
               05  LINE 19 COLUMN 15 PIC X(52) FROM ALL "ß".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 17 COLUMN 14 PIC X(53) FROM
                   "  TOTAL DAS DESPESAS".
               05  LINE 18 COLUMN 14 PIC X(53) FROM
                   "  COMPRA DE PRODUTOS".
       01  ENTR-01.
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE IND1 COLUMN 35 PIC ---.---.--9,99 FROM ACM-LIQ.
               05  LINE IND1 COLUMN 52 PIC  ZZ.ZZZ.ZZ9,99 FROM ACM-PDT.
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE  17  COLUMN 35 PIC ---.---.--9,99 FROM TOT-LIQ.
               05  LINE  17  COLUMN 52 PIC  ZZ.ZZZ.ZZ9,99 FROM TOT-PDT.

       01  ENTR-02 BCK-CLR 01 FOR-CLR 15.
           03  LINE 18 COLUMN 35 PIC ---.---.--9,99 FROM ACM-LIQ.
           03  LINE 18 COLUMN 52 PIC  ZZ.ZZZ.ZZ9,99 FROM ACM-PDT.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"      USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL    =   1  GOBACK.
           OPEN     INPUT        CADCTR   CADLDP
           DISPLAY  TELA-01      READ     CADCTR
           MOVE     DTI-CTR  TO  DTA-LIM
           MOVE     31       TO  DIA-LIM
           MOVE     09       TO  IND1
           MOVE     ZEROS    TO  PRX-AUX
                                 TOT-LIQ  TOT-PDT.
       ROT-EXBE-00.
           ADD      1        TO  PRX-AUX  IND1
           IF  PRX-AUX   >    5  GO       ROT-EXBE-20.
           MOVE     SPACES   TO  CHV-LDP
           MOVE     PRX-AUX  TO  PRX-LDP
           MOVE     ZEROS    TO  ACM-LIQ  ACM-PDT.

       ROT-EXBE-10.
           DISPLAY  ENTR-01
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       ROT-EXBE-00.
           READ     CADLDP  NEXT
           IF  PRX-LDP        >  PRX-AUX
                                 GO       ROT-EXBE-00.
           IF  FLG-LDP   =  "P"  ADD      VAL-LDP  TO  ACM-PDT
                                 ADD      VAL-LDP  TO  TOT-PDT.
           IF  DTA-LDP  NOT   >  DTA-LIM
               EVALUATE FLG-LDP
                   WHEN   "L"
                                 ADD      VAL-LDP  TO  ACM-LIQ
                                 ADD      VAL-LDP  TO  TOT-LIQ
                   WHEN   "R"
                                 SUBTRACT VAL-LDP FROM ACM-LIQ
                                 SUBTRACT VAL-LDP FROM TOT-LIQ
               END-EVALUATE.
           GO       ROT-EXBE-10.

       ROT-EXBE-20.
           MOVE     SPACES   TO  CHV-LDP
           MOVE     6        TO  PRX-LDP
           MOVE     ZEROS    TO  ACM-LIQ  ACM-PDT.

       ROT-EXBE-30.
           DISPLAY  ENTR-02
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       ROT-EXIT-00.
           READ     CADLDP  NEXT
           IF  FLG-LDP   =  "P"  ADD      VAL-LDP  TO  ACM-PDT.
           IF  DTA-LDP  NOT   >  DTA-LIM
               EVALUATE  FLG-LDP
                   WHEN    "L"
                                 ADD      VAL-LDP  TO  ACM-LIQ
                   WHEN    "R"
                                 SUBTRACT VAL-LDP FROM ACM-LIQ
               END-EVALUATE.
           GO       ROT-EXBE-30.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-EXIT-00.
           MOVE 50  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  GO       ROT-HELP-00.
           IF  TCL-MSG   >   00  OR
               HEX-MSG   =   13  GO       ROT-EXIT-00.

       ROT-EXIT-10.
           CLOSE    CADCTR  CADLDP  GOBACK.
