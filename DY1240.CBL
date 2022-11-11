       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1240.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADBCO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-BCO
                       FILE          STATUS  IS  FST-BCO.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST
                       FILE          STATUS  IS  FST-EST.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC
                       FILE          STATUS  IS  FST-LBC.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP
                       FILE          STATUS  IS  FST-LDP.

           SELECT      CADSDO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SDO
                       FILE          STATUS  IS  FST-SDO.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADBCO.CPY.
       COPY  CADCTR.CPY.
       COPY  CADEST.CPY.
       COPY  CADLBC.CPY.
       COPY  CADLDP.CPY.
       COPY  CADSDO.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-BCO          PIC  X(02).
           03  FST-CTR          PIC  X(02).
           03  FST-EST          PIC  X(02).
           03  FST-LBC          PIC  X(02).
           03  FST-LDP          PIC  X(02).
           03  FST-SDO          PIC  X(02).

           03  TECLADO          PIC  9(02).
           03  ACM-VAL          PIC S9(08)V9999.
           03  ACM-TOT          PIC S9(08)V99.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-LIM.
               05  ANO-LIM      PIC  9(02).
               05  MES-LIM      PIC  9(02).
               05  DIA-LIM      PIC  9(02).

           03  DTI-AUX.
               05  DDI-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MMI-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  AAI-AUX      PIC  9(02).

           03  DTF-AUX.
               05  DDF-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MMF-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  AAF-AUX      PIC  9(02).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  16.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1240".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 05 COLUMN 14 VALUE " Administra‡Æo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 06 COLUMN 16 VALUE " Controle de Receitas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 05 COLUMN 36 VALUE "ß".
               05  LINE 06 COLUMN 38 VALUE "ß".
               05  LINE 06 COLUMN 15 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 07 COLUMN 14 VALUE " É                        
      -            "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ» ".
               05  LINE 08 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 09 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 10 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 11 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 12 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 13 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 14 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 15 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 16 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 17 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 18 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 19 COLUMN 14 VALUE " º
      -            "                        º ".
               05  LINE 20 COLUMN 14 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -            "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 18 VALUE " Posi‡Æo do Movimento ".
           03  BCK-CLR 00.
               05  LINE 08 COLUMN 68 VALUE "  ".
               05  LINE 09 COLUMN 68 VALUE "  ".
               05  LINE 10 COLUMN 68 VALUE "  ".
               05  LINE 11 COLUMN 68 VALUE "  ".
               05  LINE 12 COLUMN 68 VALUE "  ".
               05  LINE 13 COLUMN 68 VALUE "  ".
               05  LINE 14 COLUMN 68 VALUE "  ".
               05  LINE 15 COLUMN 68 VALUE "  ".
               05  LINE 16 COLUMN 68 VALUE "  ".
               05  LINE 17 COLUMN 68 VALUE "  ".
               05  LINE 18 COLUMN 68 VALUE "  ".
               05  LINE 19 COLUMN 68 VALUE "  ".
               05  LINE 20 COLUMN 68 VALUE "  ".
               05  LINE 21 COLUMN 16 PIC X(54) FROM SPACES.

           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 08 COLUMN 18 PIC X(45) FROM ALL "ß".
               05  LINE 10 COLUMN 18 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 09 COLUMN 18 PIC X(45) FROM
                   "  PERÖODO CONCLUÖDO ÄÄ          ·".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 09 COLUMN 63 PIC X(01) FROM     " ".
               05  LINE 10 COLUMN 63 PIC X(01) FROM     " ".
               05  LINE 10 COLUMN 19 PIC X(44) FROM ALL "ß".

           03  BCK-CLR 01 FOR-CLR 07.
               05  LINE 11 COLUMN 18 PIC X(45) FROM ALL "ß".
               05  LINE 19 COLUMN 18 PIC X(01) FROM     "Ü".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 12 COLUMN 18 PIC X(45) FROM
                   "  CUSTO DO ESTOQUE ÄÄÄ                 (+)".
               05  LINE 13 COLUMN 18 PIC X(45) FROM
                   "  CONTAS · RECEBER ÄÄÄ                 (+)".
               05  LINE 14 COLUMN 18 PIC X(45) FROM
                   "  SALDOS DE CAIXA ÄÄÄÄ                 (+)".
               05  LINE 15 COLUMN 18 PIC X(45) FROM
                   "  CONTAS · PAGAR ÄÄÄÄÄ                 (-)".
               05  LINE 16 COLUMN 18 PIC X(45) FROM
                   "  SALDOS BANCµRIOS ÄÄÄ                 (+)".
               05  LINE 17 COLUMN 18 PIC X(45) FROM
                   "  ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
               05  LINE 18 COLUMN 18 PIC X(45) FROM
                   "  CAPITAL DE GIRO ÄÄÄÄ                 (=)".
           03  BCK-CLR 00 FOR-CLR 01.
               05  LINE 12 COLUMN 63 VALUE " ".
               05  LINE 13 COLUMN 63 VALUE " ".
               05  LINE 14 COLUMN 63 VALUE " ".
               05  LINE 15 COLUMN 63 VALUE " ".
               05  LINE 16 COLUMN 63 VALUE " ".
               05  LINE 17 COLUMN 63 VALUE " ".
               05  LINE 18 COLUMN 63 VALUE " ".
               05  LINE 19 COLUMN 63 VALUE " ".
               05  LINE 19 COLUMN 19 PIC X(44) FROM ALL "ß".

       01  ENTR-01  BCK-CLR 02 FOR-CLR 15.
           03  LINE 09 COLUMN 42 PIC 99/99/99 FROM DTI-AUX.
           03  LINE 09 COLUMN 53 PIC 99/99/99 FROM DTF-AUX.

       01  ENTR-02  BCK-CLR 01 FOR-CLR 15.
           03 VL1-E02 LINE 12 COLUMN 42 PIC ---.---.--9,99 FROM ACM-VAL.
           03 VL2-E02 LINE 13 COLUMN 42 PIC ---.---.--9,99 FROM ACM-VAL.
           03 VL3-E02 LINE 14 COLUMN 42 PIC ---.---.--9,99 FROM ACM-VAL.
           03 VL4-E02 LINE 15 COLUMN 42 PIC ---.---.--9,99 FROM ACM-VAL.
           03 VL5-E02 LINE 16 COLUMN 42 PIC ---.---.--9,99 FROM ACM-VAL.
           03 VL6-E02 LINE 18 COLUMN 42 PIC ---.---.--9,99 FROM ACM-TOT.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           OPEN     INPUT    CADCTR   READ     CADCTR.

       ROT-DATA-00.
           MOVE     DTI-CTR  TO  DTA-SYS  DTA-LIM
           MOVE     DIA-SYS  TO  DDI-AUX
           MOVE     MES-SYS  TO  MMI-AUX
           MOVE     ANO-SYS  TO  AAI-AUX
           MOVE     DTD-CTR  TO  DTA-SYS
           MOVE     DIA-SYS  TO  DDF-AUX
           MOVE     MES-SYS  TO  MMF-AUX
           MOVE     ANO-SYS  TO  AAF-AUX
           MOVE     31       TO  DIA-LIM
           DISPLAY  ENTR-01
           OPEN     INPUT        CADBCO   CADEST
                    CADLBC       CADLDP   CADSDO
           MOVE     1        TO  PRX-SDO
           MOVE     SPACES   TO  CHV-EST
                                 CHV-LDP  SUF-SDO
           MOVE     ZEROS    TO  CHV-BCO
                                 ACM-VAL  ACM-TOT.
       ROT-ESTQ-00.
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-ESTQ-10.
           READ     CADEST  NEXT
           COMPUTE  ACM-VAL   =  ACM-VAL
                              +  SDO-EST  *   CST-EST
           MOVE     ACM-VAL  TO  ACM-TOT
           DISPLAY  VL1-E02
           DISPLAY  VL6-E02      GO       ROT-ESTQ-00.

       ROT-ESTQ-10.
           DISPLAY  VL1-E02      VL6-E02
           MOVE     ZEROS    TO  ACM-VAL.

       ROT-CRED-00.
           START    CADSDO       KEY  >   CHV-SDO
           IF  FST-SDO   =  "23" GO       ROT-DESP-00.
           READ     CADSDO  NEXT
           IF  SUF-SDO  >    "H" GO       ROT-CRED-10.
           COMPUTE  ACM-VAL   =  ACM-VAL  +   SDI-SDO
                 +  ENT-SDO   -  SAI-SDO  +   COR-SDO
           COMPUTE  ACM-TOT   =  ACM-TOT  +   SDI-SDO
                 +  ENT-SDO   -  SAI-SDO  +   COR-SDO
           DISPLAY  VL2-E02
           DISPLAY  VL6-E02      GO       ROT-CRED-00.

       ROT-CRED-10.
           COMPUTE  ACM-VAL   =  SDI-SDO  +   ENT-SDO
                              -  SAI-SDO  +   COR-SDO
           ADD      ACM-VAL  TO  ACM-TOT
           DISPLAY  VL3-E02      VL6-E02
           MOVE     ZEROS    TO  ACM-VAL.

       ROT-DESP-00.
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       ROT-DESP-10.
           READ     CADLDP  NEXT
           IF  FLG-LDP  NOT   =  "P"  GO  ROT-DESP-00.
           ADD      VAL-LDP  TO  ACM-VAL
           COMPUTE  ACM-TOT   =  ACM-TOT  -   VAL-LDP
           DISPLAY  VL4-E02
           DISPLAY  VL6-E02      GO       ROT-DESP-00.

       ROT-DESP-10.
           DISPLAY  VL4-E02      VL6-E02
           MOVE     ZEROS    TO  ACM-VAL.

       ROT-BANK-00.
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" PERFORM  ROT-BANK-20
                                 GO       ROT-EXIT-00.
           READ     CADBCO  NEXT
           ADD      SDO-BCO  TO  ACM-VAL
           ADD      SDO-BCO  TO  ACM-TOT
           DISPLAY  VL5-E02      VL6-E02
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC.

       ROT-BANK-10.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-BANK-00.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO  OR  DTA-LBC
               >    DTA-LIM      GO       ROT-BANK-00.
           IF  COD-LBC   <    4  ADD      VAL-LBC  TO  ACM-VAL
                                                       ACM-TOT
           ELSE
                                 COMPUTE  ACM-VAL  =
                                          ACM-VAL  -   VAL-LBC
                                 COMPUTE  ACM-TOT  =
                                          ACM-TOT  -   VAL-LBC.
           DISPLAY  VL5-E02
           DISPLAY  VL6-E02  GO       ROT-BANK-10.

       ROT-BANK-20.
           DISPLAY  VL5-E02      VL6-E02.

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
           CLOSE    CADBCO   CADCTR   CADEST
                    CADLBC   CADLDP   CADSDO   GOBACK.
