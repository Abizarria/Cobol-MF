       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1531.
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

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADFAT        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-FAT
                       FILE          STATUS  IS  FST-FAT.

           SELECT      CADNTA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTA
                       FILE          STATUS  IS  FST-NTA.

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
       COPY  CADCTR.CPY.
       COPY  CADFAT.CPY.
       COPY  CADNTA.CPY.
       COPY  CADTMP.CPY.

       SD  CADSRT.

       01  REG-SRT.
           03  CHV-SRT.
               05  CLI-SRT      PIC  9(05).
               05  DTA-SRT      PIC  9(06).
               05  NUM-SRT      PIC  9(06).
               05  LCT-SRT      PIC  9(03).
           03  DES-SRT          PIC  X(25).
           03  VAL-SRT          PIC  9(06)V99.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CLI          PIC  X(02).
           03  FST-CTR          PIC  X(02).
           03  FST-FAT          PIC  X(02).
           03  FST-NTA          PIC  X(02).
           03  FST-SRT          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  IND1             PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  ACM-PSQ          PIC  9(02).
           03  ACM-QTD          PIC  9(04).
           03  ACM-VAL          PIC  9(06)V99.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  PCA-AUX.
               05  PC1-AUX      PIC  X(03).
               05  PC2-AUX      PIC  X(04).

           03  NOM-PSQ          PIC  X(20).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  20.

       01  CAB-001.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(68)  VALUE  "FATURA DE SERVICOS".
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC  X(80)  VALUE
           "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".

       01  CAB-004.
           03  FILLER           PIC  X(17)  VALUE  "³ RAZAO SOCIAL :".
           03  NOM-CB4          PIC  X(40).
           03  FILLER           PIC  X(14)  VALUE  "   ³ FATURA :".
           03  FAT-CB4          PIC  999.999.
           03  FILLER           PIC  X(02)  VALUE  " ³".

       01  CAB-005              PIC  X(80)  VALUE
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÁÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-006              PIC  X(80)  VALUE
           "³    DATA    ³  N§ NOTA  ³    PLACA   ³  DESCRICAO DA NOTA
      -    "   ³ VALOR DA NOTA ³".

       01  CAB-007              PIC  X(80)  VALUE
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-008              PIC  X(80)  VALUE
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-009              PIC  X(80)  VALUE
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-010              PIC  X(80)  VALUE
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-011              PIC  X(80)  VALUE
           "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  CAB-012              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  FILLER           PIC  X(03)  VALUE  "³".
           03  DIA-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DT1          PIC  9(02).
           03  FILLER           PIC  X(05)  VALUE  "  ³".
           03  NTA-DT1          PIC  999.999.
           03  FILLER           PIC  X(05)  VALUE  "  ³".
           03  PC1-DT1          PIC  X(03).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  PC2-DT1          PIC  X(04).
           03  FILLER           PIC  X(05)  VALUE  "  ³".
           03  DES-DT1          PIC  X(20).
           03  FILLER           PIC  X(06)  VALUE  "  ³".
           03  VAL-DT1          PIC  ZZZ.ZZ9,99.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  TOT-001.
           03  FILLER           PIC  X(18)  VALUE  "³  TOTAL NOTAS .:".
           03  QTD-TT1          PIC  Z.ZZ9.
           03  FILLER           PIC  X(44)  VALUE
               "  ³  VALOR TOTAL DA FATURA EM R$".
           03  VAL-TT1          PIC  ZZZ.ZZ9,99.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  TOT-002.
           03  FILLER           PIC  X(25)  VALUE  "³".
           03  FILLER           PIC  X(55)  VALUE  "³     **  A RELACAO
      -        "CONTINUA NA PROXIMA FOLHA  **     ³".

       01  MSG-001.
           03  FILLER           PIC  X(20)  VALUE  "³".
           03  DES-MG1          PIC  X(59).
           03  FILLER           PIC  X(01)  VALUE  "³".

       01  MSG-002.
           03  FILLER           PIC  X(20)  VALUE  "³".
           03  DES-MG2          PIC  X(59).
           03  FILLER           PIC  X(01)  VALUE  "³".

       01  MSG-003.
           03  FILLER           PIC  X(20)  VALUE  "³".
           03  DES-MG3          PIC  X(59).
           03  FILLER           PIC  X(01)  VALUE  "³".

       01  ROD-001              PIC  X(80)  VALUE
           "³ RECEBIDO EM :                     /   /     ASSINATURA:
      -    "                   ³".

       01  ROD-002              PIC  X(80)  VALUE
           "³              ________________, ___________              __
      -    "__________________ ³".

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  45.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1531".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-DTA.
           03  DTA-DTA.
               05  ANO-DTA      PIC  9(02).
               05  MES-DTA      PIC  9(02).
               05  DIA-DTA      PIC  9(02).
           03  LIN-DTA          PIC  9(02)  VALUE  10.
           03  COL-DTA          PIC  9(02)  VALUE  60.
           03  BCK-DTA          PIC  9(01)  VALUE  2.
           03  FOR-DTA          PIC  9(01)  VALUE  7.
           03  FLG-DTA          PIC  9(01).

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

       01  PRM-IMP.
           03  ARQ-IMP          PIC  9(06).
           03  PAG-IMP          PIC  9(03).
           03  FLG-IMP          PIC  9(01)  VALUE  1.
           03  LIN-IMP          PIC  9(02)  VALUE  18.
           03  CL1-IMP          PIC  9(02)  VALUE  53.
           03  CL2-IMP          PIC  9(02)  VALUE  61.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 05 COLUMN 05 VALUE " Administra‡Æo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 06 COLUMN 07 VALUE " Controle de Clientes ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 09 VALUE " Faturamento Empresas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 05 COLUMN 27 VALUE "ß".
               05  LINE 06 COLUMN 29 VALUE "ß".
               05  LINE 07 COLUMN 31 VALUE "ß".
               05  LINE 06 COLUMN 06 VALUE "Ü".
               05  LINE 07 COLUMN 08 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 08 COLUMN 05 VALUE " ÉÍÍ
      -            " ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ» ".
               05  LINE 09 COLUMN 05 VALUE " º
      -            "                                        º ".
               05  LINE 10 COLUMN 05 VALUE " º
      -            "                                        º ".
               05  LINE 11 COLUMN 05 VALUE " º
      -            "                                        º ".
               05  LINE 12 COLUMN 05 VALUE " º
      -            "                                        º ".
               05  LINE 13 COLUMN 05 VALUE " º
      -            "                                        º ".
               05  LINE 14 COLUMN 05 VALUE " º
      -            "                                        º ".
               05  LINE 15 COLUMN 05 VALUE " º
      -            "                                        º ".
               05  LINE 16 COLUMN 05 VALUE " º
      -            "                                        º ".
               05  LINE 17 COLUMN 05 VALUE " º
      -            "                                        º ".
               05  LINE 18 COLUMN 05 VALUE " º
      -            "                                        º ".
               05  LINE 19 COLUMN 05 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -            "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 08 COLUMN 11 VALUE "  Fechamento Empresa  ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 09 COLUMN 75 VALUE  "  ".
               05  LINE 10 COLUMN 75 VALUE  "  ".
               05  LINE 11 COLUMN 75 VALUE  "  ".
               05  LINE 12 COLUMN 75 VALUE  "  ".
               05  LINE 13 COLUMN 75 VALUE  "  ".
               05  LINE 14 COLUMN 75 VALUE  "  ".
               05  LINE 15 COLUMN 75 VALUE  "  ".
               05  LINE 16 COLUMN 75 VALUE  "  ".
               05  LINE 17 COLUMN 75 VALUE  "  ".
               05  LINE 18 COLUMN 75 VALUE  "  ".
               05  LINE 19 COLUMN 75 VALUE  "  ".
               05  LINE 20 COLUMN 07 PIC X(70) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 09 PIC X(61) FROM ALL "ß".
               05  LINE 15 COLUMN 09 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 10 COLUMN 09 PIC X(42) FROM
                   "  C¢digo :       -".
               05  LINE 10 COLUMN 51 PIC X(19) FROM "Limite :".
               05  LINE 11 COLUMN 09 PIC X(61) FROM SPACES.
               05  LINE 12 COLUMN 09 PIC X(61) FROM
                   "  Mensagens .....:".
               05  LINE 13 COLUMN 09 PIC X(61) FROM SPACES.
               05  LINE 14 COLUMN 09 PIC X(61) FROM SPACES.
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 10 COLUMN 70 PIC X(01) FROM     " ".
               05  LINE 11 COLUMN 70 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 70 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 70 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 70 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 70 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 10 PIC X(60) FROM ALL "ß".

       01  TELA-03.
           03  BCK-CLR 03 FOR-CLR 07.
               05  LINE 16 COLUMN 09 PIC X(23) FROM ALL "ß".
               05  LINE 18 COLUMN 09 PIC X(01) FROM ALL "Ü".
           03  BCK-CLR 03 FOR-CLR 15.
               05  LINE 17 COLUMN 09 PIC X(23) FROM
                   "   EmissÆo de Fatura   ".
           03  BCK-CLR 03 FOR-CLR 00.
               05  LINE 17 COLUMN 32 PIC X(01) FROM     "Û".
               05  LINE 18 COLUMN 32 PIC X(01) FROM     "Û".
               05  LINE 18 COLUMN 10 PIC X(22) FROM ALL "Ü".
           03  BCK-CLR 01 FOR-CLR 07.
               05  LINE 16 COLUMN 40 PIC X(30) FROM ALL "ß".
               05  LINE 18 COLUMN 40 PIC X(01) FROM ALL "Ü".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 17 COLUMN 40 PIC X(30) FROM
                   "   P ginas Montadas Ä".
               05  PAG-T02 LINE 17 COLUMN 64 PIC ZZ9 FROM PAG-CB1.
           03  BCK-CLR 01 FOR-CLR 00.
               05  LINE 17 COLUMN 70 PIC X(01) FROM     "Û".
               05  LINE 18 COLUMN 70 PIC X(01) FROM     "Û".
               05  LINE 18 COLUMN 41 PIC X(29) FROM ALL "Ü".

       01  TELA-04.
           03  BCK-CLR 04 FOR-CLR 07.
               05  LINE 16 COLUMN 09 PIC X(61) FROM ALL "ß".
               05  LINE 18 COLUMN 09 PIC X(01) FROM     "Ü".
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 17 COLUMN 09 PIC X(61) FROM
                   "             DESEJA CONFIRMAR O FATURAMENTO ?  [ ]".
           03  BCK-CLR 00 FOR-CLR 04.
               05  LINE 17 COLUMN 70 PIC X(01) FROM     " ".
               05  LINE 18 COLUMN 70 PIC X(01) FROM     " ".
               05  LINE 18 COLUMN 10 PIC X(60) FROM ALL "ß".

       01  TELA-05.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 16 COLUMN 09 PIC X(61) FROM ALL "ß".
               05  LINE 18 COLUMN 09 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 17 COLUMN 09 PIC X(61) FROM
                   "             AGUARDE, CONCLUINDO O FATURAMENTO ...".
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 17 COLUMN 70 PIC X(01) FROM     " ".
               05  LINE 18 COLUMN 70 PIC X(01) FROM     " ".
               05  LINE 18 COLUMN 10 PIC X(60) FROM ALL "ß".

       01  ENTR-01     AUTO.
           03  CHV-E02 LINE 10 COLUMN 20 PIC ZZZZ9 USING CHV-CLI.
           03  PSQ-E02 LINE 10 COLUMN 28 PIC X(20) USING NOM-PSQ.
           03  MG1-E02 LINE 12 COLUMN 28 PIC X(40) USING DES-MG1.
           03  MG2-E02 LINE 13 COLUMN 28 PIC X(40) USING DES-MG2.
           03  MG3-E02 LINE 14 COLUMN 28 PIC X(40) USING DES-MG3.

       01  ENTR-02  BCK-CLR 02 FOR-CLR 15.
           03  CHV-T02 LINE 10 COLUMN 20 PIC 9(05) FROM CHV-CLI.
           03  PSQ-T02 LINE 10 COLUMN 28 PIC X(20) FROM NOM-PSQ.
           03  NOM-T02 LINE 10 COLUMN 28 PIC X(20) FROM NOM-CLI.
           03  MG1-T02 LINE 12 COLUMN 28 PIC X(40) FROM DES-MG1.
           03  MG2-T02 LINE 13 COLUMN 28 PIC X(40) FROM DES-MG2.
           03  MG3-T02 LINE 14 COLUMN 28 PIC X(40) FROM DES-MG3.

       01  ENTR-03  BCK-CLR 04 FOR-CLR 15
                    LINE 17 COLUMN 57 PIC X(01) USING RSP-OPC AUTO.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           ACCEPT   ARQ-IMP   FROM    TIME
           OPEN     OUTPUT   CADTMP
                    INPUT    CADCLI   CADNTA.

       ROT-CODE-00.
           DISPLAY  TELA-02      MOVE  0  TO  CHV-CLI
           ACCEPT   CHV-E02      DISPLAY  CHV-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-00.
           ACCEPT   DTA-DTA FROM DATE
           IF  CHV-CLI   =    0  GO       ROT-PESQ-00.
           READ     CADCLI       MOVE 21  TO  IND-MSG
           IF  FST-CLI   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-00.
       ROT-EXBE-00.
           DISPLAY  CHV-T02      NOM-T02.

       ROT-EXBE-10.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-EXBE-10.
           IF  TCL-MSG   =   05  GO       ROT-EXBE-20.
           IF  TCL-MSG   =   06  GO       ROT-EXBE-30.
           IF  TCL-MSG   >   00  GO       ROT-EXBE-10.
           IF  HEX-MSG   =   27  GO       ROT-CODE-00.
           GO       ROT-ENTR-00.

       ROT-EXBE-20.
           START    CADCLI       KEY  <   CHV-CLI
           IF  FST-CLI   =  "23" GO       ROT-EXBE-10.
           READ     CADCLI       PREVIOUS
           GO       ROT-EXBE-00.

       ROT-EXBE-30.
           START    CADCLI       KEY  >   CHV-CLI
           IF  FST-CLI   =  "23" GO       ROT-EXBE-10.
           READ     CADCLI  NEXT GO       ROT-EXBE-00.

       ROT-PESQ-00.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  CHV-CLI
           DISPLAY  CHV-T02.

       ROT-PESQ-10.
           ACCEPT   PSQ-E02      DISPLAY  PSQ-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-10.
           IF  NOM-PSQ  =        SPACES
                                 GO       ROT-PESQ-10.
           MOVE     21  TO  IND1 PERFORM  ROT-PESQ-50.

       ROT-PESQ-20.
           COMPUTE  IND1      =  IND1  -   1
           IF  LET-PSQ (IND1) =  SPACES
                                 GO       ROT-PESQ-20.
       ROT-PESQ-30.
           MOVE     22       TO  IND-MSG
           START    CADCLI       KEY  >   CHV-CLI
           IF  FST-CLI   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           READ     CADCLI  NEXT DISPLAY  CHV-T02
           INSPECT  NOM-CLI      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-30.
           PERFORM  ROT-EXBE-00  GO       ROT-PESQ-60.

       ROT-PESQ-40.
           MOVE     22       TO  IND-MSG
           START    CADCLI       KEY  <   CHV-CLI
           IF  FST-CLI   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           READ     CADCLI       PREVIOUS
           DISPLAY  CHV-T02
           INSPECT  NOM-CLI      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-40.
           PERFORM  ROT-EXBE-00  GO       ROT-PESQ-60.

       ROT-PESQ-50.
           MOVE     0        TO  ACM-PSQ
           DISPLAY  CHV-T02      PSQ-T02.

       ROT-PESQ-60.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-60.
           IF  TCL-MSG   =   05  PERFORM  ROT-PESQ-50
                                 GO       ROT-PESQ-40.
           IF  TCL-MSG   =   06  PERFORM  ROT-PESQ-50
                                 GO       ROT-PESQ-30.
           IF  TCL-MSG   >   00  GO       ROT-PESQ-60.
           IF  HEX-MSG   =   13  GO       ROT-ENTR-00.
           DISPLAY  TELA-02      GO       ROT-PESQ-00.

       ROT-ENTR-00.
           PERFORM  ROT-DATA-00  MOVE 31  TO  IND-MSG
           IF  FLG-DTA   =    0  GO       ROT-CODE-00.
           MOVE     SPACES   TO  CHV-NTA  DES-MG1
                                 DES-MG2  DES-MG3
           MOVE     CHV-CLI  TO  CLI-NTA
           START    CADNTA       KEY  >   CHV-NTA
           IF  FST-NTA   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-00.
           READ     CADNTA  NEXT
           IF  CLI-NTA  NOT   =  CHV-CLI  OR  DTA-NTA
               >    DTA-DTA      PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-00.
       ROT-ENTR-10.
           ACCEPT   MG1-E02      DISPLAY  MG1-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =  "01" GO       ROT-ENTR-00.
           IF  TECLADO   =  "02" PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
       ROT-ENTR-20.
           ACCEPT   MG2-E02      DISPLAY  MG2-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =  "01" GO       ROT-ENTR-10.
           IF  TECLADO   =  "02" PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-20.
       ROT-ENTR-30.
           ACCEPT   MG3-E02      DISPLAY  MG3-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =  "01" GO       ROT-ENTR-20.
           IF  TECLADO   =  "02" PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-30.
           OPEN     INPUT        CADCTR
           READ     CADCTR       PERFORM  ROT-NOME-00
           COMPUTE  FAT-CB4   =  FAT-CTR  +   1
           MOVE     NOM-CLI  TO  NOM-CB4
           CLOSE    CADCTR       PERFORM  ROT-GTLA-00
           DISPLAY  TELA-03
           MOVE     ZEROS    TO  PAG-CB1  CHV-TMP
                                 ACM-QTD  ACM-VAL.
       ROT-IMPR-00.
           DISPLAY  PAG-T02
           MOVE     1        TO  LIN-TMP
           ADD      1        TO  PAG-CB1  CHV-TMP
           MOVE     PAG-CB1  TO  PAG-TMP
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
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-006  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-007  TO  DET-TMP
           WRITE    REG-TMP.

       ROT-IMPR-10.
           MOVE     DTA-NTA  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     ANO-AUX  TO  ANO-DT1
           MOVE     NUM-NTA  TO  NTA-DT1
           MOVE     PCA-NTA  TO  PCA-AUX
           MOVE     PC1-AUX  TO  PC1-DT1
           MOVE     PC2-AUX  TO  PC2-DT1
           MOVE     DES-NTA  TO  DES-DT1
           MOVE     VAL-NTA  TO  VAL-DT1
           ADD      VAL-NTA  TO  ACM-VAL
           ADD      1        TO  ACM-QTD
                                 CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           START    CADNTA       KEY  >   CHV-NTA
           IF  FST-NTA   =  "23" GO       ROT-IMPR-30.
           READ     CADNTA  NEXT
           IF  CLI-NTA  NOT   =  CHV-CLI  OR  DTA-NTA
               >    DTA-DTA      GO       ROT-IMPR-30.
           IF  LIN-TMP   <   50  GO       ROT-IMPR-10.

       ROT-IMPR-20.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-008  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     TOT-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-009  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     MSG-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     MSG-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     MSG-003  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-010  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ROD-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ROD-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-011  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     92       TO  LIN-TMP
           MOVE     CAB-012  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     93       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-00.

       ROT-IMPR-30.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-008  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     ACM-QTD  TO  QTD-TT1
           MOVE     ACM-VAL  TO  VAL-TT1
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     TOT-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-009  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     MSG-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     MSG-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     MSG-003  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-010  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ROD-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     ROD-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-011  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     PAG-CB1  TO  PAG-IMP
           DISPLAY  PAG-T02      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE     FILE       CADTMP
           OPEN      OUTPUT      CADTMP
           PERFORM  ROT-RTLA-00  DISPLAY  TELA-04.

       ROT-CONF-00.
           MOVE     SPACES   TO  RSP-OPC  CHV-NTA
           ACCEPT   ENTR-03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CONF-00.
           IF  RSP-OPC  NOT   =  "S"
                                 GO       ROT-CONF-00.
           DISPLAY  TELA-05      PERFORM  ROT-CONF-10
           PERFORM  ROT-RTLA-00  GO       ROT-CODE-00.

       ROT-CONF-10.
           SORT     CADSRT   ON  ASCENDING  KEY  CHV-SRT
                    INPUT        PROCEDURE  IS   ROT-SORT-00
                    OUTPUT       PROCEDURE  IS   ROT-SORT-30.

       ROT-SORT-00  SECTION.

       ROT-SORT-10.
           START    CADNTA       KEY  >   CHV-NTA
           IF  FST-NTA   =  "23" GO       ROT-SORT-20.
           READ     CADNTA  NEXT
           IF  DTA-NTA  NOT   >  DTA-DTA  AND CLI-NTA
               =    CHV-CLI      GO       ROT-SORT-10.
           MOVE     REG-NTA  TO  REG-SRT
           RELEASE  REG-SRT      GO       ROT-SORT-10.

       ROT-SORT-20.
           CLOSE    CADNTA       DELETE  FILE  CADNTA
           OPEN     I-O          CADCTR CADFAT CADNTA.

       ROT-SORT-30  SECTION.

       ROT-SORT-40.
           RETURN   CADSRT
           IF  FST-SRT   =  "10" GO       ROT-SORT-50.
           MOVE     REG-SRT  TO  REG-NTA
           WRITE    REG-NTA      GO       ROT-SORT-40.

       ROT-SORT-50.
           READ     CADCTR       MOVE 20  TO  IND-MSG
           IF  FST-CTR   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-SORT-50.
           ADD      1        TO  FAT-CTR
           MOVE     CHV-CLI  TO  CLI-FAT
           MOVE     DTA-DTA  TO  DTA-FAT
           MOVE     FAT-CTR  TO  NUM-FAT
           MOVE     ACM-VAL  TO  VAL-FAT
           REWRITE  REG-CTR      WRITE    REG-FAT
           CLOSE    CADCTR       CADFAT   CADNTA
           OPEN     INPUT        CADNTA.

       ROT-SORT-60  SECTION.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-DATA-00.
           CALL    "DYDATA"  USING  PRM-DTA
                                    PRM-HLP.
       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-NOME-00.
           ACCEPT   DTA-SYS  FROM   DATE
           MOVE     DIA-SYS   TO    DIA-CAB
           MOVE     MES-SYS   TO    MES-CAB
           MOVE     ANO-SYS   TO    ANO-CAB
           CALL    "DYNOME"  USING  PRM-NOM   PRM-CAB
           MOVE     CLI-NOM   TO    CLI-CB1
           MOVE     PRM-CAB   TO    DTA-CB2.

       ROT-EXIT-00.
           CLOSE    CADCLI  CADNTA  CADTMP
           DELETE   FILE    CADTMP  GOBACK.
