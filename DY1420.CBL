       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1420.
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
                       FILE          STATUS  IS  FST-BCO.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADBCO.CPY.
       COPY  CADCTR.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-BCO          PIC  X(02).
           03  FST-CTR          PIC  X(02).

           03  IND1             PIC  9(03).
           03  TECLADO          PIC  9(02).
           03  ACM-PSQ          PIC  9(02).

           03  NOM-PSQ          PIC  X(20).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  20.

           03  TAB-MOV.
               05  FILLER       PIC  X(18)  VALUE  "DEPเSITOS DIVERSOS".
               05  FILLER       PIC  X(18)  VALUE  "GANHOS COM CAPITAL".
               05  FILLER       PIC  X(18)  VALUE  "CHEQUES DEVOLVIDOS".
               05  FILLER       PIC  X(18)  VALUE  "PAGAMENTOS ท VISTA".
               05  FILLER       PIC  X(18)  VALUE  "PAGAMENTOS ท PRAZO".
               05  FILLER       PIC  X(18)  VALUE  "TRANSF. DE VALORES".
               05  FILLER       PIC  X(18)  VALUE  "REEMBOLSO DESPESAS".
           03  RED-MOV          REDEFINES   TAB-MOV.
               05  MOV-TAB      PIC  X(18)  OCCURS  07.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  33.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1420".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-DTA.
           03  DTA-DTA.
               05  ANO-DTA      PIC  9(02).
               05  MES-DTA      PIC  9(02).
               05  DIA-DTA      PIC  9(02).
           03  LIN-DTA          PIC  9(02)  VALUE  10.
           03  COL-DTA          PIC  9(02)  VALUE  58.
           03  BCK-DTA          PIC  9(01)  VALUE  2.
           03  FOR-DTA          PIC  9(01)  VALUE  7.
           03  FLG-DTA          PIC  9(01).

       01  PRM-PSQ.
           03  OPC-PSQ          PIC  9(01)  VALUE  3.
           03  IND-PSQ          PIC  X(01).
           03  LIN-PSQ          PIC  9(02)  VALUE  14.
           03  COL-PSQ          PIC  9(02)  VALUE  50.
           03  SB1-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB2-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB3-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB4-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB5-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB6-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB7-PSQ          PIC  X(26)  VALUE  SPACES.

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

       01  PRM-LCT.
           03  BCO-LCT          PIC  9(03).
           03  DTA-LCT          PIC  9(06).

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 06 COLUMN 10 VALUE " Administraฦo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 07 COLUMN 12 VALUE " Controle C. Banc ria ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 06 COLUMN 32 VALUE "฿".
               05  LINE 07 COLUMN 34 VALUE "฿".
               05  LINE 07 COLUMN 11 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 08 COLUMN 10 VALUE " ษ                        
      -            "ออออออออออออออออออออออออออออออออป ".
               05  LINE 09 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 10 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 11 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 12 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 13 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 14 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 15 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 16 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 17 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 18 COLUMN 10 VALUE " บ
      -            "                                บ ".
               05  LINE 19 COLUMN 10 VALUE " ศออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 08 COLUMN 14 VALUE " Lanamento Movimento ".
           03  BCK-CLR 00.
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
               05  LINE 20 COLUMN 12 PIC X(62) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 13 PIC X(55) FROM ALL "฿".
               05  LINE 11 COLUMN 13 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 10 COLUMN 13 PIC X(55) FROM
                   "  Banco :     -                       Data :".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 10 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 11 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 11 COLUMN 14 PIC X(54) FROM ALL "฿".

       01  E01-TL2     AUTO.
           03  CHV-E01 LINE 10 COLUMN 23 PIC ZZ9   USING CHV-BCO.
           03  PSQ-E01 LINE 10 COLUMN 29 PIC X(20) USING NOM-PSQ.

       01  E02-TL2  BCK-CLR 02 FOR-CLR 15.
           03  CHV-E02 LINE 10 COLUMN 23 PIC 9(03) FROM  CHV-BCO.
           03  PSQ-E02 LINE 10 COLUMN 29 PIC X(20) FROM  NOM-PSQ.
           03  DES-E02 LINE 10 COLUMN 29 PIC X(20) FROM  DES-BCO.

       01  TELA-03.
           03  BCK-CLR 01 FOR-CLR 07.
               05  LINE 12 COLUMN 13 PIC X(55) FROM ALL "฿".
               05  LINE 18 COLUMN 13 PIC X(01) FROM     "Ü".
           03  BCK-CLR 01 FOR-CLR 11.
               05  LINE 13 COLUMN 13 PIC X(55) FROM
                   "  Movimento ..:                       Nฃmero:".
               05  LINE 14 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 15 COLUMN 13 PIC X(55) FROM "  Histขrico ..:".
               05  LINE 16 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 17 COLUMN 13 PIC X(55) FROM "  Valor ......:".
           03  BCK-CLR 00 FOR-CLR 01.
               05  LINE 13 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 18 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 18 COLUMN 14 PIC X(54) FROM ALL "฿".

       01  E03-TL3 LINE 13 COLUMN 29 PIC X(18) FROM MOV-TAB (IND1)
                   BCK-CLR 01 FOR-CLR 15.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           PERFORM  ROT-SPOL-00
           IF  LCK-SPL    =   1  GOBACK.
           OPEN     INPUT        CADBCO   CADCTR
           READ     CADCTR       DISPLAY  TELA-01.

       ROT-CODE-00.
           DISPLAY  TELA-02      MOVE  0  TO  CHV-BCO.

       ROT-CODE-10.
           ACCEPT   CHV-E01      DISPLAY  CHV-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           IF  CHV-BCO   <   01  GO       ROT-NOME-00.
           READ     CADBCO       MOVE 21  TO  IND-MSG
           IF  FST-BCO   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-00.
       ROT-CODE-20.
           ACCEPT   DTA-DTA FROM DATE
           DISPLAY  CHV-E02      DES-E02.

       ROT-CODE-30.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-30.
           IF  TCL-MSG   =   05  GO       ROT-CODE-40.
           IF  TCL-MSG   =   06  GO       ROT-CODE-50.
           IF  TCL-MSG   >   00  GO       ROT-CODE-30.
           IF  HEX-MSG   =   13  GO       ROT-LANC-00.
           GO       ROT-CODE-00.

       ROT-CODE-40.
           START    CADBCO       KEY  <   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-CODE-30.
           READ     CADBCO  PREVIOUS  GO  ROT-CODE-20.

       ROT-CODE-50.
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-CODE-30.
           READ     CADBCO  NEXT GO       ROT-CODE-20.

       ROT-NOME-00.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  CHV-BCO
           DISPLAY  CHV-E02.

       ROT-NOME-10.
           ACCEPT   PSQ-E01      DISPLAY  PSQ-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-NOME-10.
           IF  NOM-PSQ   =       SPACES
                                 GO       ROT-NOME-10.
           MOVE     21  TO  IND1 PERFORM  ROT-NOME-50.

       ROT-NOME-20.
           SUBTRACT  1  FROM     IND1
           IF  LET-PSQ (IND1) =  " "  GO  ROT-NOME-20.

       ROT-NOME-30.
           MOVE     22       TO  IND-MSG
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           READ     CADBCO  NEXT DISPLAY  CHV-E02
           INSPECT  DES-BCO      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-NOME-30.
           PERFORM  ROT-CODE-20  GO       ROT-NOME-60.

       ROT-NOME-40.
           MOVE     22       TO  IND-MSG
           START    CADBCO       KEY  <   CHV-BCO
           IF  FST-BCO   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           READ     CADBCO       PREVIOUS
           DISPLAY  CHV-E02
           INSPECT  DES-BCO      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-NOME-40.
           PERFORM  ROT-CODE-20  GO       ROT-NOME-60.

       ROT-NOME-50.
           MOVE     ZEROS    TO  ACM-PSQ
           DISPLAY  CHV-E02      PSQ-E02.

       ROT-NOME-60.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-NOME-60.
           IF  TCL-MSG   =   05  PERFORM  ROT-NOME-50
                                 GO       ROT-NOME-40.
           IF  TCL-MSG   =   06  PERFORM  ROT-NOME-50
                                 GO       ROT-NOME-30.
           IF  TCL-MSG   >   00  GO       ROT-NOME-60.
           IF  HEX-MSG   =   27  PERFORM  ROT-NOME-50
                                 GO       ROT-NOME-00.
       ROT-LANC-00.
           PERFORM  ROT-DATA-00  MOVE 18  TO  IND-MSG
           IF  FLG-DTA   =    0  GO       ROT-CODE-00.
           IF  DTA-DTA   <       DTI-CTR
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-LANC-00.
           PERFORM  ROT-GTLA-00  MOVE  1  TO  IND-PSQ.

       ROT-LANC-10.
           DISPLAY  TELA-03      PERFORM  ROT-PESQ-00
           IF  IND-PSQ   =  " "  PERFORM  ROT-RTLA-00
                                 GO       ROT-LANC-00.
           MOVE     CHV-BCO  TO  BCO-LCT
           MOVE     DTA-DTA  TO  DTA-LCT
           MOVE     IND-PSQ  TO  IND1
           DISPLAY  E03-TL3
           PERFORM  ROT-LANC-20  GO       ROT-LANC-10.

       ROT-LANC-20.
           IF  IND1   =   1      CALL  "DY1421"  USING   PRM-LCT
                                                 CANCEL "DY1421".
           IF  IND1   =   2      CALL  "DY1422"  USING   PRM-LCT
                                                 CANCEL "DY1422".
           IF  IND1   =   3      CALL  "DY1423"  USING   PRM-LCT
                                                 CANCEL "DY1423".
           IF  IND1   =   4      CALL  "DY1424"  USING   PRM-LCT
                                                 CANCEL "DY1424".
           IF  IND1   =   5      CALL  "DY1425"  USING   PRM-LCT
                                                 CANCEL "DY1425".
           IF  IND1   =   6      CALL  "DY1426"  USING   PRM-LCT
                                                 CANCEL "DY1426".
           IF  IND1   =   7      CALL  "DY1427"  USING   PRM-LCT
                                                 CANCEL "DY1427".
       ROT-SPOL-00.
           CALL    "DYSPOL"  USING  PRM-OPR  PRM-SPL.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-DATA-00.
           CALL    "DYDATA"  USING  PRM-DTA
                                    PRM-HLP.
       ROT-PESQ-00.
           CALL    "DYPESQ"  USING  PRM-PSQ
                                    PRM-HLP.
       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-EXIT-00.
           CLOSE    CADBCO   CADCTR  GOBACK.
