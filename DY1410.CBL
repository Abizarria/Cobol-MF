       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1410.
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

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC
                       FILE          STATUS  IS  FST-LBC.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADBCO.CPY.
       COPY  CADLBC.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-BCO          PIC  X(02).
           03  FST-LBC          PIC  X(02).

           03  IND1             PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  ACM-PSQ          PIC  9(02).
           03  ACM-SDO          PIC S9(08)V99.

           03  NOM-PSQ          PIC  X(20).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  20.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  32.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1410".
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

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 07 COLUMN 13 VALUE " Administraฦo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 08 COLUMN 15 VALUE " Controle C. Banc ria ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 07 COLUMN 35 VALUE "฿".
               05  LINE 08 COLUMN 37 VALUE "฿".
               05  LINE 08 COLUMN 14 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 09 COLUMN 13 VALUE " ษ                        
      -            "ออออออออออออออออออออออออออป ".
               05  LINE 10 COLUMN 13 VALUE " บ
      -            "                          บ ".
               05  LINE 11 COLUMN 13 VALUE " บ
      -            "                          บ ".
               05  LINE 12 COLUMN 13 VALUE " บ
      -            "                          บ ".
               05  LINE 13 COLUMN 13 VALUE " บ
      -            "                          บ ".
               05  LINE 14 COLUMN 13 VALUE " บ
      -            "                          บ ".
               05  LINE 15 COLUMN 13 VALUE " บ
      -            "                          บ ".
               05  LINE 16 COLUMN 13 VALUE " บ
      -            "                          บ ".
               05  LINE 17 COLUMN 13 VALUE " บ
      -            "                          บ ".
               05  LINE 18 COLUMN 13 VALUE " ศออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออผ ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 09 COLUMN 17 VALUE " Cadastro C. Corrente ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 10 COLUMN 69 VALUE  "  ".
               05  LINE 11 COLUMN 69 VALUE  "  ".
               05  LINE 12 COLUMN 69 VALUE  "  ".
               05  LINE 13 COLUMN 69 VALUE  "  ".
               05  LINE 14 COLUMN 69 VALUE  "  ".
               05  LINE 15 COLUMN 69 VALUE  "  ".
               05  LINE 16 COLUMN 69 VALUE  "  ".
               05  LINE 17 COLUMN 69 VALUE  "  ".
               05  LINE 18 COLUMN 69 VALUE  "  ".
               05  LINE 19 COLUMN 15 PIC X(56) FROM SPACES.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 10 COLUMN 17 PIC X(47) FROM ALL "฿".
               05  LINE 14 COLUMN 17 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 11 COLUMN 17 PIC X(47) FROM
                   "  Cขdigo Conta :      -".
               05  LINE 12 COLUMN 17 PIC X(47) FROM SPACES.
               05  LINE 13 COLUMN 17 PIC X(47) FROM
                   "  Saldo Inicial .... R$".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 11 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 18 PIC X(46) FROM ALL "฿".

       01  TELA-02.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 15 COLUMN 17 PIC X(47) FROM ALL "฿".
               05  LINE 17 COLUMN 17 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 16 COLUMN 17 PIC X(47) FROM
                   "  Saldo Atual ...... R$".
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 16 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 18 PIC X(46) FROM ALL "฿".

       01  TELA-03  BCK-CLR 04 FOR-CLR 15.
           03  LINE 18 COLUMN 41 VALUE
               " ษออออออออออออออออออออออออออออออป ".
           03  LINE 19 COLUMN 41 VALUE
               " บ  Confirma a Exclusฦo ?  [ ]  บ ".
           03  LINE 20 COLUMN 41 VALUE
               " ศออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 00.
               05  LINE 19 COLUMN 75 PIC X(02) FROM SPACES.
               05  LINE 20 COLUMN 75 PIC X(02) FROM SPACES.
               05  LINE 21 COLUMN 43 PIC X(34) FROM SPACES.

       01  ENTR-01     AUTO.
           03  CHV-E01 LINE 11 COLUMN 34 PIC ZZ9   USING CHV-BCO.
           03  PSQ-E01 LINE 11 COLUMN 42 PIC X(20) USING NOM-PSQ.
           03  DES-E01 LINE 11 COLUMN 42 PIC X(20) USING DES-BCO.
           03  SDO-E01 LINE 13 COLUMN 48 PIC ---.---.--9,99
                                                   USING SDO-BCO.
       01  ENTR-02  BCK-CLR 02 FOR-CLR 15.
           03  CHV-T01 LINE 11 COLUMN 34 PIC 9(03)  FROM CHV-BCO.
           03  PSQ-T01 LINE 11 COLUMN 42 PIC X(20)  FROM NOM-PSQ.
           03  DES-T01 LINE 11 COLUMN 42 PIC X(20)  FROM DES-BCO.
           03  SDO-T01 LINE 13 COLUMN 48 PIC ---.---.--9,99
                                                    FROM SDO-BCO.
       01  ENTR-03  BCK-CLR 05 FOR-CLR 15
                    LINE 16 COLUMN 48 PIC ---.---.--9,99 FROM ACM-SDO.
               
       01  ENTR-04  BCK-CLR 04 FOR-CLR 15
                    LINE 19 COLUMN 69 PIC X(01) USING RSP-OPC AUTO.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           OPEN     I-O      CADBCO   INPUT    CADLBC.

       ROT-CODE-00.
           MOVE     ZEROS    TO  CHV-BCO
           ACCEPT   CHV-E01      DISPLAY  CHV-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-00.
           IF  CHV-BCO   =    0  GO       ROT-PESQ-00.
           READ     CADBCO
           IF  FST-BCO   =  "23" GO       ROT-INCL-00.

       ROT-EXBE-00.
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           MOVE     SDO-BCO  TO  ACM-SDO
           DISPLAY  DES-T01      SDO-T01
                    TELA-02      ENTR-03.

       ROT-EXBE-10.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-EXBE-20.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO
                                 GO       ROT-EXBE-20.
           IF  COD-LBC   <    4  COMPUTE  ACM-SDO   =
                                          ACM-SDO   +  VAL-LBC
           ELSE
                                 COMPUTE  ACM-SDO   =
                                          ACM-SDO   -  VAL-LBC.
           DISPLAY  ENTR-03      GO       ROT-EXBE-10.

       ROT-EXBE-20.
           EXIT.

       ROT-EXBE-30.
           MOVE  7  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-EXBE-30.
           IF  TCL-MSG   =   05  GO       ROT-EXBE-40.
           IF  TCL-MSG   =   06  GO       ROT-EXBE-50.
           IF  TCL-MSG   =   17  GO       ROT-EXCL-00.
           IF  TCL-MSG   >   00  GO       ROT-EXBE-30.
           IF  HEX-MSG   =   13  GO       ROT-ALTR-00.
           DISPLAY  TELA-01      GO       ROT-CODE-00.

       ROT-EXBE-40.
           START    CADBCO       KEY  <   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-EXBE-30.
           READ     CADBCO       PREVIOUS
           DISPLAY  CHV-T01      GO       ROT-EXBE-00.

       ROT-EXBE-50.
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-EXBE-30.
           READ     CADBCO  NEXT
           DISPLAY  CHV-T01      GO       ROT-EXBE-00.

       ROT-TEST-00.
           READ     CADBCO
           IF  FST-BCO   =  "9D" MOVE 20  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-30.
           IF  FST-BCO   =  "23" MOVE 36  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-30.
       ROT-INCL-00.
           MOVE     SPACES   TO  DES-BCO
           MOVE     ZEROS    TO  SDO-BCO.

       ROT-ENTR-00.
           ACCEPT   DES-E01      DISPLAY  DES-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  TELA-01
                                 GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-00.
           IF  DES-BCO   =       SPACES
                                 GO       ROT-ENTR-00.
       ROT-ENTR-10.
           ACCEPT   SDO-E01      DISPLAY  SDO-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
           WRITE    REG-BCO      MOVE 12  TO  IND-MSG
           IF  FST-BCO   =  "22" PERFORM  ROT-MSGM-00
                                 READ     CADBCO.
           GO       ROT-EXBE-00.

       ROT-ALTR-00.
           PERFORM  ROT-TEST-00  PERFORM  ROT-ENTR-00
           REWRITE  REG-BCO      GO       ROT-EXBE-30.

       ROT-EXCL-00.
           PERFORM  ROT-TEST-00  MOVE 13  TO  IND-MSG
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-EXCL-20.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO
                                 GO       ROT-EXCL-20.
       ROT-EXCL-10.
           PERFORM  ROT-MSGM-00  GO       ROT-EXBE-30.

       ROT-EXCL-20.
           IF  SDO-BCO  NOT   =  0   GO   ROT-EXCL-10.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-03.

       ROT-EXCL-30.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   ENTR-04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXBE-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-EXCL-30.
           IF  RSP-OPC  NOT   =  "S"
                                 GO       ROT-EXCL-30.
           DELETE   CADBCO
           DISPLAY  TELA-01      GO       ROT-CODE-00.

       ROT-PESQ-00.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  CHV-BCO
           DISPLAY  CHV-T01.

       ROT-PESQ-10.
           ACCEPT   PSQ-E01      DISPLAY  PSQ-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  TELA-01
                                 GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-10.
           IF  NOM-PSQ   =       SPACES
                                 GO       ROT-PESQ-10.
           MOVE     21  TO  IND1 PERFORM  ROT-PESQ-50.

       ROT-PESQ-20.
           SUBTRACT  1  FROM     IND1
           IF  LET-PSQ (IND1) =  " "  GO  ROT-PESQ-20.
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-PESQ-30.
           MOVE     22       TO  IND-MSG
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           READ     CADBCO  NEXT DISPLAY  CHV-T01
           INSPECT  DES-BCO      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-30.
           PERFORM  ROT-EXBE-00  THRU     ROT-EXBE-20
           GO       ROT-PESQ-60.

       ROT-PESQ-40.
           MOVE     22       TO  IND-MSG
           START    CADBCO       KEY  <   CHV-BCO
           IF  FST-BCO   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           READ     CADBCO       PREVIOUS
           DISPLAY  CHV-T01
           INSPECT  DES-BCO      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-40.
           PERFORM  ROT-EXBE-00  THRU     ROT-EXBE-20
           GO       ROT-PESQ-60.

       ROT-PESQ-50.
           MOVE     ZEROS    TO  ACM-PSQ
           DISPLAY  TELA-01      CHV-T01      PSQ-T01.

       ROT-PESQ-60.
           MOVE  7  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-60.
           IF  TCL-MSG   =   05  PERFORM  ROT-PESQ-50
                                 GO       ROT-PESQ-40.
           IF  TCL-MSG   =   06  PERFORM  ROT-PESQ-50
                                 GO       ROT-PESQ-30.
           IF  TCL-MSG   =   17  GO       ROT-EXCL-00.
           IF  TCL-MSG   >   00  GO       ROT-PESQ-60.
           IF  HEX-MSG   =   13  GO       ROT-ALTR-00.
           DISPLAY  TELA-01      GO       ROT-PESQ-00.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-EXIT-00.
           CLOSE    CADBCO  CADLBC  GOBACK.
