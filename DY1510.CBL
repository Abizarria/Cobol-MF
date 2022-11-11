       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1510.
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

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCLI.CPY.
       COPY  CADFAT.CPY.
       COPY  CADNTA.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-CLI          PIC  X(02).
           03  FST-FAT          PIC  X(02).
           03  FST-NTA          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(01).
           03  TECLADO          PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  ACM-PSQ          PIC  9(02).

           03  TAB-PGT.
               05  FILLER       PIC  X(09)  VALUE  "SEMANAL  ".
               05  FILLER       PIC  X(09)  VALUE  "DEZENAL  ".
               05  FILLER       PIC  X(09)  VALUE  "QUINZENAL".
               05  FILLER       PIC  X(09)  VALUE  "MENSAL   ".
               05  FILLER       PIC  X(09)  VALUE  "EVENTUAL ".
           03  RED-PGT          REDEFINES   TAB-PGT.
               05  PGT-TAB      PIC  X(09)  OCCURS  5.

           03  NOM-PSQ          PIC  X(40).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  40.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  38.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1510".
           03  BCK-HLP          PIC  9(01)  VALUE  7.
           03  FOR-HLP          PIC  9(01)  VALUE  2.
           03  LET-HLP          PIC  9(01)  VALUE  3.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-PSQ.
           03  OPC-PSQ          PIC  9(01)  VALUE  5.
           03  IND-PSQ          PIC  X(01).
           03  LIN-PSQ          PIC  9(02)  VALUE  15.
           03  COL-PSQ          PIC  9(02)  VALUE  59.
           03  SB1-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB2-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB3-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB4-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB5-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB6-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB7-PSQ          PIC  X(26)  VALUE  SPACES.

       01  PRM-SCR              OCCURS  02.
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
           03  LINE 05 COLUMN 03 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 05 COLUMN 09 VALUE " Administraฦo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 06 COLUMN 11 VALUE " Controle de Clientes ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 05 COLUMN 31 VALUE "฿".
               05  LINE 06 COLUMN 33 VALUE "฿".
               05  LINE 06 COLUMN 10 VALUE "Ü".
           03  BCK-CLR 03 FOR-CLR 14.
               05  LINE 07 COLUMN 09 VALUE " ษ                        
      -            "ออออออออออออออออออออออออออออออออออป ".
               05  LINE 08 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 09 COLUMN 09 VALUE " บ  Cขd. Empresa .:
      -            "                                  บ ".
               05  LINE 10 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 11 COLUMN 09 VALUE " บ  Razฦo Social .:
      -            "                                  บ ".
               05  LINE 12 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 13 COLUMN 09 VALUE " บ  C.N.P.J. .....:
      -            "                                  บ ".
               05  LINE 14 COLUMN 09 VALUE " บ  Telefone .....:
      -            "                                  บ ".
               05  LINE 15 COLUMN 09 VALUE " บ  Contato ......:
      -            "           Pagamento :            บ ".
               05  LINE 16 COLUMN 09 VALUE " บ  Endereo .....:
      -            "                                  บ ".
               05  LINE 17 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 18 COLUMN 09 VALUE " บ  Observaไes ..:
      -            "                                  บ ".
               05  LINE 19 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 20 COLUMN 09 VALUE " ศออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 13 VALUE " Cadastro de Empresas ".
           03  BCK-CLR  00.
               05  LINE 08 COLUMN 73 VALUE "  ".
               05  LINE 09 COLUMN 73 VALUE "  ".
               05  LINE 10 COLUMN 73 VALUE "  ".
               05  LINE 11 COLUMN 73 VALUE "  ".
               05  LINE 12 COLUMN 73 VALUE "  ".
               05  LINE 13 COLUMN 73 VALUE "  ".
               05  LINE 14 COLUMN 73 VALUE "  ".
               05  LINE 15 COLUMN 73 VALUE "  ".
               05  LINE 16 COLUMN 73 VALUE "  ".
               05  LINE 17 COLUMN 73 VALUE "  ".
               05  LINE 18 COLUMN 73 VALUE "  ".
               05  LINE 19 COLUMN 73 VALUE "  ".
               05  LINE 20 COLUMN 73 VALUE "  ".
               05  LINE 21 COLUMN 11 PIC X(64) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 4 FOR-CLR 15.
               05  LINE 12 COLUMN 24 VALUE
                   " ษออออออออออออออออออออออออออออออป ".
               05  LINE 13 COLUMN 24 VALUE
                   " บ  Confirma a Exclusฦo ?  [ ]  บ ".
               05  LINE 14 COLUMN 24 VALUE
                   " ศออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 0 FOR-CLR 8.
               05  LINE 13 COLUMN 58 PIC X(02) FROM "  ".
               05  LINE 14 COLUMN 58 PIC X(02) FROM "  ".
               05  LINE 15 COLUMN 26 PIC X(22) FROM ".:".
               05  LINE 15 COLUMN 29 PIC X(09) FROM CNT-CLI.
               05  LINE 15 COLUMN 48 PIC X(12) FROM "Pagamento :".

       01  ENTR-01     AUTO.
           03  CHV-E01 LINE 09 COLUMN 29 PIC ZZZZ9     USING CHV-CLI.
           03  NOM-E01 LINE 11 COLUMN 29 PIC X(40)     USING NOM-CLI.
           03  CGC-E01 LINE 13 COLUMN 29 PIC 99.999.999/9999.99
                                                       USING CGC-CLI.
           03  DDD-E01 LINE 14 COLUMN 29 PIC 9(02)     USING DDD-CLI.
           03  TEL-E01 LINE 14 COLUMN 32 PIC ZZZ9.9999 USING TEL-CLI.
           03  CNT-E01 LINE 15 COLUMN 29 PIC X(15)     USING CNT-CLI.
           03  END-E01 LINE 16 COLUMN 29 PIC X(40)     USING END-CLI.
           03  CEP-E01 LINE 17 COLUMN 29 PIC 99999.999 USING CEP-CLI.
           03  CID-E01 LINE 17 COLUMN 40 PIC X(25)     USING CID-CLI.
           03  EST-E01 LINE 17 COLUMN 67 PIC X(02)     USING EST-CLI.
           03  OBS-E01 LINE 18 COLUMN 29 PIC X(40)     USING OBS-CLI.
           03  PSQ-E01 LINE 11 COLUMN 29 PIC X(40)     USING NOM-PSQ.

       01  ENTR-02 BCK-CLR 03 FOR-CLR 15.
           03  CHV-T01 LINE 09 COLUMN 29 PIC 9(05)     FROM CHV-CLI.
           03  NOM-T01 LINE 11 COLUMN 29 PIC X(40)     FROM NOM-CLI.
           03  CGC-T01 LINE 13 COLUMN 29 PIC 99.999.999/9999.99
                                                       FROM CGC-CLI.
           03  DDD-T01 LINE 14 COLUMN 29 PIC 9(02)     FROM DDD-CLI.
           03  TEL-T01 LINE 14 COLUMN 32 PIC ZZZ9.9999 FROM TEL-CLI.
           03  CNT-T01 LINE 15 COLUMN 29 PIC X(15)     FROM CNT-CLI.
           03  PGT-T01 LINE 15 COLUMN 60 PIC X(09)     FROM PGT-TAB
                                                           (PGT-CLI).
           03  END-T01 LINE 16 COLUMN 29 PIC X(40)     FROM END-CLI.
           03  CEP-T01 LINE 17 COLUMN 29 PIC 99999.999 FROM CEP-CLI.
           03  CID-T01 LINE 17 COLUMN 40 PIC X(25)     FROM CID-CLI.
           03  EST-T01 LINE 17 COLUMN 67 PIC X(02)     FROM EST-CLI.
           03  OBS-T01 LINE 18 COLUMN 29 PIC X(40)     FROM OBS-CLI.

       01  ENTR-03  BCK-CLR 03 FOR-CLR 15
                    LINE 11 COLUMN 29 PIC X(40) FROM  NOM-PSQ.

       01  ENTR-04  BCK-CLR 04 FOR-CLR 15
                    LINE 13 COLUMN 52 PIC X(01) USING RSP-OPC AUTO.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.
           OPEN     I-O      CADCLI
                    INPUT    CADFAT   CADNTA.

       ROT-CODE-00.
           DISPLAY  TELA-01      MOVE  0  TO  CHV-CLI.

       ROT-CODE-10.
           ACCEPT   CHV-E01      DISPLAY  CHV-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           MOVE     1   TO  IND2 PERFORM  ROT-GTLA-00
           IF  CHV-CLI   <   01  GO       ROT-PESQ-00.
           READ     CADCLI
           IF  FST-CLI   =  "23" GO       ROT-INCL-00.

       ROT-EXBE-00.
           DISPLAY  ENTR-02.

       ROT-EXBE-10.
           MOVE  7  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-EXBE-10.
           IF  TCL-MSG   =   05  GO       ROT-RTRN-00.
           IF  TCL-MSG   =   06  GO       ROT-PROX-00.
           IF  TCL-MSG   =   17  PERFORM  ROT-TEST-00
                                 GO       ROT-EXCL-00.
           IF  TCL-MSG   >   00  GO       ROT-EXBE-10.
           IF  HEX-MSG   =   13  PERFORM  ROT-TEST-00
                                 GO       ROT-ALTR-00.
       ROT-EXBE-20.
           MOVE     1   TO IND2  MOVE  0  TO  CHV-CLI
           PERFORM  ROT-RTLA-00  GO       ROT-CODE-10.

       ROT-RTRN-00.
           START    CADCLI       KEY  <   CHV-CLI
           IF  FST-CLI   =  "23" GO       ROT-EXBE-10.
           READ     CADCLI       PREVIOUS
           GO       ROT-EXBE-00.

       ROT-PROX-00.
           START    CADCLI       KEY  >   CHV-CLI
           IF  FST-CLI   =  "23" GO       ROT-EXBE-10.
           READ     CADCLI  NEXT GO       ROT-EXBE-00.

       ROT-TEST-00.
           READ     CADCLI
           IF  FST-CLI   =  "9D" MOVE 20  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-10.
           IF  FST-CLI   =  "23" MOVE 36  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-10.
       ROT-INCL-00.
           MOVE     SPACES   TO  NOM-CLI  CNT-CLI
                                 END-CLI  CID-CLI
                                 EST-CLI  OBS-CLI
           MOVE     ZEROS    TO  DDD-CLI
                                 TEL-CLI  CGC-CLI
                                 CEP-CLI  PGT-CLI
           PERFORM  ROT-ENTR-00  THRU     ROT-ENTR-95
           WRITE    REG-CLI
           IF  FST-CLI   =  "22" MOVE 12  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 READ     CADCLI
                                 GO       ROT-EXBE-00.
           GO       ROT-EXBE-10.

       ROT-ALTR-00.
           PERFORM  ROT-ENTR-00  THRU     ROT-ENTR-95
           REWRITE  REG-CLI      GO       ROT-EXBE-10.

       ROT-EXCL-00.
           MOVE     2        TO  IND2
           MOVE     13       TO  IND-MSG
           MOVE     SPACES   TO  CHV-NTA  CHV-FAT
           MOVE     CHV-CLI  TO  CLI-NTA  CLI-FAT.

       ROT-EXCL-10.
           START    CADNTA       KEY  >   CHV-NTA
           IF  FST-NTA   =  "23" GO       ROT-EXCL-20.
           READ     CADNTA  NEXT
           IF  CLI-NTA        =  CHV-CLI
                                 GO       ROT-EXCL-30.
       ROT-EXCL-20.
           START    CADFAT       KEY  >   CHV-FAT
           IF  FST-FAT   =  "23" GO       ROT-EXCL-40.
           READ     CADFAT  NEXT
           IF  CLI-FAT  NOT   =  CHV-CLI
                                 GO       ROT-EXCL-40.
       ROT-EXCL-30.
           PERFORM  ROT-MSGM-00  GO       ROT-EXBE-10.

       ROT-EXCL-40.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-02.

       ROT-EXCL-50.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   ENTR-04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXBE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-EXCL-50.
           IF  RSP-OPC  NOT   =  "S"
                                 GO       ROT-EXCL-50.
           DELETE   CADCLI       GO       ROT-EXBE-20.

       ROT-ENTR-00.
           ACCEPT   NOM-E01      DISPLAY  NOM-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-00.
           IF  NOM-CLI   =       SPACES
                                 GO       ROT-ENTR-00.
       ROT-ENTR-10.
           ACCEPT   CGC-E01      DISPLAY  CGC-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
       ROT-ENTR-20.
           ACCEPT   DDD-E01      DISPLAY  DDD-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-20.
       ROT-ENTR-30.
           ACCEPT   TEL-E01      DISPLAY  TEL-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-30.
       ROT-ENTR-40.
           ACCEPT   CNT-E01      DISPLAY  CNT-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-40.
       ROT-ENTR-50.
           IF  PGT-CLI   =   0   MOVE  1  TO  IND-PSQ
               ELSE  MOVE        PGT-CLI  TO  IND-PSQ.
           CALL    "DYPESQ"      USING    PRM-PSQ
                                          PRM-HLP
           IF  IND-PSQ   =  " "  GO       ROT-ENTR-40.
           MOVE     IND-PSQ  TO  PGT-CLI  DISPLAY     PGT-T01.

       ROT-ENTR-60.
           ACCEPT   END-E01      DISPLAY  END-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-50.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-60.
       ROT-ENTR-70.
           ACCEPT   CEP-E01      DISPLAY  CEP-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-60.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-70.
       ROT-ENTR-80.
           ACCEPT   CID-E01      DISPLAY  CID-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-70.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-80.
       ROT-ENTR-90.
           ACCEPT   EST-E01      DISPLAY  EST-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-80.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-90.
       ROT-ENTR-95.
           ACCEPT   OBS-E01      DISPLAY  OBS-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-90.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-95.
       ROT-PESQ-00.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  CHV-CLI
           DISPLAY  CHV-T01.

       ROT-PESQ-10.
           ACCEPT   PSQ-E01      DISPLAY  ENTR-03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-CODE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-10.
           IF  NOM-PSQ  =        SPACES
                                 GO       ROT-PESQ-10.
           MOVE     41  TO  IND1 PERFORM  ROT-PESQ-50.

       ROT-PESQ-20.
           SUBTRACT  1  FROM     IND1
           IF  LET-PSQ (IND1) =  " "  GO  ROT-PESQ-20.
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-PESQ-30.
           MOVE     22       TO  IND-MSG
           START    CADCLI       KEY  >   CHV-CLI
           IF  FST-CLI   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           READ     CADCLI  NEXT DISPLAY  CHV-T01
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
           DISPLAY  CHV-T01
           INSPECT  NOM-CLI      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-40.
           PERFORM  ROT-EXBE-00  GO       ROT-PESQ-60.

       ROT-PESQ-50.
           PERFORM  ROT-RTLA-00
           MOVE     0        TO  ACM-PSQ
           DISPLAY  CHV-T01      ENTR-03.

       ROT-PESQ-60.
           MOVE  7  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-60.
           IF  TCL-MSG   =   05  PERFORM  ROT-PESQ-50
                                 GO       ROT-PESQ-40.
           IF  TCL-MSG   =   06  PERFORM  ROT-PESQ-50
                                 GO       ROT-PESQ-30.
           IF  TCL-MSG   =   17  PERFORM  ROT-TEST-00
                                 GO       ROT-EXCL-00.
           IF  TCL-MSG   >   00  GO       ROT-PESQ-60.
           IF  HEX-MSG   =   13  PERFORM  ROT-TEST-00
                                 GO       ROT-ALTR-00.
           PERFORM  ROT-RTLA-00  GO       ROT-PESQ-00.

       ROT-HELP-00.
           CALL    "DYHELP"  USING    PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING    PRM-MSG.

       ROT-GTLA-00.
           MOVE    "G"        TO      FLG-SCR (IND2)
           CALL    "DYTELA"  USING    PRM-SCR (IND2).

       ROT-RTLA-00.
           MOVE    "R"        TO      FLG-SCR (IND2)
           CALL    "DYTELA"  USING    PRM-SCR (IND2).

       ROT-EXIT-00.
           CLOSE    CADCLI   CADFAT   CADNTA  GOBACK.
