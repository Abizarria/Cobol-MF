       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY3100.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADMDR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-MDR
                       FILE          STATUS  IS  FST-MDR.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADMDR.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-MDR          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(01).
           03  TECLADO          PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  ACM-PSQ          PIC  9(02).

           03  NOM-PSQ          PIC  X(40).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  40.

           03  DTA-SYS.
               05  DIA-SYS      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-SYS      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-SYS      PIC  9(02).

           03  DTA-NSC.
               05  DIA-NSC      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-NSC      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-NSC      PIC  9(02).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  55.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "3100".
           03  BCK-HLP          PIC  9(01)  VALUE  7.
           03  FOR-HLP          PIC  9(01)  VALUE  2.
           03  LET-HLP          PIC  9(01)  VALUE  3.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-DTA.
           03  DTA-DTA.
               05  ANO-DTA      PIC  9(02).
               05  MES-DTA      PIC  9(02).
               05  DIA-DTA      PIC  9(02).
           03  LIN-DTA          PIC  9(02)  VALUE  14.
           03  COL-DTA          PIC  9(02)  VALUE  61.
           03  BCK-DTA          PIC  9(01)  VALUE  3.
           03  FOR-DTA          PIC  9(01)  VALUE  7.
           03  FLG-DTA          PIC  9(01).

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
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 06 COLUMN 07 VALUE " Mala-Direta Clientes ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 06 COLUMN 29 VALUE "฿".
           03  BCK-CLR 03 FOR-CLR 14.
               05  LINE 07 COLUMN 07 VALUE " ษ                        
      -            "ออออออออออออออออออออออออออออออออออออป ".
               05  LINE 08 COLUMN 07 VALUE " บ
      -            "                                    บ ".
               05  LINE 09 COLUMN 07 VALUE " บ  Cขdigo .........:
      -            "                                    บ ".
               05  LINE 10 COLUMN 07 VALUE " บ
      -            "                                    บ ".
               05  LINE 11 COLUMN 07 VALUE " บ  Nome Completo ..:
      -            "                                    บ ".
               05  LINE 12 COLUMN 07 VALUE " บ
      -            "                                    บ ".
               05  LINE 13 COLUMN 07 VALUE " บ  Nฃmero do R.G. .:
      -            "             Atualizado :           บ ".
               05  LINE 14 COLUMN 07 VALUE " บ  Telefone .......:
      -            "             Nascimento :           บ ".
               05  LINE 15 COLUMN 07 VALUE " บ
      -            "                                    บ ".
               05  LINE 16 COLUMN 07 VALUE " บ  Endereo .......:
      -            "                                    บ ".
               05  LINE 17 COLUMN 07 VALUE " บ
      -            "                                    บ ".
               05  LINE 18 COLUMN 07 VALUE " บ  Observaไes ....:
      -            "                                    บ ".
               05  LINE 19 COLUMN 07 VALUE " บ
      -            "                                    บ ".
               05  LINE 20 COLUMN 07 VALUE " ศออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 07 COLUMN 11 VALUE " Cadastro de Clientes ".
           03  BCK-CLR 00.
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
               05  LINE 21 COLUMN 09 PIC X(66) FROM SPACES.

       01  ENTR-01     AUTO.
           03  CHV-E01 LINE 09 COLUMN 29 PIC ZZZZ9        USING CHV-MDR.
           03  NOM-E01 LINE 11 COLUMN 29 PIC X(40)        USING NOM-MDR.
           03  DOC-E01 LINE 13 COLUMN 29 PIC 99.999.999.9 USING DOC-MDR.
           03  DDD-E01 LINE 14 COLUMN 29 PIC 9(02)        USING DDD-MDR.
           03  TEL-E01 LINE 14 COLUMN 32 PIC ZZZ9.9999    USING TEL-MDR.
           03  END-E01 LINE 16 COLUMN 29 PIC X(40)        USING END-MDR.
           03  CEP-E01 LINE 17 COLUMN 29 PIC 99999.999    USING CEP-MDR.
           03  CID-E01 LINE 17 COLUMN 40 PIC X(25)        USING CID-MDR.
           03  EST-E01 LINE 17 COLUMN 67 PIC X(02)        USING EST-MDR.
           03  OBS-E01 LINE 18 COLUMN 29 PIC X(40)        USING OBS-MDR.
           03  PSQ-E01 LINE 11 COLUMN 29 PIC X(40)        USING NOM-PSQ.

       01  ENTR-02  BCK-CLR 03 FOR-CLR 15.
           03  CHV-T01 LINE 09 COLUMN 29 PIC 9(05)        FROM  CHV-MDR.
           03  NOM-T01 LINE 11 COLUMN 29 PIC X(40)        FROM  NOM-MDR.
           03  DOC-T01 LINE 13 COLUMN 29 PIC 99.999.999.9 FROM  DOC-MDR.
           03  DDD-T01 LINE 14 COLUMN 29 PIC 9(02)        FROM  DDD-MDR.
           03  TEL-T01 LINE 14 COLUMN 32 PIC ZZZ9.9999    FROM  TEL-MDR.
           03  DTA-T01 LINE 13 COLUMN 61 PIC 99/99/99     FROM  DTA-SYS.
           03  NSC-T01 LINE 14 COLUMN 61 PIC 99/99/99     FROM  DTA-NSC.
           03  END-T01 LINE 16 COLUMN 29 PIC X(40)        FROM  END-MDR.
           03  CEP-T01 LINE 17 COLUMN 29 PIC 99999.999    FROM  CEP-MDR.
           03  CID-T01 LINE 17 COLUMN 40 PIC X(25)        FROM  CID-MDR.
           03  EST-T01 LINE 17 COLUMN 67 PIC X(02)        FROM  EST-MDR.
           03  OBS-T01 LINE 18 COLUMN 29 PIC X(40)        FROM  OBS-MDR.

       01  ENTR-03  BCK-CLR 03 FOR-CLR 15
                    LINE 11 COLUMN 29 PIC X(40) USING NOM-PSQ AUTO.
       01  TELA-02.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 12 COLUMN 23 VALUE
                   " ษออออออออออออออออออออออออออออออป ".
               05  LINE 13 COLUMN 23 VALUE
                   " บ  Confirma a Exclusฦo ?  [ ]  บ ".
               05  LINE 14 COLUMN 23 VALUE
                   " ศออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 00 FOR-CLR 8.
               05  LINE 13 COLUMN 57 PIC X(02) FROM "o ".
               05  LINE 14 COLUMN 57 PIC X(02) FROM "o ".
               05  LINE 15 COLUMN 25 PIC X(34) FROM SPACES.

       01  ENTR-04 BCK-CLR 04 FOR-CLR 15
                   LINE 13 COLUMN 51 PIC X(01) USING RSP-OPC AUTO.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.
           OPEN     I-O      CADMDR.

       ROT-CODE-00.
           DISPLAY  TELA-01      MOVE  0  TO  CHV-MDR.

       ROT-CODE-10.
           ACCEPT   CHV-E01      DISPLAY  CHV-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           MOVE     1   TO  IND2 PERFORM  ROT-GTLA-00
           IF  CHV-MDR   <   01  GO       ROT-PESQ-00.
           READ     CADMDR
           IF  FST-MDR   =  "23" GO       ROT-INCL-00.

       ROT-EXBE-00.
           MOVE     DTA-MDR  TO  DTA-DTA
           MOVE     DIA-DTA  TO  DIA-SYS
           MOVE     MES-DTA  TO  MES-SYS
           MOVE     ANO-DTA  TO  ANO-SYS
           MOVE     NSC-MDR  TO  DTA-DTA
           MOVE     DIA-DTA  TO  DIA-NSC
           MOVE     MES-DTA  TO  MES-NSC
           MOVE     ANO-DTA  TO  ANO-NSC
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
           MOVE     1   TO IND2  MOVE  0  TO  CHV-MDR
           PERFORM  ROT-RTLA-00  GO       ROT-CODE-10.

       ROT-RTRN-00.
           START    CADMDR       KEY  <   CHV-MDR
           IF  FST-MDR   =  "23" GO       ROT-EXBE-10.
           READ     CADMDR       PREVIOUS
           GO       ROT-EXBE-00.

       ROT-PROX-00.
           START    CADMDR       KEY  >   CHV-MDR
           IF  FST-MDR   =  "23" GO       ROT-EXBE-10.
           READ     CADMDR  NEXT GO       ROT-EXBE-00.

       ROT-TEST-00.
           READ     CADMDR
           IF  FST-MDR   =  "9D" MOVE 20  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-10.
           IF  FST-MDR   =  "23" MOVE 36  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-10.
       ROT-INCL-00.
           MOVE     SPACES   TO  NOM-MDR  END-MDR
                    CID-MDR      EST-MDR  OBS-MDR
           MOVE     ZEROS    TO  DOC-MDR  DDD-MDR
                                 TEL-MDR  DTA-DTA
                                 NSC-MDR  CEP-MDR
           PERFORM  ROT-ENTR-00  THRU     ROT-ENTR-90
           WRITE    REG-MDR
           IF  FST-MDR   =  "22" MOVE 12  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 READ     CADMDR
                                 GO       ROT-EXBE-00.
           GO       ROT-EXBE-10.

       ROT-ALTR-00.
           PERFORM  ROT-ENTR-00  THRU     ROT-ENTR-90
           REWRITE  REG-MDR      GO       ROT-EXBE-10.

       ROT-EXCL-00.
           MOVE     2  TO  IND2
           PERFORM  ROT-GTLA-00  DISPLAY      TELA-02.

       ROT-EXCL-10.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   ENTR-04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXBE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-EXCL-10.
           IF  RSP-OPC  NOT   =  "S"
                                 GO       ROT-EXCL-10.
           DELETE   CADMDR       GO       ROT-EXBE-20.

       ROT-ENTR-00.
           ACCEPT   NOM-E01      DISPLAY  NOM-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-00.
           IF  NOM-MDR   =       SPACES
                                 GO       ROT-ENTR-00.
       ROT-ENTR-10.
           ACCEPT   DOC-E01      DISPLAY  DOC-T01
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
           ACCEPT   DTA-DTA FROM DATE
           MOVE     DIA-DTA  TO  DIA-SYS
           MOVE     MES-DTA  TO  MES-SYS
           MOVE     ANO-DTA  TO  ANO-SYS
           DISPLAY  DTA-T01
           MOVE     DTA-DTA  TO  DTA-MDR
           MOVE     NSC-MDR  TO  DTA-DTA.

       ROT-ENTR-40.
           PERFORM  ROT-DATA-00  MOVE 17  TO  IND-MSG
           IF  FLG-DTA   =    0  GO       ROT-ENTR-30.
           MOVE     DTA-DTA  TO  NSC-MDR
           IF (DTA-MDR   +       1000000  -   NSC-MDR)
               <    180000       PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-40.
       ROT-ENTR-50.
           ACCEPT   END-E01      DISPLAY  END-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-40.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-50.
       ROT-ENTR-60.
           ACCEPT   CEP-E01      DISPLAY  CEP-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-50.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-60.
       ROT-ENTR-70.
           ACCEPT   CID-E01      DISPLAY  CID-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-60.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-70.
       ROT-ENTR-80.
           ACCEPT   EST-E01      DISPLAY  EST-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-70.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-80.
       ROT-ENTR-90.
           ACCEPT   OBS-E01      DISPLAY  OBS-T01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-80.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-90.
       ROT-PESQ-00.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  CHV-MDR
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
           START    CADMDR       KEY  >   CHV-MDR
           IF  FST-MDR   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           READ     CADMDR  NEXT DISPLAY  CHV-T01
           INSPECT  NOM-MDR      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-30.
           PERFORM  ROT-EXBE-00  GO       ROT-PESQ-60.

       ROT-PESQ-40.
           MOVE     22       TO  IND-MSG
           START    CADMDR       KEY  <   CHV-MDR
           IF  FST-MDR   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           READ     CADMDR       PREVIOUS
           DISPLAY  CHV-T01
           INSPECT  NOM-MDR      TALLYING ACM-PSQ
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
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR (IND2)
           CALL    "DYTELA"  USING  PRM-SCR (IND2).

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR (IND2)
           CALL    "DYTELA"  USING  PRM-SCR (IND2).

       ROT-DATA-00.
           CALL    "DYDATA"  USING  PRM-DTA
                                    PRM-HLP.
       ROT-EXIT-00.
           CLOSE    CADMDR   GOBACK.
