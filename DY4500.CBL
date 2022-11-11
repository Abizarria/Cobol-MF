       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY4500.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADSPL        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SPL
                       FILE          STATUS  IS  FST-SPL.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADSPL.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-SPL          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  TECLADO          PIC  9(02).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  HRA-AUX.
               05  HOR-AUX      PIC  9(02).
               05  MIN-AUX      PIC  9(02).
               05  SEG-AUX      PIC  9(02).

       COPY  TBMENU.CPY.

       01  TB-IMPR.
           03  P-1110  PIC  X(12)  VALUE  "011021031000".
           03  P-1121  PIC  X(12)  VALUE  "011021032041".
           03  P-1122  PIC  X(12)  VALUE  "011021032042".
           03  P-1130  PIC  X(12)  VALUE  "011021033000".
           03  P-1140  PIC  X(12)  VALUE  "011021034000".
           03  P-1151  PIC  X(12)  VALUE  "011021035051".
           03  P-1152  PIC  X(12)  VALUE  "011021035052".
           03  P-1153  PIC  X(12)  VALUE  "011021035053".
           03  P-1154  PIC  X(12)  VALUE  "011021035054".
           03  P-1155  PIC  X(12)  VALUE  "011021035055".
           03  P-1156  PIC  X(12)  VALUE  "011021035056".
           03  P-1210  PIC  X(12)  VALUE  "011022061000".
           03  P-1221  PIC  X(12)  VALUE  "011022062071".
           03  P-1222  PIC  X(12)  VALUE  "011022062072".
           03  P-1230  PIC  X(12)  VALUE  "011022063000".
           03  P-1240  PIC  X(12)  VALUE  "011022064000".
           03  P-1251  PIC  X(12)  VALUE  "011022065081".
           03  P-1252  PIC  X(12)  VALUE  "011022065082".
           03  P-1253  PIC  X(12)  VALUE  "011022065083".
           03  P-1254  PIC  X(12)  VALUE  "011022065084".
           03  P-1255  PIC  X(12)  VALUE  "011022065085".
           03  P-1310  PIC  X(12)  VALUE  "011023091000".
           03  P-1320  PIC  X(12)  VALUE  "011023092000".
           03  P-1330  PIC  X(12)  VALUE  "011023093000".
           03  P-1340  PIC  X(12)  VALUE  "011023094000".
           03  P-1351  PIC  X(12)  VALUE  "011023095101".
           03  P-1352  PIC  X(12)  VALUE  "011023095102".
           03  P-1353  PIC  X(12)  VALUE  "011023095103".
           03  P-1354  PIC  X(12)  VALUE  "011023095104".
           03  P-1355  PIC  X(12)  VALUE  "011023095105".
           03  P-1356  PIC  X(12)  VALUE  "011023095106".
           03  P-1410  PIC  X(12)  VALUE  "011024111000".
           03  P-1420  PIC  X(12)  VALUE  "011024112000".
           03  P-1430  PIC  X(12)  VALUE  "011024113000".
           03  P-1440  PIC  X(12)  VALUE  "011024114000".
           03  P-1451  PIC  X(12)  VALUE  "011024115121".
           03  P-1452  PIC  X(12)  VALUE  "011024115122".
           03  P-1510  PIC  X(12)  VALUE  "011025131000".
           03  P-1521  PIC  X(12)  VALUE  "011025132141".
           03  P-1522  PIC  X(12)  VALUE  "011025132142".
           03  P-1523  PIC  X(12)  VALUE  "011025132143".
           03  P-1524  PIC  X(12)  VALUE  "011025132144".
           03  P-1525  PIC  X(12)  VALUE  "011025132145".
           03  P-1526  PIC  X(12)  VALUE  "011025132146".
           03  P-1531  PIC  X(12)  VALUE  "011025133151".
           03  P-1532  PIC  X(12)  VALUE  "011025133152".
           03  P-1540  PIC  X(12)  VALUE  "011025134000".
           03  P-1551  PIC  X(12)  VALUE  "011025135161".
           03  P-1552  PIC  X(12)  VALUE  "011025135162".
           03  P-1553  PIC  X(12)  VALUE  "011025135163".
           03  P-1554  PIC  X(12)  VALUE  "011025135164".
           03  P-1555  PIC  X(12)  VALUE  "011025135165".
           03  P-1556  PIC  X(12)  VALUE  "011025135166".
           03  P-2000  PIC  X(12)  VALUE  "012000000000".
           03  P-3100  PIC  X(12)  VALUE  "013171000000".
           03  P-3210  PIC  X(12)  VALUE  "013172181000".
           03  P-3220  PIC  X(12)  VALUE  "013172182000".
           03  P-3230  PIC  X(12)  VALUE  "013172183000".
           03  P-3240  PIC  X(12)  VALUE  "013172184000".
           03  P-4100  PIC  X(12)  VALUE  "014191000000".
           03  P-4200  PIC  X(12)  VALUE  "014192000000".
           03  P-4300  PIC  X(12)  VALUE  "014193000000".
           03  P-4400  PIC  X(12)  VALUE  "014194000000".
           03  P-4500  PIC  X(12)  VALUE  "014195000000".
           03  P-5100  PIC  X(12)  VALUE  "015201000000".
           03  P-5200  PIC  X(12)  VALUE  "015202000000".
       01  RD-IMPR      REDEFINES    TB-IMPR.
           03  PROG-TB               OCCURS  66.
               05  QTDE-TB           OCCURS  04.
                   07  MN-IMPR  PIC  9(02).
                   07  OP-IMPR  PIC  9(01).

       01  CAB-001.
           03  FILLER           PIC  X(47)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(38)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC X(125)  VALUE
               "RELA€ÇO DAS OP€åES EXECUTADAS NO SISTEMA".
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC X(137)  VALUE  ALL  "-".

       01  CAB-004.
           03  FILLER           PIC  X(67)  VALUE
               "  DATA      HORA    OPERADOR".
           03  FILLER           PIC  X(70)  VALUE
               "D E S C R I € Ç O   D A S   O P € å E S".

       01  CAB-005              PIC X(137)  VALUE  ALL  "- ".

       01  DET-001.
           03  DIA-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DT1          PIC  9(02).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  HOR-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  ":".
           03  MIN-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  ":".
           03  SEG-DT1          PIC  9(02).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  OPR-DT1          PIC  X(12).
           03  OP1-DT1          PIC  X(25).
           03  FILLER           PIC  X(03)  VALUE  " /".
           03  OP2-DT1          PIC  X(25).
           03  FILLER           PIC  X(03)  VALUE  " /".
           03  OP3-DT1          PIC  X(25).
           03  FILLER           PIC  X(03)  VALUE  " /".
           03  OP4-DT1          PIC  X(25).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  64.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-DTA.
           03  DTA-DTA.
               05  ANO-DTA      PIC  9(02).
               05  MES-DTA      PIC  9(02).
               05  DIA-DTA      PIC  9(02).
           03  LIN-DTA          PIC  9(02)  VALUE  12.
           03  COL-DTA          PIC  9(02).
           03  BCK-DTA          PIC  9(01)  VALUE  2.
           03  FOR-DTA          PIC  9(01)  VALUE  7.
           03  FLG-DTA          PIC  9(01).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "4500".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

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
           03  FLG-IMP          PIC  9(01)  VALUE  2.
           03  LIN-IMP          PIC  9(02)  VALUE  16.
           03  CL1-IMP          PIC  9(02)  VALUE  40.
           03  CL2-IMP          PIC  9(02)  VALUE  46.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 09 COLUMN 21 VALUE " Manuten‡Æo de Senhas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 09 COLUMN 43 VALUE "ß".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 10 COLUMN 21 VALUE
                   " É                        ÍÍÍÍÍÍÍÍÍ» ".
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
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 10 COLUMN 25 VALUE " EmissÆo de Relat¢rio ".
           03 BCK-CLR 00.
               05  LINE 11 COLUMN 60 VALUE "  ".
               05  LINE 12 COLUMN 60 VALUE "  ".
               05  LINE 13 COLUMN 60 VALUE "  ".
               05  LINE 14 COLUMN 60 VALUE "  ".
               05  LINE 15 COLUMN 60 VALUE "  ".
               05  LINE 16 COLUMN 60 VALUE "  ".
               05  LINE 17 COLUMN 60 VALUE "  ".
               05  LINE 18 COLUMN 23 PIC X(39) FROM SPACES.

       01  TELA-02 BCK-CLR 02.
           03  FOR-CLR  07.
               05  LINE 11 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 13 COLUMN 25 PIC X(01) FROM     "Ü".
           03  FOR-CLR  14.
               05  LINE 12 COLUMN 25 PIC X(30) FROM
                   " Per¡odo:          …".
           03  FOR-CLR  00.
               05  LINE 12 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "Ü".

       01  TELA-03 BCK-CLR 01.
           03  FOR-CLR  07.
               05  LINE 14 COLUMN 25 PIC X(30) FROM ALL "ß".
               05  LINE 16 COLUMN 25 PIC X(01) FROM     "Ü".
           03  FOR-CLR  15.
               05  LINE 15 COLUMN 25 PIC X(30) FROM
                   "   P ginas Montadas Ä    ".
               05  PAG-T03 LINE 15 COLUMN 49 PIC ZZ9 FROM PAG-CB1.
           03  FOR-CLR  00.
               05  LINE 15 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 16 COLUMN 26 PIC X(29) FROM ALL "Ü".

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING   PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.
           DISPLAY  TELA-01  TELA-02
           ACCEPT   ARQ-IMP   FROM   TIME
           OPEN     INPUT    CADSPL  OUTPUT   CADTMP
           MOVE     ZEROS      TO    DTA-DTA.

       ROT-ENTR-00.
           MOVE 35  TO  COL-DTA  PERFORM  ROT-DATA-00
           IF  FLG-DTA   =   0   GO       ROT-EXIT-00.
           MOVE     DTA-DTA  TO  DTA-SPL.

       ROT-ENTR-10.
           MOVE 46  TO  COL-DTA  PERFORM  ROT-DATA-00
           IF  FLG-DTA   >   0   GO       ROT-TEST-00.
           MOVE     DTA-SPL  TO  DTA-DTA
           DISPLAY  TELA-02      GO       ROT-ENTR-00.

       ROT-TEST-00.
           MOVE     19       TO  IND-MSG
           IF  DTA-DTA   <       DTA-SPL
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-10.
       ROT-IMPR-00.
           MOVE     31       TO  IND-MSG
           MOVE     ZEROS    TO  HRA-SPL
                                 PAG-CB1  CHV-TMP
           START    CADSPL       KEY  >   CHV-SPL
           IF  FST-SPL   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-10.
           READ     CADSPL  NEXT
           IF  DTA-SPL        >  DTA-DTA
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-10.
           DISPLAY  TELA-03      PERFORM  ROT-NOME-00.

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
           MOVE     DTA-SPL  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     ANO-AUX  TO  ANO-DT1
           MOVE     HRA-SPL  TO  HRA-AUX
           MOVE     HOR-AUX  TO  HOR-DT1
           MOVE     MIN-AUX  TO  MIN-DT1
           MOVE     SEG-AUX  TO  SEG-DT1
           MOVE     OPR-SPL  TO  OPR-DT1
           MOVE     MN-IMPR (IND-SPL, 1)  TO  IND1
           MOVE     OP-IMPR (IND-SPL, 1)  TO  IND2
           MOVE     DC-MENU (IND1, IND2)  TO  OP1-DT1
           IF       MN-IMPR (IND-SPL, 2)  =   00
                    MOVE     SPACES       TO  OP2-DT1
           ELSE
               MOVE MN-IMPR (IND-SPL, 2)  TO  IND1
               MOVE OP-IMPR (IND-SPL, 2)  TO  IND2
               MOVE DC-MENU (IND1, IND2)  TO  OP2-DT1.
           IF       MN-IMPR (IND-SPL, 3)  =   00
                    MOVE     SPACES       TO  OP3-DT1
           ELSE
               MOVE MN-IMPR (IND-SPL, 3)  TO  IND1
               MOVE OP-IMPR (IND-SPL, 3)  TO  IND2
               MOVE DC-MENU (IND1, IND2)  TO  OP3-DT1.
           IF       MN-IMPR (IND-SPL, 4)  =   00
                    MOVE     SPACES       TO  OP4-DT1
           ELSE
               MOVE MN-IMPR (IND-SPL, 4)  TO  IND1
               MOVE OP-IMPR (IND-SPL, 4)  TO  IND2
               MOVE DC-MENU (IND1, IND2)  TO  OP4-DT1.
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           READ     CADSPL  NEXT
           IF  FST-SPL   =  "10" OR       DTA-SPL
               >   DTA-DTA       PERFORM  ROT-IMPR-30
                                 GO       ROT-ENTR-00.
           IF  LIN-TMP   <   62  GO       ROT-IMPR-20.
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
           WRITE    REG-TMP      GO       ROT-IMPR-10.

       ROT-IMPR-30.
           MOVE     ZEROS    TO  DTA-DTA
           MOVE     PAG-CB1  TO  PAG-IMP
           DISPLAY  PAG-T03      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE   FILE         CADTMP
           OPEN     OUTPUT       CADTMP
           DISPLAY  TELA-01      TELA-02.

       ROT-DATA-00.
           CALL   "DYDATA"  USING  PRM-DTA
                                   PRM-HLP.
       ROT-MSGM-00.
           CALL   "DYMSGM"  USING  PRM-MSG.

       ROT-HELP-00.
           CALL   "DYHELP"  USING  PRM-HLP.

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
           CLOSE   CADSPL  CADTMP
           DELETE   FILE   CADTMP  GOBACK.
