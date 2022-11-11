       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1526.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCRD        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CRD
                       FILE          STATUS  IS  FST-CRD.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADLCR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCR
                       FILE          STATUS  IS  FST-LCR.

           SELECT      CADSDO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SDO
                       FILE          STATUS  IS  FST-SDO.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCRD.CPY.
       COPY  CADCTR.CPY.
       COPY  CADLCR.CPY.
       COPY  CADSDO.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-CRD          PIC  X(02).
           03  FST-CTR          PIC  X(02).
           03  FST-LCR          PIC  X(02).
           03  FST-SDO          PIC  X(02).

           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  IND3             PIC  9(03).
           03  IND4             PIC  9(03).
           03  LIN1             PIC  9(02).
           03  LIN2             PIC  9(02).

           03  NUM-OPC          PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  MSG-AUX          PIC  X(56).
           03  DTA-LIM          PIC  9(06).
           03  ACM-QTD          PIC  9(03).
           03  VAL-LCT          PIC  9(06)V99.
           03  ACM-VAL          PIC  9(07)V99.

           03  DTA-DET.
               05  ANO-DET      PIC  9(02).
               05  MES-DET      PIC  9(02).
               05  DIA-DET      PIC  9(02).

           03  TAB-OPC          PIC  X(44)  VALUE
                   " Lan‡amento Movimento  Corre‡Æo Lan‡amentos ".
           03  RED-OPC   REDEFINES   TAB-OPC.
               05  OPC-TAB      PIC  X(22)  OCCURS  02.

           03  MG1-AUX          PIC  X(56)  VALUE
               "        DESEJA CONFIRMAR OS VALES LAN€ADOS ? [ ]".
           03  MG2-AUX          PIC  X(56)  VALUE
               "      AGUARDE, ATUALIZANDO O ARQUIVO DE CARTåES ...".

           03  REG-AX1.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  NUM-AX1      PIC  9(06).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  DIA-AX1      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AX1      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-AX1      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  NOM-AX1      PIC  X(26).
               05  VAL-AX1      PIC  ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  REG-AX2.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  NUM-AX2      PIC  9(06).
               05  FILLER       PIC  X(06)  VALUE  SPACES.
               05  NOM-AX2      PIC  X(30).
               05  VAL-AX2      PIC  ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  TAB-REG          OCCURS 300.
               05  REG-TAB      PIC  X(54).
               05  CH1-TAB      PIC  X(14).
               05  CH2-TAB      PIC  X(16).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  43.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1520".
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
           03  LIN-DTA          PIC  9(02).
           03  COL-DTA          PIC  9(02).
           03  BCK-DTA          PIC  9(01).
           03  FOR-DTA          PIC  9(01).
           03  FLG-DTA          PIC  9(01).

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
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 31 VALUE
                   " ÉÍÍÍÍÍ Vale Frete ÍÍÍÍÍ» ".
               05  LINE 10 COLUMN 31 VALUE
                   " º                        º ".
               05  LINE 11 COLUMN 31 VALUE
                   " º                        º ".
               05  LINE 12 COLUMN 31 VALUE
                   " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 09 COLUMN 39 VALUE " Vale Frete ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 10 COLUMN 59 VALUE "  ".
               05  LINE 11 COLUMN 59 VALUE "  ".
               05  LINE 12 COLUMN 59 VALUE "  ".
               05  LINE 13 COLUMN 33 PIC X(28) FROM
                   "o Visa e Outros  º".

       01  TELA-02.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 05 COLUMN 08 VALUE " Administra‡Æo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 06 COLUMN 10 VALUE " Controle de Clientes ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 12 VALUE " Movimentos e Acertos ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 05 COLUMN 30 VALUE "ß".
               05  LINE 06 COLUMN 32 VALUE "ß".
               05  LINE 07 COLUMN 34 VALUE "ß".
               05  LINE 06 COLUMN 09 VALUE "Ü".
               05  LINE 07 COLUMN 11 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 08 COLUMN 08 VALUE " ÉÍÍ
      -            " ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ» ".
               05  LINE 09 COLUMN 08 VALUE " º
      -            "                                   º ".
               05  LINE 10 COLUMN 08 VALUE " º
      -            "                                   º ".
               05  LINE 11 COLUMN 08 VALUE " º
      -            "                                   º ".
               05  LINE 12 COLUMN 08 VALUE " º
      -            "                                   º ".
               05  LINE 13 COLUMN 08 VALUE " º
      -            "                                   º ".
               05  LINE 14 COLUMN 08 VALUE " º
      -            "                                   º ".
               05  LINE 15 COLUMN 08 VALUE " º
      -            "                                   º ".
               05  LINE 16 COLUMN 08 VALUE " º
      -            "                                   º ".
               05  LINE 17 COLUMN 08 VALUE " º
      -            "                                   º ".
               05  LINE 18 COLUMN 08 VALUE " º
      -            "                                   º ".
               05  LINE 19 COLUMN 08 VALUE " º
      -            "                                   º ".
               05  LINE 20 COLUMN 08 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -            "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 08 COLUMN 14 VALUE " Vales Frete Diversos ".
           03  BCK-CLR 00 FOR-CLR 08.
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
               05  LINE 21 COLUMN 10 PIC X(65) FROM SPACES.

       01  TELA-03.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 12 PIC X(56) FROM ALL "ß".
               05  LINE 16 COLUMN 12 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 00.
               05  LINE 16 COLUMN 13 PIC X(55) FROM ALL "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 10 COLUMN 12 PIC X(56) FROM
                   "  Data do Cr‚dito .:".
               05  LINE 11 COLUMN 12 PIC X(56) FROM SPACES.
               05  LINE 12 COLUMN 12 PIC X(56) FROM
                   "  N£mero Documento :".
               05  LINE 13 COLUMN 12 PIC X(56) FROM
                   "  Nome do Emitente :".
               05  LINE 14 COLUMN 12 PIC X(56) FROM SPACES.
               05  LINE 15 COLUMN 12 PIC X(56) FROM
                   "  Valor do Cr‚dito :".
           03  BCK-CLR 00 FOR-CLR 07.
               05  LINE 10 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 11 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 12 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 13 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 14 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 15 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 16 COLUMN 68 PIC X(01) FROM " ".

       01  TELA-04.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 09 COLUMN 12 PIC X(56) FROM ALL "ß".
               05  LINE 16 COLUMN 12 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 00.
               05  LINE 16 COLUMN 13 PIC X(55) FROM ALL "Ü".
           03  BCK-CLR 05 FOR-CLR 14.
               05  LINE 10 COLUMN 12 PIC X(56) FROM SPACES.
               05  LINE 11 COLUMN 12 PIC X(56) FROM SPACES.
               05  LINE 12 COLUMN 12 PIC X(56) FROM SPACES.
               05  LINE 13 COLUMN 12 PIC X(56) FROM SPACES.
               05  LINE 14 COLUMN 12 PIC X(56) FROM SPACES.
               05  LINE 15 COLUMN 12 PIC X(56) FROM SPACES.
           03  BCK-CLR 00 FOR-CLR 07.
               05  LINE 10 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 11 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 12 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 13 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 14 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 15 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 16 COLUMN 68 PIC X(01) FROM " ".

       01  TELA-05.
           03  BCK-CLR 01 FOR-CLR 07.
               05  LINE 17 COLUMN 12 PIC X(56) FROM ALL "ß".
               05  LINE 19 COLUMN 12 PIC X(01) FROM     "Ü".
           03  BCK-CLR 01 FOR-CLR 00.
               05  LINE 19 COLUMN 13 PIC X(55) FROM ALL "Ü".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 18 COLUMN 12 PIC X(56) FROM
                   "  Vales Lan‡ados ..:       Valor Total R$".
               05  LINE 18 COLUMN 33 PIC ZZ9          FROM ACM-QTD.
               05  LINE 18 COLUMN 54 PIC Z.ZZZ.ZZ9,99 FROM ACM-VAL.
           03  BCK-CLR 00 FOR-CLR 07.
               05  LINE 18 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 19 COLUMN 68 PIC X(01) FROM " ".

       01  TELA-06.
           03  BCK-CLR 04 FOR-CLR 07.
               05  LINE 17 COLUMN 12 PIC X(56) FROM ALL "ß".
               05  LINE 19 COLUMN 12 PIC X(01) FROM     "Ü".
           03  BCK-CLR 04 FOR-CLR 00.
               05  LINE 19 COLUMN 13 PIC X(55) FROM ALL "Ü".
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 18 COLUMN 12 PIC X(56) FROM MSG-AUX.
           03  BCK-CLR 00 FOR-CLR 07.
               05  LINE 18 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 19 COLUMN 68 PIC X(01) FROM " ".

       01  TELA-07.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 12 PIC X(56) FROM ALL "ß".
               05  LINE 11 COLUMN 12 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 00.
               05  LINE 11 COLUMN 13 PIC X(55) FROM ALL "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 10 COLUMN 12 PIC X(56) FROM
                   "  Data do Cr‚dito .:".
           03  BCK-CLR 00 FOR-CLR 07.
               05  LINE 10 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 11 COLUMN 68 PIC X(01) FROM " ".
           03  TELA-08.
               05  BCK-CLR 05 FOR-CLR 07.
                   07  LINE 12 COLUMN 12 PIC X(56) FROM ALL "ß".
                   07  LINE 19 COLUMN 12 PIC X(01) FROM     "Ü".
               05  BCK-CLR 05 FOR-CLR 00.
                   07  LINE 19 COLUMN 13 PIC X(55) FROM ALL "Ü".
               05  BCK-CLR 05 FOR-CLR 14.
                   07  LINE 13 COLUMN 12 PIC X(56) FROM SPACES.
                   07  LINE 14 COLUMN 12 PIC X(56) FROM SPACES.
                   07  LINE 15 COLUMN 12 PIC X(56) FROM SPACES.
                   07  LINE 16 COLUMN 12 PIC X(56) FROM SPACES.
                   07  LINE 17 COLUMN 12 PIC X(56) FROM SPACES.
                   07  LINE 18 COLUMN 12 PIC X(56) FROM SPACES.
               05  BCK-CLR 00 FOR-CLR 07.
                   07  LINE 13 COLUMN 68 PIC X(01) FROM " ".
                   07  LINE 14 COLUMN 68 PIC X(01) FROM " ".
                   07  LINE 15 COLUMN 68 PIC X(01) FROM " ".
                   07  LINE 16 COLUMN 68 PIC X(01) FROM " ".
                   07  LINE 17 COLUMN 68 PIC X(01) FROM " ".
                   07  LINE 18 COLUMN 68 PIC X(01) FROM " ".
                   07  LINE 19 COLUMN 68 PIC X(01) FROM " ".

       01  ENTR-01     AUTO.
           03  NUM-E01 LINE 12 COLUMN 33 PIC ZZZZZ9     USING NUM-LCR.
           03  DES-E01 LINE 13 COLUMN 33 PIC X(25)      USING DES-LCR.
           03  VAL-E01 LINE 15 COLUMN 33 PIC ZZZ.ZZ9,99 USING VAL-LCR.

       01  ENTR-02 BCK-CLR 02 FOR-CLR 15.
           02  NUM-E02 LINE 12 COLUMN 33 PIC 9(06)      FROM NUM-LCR.
           02  DES-E02 LINE 13 COLUMN 33 PIC X(25)      FROM DES-LCR.
           02  VAL-E02 LINE 15 COLUMN 33 PIC ZZZ.ZZ9,99 FROM VAL-LCR.

       01  ENTR-03     AUTO.
           03  DES-E03 LINE IND4 COLUMN 30 PIC X(25)      USING NOM-AX1.
           03  VAL-E03 LINE IND4 COLUMN 56 PIC ZZZ.ZZ9,99 USING VAL-LCT.

       01  ENTR-04  BCK-CLR 07 FOR-CLR 12.
           03  DES-E04 LINE IND4 COLUMN 30 PIC X(25)      FROM NOM-AX1.
           03  VAL-E04 LINE IND4 COLUMN 56 PIC ZZZ.ZZ9,99 FROM VAL-LCT.

       01  ENTR-05  BCK-CLR 04 FOR-CLR 15
                    LINE 18 COLUMN 58 PIC X(01) USING RSP-OPC AUTO.

       01  LIMP-01 BCK-CLR 02.
           03  LINE 12 COLUMN 33 PIC X(06) FROM SPACES.
           03  LINE 13 COLUMN 33 PIC X(25) FROM SPACES.
           03  LINE 15 COLUMN 33 PIC X(10) FROM SPACES.

       01  MENU-01 BCK-CLR 05 FOR-CLR 15.
           03  LINE LIN1 COLUMN 13 PIC X(54) FROM REG-TAB (IND2).
           03  LINE PLUS COLUMN 13 PIC X(54) FROM REG-TAB (IND2 + 1).
           03  LINE PLUS COLUMN 13 PIC X(54) FROM REG-TAB (IND2 + 2).
           03  LINE PLUS COLUMN 13 PIC X(54) FROM REG-TAB (IND2 + 3).
           03  LINE PLUS COLUMN 13 PIC X(54) FROM REG-TAB (IND2 + 4).
           03  LINE PLUS COLUMN 13 PIC X(54) FROM REG-TAB (IND2 + 5).

       01  MENU-02  BCK-CLR 05.
           03  TRC-ACM LINE LIN1 COLUMN 67 VALUE " ".
           03  TRC-ABX LINE LIN2 COLUMN 67 VALUE " ".
           03  STA-ACM LINE LIN1 COLUMN 67 VALUE "" FOR-CLR 14.
           03  STA-ABX LINE LIN2 COLUMN 67 VALUE "" FOR-CLR 14.

       01  SETA-01.
           03  BCK-CLR 07 FOR-CLR 12
               LINE 10 COLUMN 34 PIC X(22) FROM OPC-TAB (1).
           03  BCK-CLR 02 FOR-CLR 07
               LINE 11 COLUMN 34 PIC X(22) FROM OPC-TAB (2).

       01  SETA-02.
           03  BCK-CLR 02 FOR-CLR 07
               LINE 10 COLUMN 34 PIC X(22) FROM OPC-TAB (1).
           03  BCK-CLR 07 FOR-CLR 12
               LINE 11 COLUMN 34 PIC X(22) FROM OPC-TAB (2).

       01  SETA-03  BCK-CLR 05 FOR-CLR 15.
           03  LINE IND4 COLUMN 13 PIC X(54) FROM REG-TAB (IND3).

       01  SETA-04  BCK-CLR 07 FOR-CLR 12.
           03  LINE IND4 COLUMN 13 PIC X(54) FROM REG-TAB (IND3).

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           MOVE     1   TO   NUM-OPC  DISPLAY  SETA-01.

       ROT-MENU-00.
           MOVE  1  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-00.
           IF  TCL-MSG = 5 OR 6  PERFORM  ROT-MENU-10
                                 GO       ROT-MENU-00.
           IF  TCL-MSG   >   00  GO       ROT-MENU-00.
           IF  HEX-MSG   =   27  GOBACK.
           PERFORM  ROT-GTLA-00  GO       RT1-PESQ-00,
                    RT2-PESQ-00, DEPENDING ON NUM-OPC.
       ROT-MENU-10.
           IF  NUM-OPC   =    1  MOVE  2  TO  NUM-OPC
                                 DISPLAY      SETA-02
           ELSE
               MOVE      1   TO  NUM-OPC
               DISPLAY           SETA-01.

       RT1-PESQ-00.
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           DISPLAY  TELA-02      TELA-05
           OPEN     I-O          CADCRD   CADLCR
                    INPUT        CADCTR   READ     CADCTR
           COMPUTE  DTA-LIM   =  10000 +  DTD-CTR.

       RT1-PESQ-10.
           MOVE     SPACES   TO  CHV-LCR
           MOVE     6        TO  PRX-LCR
           START    CADLCR       KEY  >   CHV-LCR
           IF  FST-LCR   =  "23" GO       ROT-DATA-00.
           READ     CADLCR  NEXT
           IF  PRX-LCR   >    6  GO       ROT-DATA-00.
           DISPLAY  TELA-04      GO       RT1-EXBE-00.

       RT1-ENTR-00.
           MOVE     SPACES   TO  DES-LCR
           MOVE     6        TO  PRX-LCR
           MOVE     ZEROS    TO  NUM-LCR
                                 LCT-LCR  VAL-LCR
           ACCEPT   NUM-E01      DISPLAY  NUM-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-DATA-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-ENTR-00.
           IF  NUM-LCR   =    0  GO       RT1-ENTR-00.

       RT1-ENTR-10.
           ACCEPT   DES-E01      DISPLAY  DES-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  RT1-ENTR-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-ENTR-10.
           IF  DES-LCR   =       SPACES
                                 GO       RT1-ENTR-10.
       RT1-ENTR-20.
           ACCEPT   VAL-E01      DISPLAY  VAL-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-ENTR-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-ENTR-20.
           IF  VAL-LCR   =    0  GO       RT1-ENTR-20.
           MOVE     DTA-DTA  TO  AUX-LCR
           MOVE     ZEROS    TO  LCT-LCR
           ADD      1        TO  ACM-QTD
           ADD      VAL-LCR  TO  ACM-VAL
           DISPLAY  LIMP-01      TELA-05.

       RT1-ENTR-30.
           WRITE    REG-LCR
           IF  FST-LCR   =  "00" GO       RT1-ENTR-00.
           ADD  1   TO  LCT-LCR  GO       RT1-ENTR-30.

       RT1-EXBE-00.
           MOVE     00  TO  IND1 MOVE     1  TO  IND2
           MOVE     10  TO  IND4 MOVE     1  TO  IND3
           MOVE     10  TO  LIN1 MOVE    15  TO  LIN2.

       RT1-EXBE-10.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1   <     300  GO       RT1-EXBE-10.
           MOVE     SPACES   TO  CHV-LCR
           MOVE     6        TO  PRX-LCR
           MOVE     ZEROS    TO  ACM-QTD
                                 ACM-VAL  IND1.
       RT1-EXBE-20.
           START    CADLCR       KEY  >   CHV-LCR
           IF  FST-LCR   =  "23" GO       RT1-EXBE-30.
           READ     CADLCR  NEXT
           IF  PRX-LCR   >    6  GO       RT1-EXBE-30.
           ADD      VAL-LCR  TO  ACM-VAL
           MOVE     AUX-LCR  TO  DTA-DET
           MOVE     DIA-DET  TO  DIA-AX1
           MOVE     MES-DET  TO  MES-AX1
           MOVE     ANO-DET  TO  ANO-AX1
           MOVE     NUM-LCR  TO  NUM-AX1
           MOVE     DES-LCR  TO  NOM-AX1
           MOVE     VAL-LCR  TO  VAL-AX1
           ADD      1        TO  ACM-QTD  IND1
           MOVE     REG-AX1  TO  REG-TAB (IND1)
           MOVE     CHV-LCR  TO  CH1-TAB (IND1)
           DISPLAY  TELA-05      GO       RT1-EXBE-20.

       RT1-EXBE-30.
           IF (IND2 + 5) < IND1  DISPLAY  STA-ABX
               IF  IND2    >  1  DISPLAY  STA-ACM
                   ELSE          DISPLAY  TRC-ACM
           ELSE    IF  IND2 > 1  DISPLAY  STA-ACM  TRC-ABX
                       ELSE      DISPLAY  TRC-ACM.
           DISPLAY  MENU-01      SETA-04.

       RT1-EXBE-40.
           MOVE  5  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       RT1-EXBE-40.
           IF  TCL-MSG   =   05  GO       RT1-EXBE-50.
           IF  TCL-MSG   =   06  GO       RT1-EXBE-60.
           IF  TCL-MSG   =   53  GO       RT1-EXBE-70.
           IF  TCL-MSG   =   54  GO       RT1-EXBE-80.
           IF  TCL-MSG   =   17  GO       RT1-EXCL-00.
           IF  TCL-MSG   =   23  GO       ROT-DATA-00.
           IF  TCL-MSG   >   00  GO       RT1-EXBE-40.
           IF  HEX-MSG   =   13  GO       RT1-ALTR-00.
           GO       RT1-CONF-00.

       RT1-EXBE-50.
           IF  IND3      =   01  GO       RT1-EXBE-40.
           IF  IND4      =   10  SUBTRACT 1 FROM IND2
                                 SUBTRACT 1 FROM IND3
                                 GO       RT1-EXBE-30.
           DISPLAY  SETA-03      SUBTRACT 1 FROM IND3
           COMPUTE  IND4     =   IND4  -  1
           DISPLAY  SETA-04      GO       RT1-EXBE-40.

       RT1-EXBE-60.
           IF  IND3      =  IND1 GO       RT1-EXBE-40.
           IF  IND4      =   15  ADD      1  TO  IND2
                                 ADD      1  TO  IND3
                                 GO       RT1-EXBE-30.
           DISPLAY  SETA-03      ADD      1  TO  IND3
           COMPUTE  IND4     =   IND4  +  1
           DISPLAY  SETA-04      GO       RT1-EXBE-40.

       RT1-EXBE-70.
           IF  IND2      =   01  GO       RT1-EXBE-40.
           MOVE     10       TO  IND4
           IF  IND2      >   10  COMPUTE  IND3 = IND2 - 06
                                 COMPUTE  IND2 = IND2 - 06
           ELSE
                                 MOVE     1  TO  IND2   IND3.
           GO      RT1-EXBE-30.

       RT1-EXBE-80.
           IF  IND1      <      (IND2  +  06)
                                 GO       RT1-EXBE-40.
           MOVE     10       TO  IND4
           COMPUTE  IND3     =   IND2  +  06
           COMPUTE  IND2     =   IND2  +  06
           GO       RT1-EXBE-30.

       RT1-EXCL-00.
           MOVE     CH1-TAB      (IND3)   TO  CHV-LCR
           READ     CADLCR       MOVE 20  TO  IND-MSG
           IF  FST-LCR   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       RT1-EXBE-40.
           DELETE   CADLCR
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           DISPLAY  TELA-05      GO       RT1-PESQ-10.

       RT1-ALTR-00.
           MOVE     CH1-TAB      (IND3)   TO  CHV-LCR
           MOVE     REG-TAB      (IND3)   TO  REG-AX1
           READ     CADLCR       MOVE 20  TO  IND-MSG
           IF  FST-LCR   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       RT1-EXBE-40.
           MOVE     AUX-LCR  TO  DTA-DTA
           MOVE     IND4     TO  LIN-DTA
           MOVE     21       TO  COL-DTA
           MOVE     07       TO  BCK-DTA
           MOVE     04       TO  FOR-DTA
           MOVE     DES-LCR  TO  NOM-AX1
           MOVE     VAL-LCR  TO  VAL-LCT.

       RT1-ALTR-10.
           PERFORM  ROT-DATA-20  MOVE 18  TO  IND-MSG
           IF  FLG-DTA   =    0  DISPLAY  SETA-04
                                 GO       RT1-EXBE-40.
           IF  DTA-DTA   >       DTD-CTR
               AND   <  DTA-LIM  GO       RT1-ALTR-20.
           PERFORM  ROT-MSGM-00  GO       RT1-ALTR-10.

       RT1-ALTR-20.
           ACCEPT   DES-E03      DISPLAY  DES-E04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-ALTR-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-ALTR-20.
           IF  NOM-AX1   =       SPACES
                                 GO       RT1-ALTR-20.
       RT1-ALTR-30.
           ACCEPT   VAL-E03      DISPLAY  VAL-E04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-ALTR-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-ALTR-30.
           IF  VAL-LCT   =    0  GO       RT1-ALTR-30.
           COMPUTE  ACM-VAL   =  ACM-VAL  -
                                 VAL-LCR  +   VAL-LCT
           MOVE     DTA-DTA  TO  AUX-LCR  DTA-DET
           MOVE     DIA-DET  TO  DIA-AX1
           MOVE     MES-DET  TO  MES-AX1
           MOVE     ANO-DET  TO  ANO-AX1
           MOVE     NOM-AX1  TO  DES-LCR
           MOVE     VAL-LCT  TO  VAL-AX1  VAL-LCR
           MOVE     REG-AX1  TO  REG-TAB  (IND3)
           REWRITE  REG-LCR
           DISPLAY  TELA-05      GO       RT1-EXBE-40.

       RT1-CONF-00.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     MG1-AUX  TO  MSG-AUX
           DISPLAY  TELA-06      ACCEPT   ENTR-05
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-RTRN-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-CONF-00.
           IF  RSP-OPC  NOT   =  "S"  GO  RT1-CONF-00.
           MOVE     MG2-AUX  TO  MSG-AUX
           MOVE     SPACES   TO  CHV-LCR
           DISPLAY  TELA-06      OPEN     I-O  CADSDO
           MOVE     1        TO  PRX-SDO
           MOVE     6        TO  SUF-SDO  PRX-LCR.

       RT1-CONF-10.
           READ     CADSDO       MOVE 20  TO  IND-MSG
           IF  FST-SDO   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       RT1-CONF-10.
       RT1-CONF-20.
           START    CADLCR       KEY  >   CHV-LCR
           IF  FST-LCR   =  "23" GO       RT1-CONF-50.
           READ     CADLCR  NEXT
           IF  PRX-LCR   >    6  GO       RT1-CONF-50.
           MOVE     6        TO  PRX-CRD
           MOVE     AUX-LCR  TO  DTA-CRD
           MOVE     NUM-LCR  TO  NUM-CRD
           MOVE     DES-LCR  TO  NOM-CRD
           MOVE     VAL-LCR  TO  VAL-CRD
           ADD      VAL-LCR  TO  ENT-SDO
           MOVE     ZEROS    TO  LCT-CRD.

       RT1-CONF-30.
           WRITE    REG-CRD      ADD   1  TO  LCT-CRD
           IF  FST-CRD   =  "22" GO       RT1-CONF-30.

       RT1-CONF-40.
           DELETE   CADLCR       MOVE 20  TO  IND-MSG
           IF  FST-LCR   =  "00" GO       RT1-CONF-20.
           PERFORM  ROT-MSGM-00  GO       RT1-CONF-40.

       RT1-CONF-50.
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           REWRITE  REG-SDO      CLOSE    CADSDO
           DISPLAY  TELA-05      GO       ROT-DATA-00.

       RT1-RTRN-00.
           IF  ACM-QTD   >    0  DISPLAY  TELA-04
                                 GO       RT1-EXBE-00.
       RT1-RTRN-10.
           CLOSE    CADCRD       CADCTR   CADLCR
           PERFORM  ROT-RTLA-00  GO       ROT-MENU-00.

       RT2-PESQ-00.
           OPEN     I-O          CADCRD   CADSDO
           DISPLAY  TELA-02      GO       ROT-DATA-00.

       RT2-PESQ-10.
           MOVE     31       TO  IND-MSG
           MOVE     SPACES   TO  CHV-CRD
           MOVE     6        TO  PRX-CRD
           MOVE     DTA-DTA  TO  DTA-CRD
           START    CADCRD       KEY  >   CHV-CRD
           IF  FST-CRD   =  "23" GO       RT2-PESQ-20.
           READ     CADCRD  NEXT
           IF  PRX-CRD   =    6  AND      DTA-CRD
               =    DTA-DTA      GO       RT2-PESQ-30.

       RT2-PESQ-20.
           PERFORM  ROT-MSGM-00  GO       ROT-DATA-00.

       RT2-PESQ-30.
           MOVE     00  TO  IND1 MOVE     1  TO  IND2
           MOVE     13  TO  IND4 MOVE     1  TO  IND3.
           MOVE     13  TO  LIN1 MOVE    18  TO  LIN2.

       RT2-PESQ-40.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1   <     300  GO       RT2-PESQ-40.
           MOVE     ZEROS    TO  IND1
           MOVE     SPACES   TO  CHV-CRD
           MOVE     6        TO  PRX-CRD
           MOVE     DTA-DTA  TO  DTA-CRD.

       RT2-PESQ-50.
           START    CADCRD       KEY  >   CHV-CRD
           IF  FST-CRD   =  "23" GO       RT2-PESQ-60.
           READ     CADCRD  NEXT
           IF  PRX-CRD   >    6  OR       DTA-CRD
               >    DTA-DTA      GO       RT2-PESQ-60.
           MOVE     NUM-CRD  TO  NUM-AX2
           MOVE     NOM-CRD  TO  NOM-AX2
           MOVE     VAL-CRD  TO  VAL-AX2
           ADD      1        TO  IND1
           MOVE     REG-AX2  TO  REG-TAB (IND1)
           MOVE     CHV-CRD  TO  CH2-TAB (IND1)
           GO       RT2-PESQ-50.

       RT2-PESQ-60.
           IF (IND2 + 5) < IND1  DISPLAY  STA-ABX
               IF  IND2    >  1  DISPLAY  STA-ACM
                   ELSE          DISPLAY  TRC-ACM
           ELSE    IF  IND2 > 1  DISPLAY  STA-ACM  TRC-ABX
                       ELSE      DISPLAY  TRC-ACM.
           DISPLAY  MENU-01      SETA-04.

       RT2-PESQ-70.
           MOVE  7  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       RT2-PESQ-60.
           IF  TCL-MSG   =   05  GO       RT2-MOVE-05.
           IF  TCL-MSG   =   06  GO       RT2-MOVE-06.
           IF  TCL-MSG   =   53  GO       RT2-MOVE-53.
           IF  TCL-MSG   =   54  GO       RT2-MOVE-54.
           IF  TCL-MSG   =   17  GO       RT2-TEST-00.
           IF  TCL-MSG   >   00  GO       RT2-PESQ-70.
           IF  HEX-MSG   =   27  GO       ROT-DATA-00.
           PERFORM  RT2-TEST-00  GO       RT2-ALTR-00.

       RT2-RTRN-00.
           CLOSE    CADCRD        CADSDO
           PERFORM  ROT-RTLA-00   GO       ROT-MENU-00.

       RT2-MOVE-05.
           IF  IND3      =   01  GO       RT2-PESQ-70.
           IF  IND4      =   13  SUBTRACT 1 FROM IND2
                                 SUBTRACT 1 FROM IND3
                                 GO       RT2-PESQ-60.
           DISPLAY  SETA-03      SUBTRACT 1 FROM IND3
           COMPUTE  IND4     =   IND4  -  1
           DISPLAY  SETA-04      GO       RT2-PESQ-70.

       RT2-MOVE-06.
           IF  IND3      =  IND1 GO       RT2-PESQ-70.
           IF  IND4      =   18  ADD      1  TO  IND2
                                 ADD      1  TO  IND3
                                 GO       RT2-PESQ-60.
           DISPLAY  SETA-03      ADD      1  TO  IND3
           COMPUTE  IND4     =   IND4  +  1
           DISPLAY  SETA-04      GO       RT2-PESQ-70.

       RT2-MOVE-53.
           IF  IND2      =   01  GO       RT2-PESQ-70.
           MOVE     13       TO  IND4
           IF  IND2      >   10  COMPUTE  IND3 = IND2 - 06
                                 COMPUTE  IND2 = IND2 - 06
           ELSE
                                 MOVE     1  TO  IND2   IND3.
           GO      RT2-PESQ-60.

       RT2-MOVE-54.
           IF  IND1      <      (IND2  +  06)
                                 GO       RT2-PESQ-70.
           MOVE     13       TO  IND4
           COMPUTE  IND3     =   IND2  +  06
           COMPUTE  IND2     =   IND2  +  06
           GO       RT2-PESQ-60.

       RT2-TEST-00.
           MOVE     CH2-TAB      (IND3)   TO  CHV-CRD
           READ     CADCRD       MOVE 20  TO  IND-MSG
           IF  FST-CRD   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       RT2-PESQ-70.
           MOVE     1        TO  PRX-SDO
           MOVE     6        TO  SUF-SDO
           READ     CADSDO       MOVE 20  TO  IND-MSG
           IF  FST-SDO   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       RT2-PESQ-70.
       RT2-EXCL-00.
           COMPUTE  COR-SDO   =  COR-SDO  -   VAL-CRD
           DELETE   CADCRD       REWRITE  REG-SDO
           DISPLAY  TELA-08      GO       RT2-PESQ-10.

       RT2-ALTR-00.
           MOVE     VAL-CRD  TO  VAL-LCT
           MOVE     REG-TAB      (IND3)   TO  REG-AX2.

       RT2-ALTR-10.
           ACCEPT   VAL-E03      DISPLAY  VAL-E04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  SETA-04
                                 GO       RT2-PESQ-70.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT2-ALTR-10.
           IF  VAL-LCT   =    0  GO       RT2-ALTR-10.
           COMPUTE  COR-SDO   =  COR-SDO
                              +  VAL-LCT  -   VAL-CRD
           MOVE     VAL-LCT  TO  VAL-AX2  VAL-CRD
           MOVE     REG-AX2  TO  REG-TAB  (IND3)
           REWRITE  REG-SDO
           REWRITE  REG-CRD      GO       RT2-PESQ-70.

       ROT-DATA-00.
           IF  NUM-OPC   =    1  DISPLAY  TELA-03.
           IF  NUM-OPC   =    2  DISPLAY  TELA-07.

       ROT-DATA-10.
           MOVE     10       TO  LIN-DTA
           MOVE     33       TO  COL-DTA
           MOVE     02       TO  BCK-DTA
           MOVE     07       TO  FOR-DTA
           MOVE     ZEROS    TO  DTA-DTA
           PERFORM  ROT-DATA-20  MOVE 18  TO  IND-MSG
           IF  FLG-DTA   =    0  GO       RT1-RTRN-00,
                                          RT2-RTRN-00,
                                 DEPENDING ON NUM-OPC.
           IF  NUM-OPC   =    2  GO       RT2-PESQ-10.
           IF  DTA-DTA   >       DTD-CTR
               AND   <  DTA-LIM  GO       RT1-ENTR-00.
           PERFORM  ROT-MSGM-00  GO       ROT-DATA-00.

       ROT-DATA-20.
           CALL   "DYDATA"  USING  PRM-DTA
                                   PRM-HLP.
       ROT-HELP-00.
           CALL   "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING  PRM-MSG.

       ROT-GTLA-00.
           MOVE   "G"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE   "R"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.
