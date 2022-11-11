       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1521.
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

           SELECT      CADLCR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCR
                       FILE          STATUS  IS  FST-LCR.

           SELECT      CADNTA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTA
                       FILE          STATUS  IS  FST-NTA.

           SELECT      CADSDO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SDO
                       FILE          STATUS  IS  FST-SDO.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCLI.CPY.
       COPY  CADLCR.CPY.
       COPY  CADNTA.CPY.
       COPY  CADSDO.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-CLI          PIC  X(02).
           03  FST-LCR          PIC  X(02).
           03  FST-NTA          PIC  X(02).
           03  FST-SDO          PIC  X(02).

           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  IND3             PIC  9(03).
           03  IND4             PIC  9(03).
           03  LIN1             PIC  9(02).
           03  LIN2             PIC  9(02).
           03  COL1             PIC  9(02).
           03  COL2             PIC  9(02).

           03  TECLADO          PIC  9(02).
           03  NUM-OPC          PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  MSG-AUX          PIC  X(56).
           03  ACM-PSQ          PIC  9(02).
           03  ACM-QTD          PIC  9(03).
           03  VAL-LCT          PIC  9(06)V99.
           03  ACM-VAL          PIC  9(07)V99.

           03  PCA-AUX.
               05  PC1-AUX      PIC  X(03).
               05  PC2-AUX      PIC  9(04).

           03  TAB-OPC          PIC  X(44)  VALUE
                   " Lan‡amento Movimento  Corre‡Æo Lan‡amentos ".
           03  RED-OPC   REDEFINES   TAB-OPC.
               05  OPC-TAB      PIC  X(22)  OCCURS  02.

           03  MG1-AUX          PIC  X(56)  VALUE
               "     DATA DE ENTRADA DAS NOTAS LAN€ADAS .:".
           03  MG2-AUX.
               05  FILLER       PIC  X(40)  VALUE
                   "  DESEJA CONFIRMAR AS NOTAS LAN€ADAS EM".
               05  DIA-MG2      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-MG2      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-MG2      PIC  9(02).
               05  FILLER       PIC  X(08)  VALUE  " ? [ ]".
           03  MG3-AUX          PIC  X(56)  VALUE
               "       AGUARDE, ATUALIZANDO O ARQUIVO DE NOTAS ...".

           03  REG-AX1.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  NUM-AX1      PIC  9(06).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  CLI-AX1      PIC  X(15).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  DES-AX1      PIC  X(19).
               05  VAL-AX1      PIC  ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  REG-AX2.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  NUM-AX2      PIC  9(06).
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  PC1-AX2      PIC  X(03).
               05  FILLER       PIC  X(01)  VALUE  "-".
               05  PC2-AX2      PIC  X(04).
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  DES-AX2      PIC  X(22).
               05  VAL-AX2      PIC  ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  NOM-PSQ          PIC  X(25).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  25.

           03  TAB-REG          OCCURS 300.
               05  REG-TAB      PIC  X(54).
               05  CH1-TAB      PIC  X(10).
               05  CH2-TAB      PIC  X(20).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  39.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1521".
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
           03  FOR-DTA          PIC  9(01)  VALUE  7.
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
                   " ÉÍÍÍÍ Requisi‡äes ÍÍÍÍÍ» ".
               05  LINE 10 COLUMN 31 VALUE
                   " º                        º ".
               05  LINE 11 COLUMN 31 VALUE
                   " º                        º ".
               05  LINE 12 COLUMN 31 VALUE
                   " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 09 COLUMN 38 VALUE " Requisi‡äes ".
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
               05  LINE 08 COLUMN 14 VALUE " Requisi‡äes Empresas ".
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
                   "  C¢digo Empresa :        -".
               05  LINE 11 COLUMN 12 PIC X(56) FROM SPACES.
               05  LINE 12 COLUMN 12 PIC X(56) FROM
                   "  N£mero da Nota :           Placa Ve¡culo .:    -".
               05  LINE 13 COLUMN 12 PIC X(56) FROM
                   "  Descri‡Æo .....:".
               05  LINE 14 COLUMN 12 PIC X(56) FROM SPACES.
               05  LINE 15 COLUMN 12 PIC X(56) FROM
                   "  Valor da Nota .:".
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
                   "  Notas Lan‡adas :         Valor Total R$".
               05  LINE 18 COLUMN 31 PIC ZZ9          FROM ACM-QTD.
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
               05  LINE 10 COLUMN 12 PIC X(56) FROM SPACES.
               05  LINE 10 COLUMN 24 PIC X(16) FROM "Empresa:       -".
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
           03  CHV-E01 LINE 10 COLUMN COL1 PIC ZZZZ9    USING CHV-CLI.
           03  PSQ-E01 LINE 10 COLUMN COL2 PIC X(25)    USING NOM-PSQ.
           03  NUM-E01 LINE 12 COLUMN 31 PIC ZZZZZ9     USING NUM-LCR.
           03  PC1-E01 LINE 12 COLUMN 58 PIC X(03)      USING PC1-AUX.
           03  PC2-E01 LINE 12 COLUMN 62 PIC 9(04)      USING PC2-AUX.
           03  DES-E01 LINE 13 COLUMN 31 PIC X(18)      USING HST-LCR.
           03  VAL-E01 LINE 15 COLUMN 31 PIC ZZZ.ZZ9,99 USING VAL-LCR.

       01  ENTR-02  BCK-CLR 02 FOR-CLR 15.
           02  CHV-E02 LINE 10 COLUMN COL1 PIC 9(05)    FROM CHV-CLI.
           02  PSQ-E02 LINE 10 COLUMN COL2 PIC X(25)    FROM NOM-PSQ.
           02  NOM-E02 LINE 10 COLUMN COL2 PIC X(25)    FROM NOM-CLI.
           02  NUM-E02 LINE 12 COLUMN 31 PIC 9(06)      FROM NUM-LCR.
           02  PC1-E02 LINE 12 COLUMN 58 PIC X(03)      FROM PC1-AUX.
           02  PC2-E02 LINE 12 COLUMN 62 PIC 9(04)      FROM PC2-AUX.
           02  DES-E02 LINE 13 COLUMN 31 PIC X(18)      FROM HST-LCR.
           02  VAL-E02 LINE 15 COLUMN 31 PIC ZZZ.ZZ9,99 FROM VAL-LCR.

       01  ENTR-03     AUTO.
           03  DES-E03 LINE IND4 COLUMN 37 PIC X(18)      USING DES-AX1.
           03  VAL-E03 LINE IND4 COLUMN 56 PIC ZZZ.ZZ9,99 USING VAL-LCT.

       01  ENTR-04  BCK-CLR 07  FOR-CLR 12.
           03  DES-E04 LINE IND4 COLUMN 37 PIC X(18)      FROM DES-AX1.
           03  VAL-E04 LINE IND4 COLUMN 56 PIC ZZZ.ZZ9,99 FROM VAL-LCT.

       01  ENTR-05  BCK-CLR 04 FOR-CLR 15
                    LINE 18 COLUMN 64 PIC X(01) USING RSP-OPC AUTO.

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
           MOVE     1  TO    NUM-OPC  DISPLAY  SETA-01.

       ROT-MENU-00.
           MOVE  1  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-00.
           IF  TCL-MSG = 5 OR 6  PERFORM  ROT-MENU-10
                                 GO       ROT-MENU-00.
           IF  TCL-MSG   >   00  GO       ROT-MENU-00.
           IF  HEX-MSG   =   27  GOBACK.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-02
           GO       RT1-PESQ-00, RT2-ENTR-00,
                                 DEPENDING ON NUM-OPC.
       ROT-MENU-10.
           IF  NUM-OPC   =    1  MOVE  2  TO  NUM-OPC
                                 DISPLAY      SETA-02
           ELSE
               MOVE      1   TO  NUM-OPC
               DISPLAY           SETA-01.

       RT1-PESQ-00.
           MOVE     31       TO  COL1
           MOVE     41       TO  COL2
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           DISPLAY  TELA-05
           OPEN     INPUT        CADCLI
                    I-O          CADLCR   CADNTA.
       RT1-PESQ-10.
           MOVE     SPACES   TO  CHV-LCR
           MOVE     1        TO  PRX-LCR
           START    CADLCR       KEY  >   CHV-LCR
           IF  FST-LCR   =  "23" GO       ROT-PESQ-00.
           READ     CADLCR  NEXT
           IF  PRX-LCR   >    1  GO       ROT-PESQ-00.
           DISPLAY  TELA-04      GO       RT1-EXBE-00.

       RT1-INCL-00.
           MOVE     1        TO  PRX-LCR
           MOVE     SPACES   TO  HST-LCR  PC1-AUX
           MOVE     ZEROS    TO  NUM-LCR  PC2-AUX
                                 LCT-LCR  VAL-LCR.
       RT1-INCL-10.
           ACCEPT   NUM-E01      DISPLAY  NUM-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-PESQ-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-INCL-10.
           IF  NUM-LCR   =    0  GO       RT1-INCL-10.
           READ     CADLCR       MOVE 37  TO  IND-MSG
           IF  FST-LCR   =  "00" PERFORM  ROT-MSGM-00
                                 GO       RT1-INCL-10.
       RT1-INCL-20.
           ACCEPT   PC1-E01      DISPLAY  PC1-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-INCL-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-INCL-20.
       RT1-INCL-30.
           ACCEPT   PC2-E01      DISPLAY  PC2-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-INCL-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-INCL-30.
       RT1-INCL-40.
           ACCEPT   DES-E01      DISPLAY  DES-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-INCL-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-INCL-40.
           IF  HST-LCR   =       SPACES
                                 GO       RT1-INCL-40.
       RT1-INCL-50.
           ACCEPT   VAL-E01      DISPLAY  VAL-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-INCL-40.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-INCL-50.
           IF  VAL-LCR   =    0  GO       RT1-INCL-50.
           MOVE     CHV-CLI  TO  AUX-LCR
           MOVE     PCA-AUX  TO  PCA-LCR
           ADD      1        TO  ACM-QTD
           ADD      VAL-LCR  TO  ACM-VAL
           DISPLAY  TELA-03      TELA-05
           WRITE    REG-LCR      GO       ROT-PESQ-10.

       RT1-EXBE-00.
           MOVE     00  TO  IND1 MOVE     1  TO  IND2
           MOVE     10  TO  IND4 MOVE     1  TO  IND3
           MOVE     10  TO  LIN1 MOVE    15  TO  LIN2.

       RT1-EXBE-10.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1   <     300  GO       RT1-EXBE-10.
           MOVE     SPACES   TO  CHV-LCR
           MOVE     1        TO  PRX-LCR
           MOVE     ZEROS    TO  ACM-QTD
                                 ACM-VAL  IND1.
       RT1-EXBE-20.
           START    CADLCR       KEY  >   CHV-LCR
           IF  FST-LCR   =  "23" GO       RT1-EXBE-30.
           READ     CADLCR  NEXT
           IF  PRX-LCR   >    1  GO       RT1-EXBE-30.
           MOVE     NUM-LCR  TO  NUM-AX1
           MOVE     AUX-LCR  TO  CHV-CLI
           READ     CADCLI
           MOVE     NOM-CLI  TO  CLI-AX1
           MOVE     HST-LCR  TO  DES-AX1
           MOVE     VAL-LCR  TO  VAL-AX1
           ADD      VAL-LCR  TO  ACM-VAL
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
           IF  TCL-MSG   =   23  GO       ROT-PESQ-00.
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
           MOVE     HST-LCR  TO  DES-AX1
           MOVE     VAL-LCR  TO  VAL-LCT.

       RT1-ALTR-10.
           ACCEPT   DES-E03      DISPLAY  DES-E04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  SETA-04
                                 GO       RT1-EXBE-40.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-ALTR-10.
           IF  DES-AX1   =       SPACES
                                 GO       RT1-ALTR-10.
       RT1-ALTR-20.
           ACCEPT   VAL-E03      DISPLAY  VAL-E04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-ALTR-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-ALTR-20.
           IF  VAL-LCT   =    0  GO       RT1-ALTR-20.
           COMPUTE  ACM-VAL   =  ACM-VAL  -
                                 VAL-LCR  +   VAL-LCT
           MOVE     DES-AX1  TO  HST-LCR
           MOVE     VAL-LCT  TO  VAL-AX1  VAL-LCR
           MOVE     REG-AX1  TO  REG-TAB  (IND3)
           REWRITE  REG-LCR
           DISPLAY  TELA-05      GO       RT1-EXBE-40.

       RT1-CONF-00.
           MOVE     MG1-AUX  TO  MSG-AUX
           ACCEPT   DTA-DTA FROM DATE
           MOVE     18       TO  LIN-DTA
           MOVE     55       TO  COL-DTA
           MOVE     4        TO  BCK-DTA
           DISPLAY  TELA-06      PERFORM  ROT-DATA-00
           IF  FLG-DTA   =    0  GO       RT1-RTRN-10.

       RT1-CONF-10.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     DIA-DTA  TO  DIA-MG2
           MOVE     MES-DTA  TO  MES-MG2
           MOVE     ANO-DTA  TO  ANO-MG2
           MOVE     MG2-AUX  TO  MSG-AUX
           DISPLAY  TELA-06      ACCEPT   ENTR-05
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-CONF-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT1-CONF-10.
           IF  RSP-OPC  NOT   =  "S"  GO  RT1-CONF-10.
           MOVE     MG3-AUX  TO  MSG-AUX
           MOVE     SPACES   TO  CHV-LCR
           DISPLAY  TELA-06      OPEN     I-O  CADSDO
           MOVE     1        TO  PRX-LCR
                                 PRX-SDO  SUF-SDO.
       RT1-CONF-20.
           READ     CADSDO       MOVE 20  TO  IND-MSG
           IF  FST-SDO   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       RT1-CONF-20.
       RT1-CONF-30.
           START    CADLCR       KEY  >   CHV-LCR
           IF  FST-LCR   =  "23" GO       RT1-CONF-60.
           READ     CADLCR  NEXT
           IF  PRX-LCR   >    1  GO       RT1-CONF-60.
           MOVE     AUX-LCR  TO  CLI-NTA
           MOVE     DTA-DTA  TO  DTA-NTA
           MOVE     NUM-LCR  TO  NUM-NTA
           MOVE     PCA-LCR  TO  PCA-NTA
           MOVE     HST-LCR  TO  DES-NTA
           MOVE     VAL-LCR  TO  VAL-NTA
           ADD      VAL-LCR  TO  ENT-SDO
           MOVE     ZEROS    TO  LCT-NTA.

       RT1-CONF-40.
           WRITE    REG-NTA      ADD   1  TO  LCT-NTA
           IF  FST-NTA   =  "22" GO       RT1-CONF-40.

       RT1-CONF-50.
           DELETE   CADLCR       MOVE 20  TO  IND-MSG
           IF  FST-LCR   =  "00" GO       RT1-CONF-30.
           PERFORM  ROT-MSGM-00  GO       RT1-CONF-50.

       RT1-CONF-60.
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           REWRITE  REG-SDO      CLOSE    CADSDO
           DISPLAY  TELA-05      GO       ROT-PESQ-00.

       RT1-RTRN-00.
           IF  ACM-QTD   >    0  DISPLAY  TELA-04
                                 GO       RT1-EXBE-00.
       RT1-RTRN-10.
           CLOSE    CADCLI       CADLCR   CADNTA
           PERFORM  ROT-RTLA-00  GO       ROT-MENU-00.

       RT2-ENTR-00.
           MOVE     33       TO  COL1
           MOVE     41       TO  COL2
           OPEN     INPUT        CADCLI
                    I-O          CADNTA   CADSDO.
       RT2-ENTR-10.
           MOVE     ZEROS    TO  DTA-DTA
           MOVE     10       TO  LIN-DTA
           MOVE     14       TO  COL-DTA
           MOVE     2        TO  BCK-DTA
           DISPLAY  TELA-07      PERFORM  ROT-DATA-00
           IF  FLG-DTA   >    0  GO       ROT-PESQ-00.

       RT2-RTRN-00.
           CLOSE    CADCLI       CADNTA   CADSDO
           PERFORM  ROT-RTLA-00  GO       ROT-MENU-00.

       RT2-EXBE-00.
           MOVE     31       TO  IND-MSG
           MOVE     SPACES   TO  CHV-NTA
           MOVE     CHV-CLI  TO  CLI-NTA
           MOVE     DTA-DTA  TO  DTA-NTA
           START    CADNTA       KEY  >   CHV-NTA
           IF  FST-NTA   =  "23" GO       RT2-EXBE-60.
           READ     CADNTA  NEXT
           IF  CLI-NTA   NOT  =  CHV-CLI  OR  DTA-NTA
               >    DTA-DTA      GO       RT2-EXBE-60.

       RT2-EXBE-10.
           MOVE     00  TO  IND1 MOVE     1  TO  IND2
           MOVE     13  TO  IND4 MOVE     1  TO  IND3.
           MOVE     13  TO  LIN1 MOVE    18  TO  LIN2.

       RT2-EXBE-20.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1   <     300  GO       RT2-EXBE-20.
           MOVE     SPACES   TO  CHV-NTA
           MOVE     CHV-CLI  TO  CLI-NTA
           MOVE     DTA-DTA  TO  DTA-NTA
           DISPLAY  TELA-08      MOVE     0  TO  IND1.

       RT2-EXBE-30.
           START    CADNTA       KEY  >   CHV-NTA
           IF  FST-NTA   =  "23" GO       RT2-EXBE-40.
           READ     CADNTA  NEXT
           IF  CLI-NTA  NOT   =  CHV-CLI  OR  DTA-NTA
               >    DTA-DTA      GO       RT2-EXBE-40.
           MOVE     NUM-NTA  TO  NUM-AX2
           MOVE     PCA-NTA  TO  PCA-AUX
           MOVE     PC1-AUX  TO  PC1-AX2
           MOVE     PC2-AUX  TO  PC2-AX2
           MOVE     DES-NTA  TO  DES-AX2
           MOVE     VAL-NTA  TO  VAL-AX2
           ADD      1        TO  IND1
           MOVE     REG-AX2  TO  REG-TAB (IND1)
           MOVE     CHV-NTA  TO  CH2-TAB (IND1)
           GO       RT2-EXBE-30.

       RT2-EXBE-40.
           IF (IND2 + 5) < IND1  DISPLAY  STA-ABX
               IF  IND2    >  1  DISPLAY  STA-ACM
                   ELSE          DISPLAY  TRC-ACM
           ELSE    IF  IND2 > 1  DISPLAY  STA-ACM  TRC-ABX
                       ELSE      DISPLAY  TRC-ACM.
           DISPLAY  MENU-01      SETA-04.

       RT2-EXBE-50.
           MOVE  7  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       RT2-EXBE-40.
           IF  TCL-MSG   =   05  GO       RT2-MOVE-05.
           IF  TCL-MSG   =   06  GO       RT2-MOVE-06.
           IF  TCL-MSG   =   53  GO       RT2-MOVE-53.
           IF  TCL-MSG   =   54  GO       RT2-MOVE-54.
           IF  TCL-MSG   =   17  GO       RT2-TEST-00.
           IF  TCL-MSG   >   00  GO       RT2-EXBE-50.
           IF  HEX-MSG   =   27  DISPLAY  TELA-08
                                 GO       ROT-PESQ-15.
           PERFORM  RT2-TEST-00  GO       RT2-ALTR-00.

       RT2-EXBE-60.
           PERFORM  ROT-MSGM-00  GO       ROT-PESQ-15.

       RT2-MOVE-05.
           IF  IND3      =   01  GO       RT2-EXBE-50.
           IF  IND4      =   13  SUBTRACT 1 FROM IND2
                                 SUBTRACT 1 FROM IND3
                                 GO       RT2-EXBE-40.
           DISPLAY  SETA-03      SUBTRACT 1 FROM IND3
           COMPUTE  IND4     =   IND4  -  1
           DISPLAY  SETA-04      GO       RT2-EXBE-50.

       RT2-MOVE-06.
           IF  IND3      =  IND1 GO       RT2-EXBE-50.
           IF  IND4      =   18  ADD      1  TO  IND2
                                 ADD      1  TO  IND3
                                 GO       RT2-EXBE-40.
           DISPLAY  SETA-03      ADD      1  TO  IND3
           COMPUTE  IND4     =   IND4  +  1
           DISPLAY  SETA-04      GO       RT2-EXBE-50.

       RT2-MOVE-53.
           IF  IND2      =   01  GO       RT2-EXBE-50.
           MOVE     13       TO  IND4
           IF  IND2      >   10  COMPUTE  IND3 = IND2 - 06
                                 COMPUTE  IND2 = IND2 - 06
           ELSE
                                 MOVE     1  TO  IND2   IND3.
           GO      RT2-EXBE-40.

       RT2-MOVE-54.
           IF  IND1      <      (IND2  +  06)
                                 GO       RT2-EXBE-50.
           MOVE     13       TO  IND4
           COMPUTE  IND3     =   IND2  +  06
           COMPUTE  IND2     =   IND2  +  06
           GO       RT2-EXBE-40.

       RT2-TEST-00.
           MOVE     CH2-TAB      (IND3)   TO  CHV-NTA
           READ     CADNTA       MOVE 20  TO  IND-MSG
           IF  FST-NTA   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       RT2-EXBE-50.
           MOVE     1        TO  PRX-SDO  SUF-SDO
           READ     CADSDO       MOVE 20  TO  IND-MSG
           IF  FST-SDO   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       RT2-EXBE-50.
       RT2-EXCL-00.
           COMPUTE  COR-SDO   =  COR-SDO  -   VAL-NTA
           DELETE   CADNTA       REWRITE  REG-SDO
           DISPLAY  TELA-08      GO       RT2-EXBE-00.

       RT2-ALTR-00.
           MOVE     VAL-NTA  TO  VAL-LCT
           MOVE     REG-TAB      (IND3)   TO  REG-AX2.

       RT2-ALTR-10.
           ACCEPT   VAL-E03      DISPLAY  VAL-E04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  SETA-04
                                 GO       RT2-EXBE-50.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       RT2-ALTR-10.
           IF  VAL-LCT   =    0  GO       RT2-ALTR-10.
           COMPUTE  COR-SDO   =  COR-SDO
                              +  VAL-LCT  -   VAL-NTA
           MOVE     VAL-LCT  TO  VAL-AX2  VAL-NTA
           MOVE     REG-AX2  TO  REG-TAB  (IND3)
           REWRITE  REG-SDO
           REWRITE  REG-NTA      GO       RT2-EXBE-50.

       ROT-PESQ-00.
           MOVE     SPACES   TO  NOM-PSQ
           IF  NUM-OPC   =    1  DISPLAY  TELA-03.
           IF  NUM-OPC   =    2  DISPLAY  PSQ-E02.

       ROT-PESQ-05.
           MOVE     ZEROS    TO  CHV-CLI
           ACCEPT   CHV-E01      DISPLAY  CHV-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       RT1-RTRN-00,
                                          RT2-ENTR-10,
                                 DEPENDING ON NUM-OPC.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-05.
           IF  CHV-CLI   <   01  GO       ROT-PESQ-30.
           READ     CADCLI       MOVE 14  TO  IND-MSG
           IF  FST-CLI   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-05.
       ROT-PESQ-10.
           DISPLAY  CHV-E02      NOM-E02.

       ROT-PESQ-15.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-15.
           IF  TCL-MSG   =   05  GO       ROT-PESQ-20.
           IF  TCL-MSG   =   06  GO       ROT-PESQ-25.
           IF  TCL-MSG   >   00  GO       ROT-PESQ-15.
           IF  HEX-MSG   =   27  GO       ROT-PESQ-00.
           GO       RT1-INCL-00, RT2-EXBE-00,
                    DEPENDING    ON  NUM-OPC.
       ROT-PESQ-20.
           START    CADCLI       KEY  <   CHV-CLI
           IF  FST-CLI   =  "23" GO       ROT-PESQ-15.
           READ     CADCLI  PREVIOUS  GO  ROT-PESQ-10.

       ROT-PESQ-25.
           START    CADCLI       KEY  >   CHV-CLI
           IF  FST-CLI   =  "23" GO       ROT-PESQ-15.
           READ     CADCLI  NEXT GO       ROT-PESQ-10.

       ROT-PESQ-30.
           MOVE     ZEROS    TO  CHV-CLI
           MOVE     SPACES   TO  NOM-PSQ  NOM-CLI
           MOVE     26  TO  IND1 DISPLAY  CHV-E02.

       ROT-PESQ-35.
           ACCEPT   PSQ-E01      DISPLAY  PSQ-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-PESQ-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-35.
           IF  NOM-PSQ   =       SPACES
                                 GO       ROT-PESQ-35.
       ROT-PESQ-40.
           SUBTRACT  1  FROM     IND1
           IF  LET-PSQ (IND1) =  " "  GO  ROT-PESQ-40.
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-PESQ-45.
           MOVE     22       TO  IND-MSG
           START    CADCLI       KEY  >   CHV-CLI
           IF  FST-CLI   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-30.
           READ     CADCLI  NEXT
           DISPLAY  CHV-E02      MOVE  0  TO  ACM-PSQ
           INSPECT  NOM-CLI      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-45.
           PERFORM  ROT-PESQ-10  GO       ROT-PESQ-55.

       ROT-PESQ-50.
           MOVE     22       TO  IND-MSG
           START    CADCLI       KEY  <   CHV-CLI
           IF  FST-CLI   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-30.
           READ     CADCLI       PREVIOUS
           DISPLAY  CHV-E02      MOVE  0  TO  ACM-PSQ
           INSPECT  NOM-CLI      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-50.
           PERFORM  ROT-PESQ-10.

       ROT-PESQ-55.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-55.
           IF  TCL-MSG   =   05  DISPLAY  PSQ-E02
                                 GO       ROT-PESQ-50.
           IF  TCL-MSG   =   06  DISPLAY  PSQ-E02
                                 GO       ROT-PESQ-45.
           IF  TCL-MSG   >   00  GO       ROT-PESQ-55.
           IF  HEX-MSG   =   27  GO       ROT-PESQ-30.
           GO       RT1-INCL-00, RT2-EXBE-00,
                    DEPENDING    ON  NUM-OPC.
       ROT-HELP-00.
           CALL   "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING  PRM-MSG.

       ROT-DATA-00.
           CALL   "DYDATA"  USING  PRM-DTA
                                   PRM-HLP.
       ROT-GTLA-00.
           MOVE   "G"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE   "R"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.
