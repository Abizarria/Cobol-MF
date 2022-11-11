       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1110.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADATV        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ATV
                       FILE          STATUS  IS  FST-ATV.

           SELECT      CADCMB        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CMB
                       FILE          STATUS  IS  FST-CMB.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST
                       FILE          STATUS  IS  FST-EST.

           SELECT      CADLCP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCP
                       FILE          STATUS  IS  FST-LCP.

           SELECT      CADLES        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LES
                       FILE          STATUS  IS  FST-LES.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADATV.CPY.
       COPY  CADCMB.CPY.
       COPY  CADCTR.CPY.
       COPY  CADEST.CPY.
       COPY  CADLCP.CPY.
       COPY  CADLES.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-ATV          PIC  X(02).
           03  FST-CMB          PIC  X(02).
           03  FST-CTR          PIC  X(02).
           03  FST-EST          PIC  X(02).
           03  FST-LCP          PIC  X(02).
           03  FST-LES          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(01).
           03  TECLADO          PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  ACM-PSQ          PIC  9(02).
           03  SDO-AUX          PIC S9(06)V9.

           03  DTA-LCT.
               05  ANO-LCT      PIC  9(02).
               05  MES-LCT      PIC  9(02).
               05  DIA-LCT      PIC  9(02).

           03  NOM-PSQ          PIC  X(25).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  25.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  01.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1110".
           03  BCK-HLP          PIC  9(01)  VALUE  7.
           03  FOR-HLP          PIC  9(01)  VALUE  2.
           03  LET-HLP          PIC  9(01)  VALUE  3.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-PSQ.
           03  OPC-PSQ          PIC  9(01)  VALUE  1.
           03  IND-PSQ          PIC  X(01)  VALUE  "1".
           03  LIN-PSQ          PIC  9(02)  VALUE  09.
           03  COL-PSQ          PIC  9(02)  VALUE  28.
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
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 05 COLUMN 07 VALUE " Administraฦo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 06 COLUMN 09 VALUE " Controle de Estoques ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 05 COLUMN 29 VALUE "฿".
               05  LINE 06 COLUMN 31 VALUE "฿".
               05  LINE 06 COLUMN 08 VALUE "Ü".

       01  TELA-02.
           03  BCK-CLR 03 FOR-CLR 14.
               05  LINE 07 COLUMN 07 VALUE " ษ                        
      -            "ออออออออออออออออออออออออออออออออออออออป ".
               05  LINE 08 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 09 COLUMN 07 VALUE " บ  Cขdigo Produto :  -
      -            "-                                     บ ".
               05  LINE 10 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 11 COLUMN 07 VALUE " บ  Descriฦo .....:
      -            "                      Volume :        บ ".
               05  LINE 12 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 13 COLUMN 07 VALUE " บ  Saldo Estoque .:
      -            "                                      บ ".
               05  LINE 14 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 15 COLUMN 07 VALUE " บ  Preo de Custo :
      -            "                                      บ ".
               05  LINE 16 COLUMN 07 VALUE " บ  Custo Mdio ...:
      -            "                                      บ ".
               05  LINE 17 COLUMN 07 VALUE " บ  Preo de Venda :
      -            "                                      บ ".
               05  LINE 18 COLUMN 07 VALUE " บ  Margem Lucro ..:
      -            " %                                    บ ".
               05  LINE 19 COLUMN 07 VALUE " บ
      -            "                                      บ ".
               05  LINE 20 COLUMN 07 VALUE " ศออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 11 VALUE " Cadastro de Produtos ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 08 COLUMN 75 VALUE  "  ".
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
               05  LINE 20 COLUMN 75 VALUE  "  ".
               05  LINE 21 COLUMN 09 PIC X(68) FROM SPACES.

       01  ENTR-01     AUTO.
           03  SUF-E02 LINE 09 COLUMN 30 PIC ZZ9          USING SUF-EST.
           03  DES-E02 LINE 11 COLUMN 28 PIC X(25)        USING DES-EST.
           03  VOL-E02 LINE 11 COLUMN 66 PIC Z9,99        USING VOL-EST.
           03  SDO-E02 LINE 13 COLUMN 28 PIC ----.--9,9   USING SDO-AUX.
           03  CST-E02 LINE 15 COLUMN 28 PIC ZZZ.ZZ9,9999 USING CST-EST.
           03  CTM-E02 LINE 16 COLUMN 28 PIC ZZZ.ZZ9,9999 USING CTM-EST.
           03  VDA-E02 LINE 17 COLUMN 28 PIC ZZZ.ZZ9,9999 USING VDA-EST.
           03  PCT-E02 LINE 18 COLUMN 28 PIC Z9,99        USING PCT-EST.
           03  PSQ-E02 LINE 11 COLUMN 28 PIC X(25)        USING NOM-PSQ.

       01  ENTR-02  BCK-CLR 03 FOR-CLR 15.
           03  PRX-T02 LINE 09 COLUMN 28 PIC X(01)        FROM CHV-ATV.
           03  ATV-T02 LINE 09 COLUMN 38 PIC X(15)        FROM DES-ATV.
           03  SUF-T02 LINE 09 COLUMN 30 PIC 9(03)        FROM SUF-EST.
           03  DES-T02 LINE 11 COLUMN 28 PIC X(25)        FROM DES-EST.
           03  VOL-T02 LINE 11 COLUMN 66 PIC Z9,99        FROM VOL-EST.
           03  SDO-T02 LINE 13 COLUMN 28 PIC ----.--9,9   FROM SDO-AUX.
           03  CST-T02 LINE 15 COLUMN 28 PIC ZZZ.ZZ9,9999 FROM CST-EST.
           03  CTM-T02 LINE 16 COLUMN 28 PIC ZZZ.ZZ9,9999 FROM CTM-EST.
           03  VDA-T02 LINE 17 COLUMN 28 PIC ZZZ.ZZ9,9999 FROM VDA-EST.
           03  PCT-T02 LINE 18 COLUMN 28 PIC Z9,99        FROM PCT-EST.

       01  PSQ-T02  BCK-CLR 03 FOR-CLR 15
                    LINE 11 COLUMN 28 PIC X(25) USING NOM-PSQ AUTO.

       01  TELA-03.
           03  BCK-CLR 07 FOR-CLR 03.
               05  LINE 12 COLUMN 43 PIC X(27) FROM ALL "฿".
               05  LINE 14 COLUMN 43 PIC X(01) FROM     "Ü".
           03  BCK-CLR 07 FOR-CLR 12.
               05  LINE 13 COLUMN 43 PIC X(27) FROM "  Estoque Mกnimo".
               05  LINE 13 COLUMN 61 PIC ZZZ.ZZ9 USING MIN-EST.
           03  BCK-CLR 07 FOR-CLR 00.
               05  LINE 13 COLUMN 70 PIC X(01) FROM     "Û".
               05  LINE 14 COLUMN 70 PIC X(01) FROM     "Û".
               05  LINE 14 COLUMN 44 PIC X(26) FROM ALL "Ü".

           03  BCK-CLR 01 FOR-CLR 03.
               05  LINE 15 COLUMN 43 PIC X(27) FROM ALL "฿".
               05  LINE 18 COLUMN 43 PIC X(01) FROM     "Ü".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 16 COLUMN 43 PIC X(27) FROM "  Vendas Ms".
               05  LINE 17 COLUMN 43 PIC X(27) FROM "  Vendas Ano".
               05  LINE 16 COLUMN 57 PIC  Z.ZZZ.ZZ9,9 USING QVM-EST.
               05  LINE 17 COLUMN 56 PIC ZZ.ZZZ.ZZ9,9 USING QVA-EST.
           03  BCK-CLR 01 FOR-CLR 00.
               05  LINE 16 COLUMN 70 PIC X(01) FROM     "Û".
               05  LINE 17 COLUMN 70 PIC X(01) FROM     "Û".
               05  LINE 18 COLUMN 70 PIC X(01) FROM     "Û".
               05  LINE 18 COLUMN 44 PIC X(26) FROM ALL "Ü".

       01  TELA-04.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 12 COLUMN 23 VALUE
                   " ษออออออออออออออออออออออออออออออป ".
               05  LINE 13 COLUMN 23 VALUE
                   " บ  Confirma a Exclusฦo ?  [ ]  บ ".
               05  LINE 14 COLUMN 23 VALUE
                   " ศออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 13 COLUMN 57 PIC X(02)        FROM "mo".
               05  LINE 14 COLUMN 57 PIC X(02)        FROM "  ".
               05  LINE 15 COLUMN 25 PIC X(34)        FROM " :".
               05  LINE 15 COLUMN 28 PIC ZZZ.ZZ9,9999 FROM CST-EST.

       01  ENTR-04  BCK-CLR 04 FOR-CLR 15
                    LINE 13 COLUMN 51 PIC X(01) USING RSP-OPC AUTO.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           OPEN    INPUT     CADCTR   READ     CADCTR
           MOVE    DTD-CTR     TO     DTA-LCT
           CLOSE   CADCTR
           OPEN    I-O       CADEST   CADLES
                   INPUT     CADATV   CADCMB   CADLCP
           IF  DIA-LCT = 0    MOVE    1   TO   DIA-LCT.

       ROT-CODE-00.
           DISPLAY  TELA-02      PERFORM  ROT-PESQ-00
           IF  IND-PSQ   <    1  GO       ROT-EXIT-00.
           MOVE     IND-PSQ  TO  CHV-ATV
           READ     CADATV
           DISPLAY  PRX-T02      ATV-T02.

       ROT-CODE-10.
           MOVE     ZEROS    TO  SUF-EST
           ACCEPT   SUF-E02      DISPLAY  SUF-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           MOVE     1   TO  IND2 PERFORM  ROT-GTLA-00.
           IF  SUF-EST   <   1   GO       ROT-PESQ-10.
           MOVE     CHV-ATV  TO  PRX-EST
           READ     CADEST
           IF  FST-EST   =  "23" GO       ROT-INCL-00.

       ROT-EXBE-00.
           MOVE     SDO-EST  TO  SDO-AUX
           DISPLAY  TELA-03      ENTR-02.

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
           MOVE     CHV-ATV  TO  PRX-EST
           MOVE     1        TO  IND2
           PERFORM  ROT-RTLA-00  GO       ROT-CODE-10.

       ROT-RTRN-00.
           START    CADEST       KEY  <   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-EXBE-10.
           READ     CADEST       PREVIOUS
           IF  PRX-EST       =   CHV-ATV
                                 GO       ROT-EXBE-00.
           READ     CADEST  NEXT GO       ROT-EXBE-10.

       ROT-PROX-00.
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-EXBE-10.
           READ     CADEST  NEXT
           IF  PRX-EST       =   CHV-ATV
                                 GO       ROT-EXBE-00.
           READ     CADEST       PREVIOUS
           GO       ROT-EXBE-10.

       ROT-TEST-00.
           READ     CADEST       MOVE 20  TO  IND-MSG
           IF  FST-EST   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-10.
           IF  FST-EST   =  "23" MOVE 36  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-10.
       ROT-INCL-00.
           MOVE     SPACES   TO  DES-CMB
           IF  PRX-EST   >  "1"  GO       ROT-INCL-10.
           MOVE     SUF-EST  TO  CHV-CMB
           READ     CADCMB       MOVE 42  TO  IND-MSG
           IF  FST-CMB   =  "23" OR
               SUF-EST   >   9   PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-10.
       ROT-INCL-10.
           MOVE     DES-CMB  TO  DES-EST
           MOVE     ZEROS    TO  VOL-EST  SDO-AUX  MIN-EST
                                 CST-EST  CTM-EST  VDA-EST
                                 PCT-EST  QVM-EST  QVA-EST
           PERFORM  ROT-ENTR-00  THRU     ROT-ENTR-70
           MOVE     SDO-AUX  TO  SDO-EST
           WRITE    REG-EST      MOVE 12  TO  IND-MSG
           IF  FST-EST   =  "22" READ     CADEST
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-00.
           DISPLAY  TELA-03      GO       ROT-EXBE-10.

       ROT-ALTR-00.
           PERFORM  ROT-ENTR-00  THRU     ROT-ENTR-70
           IF  SDO-AUX     =     SDO-EST
                                 REWRITE  REG-EST
                                 GO       ROT-EXBE-10.
           MOVE     PRX-EST  TO  PRX-LES
           MOVE     SUF-EST  TO  SUF-LES
           MOVE     DTA-LCT  TO  DTA-LES
           MOVE     1        TO  LCT-LES
           MOVE     PRM-OPR  TO  OPR-LES
           MOVE     SDO-EST  TO  SDA-LES
           MOVE     SDO-AUX  TO  SDC-LES.

       ROT-ALTR-10.
           WRITE    REG-LES      ADD   1  TO  LCT-LES
           IF  FST-LES   =  "22" GO       ROT-ALTR-10.
           MOVE     SDO-AUX  TO  SDO-EST
           REWRITE  REG-EST      GO       ROT-EXBE-10.

       ROT-EXCL-00.
           IF  SDO-EST  NOT   =  ZEROS
                                 GO       ROT-EXCL-20.
           MOVE     SPACES   TO  CHV-LCP  CHV-LES
           MOVE     PRX-EST  TO  PRX-LCP  PRX-LES
           MOVE     SUF-EST  TO  SUF-LCP  SUF-LES
           START    CADLCP       KEY  >   CHV-LCP
           IF  FST-LCP   =  "23" GO       ROT-EXCL-10.
           READ     CADLCP  NEXT
           IF  PRX-LCP   =       PRX-EST  AND
               SUF-LCP   =       SUF-EST
                                 GO       ROT-EXCL-20.
       ROT-EXCL-10.
           START    CADLES       KEY  >   CHV-LES
           IF  FST-LES   =  "23" GO       ROT-EXCL-30.
           READ     CADLES  NEXT
           IF  PRX-LES  NOT   =  PRX-EST  OR  SUF-LES
               NOT   =  SUF-EST  GO       ROT-EXCL-30.

       ROT-EXCL-20.
           MOVE     13       TO  IND-MSG
           PERFORM  ROT-MSGM-00  GO       ROT-EXBE-10.

       ROT-EXCL-30.
           MOVE     2        TO  IND2
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-04.

       ROT-EXCL-40.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   ENTR-04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXBE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-EXCL-40.
           IF  RSP-OPC  NOT   =  "S"
                                 GO       ROT-EXCL-40.
           DELETE   CADEST       GO       ROT-EXBE-20.

       ROT-ENTR-00.
           ACCEPT   DES-E02      DISPLAY  DES-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXBE-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-00.
           IF  DES-EST   =       SPACES
                                 GO       ROT-ENTR-00.
       ROT-ENTR-10.
           ACCEPT   VOL-E02      DISPLAY  VOL-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
           IF  VOL-EST   =   0   GO       ROT-ENTR-10.

       ROT-ENTR-20.
           ACCEPT   SDO-E02      DISPLAY  SDO-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-20.
       ROT-ENTR-30.
           ACCEPT   CST-E02      DISPLAY  CST-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-30.
           IF  FST-EST   =  "23" MOVE     CST-EST  TO  CTM-EST
                                 DISPLAY  CTM-T02
                                 GO       ROT-ENTR-50.
       ROT-ENTR-40.
           IF  FST-EST  =  "23"  GO       ROT-ENTR-30.
           ACCEPT   CTM-E02      DISPLAY  CTM-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-40.
       ROT-ENTR-50.
           ACCEPT   VDA-E02      DISPLAY  VDA-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-40.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-50.
           IF  VDA-EST   =   0   GO       ROT-ENTR-60.
           MOVE     46       TO  IND-MSG
           IF  VDA-EST   <       CST-EST
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-50.
           COMPUTE  PCT-EST  =  (VDA-EST  -   CTM-EST)
                                 *   100  /   VDA-EST
           DISPLAY  PCT-T02      GO       ROT-ENTR-70.

       ROT-ENTR-60.
           ACCEPT   PCT-E02      DISPLAY  PCT-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-50.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-60.
           COMPUTE  VDA-EST  =   CTM-EST  *   100  /  (100  -  PCT-EST)
           DISPLAY  VDA-T02.

       ROT-ENTR-70.
           EXIT.

       ROT-PESQ-00.
           CALL    "DYPESQ"      USING    PRM-PSQ
                                          PRM-HLP.
       ROT-PESQ-10.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  SUF-EST
           DISPLAY  SUF-T02.

       ROT-PESQ-20.
           ACCEPT   PSQ-E02      DISPLAY  PSQ-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-CODE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-20.
           IF  NOM-PSQ   =       SPACES
                                 GO       ROT-PESQ-20.
           MOVE     26  TO  IND1 PERFORM  ROT-PESQ-60.

       ROT-PESQ-30.
           SUBTRACT  1  FROM     IND1
           IF  LET-PSQ (IND1) =  " "  GO  ROT-PESQ-30.
           MOVE     CHV-ATV  TO  PRX-EST
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-PESQ-40.
           MOVE     22       TO  IND-MSG
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           READ     CADEST  NEXT
           IF  PRX-EST  NOT  =   CHV-ATV
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           DISPLAY  SUF-T02
           INSPECT  DES-EST      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-40.
           PERFORM  ROT-EXBE-00  GO       ROT-PESQ-70.

       ROT-PESQ-50.
           MOVE     22       TO  IND-MSG
           START    CADEST       KEY  <   CHV-EST
           IF  FST-EST   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           READ     CADEST       PREVIOUS
           IF  PRX-EST  NOT  =   CHV-ATV
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           DISPLAY  SUF-T02
           INSPECT  DES-EST      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-50.
           PERFORM  ROT-EXBE-00  GO       ROT-PESQ-70.

       ROT-PESQ-60.
           MOVE     0        TO  ACM-PSQ
           PERFORM  ROT-RTLA-00
           DISPLAY  SUF-T02      PSQ-T02.

       ROT-PESQ-70.
           MOVE  7  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-70.
           IF  TCL-MSG   =   05  PERFORM  ROT-PESQ-60
                                 GO       ROT-PESQ-50.
           IF  TCL-MSG   =   06  PERFORM  ROT-PESQ-60
                                 GO       ROT-PESQ-40.
           IF  TCL-MSG   =   17  PERFORM  ROT-TEST-00
                                 GO       ROT-EXCL-00.
           IF  TCL-MSG   >   00  GO       ROT-PESQ-70.
           IF  HEX-MSG   =   13  PERFORM  ROT-TEST-00
                                 GO       ROT-ALTR-00.
           PERFORM  ROT-RTLA-00  GO       ROT-PESQ-10.

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

       ROT-EXIT-00.
           CLOSE    CADATV  CADCMB  CADEST
                    CADLCP  CADLES  GOBACK.
