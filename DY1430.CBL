       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1430.
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

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC
                       FILE          STATUS  IS  FST-LBC.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADBCO.CPY.
       COPY  CADCTR.CPY.
       COPY  CADLBC.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-BCO          PIC  X(02).
           03  FST-CTR          PIC  X(02).
           03  FST-LBC          PIC  X(02).

           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  IND3             PIC  9(03).
           03  IND4             PIC  9(02).
           03  IND5             PIC  9(01).
           03  TECLADO          PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  ACM-PSQ          PIC  9(02).
           03  DES-AUX          PIC  X(25)  VALUE
               "ESTORNO DE LANCAMENTO".

           03  REG-AUX.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  HST-AUX      PIC  X(27).
               05  DOC-AUX      PIC  9(07).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  COD-AUX      PIC  9(01).
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  VAL-AUX      PIC  ZZ.ZZZ.ZZ9,99.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  NOM-PSQ          PIC  X(20).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  20.

           03  TAB-LCT          OCCURS 300.
               05  CHV-TAB      PIC  9(12).
               05  REG-TAB      PIC  X(53).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  34.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1430".
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
           03  LIN-DTA          PIC  9(02)  VALUE  09.
           03  COL-DTA          PIC  9(02)  VALUE  58.
           03  BCK-DTA          PIC  9(01)  VALUE  2.
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
           03  BCK-CLR  04 FOR-CLR   15.
               05  LINE 05 COLUMN 09 VALUE " Administraฦo Global ".
           03  BCK-CLR  01 FOR-CLR   15.
               05  LINE 06 COLUMN 11 VALUE " Controle C. Banc ria ".
           03  BCK-CLR  00 FOR-CLR   08.
               05  LINE 05 COLUMN 31 VALUE "฿".
               05  LINE 06 COLUMN 33 VALUE "฿".
               05  LINE 06 COLUMN 10 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 07 COLUMN 09 VALUE " ษ                        
      -            "ออออออออออออออออออออออออออออออออออป ".
               05  LINE 08 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 09 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 10 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 11 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 12 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 13 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 14 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 15 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 16 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 17 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 18 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 19 COLUMN 09 VALUE " บ
      -            "                                  บ ".
               05  LINE 20 COLUMN 09 VALUE " ศออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 07 COLUMN 13 VALUE " Correฦo Lanamentos ".
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
               05  LINE 21 COLUMN 11 PIC X(64) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 08 COLUMN 13 PIC X(55) FROM ALL "฿".
               05  LINE 10 COLUMN 13 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 00.
               05  LINE 10 COLUMN 14 PIC X(54) FROM ALL "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 09 COLUMN 13 PIC X(55) FROM
                   "  Banco :     -                       Data :".
           03  BCK-CLR 00 FOR-CLR 07.
               05  LINE 09 COLUMN 68 PIC X(01) FROM " ".
               05  LINE 10 COLUMN 68 PIC X(01) FROM " ".

       01  ENTR-01.
           03  CHV-E02 LINE 09 COLUMN 23 PIC ZZ9   USING CHV-BCO.
           03  PSQ-E02 LINE 09 COLUMN 29 PIC X(20) USING NOM-PSQ.

       01  ENTR-02  BCK-CLR 02 FOR-CLR 15.
           03  CHV-T02 LINE 09 COLUMN 23 PIC 9(03) FROM  CHV-BCO.
           03  DES-T02 LINE 09 COLUMN 29 PIC X(20) FROM  DES-BCO.
           03  PSQ-T02 LINE 09 COLUMN 29 PIC X(20) FROM  NOM-PSQ.

       01  TELA-03.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 11 COLUMN 13 PIC X(55) FROM ALL "฿".
               05  LINE 19 COLUMN 13 PIC X(01) FROM ALL "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 12 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 13 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 14 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 15 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 16 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 17 COLUMN 13 PIC X(55) FROM SPACES.
               05  LINE 18 COLUMN 13 PIC X(55) FROM SPACES.
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 12 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 18 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 19 COLUMN 68 PIC X(01) FROM     " ".
               05  LINE 19 COLUMN 14 PIC X(54) FROM ALL "฿".

       01  MENU-01  BCK-CLR 5 FOR-CLR 15.
           03  LINE 12 COLUMN 14 PIC X(53) FROM REG-TAB (IND2).
           03  LINE 13 COLUMN 14 PIC X(53) FROM REG-TAB (IND2 + 1).
           03  LINE 14 COLUMN 14 PIC X(53) FROM REG-TAB (IND2 + 2).
           03  LINE 15 COLUMN 14 PIC X(53) FROM REG-TAB (IND2 + 3).
           03  LINE 16 COLUMN 14 PIC X(53) FROM REG-TAB (IND2 + 4).
           03  LINE 17 COLUMN 14 PIC X(53) FROM REG-TAB (IND2 + 5).
           03  LINE 18 COLUMN 14 PIC X(53) FROM REG-TAB (IND2 + 6).

       01  SETA-01 BCK-CLR 05.
           03  TRC-ACM LINE 12 COLUMN 67 VALUE " ".
           03  TRC-ABX LINE 18 COLUMN 67 VALUE " ".
           03  STA-ACM LINE 12 COLUMN 67 VALUE "" FOR-CLR 11.
           03  STA-ABX LINE 18 COLUMN 67 VALUE "" FOR-CLR 11.

       01  LN-SETA BCK-CLR 07 FOR-CLR 12
                   LINE IND4 COLUMN 14 PIC X(53) FROM REG-TAB (IND3).

       01  LN-MENU BCK-CLR 05 FOR-CLR 15
                   LINE IND4 COLUMN 14 PIC X(53) FROM REG-TAB (IND3).

       01  TELA-04  BCK-CLR 4 FOR-CLR 15.
           03  LINE 19 COLUMN 43 VALUE
               " ษอออออออออออออออออออออออออออออป ".
           03  LINE 20 COLUMN 43 VALUE
               " บ  Confirma o Estorno ?  [ ]  บ ".
           03  LINE 21 COLUMN 43 VALUE
               " ศอออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 0 FOR-CLR 8.
               05  LINE 20 COLUMN 76 PIC X(02) FROM SPACES.
               05  LINE 21 COLUMN 76 PIC X(02) FROM SPACES.
               05  LINE 22 COLUMN 45 PIC X(33) FROM SPACES.

       01  ENTR-04  BCK-CLR 04 FOR-CLR 15 AUTO
                    LINE 20 COLUMN 70 PIC X(01) USING RSP-OPC.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"      USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL  =  1     GOBACK.
           DISPLAY  TELA-01      TELA-02
           OPEN     INPUT        CADBCO   CADCTR
                    I-O          CADLBC   READ     CADCTR.

       ROT-CODE-00.
           MOVE     ZEROS    TO  CHV-BCO  DTA-DTA
           ACCEPT   CHV-E02      DISPLAY  CHV-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =  "01" GO       ROT-EXIT-00.
           IF  TECLADO   =  "02" PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-00.
           IF  CHV-BCO   =    0  GO       ROT-NOME-00.
           READ     CADBCO       MOVE 21  TO  IND-MSG
           IF  FST-BCO   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-00.
       ROT-EXBE-00.
           DISPLAY  CHV-T02      DES-T02.

       ROT-EXBE-10.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-EXBE-10.
           IF  TCL-MSG   =   05  GO       ROT-EXBE-20.
           IF  TCL-MSG   =   06  GO       ROT-EXBE-30.
           IF  TCL-MSG   >   00  GO       ROT-EXBE-10.
           IF  HEX-MSG   =   13  GO       ROT-PESQ-00.
           DISPLAY  TELA-02      GO       ROT-CODE-00.

       ROT-EXBE-20.
           START    CADBCO       KEY  <   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-EXBE-10.
           READ     CADBCO       PREVIOUS
           GO       ROT-EXBE-00.

       ROT-EXBE-30.
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-EXBE-10.
           READ     CADBCO  NEXT GO       ROT-EXBE-00.

       ROT-PESQ-00.
           PERFORM  ROT-DATA-00  MOVE 18  TO  IND-MSG
           IF  FLG-DTA   =    0  DISPLAY  TELA-02
                                 GO       ROT-CODE-00.
           IF  DTA-DTA   <       DTI-CTR
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
       ROT-PESQ-10.
           MOVE     29       TO  IND-MSG
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           MOVE     DTA-DTA  TO  DTA-LBC.

       ROT-PESQ-20.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO  OR  DTA-LBC
               NOT  =  DTA-DTA   PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-00.
           IF  COD-LBC   =    0  OR   2   OR   6
                                 GO       ROT-PESQ-20.
           MOVE     1        TO  IND1     IND2
                                 IND3     IND5
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-03.

       ROT-MENU-00.
           MOVE     SPACES   TO  REG-TAB     (IND1)
           IF  IND1      <  300  ADD  1   TO  IND1
                                 GO       ROT-MENU-00.
           MOVE     ZEROS    TO  IND1
           MOVE     12       TO  IND4
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           MOVE     DTA-DTA  TO  DTA-LBC.

       ROT-MENU-10.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-MENU-20.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO  OR
               DTA-LBC  NOT   =  DTA-DTA
                                 GO       ROT-MENU-20.
           IF  COD-LBC   =    0  OR   2   OR   6
                                 GO       ROT-MENU-10.
           ADD      1        TO  IND1
           MOVE     DOC-LBC  TO  DOC-AUX
           MOVE     COD-LBC  TO  COD-AUX
           MOVE     HST-LBC  TO  HST-AUX
           MOVE     VAL-LBC  TO  VAL-AUX
           MOVE     CHV-LBC  TO  CHV-TAB (IND1)
           MOVE     REG-AUX  TO  REG-TAB (IND1)
           GO       ROT-MENU-10.

       ROT-MENU-20.
           IF (IND2 + 06) < IND1 DISPLAY  STA-ABX
               IF  IND2     >  1 DISPLAY  STA-ACM
                   ELSE          DISPLAY  TRC-ACM
           ELSE    IF  IND2 >  1 DISPLAY  STA-ACM  TRC-ABX
                       ELSE      DISPLAY  TRC-ACM.
           DISPLAY     MENU-01   LN-SETA.

       ROT-MENU-30.
           MOVE  3  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-30.
           IF  TCL-MSG   =   05  GO       ROT-MENU-40.
           IF  TCL-MSG   =   06  GO       ROT-MENU-50.
           IF  TCL-MSG   =   53  GO       ROT-MENU-60.
           IF  TCL-MSG   =   54  GO       ROT-MENU-70.
           IF  TCL-MSG   >   00  GO       ROT-MENU-30.
           IF  HEX-MSG   =   13  GO       ROT-ATUA-00.
           PERFORM  ROT-RTLA-00  GO       ROT-PESQ-00.

       ROT-MENU-40.
           IF  IND3   =   01     GO       ROT-MENU-30.
           IF  IND4   =   12     COMPUTE  IND2 = IND2  -  1
                                 COMPUTE  IND3 = IND3  -  1
                                 GO       ROT-MENU-20.
           DISPLAY  LN-MENU      COMPUTE  IND3 = IND3  -  1
                                 COMPUTE  IND4 = IND4  -  1
           DISPLAY  LN-SETA      GO       ROT-MENU-30.

       ROT-MENU-50.
           IF  IND3   =  IND1    GO       ROT-MENU-30.
           IF  IND4   =   18     COMPUTE  IND2 = IND2  +  1
                                 COMPUTE  IND3 = IND3  +  1
                                 GO       ROT-MENU-20.
           DISPLAY  LN-MENU      COMPUTE  IND3 = IND3  +  1
                                 COMPUTE  IND4 = IND4  +  1
           DISPLAY  LN-SETA      GO       ROT-MENU-30.

       ROT-MENU-60.
           IF  IND2   =   01     GO       ROT-MENU-30.
           IF  IND2   >   07     COMPUTE  IND3 = IND2  -  7
                                 COMPUTE  IND2 = IND2  -  7
           ELSE
                                 MOVE     1  TO  IND2  IND3.
           MOVE  12  TO  IND4    GO       ROT-MENU-20.

       ROT-MENU-70.
           IF  IND1   <         (IND2  +  7)
                                 GO       ROT-MENU-30.
           COMPUTE  IND3   =     IND2  +  7
           COMPUTE  IND2   =     IND2  +  7
           MOVE  12  TO  IND4    GO       ROT-MENU-20.

       ROT-ATUA-00.
           MOVE     CHV-TAB     (IND3)    TO  CHV-LBC
           READ     CADLBC       MOVE 20  TO  IND-MSG
           IF  FST-LBC   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-MENU-30.
           IF  COD-LBC   =    0  OR   6
                                 MOVE 43  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-MENU-30.
           MOVE     2        TO  IND5
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-04.

       ROT-ATUA-10.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   ENTR-04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 MOVE     1  TO  IND5
                                 GO       ROT-MENU-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ATUA-10.
           IF  RSP-OPC  NOT  =   "S"
                                 GO       ROT-ATUA-10.
           IF  COD-LBC   >    3  MOVE  6  TO  COD-LBC
               ELSE              MOVE  0  TO  COD-LBC.
           REWRITE  REG-LBC      MOVE  1  TO  IND5
           IF  COD-LBC   =    0  MOVE  6  TO  COD-LBC
               ELSE              MOVE  0  TO  COD-LBC.
           ADD      1        TO  LCT-LBC
           MOVE     DES-AUX  TO  HST-LBC.

       ROT-ATUA-20.
           WRITE    REG-LBC      ADD   1  TO  LCT-LBC
           IF  FST-LBC   =  "22" GO       ROT-ATUA-20.
           PERFORM  ROT-RTLA-00  GO       ROT-PESQ-10.

       ROT-NOME-00.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  CHV-BCO
           DISPLAY  CHV-T02.

       ROT-NOME-10.
           ACCEPT   PSQ-E02      DISPLAY  PSQ-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  TELA-02
                                 GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-NOME-10.
           IF  NOM-PSQ   =       SPACES
                                 GO       ROT-NOME-10.
           MOVE     21  TO  IND1 PERFORM  ROT-NOME-50.

       ROT-NOME-20.
           SUBTRACT  1  FROM  IND1
           IF  LET-PSQ (IND1) =  " "  GO  ROT-NOME-20.
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-NOME-30.
           MOVE     22       TO  IND-MSG
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           READ     CADBCO  NEXT DISPLAY  CHV-T02
           INSPECT  DES-BCO      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-NOME-30.
           PERFORM  ROT-EXBE-00  GO       ROT-NOME-60.

       ROT-NOME-40.
           MOVE     22       TO  IND-MSG
           START    CADBCO       KEY  <   CHV-BCO
           IF  FST-BCO   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           READ     CADBCO       PREVIOUS
           DISPLAY  CHV-T02
           INSPECT  DES-BCO      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-NOME-40.
           PERFORM  ROT-EXBE-00  GO       ROT-NOME-60.

       ROT-NOME-50.
           MOVE     0        TO  ACM-PSQ
           DISPLAY  CHV-T02      PSQ-T02.

       ROT-NOME-60.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-NOME-60.
           IF  TCL-MSG   =   05  PERFORM  ROT-NOME-50
                                 GO       ROT-NOME-40.
           IF  TCL-MSG   =   06  PERFORM  ROT-NOME-50
                                 GO       ROT-NOME-30.
           IF  TCL-MSG   >   00  GO       ROT-NOME-60.
           IF  HEX-MSG   =   13  GO       ROT-PESQ-00.
           PERFORM  ROT-RTLA-00  GO       ROT-NOME-00.

       ROT-DATA-00.
           CALL    "DYDATA"  USING  PRM-DTA
                                    PRM-HLP.
       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR (IND5)
           CALL    "DYTELA"  USING  PRM-SCR (IND5).

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR (IND5)
           CALL    "DYTELA"  USING  PRM-SCR (IND5).

       ROT-EXIT-00.
           CLOSE    CADBCO  CADCTR  CADLBC  GOBACK.
