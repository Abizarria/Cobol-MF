       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYPESQ.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.

           SELECT      CADATV        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ATV
                       FILE          STATUS  IS  FST-ATV.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADATV.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-ATV          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  IND3             PIC  9(02).
           03  LIN-AX1          PIC  9(02).
           03  LIN-AX2          PIC  9(02).
           03  LIN-AX3          PIC  9(02).
           03  COL-AX1          PIC  9(02).
           03  COL-AX2          PIC  9(02).
           03  COL-AX3          PIC  9(02).
           03  COL-AX4          PIC  9(02).
           03  LARGURA          PIC  9(02).

           03  TAB-IND          PIC  X(35)  VALUE
               "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           03  RED-IND          REDEFINES   TAB-IND.
               05  IND-TAB      PIC  X(01)  OCCURS  35  INDEXED BY I.

           03  TAB-DES                      OCCURS  40.
               05  FILLER       PIC  X(01).
               05  DES-TAB      PIC  X(21).

           03  TAB-DSP.
               05  FILLER       PIC  X(18)  VALUE  "Obrig. Trabalhista".
               05  FILLER       PIC  X(18)  VALUE  "Obrigaไes Fiscais".
               05  FILLER       PIC  X(18)  VALUE  "Desp. Operacionais".
               05  FILLER       PIC  X(18)  VALUE  "Despesa de Capital".
               05  FILLER       PIC  X(18)  VALUE  "Crditos de Sขcios".
               05  FILLER       PIC  X(18)  VALUE  "Compra de Produtos".
           03  RED-DSP          REDEFINES   TAB-DSP.
               05  DSP-TAB      PIC  X(18)  OCCURS  6.

           03  TAB-MOV.
               05  FILLER       PIC  X(18)  VALUE  "Depขsitos Diversos".
               05  FILLER       PIC  X(18)  VALUE  "Ganhos com Capital".
               05  FILLER       PIC  X(18)  VALUE  "Cheques Devolvidos".
               05  FILLER       PIC  X(18)  VALUE  "Pagamentos … Vista".
               05  FILLER       PIC  X(18)  VALUE  "Pagamentos … Prazo".
               05  FILLER       PIC  X(18)  VALUE  "Transf. de Valores".
               05  FILLER       PIC  X(18)  VALUE  "Reembolso Despesas".
           03  RED-MOV          REDEFINES   TAB-MOV.
               05  MOV-TAB      PIC  X(18)  OCCURS  7.

           03  TAB-CRD.
               05  FILLER       PIC  X(20) VALUE "Requisiไes Empresas".
               05  FILLER       PIC  X(20) VALUE "Cheques  Pr-Datados".
               05  FILLER       PIC  X(20) VALUE "America Express Card".
               05  FILLER       PIC  X(20) VALUE "Mastercard e Diner's".
               05  FILLER       PIC  X(20) VALUE "Cartฦo Visa e Outros".
               05  FILLER       PIC  X(20) VALUE "Vales Frete Diversos".
           03  RED-CRD          REDEFINES   TAB-CRD.
               05  CRD-TAB      PIC  X(20)  OCCURS  6.

           03  TAB-PGT.
               05  FILLER       PIC  X(09)  VALUE  "Semanal  ".
               05  FILLER       PIC  X(09)  VALUE  "Dezenal  ".
               05  FILLER       PIC  X(09)  VALUE  "Quinzenal".
               05  FILLER       PIC  X(09)  VALUE  "Mensal   ".
               05  FILLER       PIC  X(09)  VALUE  "Eventual ".
           03  RED-PGT          REDEFINES   TAB-PGT.
               05  PGT-TAB      PIC  X(09)  OCCURS  5.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  01.
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

       01  PRM-PSQ.
           03  OPC-PSQ          PIC  9(01).
           03  IND-PSQ          PIC  X(01).
           03  LIN-PSQ          PIC  9(02).
           03  COL-PSQ          PIC  9(02).
           03  SB1-PSQ          PIC  X(02).
           03  SB2-PSQ          PIC  X(02).
           03  SB3-PSQ          PIC  X(02).
           03  SB4-PSQ          PIC  X(02).
           03  SB5-PSQ          PIC  X(02).
           03  SB6-PSQ          PIC  X(02).
           03  SB7-PSQ          PIC  X(26).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04).
           03  BCK-HLP          PIC  9(01).
           03  FOR-HLP          PIC  9(01).
           03  LET-HLP          PIC  9(01).

       SCREEN          SECTION.

       01  TELA-01 BCK-CLR 05 FOR-CLR 11.
           03 LINE LIN-PSQ COLUMN COL-PSQ VALUE
              " ษออออ        อออออป ".
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                   บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB1-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                   บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB2-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                   บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB3-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                   บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB4-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                   บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB5-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " ศอออออออออออออออออออผ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB6-PSQ.
           03 LINE PLUS 01 COLUMN COL-AX1 PIC X(23) FROM SB7-PSQ
              BCK-CLR 0 FOR-CLR 8.
           03 LINE LIN-PSQ COLUMN COL-AX4 VALUE "Grupos" FOR-CLR 14.

       01  TELA-02 BCK-CLR 05 FOR-CLR 11.
           03 LINE LIN-PSQ COLUMN COL-PSQ VALUE
              " ษออออออ        ออออออป ".
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                      บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB1-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                      บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB2-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                      บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB3-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                      บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB4-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                      บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB5-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " ศออออออออออออออออออออออผ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB6-PSQ.
           03 LINE PLUS 01 COLUMN COL-AX1 PIC X(26) FROM SB7-PSQ
              BCK-CLR 0 FOR-CLR 8.
           03 LINE LIN-PSQ COLUMN COL-AX4 VALUE "Grupos" FOR-CLR 14.

       01  TELA-03 BCK-CLR 05 FOR-CLR 11.
           03 LINE LIN-PSQ COLUMN COL-PSQ VALUE
              " ษออออ            ออออป ".
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                      บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB1-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                      บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB2-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                      บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB3-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                      บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB4-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                      บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB5-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " ศออออออออออออออออออออออผ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB6-PSQ.
           03 LINE PLUS 01 COLUMN COL-AX1 PIC X(26) FROM SB7-PSQ
              BCK-CLR 0 FOR-CLR 8.
           03 LINE LIN-PSQ COLUMN COL-AX4 VALUE "Movimentos" FOR-CLR 14.

       01  TELA-04 BCK-CLR 05 FOR-CLR 11.
           03 LINE LIN-PSQ COLUMN COL-PSQ VALUE
              " ษออออออ          ออออออป ".
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                        บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB1-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                        บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB2-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                        บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB3-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                        บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB4-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " บ                        บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB5-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE
              " ศออออออออออออออออออออออออผ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB6-PSQ.
           03 LINE PLUS 01 COLUMN COL-AX1 PIC X(28) FROM SB7-PSQ
              BCK-CLR 0 FOR-CLR 8.
           03 LINE LIN-PSQ COLUMN COL-AX4 VALUE "Crditos" FOR-CLR 14.

       01  TELA-05 BCK-CLR 05 FOR-CLR 11.
           03 LINE LIN-PSQ COLUMN COL-PSQ VALUE " ษ           ป ".
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE " บ             บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB1-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE " บ             บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB2-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE " บ             บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB3-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE " บ             บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB4-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE " บ             บ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB5-PSQ.
           03 LINE PLUS 01 COLUMN COL-PSQ VALUE " ศอออออออออออออผ ".
           03 BCK-CLR 0 FOR-CLR 8 PIC X(02) FROM SB6-PSQ.
           03 LINE PLUS 01 COLUMN COL-AX1 PIC X(17) FROM SB7-PSQ
              BCK-CLR 0 FOR-CLR 8.
           03 LINE LIN-PSQ COLUMN COL-AX4 VALUE "Categoria" FOR-CLR 14.

       01  MENU-01  BCK-CLR 05 FOR-CLR 15.
           03  LINE LIN-AX1 COLUMN COL-AX2
               SIZE LARGURA PIC X(21) FROM TAB-DES (IND2).
           03  LINE PLUS 01 COLUMN COL-AX2
               SIZE LARGURA PIC X(21) FROM TAB-DES (IND2 + 1).
           03  LINE PLUS 01 COLUMN COL-AX2
               SIZE LARGURA PIC X(21) FROM TAB-DES (IND2 + 2).
           03  LINE PLUS 01 COLUMN COL-AX2
               SIZE LARGURA PIC X(21) FROM TAB-DES (IND2 + 3).
           03  LINE PLUS 01 COLUMN COL-AX2
               SIZE LARGURA PIC X(21) FROM TAB-DES (IND2 + 4).

       01  SETA-01 BCK-CLR 05.
           03  FOR-CLR 11.
               05  TRC-ACM LINE LIN-AX1 COLUMN COL-AX3 VALUE "บ".
               05  TRC-ABX LINE LIN-AX3 COLUMN COL-AX3 VALUE "บ".
           03  FOR-CLR 15.
               05  STA-ACM LINE LIN-AX1 COLUMN COL-AX3 VALUE "".
               05  STA-ABX LINE LIN-AX3 COLUMN COL-AX3 VALUE "".

       01  LN-SETA BCK-CLR 07 FOR-CLR 12
                   LINE LIN-AX2 COLUMN COL-AX2
                   SIZE LARGURA PIC X(21) FROM TAB-DES (IND3).

       01  LN-MENU BCK-CLR 05 FOR-CLR 15
                   LINE LIN-AX2 COLUMN COL-AX2
                   SIZE LARGURA PIC X(21) FROM TAB-DES (IND3).

       PROCEDURE       DIVISION  USING  PRM-PSQ  PRM-HLP.

       ROT-0000-00.
           MOVE     1        TO  IND1
           OPEN     INPUT        CADATV.

       ROT-0000-10.
           MOVE     SPACES   TO  TAB-DES        (IND1)
           IF  IND1  <  40       ADD      1  TO  IND1
                                 GO       ROT-0000-10.
           MOVE     0        TO  IND1
           MOVE     1        TO  IND2     IND3
           COMPUTE  LIN-AX1  =   LIN-PSQ  +  1
           COMPUTE  LIN-AX2  =   LIN-PSQ  +  1
           COMPUTE  LIN-AX3  =   LIN-PSQ  +  5
           COMPUTE  COL-AX1  =   COL-PSQ  +  2
           COMPUTE  COL-AX2  =   COL-PSQ  +  3
           PERFORM  ROT-GTLA-00
           GO       ROT-ESTQ-00, ROT-DESP-00, ROT-BANK-00,
                    ROT-CRED-00, ROT-PGTO-00, DEPENDING ON  OPC-PSQ.

       ROT-ESTQ-00.
           MOVE     SPACES   TO  CHV-ATV
           MOVE     17       TO  LARGURA
           COMPUTE  COL-AX3  =   COL-PSQ   +   21
           COMPUTE  COL-AX4  =   COL-PSQ   +   08
           START    CADATV       KEY  >   CHV-ATV.

       ROT-ESTQ-10.
           READ     CADATV  NEXT
           IF  FST-ATV   =  "00" PERFORM  ROT-ESTQ-20
                                 GO       ROT-ESTQ-10.
           SET      I        TO  1
           SEARCH   IND-TAB      WHEN     IND-PSQ  =
                                          IND-TAB (I)
           SET      IND3     TO  I
           IF  IND3    >     05  COMPUTE  IND2 = IND2
                                       +  IND3   -  5
                                 COMPUTE  LIN-AX2   =
                                          LIN-AX2   +
                                          IND3 - IND2
           ELSE
                                 COMPUTE  LIN-AX2   =
                                          LIN-AX2   +
                                          IND3   -  1.
           DISPLAY  TELA-01      GO       ROT-MENU-00.

       ROT-ESTQ-20.
           ADD      1        TO  IND1
           MOVE     DES-ATV  TO  DES-TAB  (IND1).

       ROT-DESP-00.
           MOVE     20       TO  LARGURA
           COMPUTE  COL-AX3  =   COL-PSQ   +   24
           COMPUTE  COL-AX4  =   COL-PSQ   +   10.

       ROT-DESP-10.
           ADD      1        TO  IND1
           MOVE     DSP-TAB     (IND1)    TO  DES-TAB (IND1)
           IF  IND1    <     06  GO       ROT-DESP-10.
           SET      I        TO  1
           SEARCH   IND-TAB      WHEN     IND-PSQ  =
                                          IND-TAB (I)
           SET      IND3     TO  I
           IF  IND3    >     05  COMPUTE  IND2 = IND2
                                       +  IND3   -  5
                                 COMPUTE  LIN-AX2   =
                                          LIN-AX2   +
                                          IND3 - IND2
           ELSE
                                 COMPUTE  LIN-AX2   =
                                          LIN-AX2   +
                                          IND3   -  1.
           DISPLAY  TELA-02      GO       ROT-MENU-00.

       ROT-BANK-00.
           MOVE     20       TO  LARGURA
           COMPUTE  COL-AX3  =   COL-PSQ   +   24
           COMPUTE  COL-AX4  =   COL-PSQ   +   08.

       ROT-BANK-10.
           ADD      1        TO  IND1
           MOVE     MOV-TAB     (IND1)    TO  DES-TAB (IND1)
           IF  IND1    <     07  GO       ROT-BANK-10.
           MOVE     IND-PSQ  TO  IND3
           IF  IND3    >     05  COMPUTE  IND2 = IND2
                                       +  IND3   -  5
                                 COMPUTE  LIN-AX2   =
                                          LIN-AX2   +
                                          IND3 - IND2
           ELSE
                                 COMPUTE  LIN-AX2   =
                                          LIN-AX2   +
                                          IND3   -  1.
           DISPLAY  TELA-03      GO       ROT-MENU-00.

       ROT-CRED-00.
           MOVE     22       TO  LARGURA
           COMPUTE  COL-AX3  =   COL-PSQ   +   26
           COMPUTE  COL-AX4  =   COL-PSQ   +   10.

       ROT-CRED-10.
           ADD      1        TO  IND1
           MOVE     CRD-TAB     (IND1)    TO  DES-TAB (IND1)
           IF  IND1    <     06  GO       ROT-CRED-10.
           SET      I        TO  1
           SEARCH   IND-TAB      WHEN     IND-PSQ  =
                                          IND-TAB (I)
           SET      IND3     TO  I
           IF  IND3    >     05  COMPUTE  IND2 = IND2
                                       +  IND3   -  5
                                 COMPUTE  LIN-AX2   =
                                          LIN-AX2   +
                                          IND3 - IND2
           ELSE
                                 COMPUTE  LIN-AX2   =
                                          LIN-AX2   +
                                          IND3   -  1.
           DISPLAY  TELA-04      GO       ROT-MENU-00.

       ROT-PGTO-00.
           MOVE     11       TO  LARGURA
           COMPUTE  COL-AX3  =   COL-PSQ   +   13
           COMPUTE  COL-AX4  =   COL-PSQ   +   04.

       ROT-PGTO-10.
           ADD      1        TO  IND1
           MOVE     PGT-TAB     (IND1)    TO  DES-TAB (IND1)
           IF  IND1     <    05  GO       ROT-PGTO-10.
           DISPLAY  TELA-05      MOVE     IND-PSQ  TO  IND3
           COMPUTE  LIN-AX2   =  LIN-AX2  +  IND3   -  1.

       ROT-MENU-00.
           IF (IND2 + 04) < IND1 DISPLAY  STA-ABX
               IF  IND2     >  1 DISPLAY  STA-ACM
                   ELSE          DISPLAY  TRC-ACM
           ELSE    IF  IND2 >  1 DISPLAY  STA-ACM  TRC-ABX
                       ELSE      DISPLAY  TRC-ACM.
           DISPLAY     MENU-01   LN-SETA.

       ROT-MENU-10.
           PERFORM  ROT-MSGM-00  MOVE " " TO  IND-PSQ
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-MENU-10.
           IF  TCL-MSG   =   05  GO       ROT-MOVE-05.
           IF  TCL-MSG   =   06  GO       ROT-MOVE-06.
           IF  TCL-MSG   =   53  GO       ROT-MOVE-53.
           IF  TCL-MSG   =   54  GO       ROT-MOVE-54.
           IF  TCL-MSG   >   00  GO       ROT-MENU-10.
           IF  HEX-MSG   =   27  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
           MOVE     IND-TAB      (IND3)   TO  IND-PSQ
           PERFORM  ROT-RTLA-00  GO       ROT-EXIT-00.

       ROT-MOVE-05.
           IF  IND3      =    1  GO       ROT-MENU-10.
           IF  LIN-AX2  NOT   >  LIN-AX1
                                 SUBTRACT 1 FROM IND2
                                 SUBTRACT 1 FROM IND3
                                 GO       ROT-MENU-00.
           DISPLAY  LN-MENU      SUBTRACT 1 FROM IND3
           COMPUTE  LIN-AX2   =  LIN-AX2  -   1
           DISPLAY  LN-SETA      GO       ROT-MENU-10.

       ROT-MOVE-06.
           IF  IND3  =  IND1     GO       ROT-MENU-10.
           IF  LIN-AX2  NOT   < (LIN-AX1  +   4)
                                 ADD      1  TO  IND2
                                 ADD      1  TO  IND3
                                 GO       ROT-MENU-00.
           DISPLAY  LN-MENU      ADD      1  TO  IND3
           COMPUTE  LIN-AX2   =  LIN-AX2  +   1
           DISPLAY  LN-SETA      GO       ROT-MENU-10.

       ROT-MOVE-53.
           IF  IND2  =  1        GO       ROT-MENU-10.
           MOVE    LIN-AX1   TO  LIN-AX2
           IF  IND2     >     5  COMPUTE  IND3 = IND2 - 5
                                 COMPUTE  IND2 = IND2 - 5
           ELSE
                                 MOVE  1  TO   IND2  IND3.
           GO      ROT-MENU-00.

       ROT-MOVE-54.
           IF  IND1 < (IND2 + 5) GO       ROT-MENU-10.
           MOVE     LIN-AX1  TO  LIN-AX2
           COMPUTE  IND3     =   IND2  +  5
           COMPUTE  IND2     =   IND2  +  5
           GO       ROT-MENU-00.

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
           CLOSE    CADATV   GOBACK.
