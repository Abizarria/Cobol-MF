       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1310.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADDSP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-DSP
                       FILE          STATUS  IS  FST-DSP.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP
                       FILE          STATUS  IS  FST-LDP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCTR.CPY.
       COPY  CADDSP.CPY.
       COPY  CADLDP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-CTR          PIC  X(02).
           03  FST-DSP          PIC  X(02).
           03  FST-LDP          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(01).
           03  TECLADO          PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  PRX-AUX          PIC  9(01).
           03  ACM-PSQ          PIC  9(02).
           03  ACM-PDT          PIC  9(08)V99.
           03  ACM-LIQ          PIC S9(08)V99.

           03  DTA-LIM.
               05  ANO-LIM      PIC  9(02).
               05  MES-LIM      PIC  9(02).
               05  DIA-LIM      PIC  9(02).

           03  TAB-ATV.
               05  FILLER       PIC  X(18)  VALUE  "OBRIG. TRABALHISTA".
               05  FILLER       PIC  X(18)  VALUE  "OBRIGA€ๅES FISCAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESP. OPERACIONAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESPESA DE CAPITAL".
               05  FILLER       PIC  X(18)  VALUE  "CRDITOS DE SเCIOS".
               05  FILLER       PIC  X(18)  VALUE  "COMPRA DE PRODUTOS".
           03  RED-ATV          REDEFINES   TAB-ATV.
               05  ATV-TAB      PIC  X(18)  OCCURS  6.

           03  NOM-PSQ          PIC  X(25).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  25.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  22.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1310".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-PSQ.
           03  OPC-PSQ          PIC  9(01)  VALUE  2.
           03  IND-PSQ          PIC  X(01)  VALUE  "1".
           03  LIN-PSQ          PIC  9(02)  VALUE  10.
           03  COL-PSQ          PIC  9(02)  VALUE  34.
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
               05  LINE 06 COLUMN 13 VALUE " Administraฦo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 07 COLUMN 15 VALUE " Controle de Despesas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 06 COLUMN 35 VALUE "฿".
               05  LINE 07 COLUMN 37 VALUE "฿".
               05  LINE 07 COLUMN 14 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 08 COLUMN 13 VALUE " ษ                        
      -            "ออออออออออออออออออออออออออป ".
               05  LINE 09 COLUMN 13 VALUE " บ
      -            "                          บ ".
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
               05  LINE 08 COLUMN 17 VALUE " Cadastro de Despesas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 09 COLUMN 69 VALUE  "  ".
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

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 17 PIC X(47) FROM ALL "฿".
               05  LINE 13 COLUMN 17 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 10 COLUMN 17 PIC X(47) FROM
                   "  Cขdigo Conta :  -     -".
               05  LINE 11 COLUMN 17 PIC X(47) FROM SPACES.
               05  LINE 12 COLUMN 17 PIC X(47) FROM "  Descriฦo ...:".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 13 COLUMN 18 PIC X(46) FROM ALL "฿".
               05  LINE 10 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 11 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 64 PIC X(01) FROM     " ".

       01  ENTR-01     AUTO.
           03  SUF-E02 LINE 10 COLUMN 36 PIC ZZ9   USING SUF-DSP.
           03  DES-E02 LINE 12 COLUMN 34 PIC X(25) USING DES-DSP.
           03  PSQ-E02 LINE 12 COLUMN 34 PIC X(25) USING NOM-PSQ.

       01  ENTR-02.
           03  BCK-CLR 02 FOR-CLR 15.
               05  PRX-T02 LINE 10 COLUMN 34 PIC 9(01) FROM PRX-DSP.
               05  ATV-T02 LINE 10 COLUMN 44 PIC X(18) FROM ATV-TAB
                  (PRX-DSP).
               05  SUF-T02 LINE 10 COLUMN 36 PIC 9(03) FROM SUF-DSP.
               05  DES-T02 LINE 12 COLUMN 34 PIC X(25) FROM DES-DSP.
               05  PSQ-T02 LINE 12 COLUMN 34 PIC X(25) FROM NOM-PSQ.

       01  TELA-03.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 14 COLUMN 17 PIC X(47) FROM ALL "฿".
               05  LINE 17 COLUMN 17 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 15 COLUMN 17 PIC X(47) FROM
                   "  Total Liquidado ..... R$".
               05  LINE 16 COLUMN 17 PIC X(47) FROM
                   "  Total Pendente ...... R$".
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 17 COLUMN 18 PIC X(46) FROM ALL "฿".
               05  LINE 15 COLUMN 64 PIC X(01) FROM " ".
               05  LINE 16 COLUMN 64 PIC X(01) FROM " ".
               05  LINE 17 COLUMN 64 PIC X(01) FROM " ".

       01  ENTR-03  BCK-CLR 05 FOR-CLR 15.
           03  LINE 15 COLUMN 48 PIC ---.---.--9,99 FROM ACM-LIQ.
           03  LINE 16 COLUMN 49 PIC  ZZ.ZZZ.ZZ9,99 FROM ACM-PDT.

       01  TELA-04  BCK-CLR 4 FOR-CLR 15.
           03  LINE 17 COLUMN 39 VALUE
               " ษออออออออออออออออออออออออออออออป ".
           03  LINE 18 COLUMN 39 VALUE
               " บ  Confirma a Exclusฦo ?  [ ]  บ ".
           03  LINE 19 COLUMN 39 VALUE
               " ศออออออออออออออออออออออออออออออผ ".
           03  BCK-CLR 0 FOR-CLR 8.
               05  LINE 18 COLUMN 73 PIC X(02) FROM SPACES.
               05  LINE 19 COLUMN 73 PIC X(02) FROM SPACES.
               05  LINE 20 COLUMN 41 PIC X(34) FROM SPACES.

       01  ENTR-04  BCK-CLR 04 FOR-CLR 15
                    LINE 18 COLUMN 67 PIC X(01) USING RSP-OPC AUTO.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"      USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL   =   1   GOBACK.  DISPLAY  TELA-01
           OPEN     INPUT        CADCTR   CADLDP
                    I-O          CADDSP   READ     CADCTR
           MOVE     DTI-CTR  TO  DTA-LIM
           MOVE     31       TO  DIA-LIM.

       ROT-CODE-00.
           DISPLAY  TELA-02      PERFORM  ROT-PESQ-00.
           IF  IND-PSQ   <    1  GO       ROT-EXIT-00.
           MOVE     IND-PSQ  TO  PRX-DSP  PRX-AUX
           DISPLAY  PRX-T02      ATV-T02.

       ROT-CODE-10.
           MOVE     ZEROS    TO  SUF-DSP
           ACCEPT   SUF-E02      DISPLAY  SUF-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           MOVE     1   TO  IND2 PERFORM  ROT-GTLA-00
           IF  SUF-DSP   <   1   GO       ROT-PESQ-10.
           MOVE     PRX-AUX  TO  PRX-DSP
           READ     CADDSP
           IF  FST-DSP   =  "23" GO       ROT-INCL-00.

       ROT-EXBE-00.
           MOVE     SPACES   TO  CHV-LDP
           MOVE     CHV-DSP  TO  DSP-LDP
           MOVE     ZEROS    TO  ACM-PDT  ACM-LIQ
           DISPLAY  SUF-T02      DES-T02
                    TELA-03      ENTR-03.

       ROT-EXBE-10.
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       ROT-EXBE-20.
           READ     CADLDP  NEXT
           IF  DSP-LDP   >       CHV-DSP
                                 GO       ROT-EXBE-20.
           IF  FLG-LDP   =  "P"  ADD      VAL-LDP  TO  ACM-PDT.
           IF  DTA-LDP  NOT  >   DTA-LIM
               EVALUATE FLG-LDP
                   WHEN   "L"
                                 ADD      VAL-LDP  TO  ACM-LIQ
                   WHEN   "R"
                                 SUBTRACT VAL-LDP FROM ACM-LIQ
               END-EVALUATE.
           DISPLAY  ENTR-03      GO       ROT-EXBE-10.

       ROT-EXBE-20.
           EXIT.

       ROT-EXBE-30.
           MOVE  7  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-EXBE-30.
           IF  TCL-MSG   =   05  GO       ROT-RTRN-00.
           IF  TCL-MSG   =   06  GO       ROT-PROX-00.
           IF  TCL-MSG   =   17  PERFORM  ROT-TEST-00
                                 GO       ROT-EXCL-00.
           IF  TCL-MSG   >   00  GO       ROT-EXBE-30.
           IF  HEX-MSG   =   13  PERFORM  ROT-TEST-00
                                 GO       ROT-ALTR-00.
       ROT-EXBE-40.
           MOVE     PRX-AUX  TO  PRX-DSP
           MOVE     1  TO  IND2  MOVE  0  TO  SUF-DSP
           PERFORM  ROT-RTLA-00  GO       ROT-CODE-10.

       ROT-RTRN-00.
           START    CADDSP       KEY  <   CHV-DSP
           IF  FST-DSP   =  "23" GO       ROT-EXBE-30.
           READ     CADDSP       PREVIOUS
           IF  PRX-DSP       =   PRX-AUX
                                 GO       ROT-EXBE-00.
           READ     CADDSP  NEXT GO       ROT-EXBE-30.

       ROT-PROX-00.
           START    CADDSP       KEY  >   CHV-DSP
           IF  FST-DSP   =  "23" GO       ROT-EXBE-30.
           READ     CADDSP  NEXT
           IF  PRX-DSP       =   PRX-AUX
                                 GO       ROT-EXBE-00.
           READ     CADDSP       PREVIOUS
           GO       ROT-EXBE-30.

       ROT-TEST-00.
           READ     CADDSP       MOVE 20  TO  IND-MSG
           IF  FST-DSP   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-30.
           IF  FST-DSP   =  "23" MOVE 36  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-30.
       ROT-INCL-00.
           MOVE     SPACES   TO  DES-DSP
           PERFORM  ROT-ENTR-00  WRITE    REG-DSP
           IF  FST-DSP   =  "22" MOVE 12  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 READ     CADDSP
                                 GO       ROT-EXBE-00.
           MOVE     ZEROS    TO  ACM-PDT  ACM-LIQ
           GO       ROT-EXBE-00.

       ROT-ALTR-00.
           PERFORM  ROT-ENTR-00  REWRITE  REG-DSP
           GO       ROT-EXBE-30.

       ROT-EXCL-00.
           MOVE     13       TO  IND-MSG
           MOVE     SPACES   TO  CHV-LDP
           MOVE     CHV-DSP  TO  DSP-LDP
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       ROT-EXCL-10.
           READ     CADLDP  NEXT
           IF  DSP-LDP       =   CHV-DSP
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-EXBE-30.
       ROT-EXCL-10.
           MOVE     2        TO  IND2
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-04.

       ROT-EXCL-20.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   ENTR-04
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXBE-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-EXCL-20.
           IF  RSP-OPC  NOT   =  "S"
                                 GO       ROT-EXCL-20.
           DELETE   CADDSP       GO       ROT-EXBE-40.

       ROT-ENTR-00.
           ACCEPT   DES-E02      DISPLAY  DES-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXBE-40.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-00.
           IF  DES-DSP   =       SPACES
                                 GO       ROT-ENTR-00.
       ROT-PESQ-00.
           CALL    "DYPESQ"      USING    PRM-PSQ
                                          PRM-HLP.
       ROT-PESQ-10.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  SUF-DSP
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
           SUBTRACT  1  FROM  IND1
           IF  LET-PSQ (IND1) =  " "  GO  ROT-PESQ-30.
           MOVE     PRX-AUX  TO  PRX-DSP
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-PESQ-40.
           MOVE     22       TO  IND-MSG
           START    CADDSP       KEY  >   CHV-DSP
           IF  FST-DSP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           READ     CADDSP  NEXT
           IF  PRX-DSP  NOT  =   PRX-AUX
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           DISPLAY  SUF-T02
           INSPECT  DES-DSP      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-40.
           PERFORM  ROT-EXBE-00  THRU     ROT-EXBE-20
           GO       ROT-PESQ-70.

       ROT-PESQ-50.
           MOVE     22       TO  IND-MSG
           START    CADDSP       KEY  <   CHV-DSP
           IF  FST-DSP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           READ     CADDSP       PREVIOUS
           IF  PRX-DSP  NOT  =   PRX-AUX
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-PESQ-10.
           DISPLAY  SUF-T02
           INSPECT  DES-DSP      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PESQ-50.
           PERFORM  ROT-EXBE-00  THRU     ROT-EXBE-20
           GO       ROT-PESQ-70.

       ROT-PESQ-60.
           PERFORM  ROT-RTLA-00
           MOVE     0        TO  ACM-PSQ
           DISPLAY  SUF-T02  PSQ-T02.

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
           CLOSE    CADCTR  CADDSP  CADLDP  GOBACK.
