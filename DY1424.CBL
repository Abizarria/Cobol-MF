       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1424.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADDSP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-DSP
                       FILE          STATUS  IS  FST-DSP.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC
                       FILE          STATUS  IS  FST-LBC.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP
                       FILE          STATUS  IS  FST-LDP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADDSP.CPY.
       COPY  CADLBC.CPY.
       COPY  CADLDP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-DSP          PIC  X(02).
           03  FST-LBC          PIC  X(02).
           03  FST-LDP          PIC  X(02).

           03  IND1             PIC  9(02).
           03  ACM-PSQ          PIC  9(02).
           03  PRX-AUX          PIC  9(01).
           03  TECLADO          PIC  9(02).

           03  TAB-ATV.
               05  FILLER       PIC  X(18)  VALUE  "OBRIG. TRABALHISTA".
               05  FILLER       PIC  X(18)  VALUE  "OBRIGAÄÂES FISCAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESP. OPERACIONAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESPESA DE CAPITAL".
               05  FILLER       PIC  X(18)  VALUE  "CRêDITOS DE S‡CIOS".
               05  FILLER       PIC  X(18)  VALUE  "COMPRA DE PRODUTOS".
           03  RED-ATV          REDEFINES   TAB-ATV.
               05  ATV-TAB      PIC  X(18)  OCCURS  6.

           03  NOM-PSQ          PIC  X(25).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  25.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1424".
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
           03  LIN-PSQ          PIC  9(02)  VALUE  14.
           03  COL-PSQ          PIC  9(02)  VALUE  34.
           03  SB1-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB2-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB3-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB4-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB5-PSQ          PIC  X(02)  VALUE  "ÕÕ".
           03  SB6-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB7-PSQ          PIC  X(26)  VALUE  SPACES.

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

       01  PRM-LCT.
           03  BCO-LCT          PIC  9(03).
           03  DTA-LCT          PIC  9(06).

       SCREEN          SECTION.

       01  TELA-01.
           03  BCK-CLR 06 FOR-CLR 01.
               05  LINE 14 COLUMN 18 PIC X(45) FROM ALL "ﬂ".
               05  LINE 17 COLUMN 18 PIC X(01) FROM     "‹".
           03  BCK-CLR 06 FOR-CLR 14.
               05  LINE 15 COLUMN 18 PIC X(46) FROM
                   "  C¢d. Despesa :  -    -".
               05  LINE 16 COLUMN 18 PIC X(46) FROM "  Descriá∆o ...:".
           03  BCK-CLR 00 FOR-CLR 06.
               05  LINE 15 COLUMN 63 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 63 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 63 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 19 PIC X(44) FROM ALL "ﬂ".

       01  ENTR-01     AUTO.
           03  SUF-E01 LINE 15 COLUMN 37 PIC ZZ9          USING SUF-DSP.
           03  PSQ-E01 LINE 16 COLUMN 35 PIC X(25)        USING NOM-PSQ.
           03  DOC-E01 LINE 13 COLUMN 60 PIC ZZZZZZ9      USING DOC-LBC.
           03  HST-E01 LINE 15 COLUMN 29 PIC X(25)        USING HST-LBC.
           03  VAL-E01 LINE 17 COLUMN 29 PIC Z.ZZZ.ZZ9,99 USING VAL-LBC.

       01  ENTR-02  BCK-CLR 06 FOR-CLR 15.
           03  PRX-E02 LINE 15 COLUMN 35 PIC 9(01)         FROM PRX-DSP.
           03  ATV-E02 LINE 15 COLUMN 43 PIC X(18)         FROM ATV-TAB
              (PRX-DSP).
           03  SUF-E02 LINE 15 COLUMN 37 PIC 9(03)         FROM SUF-DSP.
           03  PSQ-E02 LINE 16 COLUMN 35 PIC X(25)         FROM NOM-PSQ.
           03  DES-E02 LINE 16 COLUMN 35 PIC X(25)         FROM DES-DSP.

       01  ENTR-03  BCK-CLR 01 FOR-CLR 15.
           03  DOC-E03 LINE 13 COLUMN 60 PIC 9(07)         FROM DOC-LBC.
           03  HST-E03 LINE 15 COLUMN 29 PIC X(25)         FROM HST-LBC.
           03  VAL-E03 LINE 17 COLUMN 29 PIC Z.ZZZ.ZZ9,99  FROM VAL-LBC.

       PROCEDURE       DIVISION  USING  PRM-LCT.

       ROT-0000-00.
           OPEN  I-O    CADLBC   CADLDP
                 INPUT  CADDSP   PERFORM  ROT-GTLA-00.

       ROT-CODE-00.
           DISPLAY  TELA-01      PERFORM  ROT-PESQ-00
           IF  IND-PSQ   <    1  PERFORM  ROT-RTLA-00
                                 GO       ROT-EXIT-00.
           MOVE     IND-PSQ  TO  PRX-DSP  PRX-AUX
           DISPLAY  PRX-E02      ATV-E02.

       ROT-CODE-10.
           MOVE     ZEROS    TO  SUF-DSP
           ACCEPT   SUF-E01      DISPLAY  SUF-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           IF  SUF-DSP   =    0  GO       ROT-NOME-00.
           MOVE     PRX-AUX  TO  PRX-DSP
           READ     CADDSP       MOVE 21  TO  IND-MSG
           IF  FST-DSP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-10.
       ROT-CODE-20.
           DISPLAY  SUF-E02      DES-E02.

       ROT-CODE-30.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-30.
           IF  TCL-MSG   =   05  GO       ROT-CODE-40.
           IF  TCL-MSG   =   06  GO       ROT-CODE-50.
           IF  TCL-MSG   >   00  GO       ROT-CODE-30.
           IF  HEX-MSG   =   13  PERFORM  ROT-RTLA-00
                                 GO       ROT-ENTR-00.
           MOVE     SPACES   TO  DES-DSP
           DISPLAY  DES-E02      GO       ROT-CODE-10.

       ROT-CODE-40.
           START    CADDSP       KEY  <   CHV-DSP
           IF  FST-DSP   =  "23" GO       ROT-CODE-30.
           READ     CADDSP       PREVIOUS
           IF  PRX-DSP       =   PRX-AUX
                                 GO       ROT-CODE-20.
           READ     CADDSP  NEXT GO       ROT-CODE-30.

       ROT-CODE-50.
           START    CADDSP       KEY  >   CHV-DSP
           IF  FST-DSP   =  "23" GO       ROT-CODE-30.
           READ     CADDSP  NEXT
           IF  PRX-DSP       =   PRX-AUX
                                 GO       ROT-CODE-20.
           READ     CADDSP       PREVIOUS
           GO       ROT-CODE-30.

       ROT-NOME-00.
           MOVE     SPACES   TO  NOM-PSQ  DES-DSP
           MOVE  0  TO  SUF-DSP  DISPLAY  SUF-E02.

       ROT-NOME-10.
           ACCEPT   PSQ-E01      DISPLAY  PSQ-E02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  DES-E02
                                 GO       ROT-CODE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-NOME-10.
           IF  NOM-PSQ   =       SPACES
                                 GO       ROT-NOME-10.
           MOVE     26  TO  IND1 PERFORM  ROT-NOME-50.

       ROT-NOME-20.
           SUBTRACT  1  FROM  IND1
           IF  LET-PSQ (IND1) =  " "  GO  ROT-NOME-20.
           MOVE     PRX-AUX  TO  PRX-DSP
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-NOME-30.
           MOVE     22       TO  IND-MSG
           START    CADDSP       KEY  >   CHV-DSP
           IF  FST-DSP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           READ     CADDSP  NEXT
           IF  PRX-DSP  NOT  =   PRX-AUX
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           DISPLAY  SUF-E02
           INSPECT  DES-DSP      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-NOME-30.
           PERFORM  ROT-CODE-20  GO       ROT-NOME-60.

       ROT-NOME-40.
           MOVE     22       TO  IND-MSG
           START    CADDSP       KEY  <   CHV-DSP
           IF  FST-DSP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           READ     CADDSP       PREVIOUS
           IF  PRX-DSP  NOT  =   PRX-AUX
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-NOME-00.
           DISPLAY  SUF-E02
           INSPECT  DES-DSP      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-NOME-40.
           PERFORM  ROT-CODE-20  GO       ROT-NOME-60.

       ROT-NOME-50.
           MOVE     0        TO  ACM-PSQ
           DISPLAY  SUF-E02      PSQ-E02.

       ROT-NOME-60.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-NOME-60.
           IF  TCL-MSG   =   05  PERFORM  ROT-NOME-50
                                 GO       ROT-NOME-40.
           IF  TCL-MSG   =   06  PERFORM  ROT-NOME-50
                                 GO       ROT-NOME-30.
           IF  TCL-MSG   >   00  GO       ROT-NOME-60.
           IF  HEX-MSG   =   27  GO       ROT-NOME-00.
           PERFORM  ROT-RTLA-00.

       ROT-ENTR-00.
           MOVE     SPACES   TO  HST-LBC
           MOVE     ZEROS    TO  LCT-LBC  LCT-LDP
                                 DOC-LBC  VAL-LBC.
       ROT-ENTR-10.
           ACCEPT   DOC-E01      DISPLAY  DOC-E03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
           IF  DOC-LBC   =    0  GO       ROT-ENTR-10.

       ROT-ENTR-20.
           ACCEPT   HST-E01      DISPLAY  HST-E03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-20.
           IF  HST-LBC   =       SPACES
                                 GO       ROT-ENTR-20.
       ROT-ENTR-30.
           ACCEPT   VAL-E01      DISPLAY  VAL-E03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-30.
           IF  VAL-LBC   =    0  GO       ROT-ENTR-30.

       ROT-ENTR-40.
           MOVE 10  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-40.
           IF  TCL-MSG   >   00  GO       ROT-ENTR-40.
           IF  HEX-MSG   =   27  GO       ROT-ENTR-30.
           MOVE     BCO-LCT  TO  BCO-LBC
           MOVE     DTA-LCT  TO  DTA-LBC  DTA-LDP
           MOVE      5       TO  COD-LBC
           MOVE     "L"      TO  FLG-LDP
           MOVE     CHV-DSP  TO  DSP-LDP
           MOVE     HST-LBC  TO  HST-LDP
           MOVE     VAL-LBC  TO  VAL-LDP.

       ROT-ENTR-50.
           WRITE    REG-LDP      ADD   1  TO  LCT-LDP
           IF  FST-LDP   =  "22" GO       ROT-ENTR-50.

       ROT-ENTR-60.
           WRITE    REG-LBC      ADD   2  TO  LCT-LBC
           IF  FST-LBC   =  "22" GO       ROT-ENTR-60.
           PERFORM  ROT-RTLA-00  GO       ROT-EXIT-00.

       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-PESQ-00.
           CALL    "DYPESQ"  USING  PRM-PSQ
                                    PRM-HLP.
       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-EXIT-00.
           CLOSE    CADDSP  CADLBC  CADLDP  GOBACK.
