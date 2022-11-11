       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1320.
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

           SELECT      CADSDO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SDO
                       FILE          STATUS  IS  FST-SDO.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCTR.CPY.
       COPY  CADDSP.CPY.
       COPY  CADLDP.CPY.
       COPY  CADSDO.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-CTR          PIC  X(02).
           03  FST-DSP          PIC  X(02).
           03  FST-LDP          PIC  X(02).
           03  FST-SDO          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(01).
           03  TECLADO          PIC  9(02).
           03  PRX-AUX          PIC  9(01).
           03  LIN-AUX          PIC  9(02).
           03  ACM-PSQ          PIC  9(02).

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

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  23.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1320".
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

       01  PRM-DTA.
           03  DTA-DTA.
               05  ANO-DTA      PIC  9(02).
               05  MES-DTA      PIC  9(02).
               05  DIA-DTA      PIC  9(02).
           03  LIN-DTA          PIC  9(02)  VALUE  17.
           03  COL-DTA          PIC  9(02)  VALUE  34.
           03  BCK-DTA          PIC  9(01)  VALUE  6.
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
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "€" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 06 COLUMN 13 VALUE " Administraá∆o Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 07 COLUMN 15 VALUE " Controle de Despesas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 06 COLUMN 35 VALUE "ﬂ".
               05  LINE 07 COLUMN 37 VALUE "ﬂ".
               05  LINE 07 COLUMN 14 VALUE "‹".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 08 COLUMN 13 VALUE " …                        
      -            "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª ".
               05  LINE 09 COLUMN 13 VALUE " ∫
      -            "                          ∫ ".
               05  LINE 10 COLUMN 13 VALUE " ∫
      -            "                          ∫ ".
               05  LINE 11 COLUMN 13 VALUE " ∫
      -            "                          ∫ ".
               05  LINE 12 COLUMN 13 VALUE " ∫
      -            "                          ∫ ".
               05  LINE 13 COLUMN 13 VALUE " ∫
      -            "                          ∫ ".
               05  LINE 14 COLUMN 13 VALUE " ∫
      -            "                          ∫ ".
               05  LINE 15 COLUMN 13 VALUE " ∫
      -            "                          ∫ ".
               05  LINE 16 COLUMN 13 VALUE " ∫
      -            "                          ∫ ".
               05  LINE 17 COLUMN 13 VALUE " ∫
      -            "                          ∫ ".
               05  LINE 18 COLUMN 13 VALUE " ∫
      -            "                          ∫ ".
               05  LINE 19 COLUMN 13 VALUE " »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -            "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 08 COLUMN 17 VALUE " Lanáa Contas Ö Pagar ".
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
               05  LINE 19 COLUMN 69 VALUE  "  ".
               05  LINE 20 COLUMN 15 PIC X(56) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 09 COLUMN 17 PIC X(47) FROM ALL "ﬂ".
               05  LINE 13 COLUMN 17 PIC X(01) FROM     "‹".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 10 COLUMN 17 PIC X(47) FROM
                   "  C¢digo Conta :  -     -".
               05  LINE 11 COLUMN 17 PIC X(47) FROM SPACES.
               05  LINE 12 COLUMN 17 PIC X(47) FROM "  Descriá∆o ...:".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 13 COLUMN 18 PIC X(46) FROM ALL "ﬂ".
               05  LINE 10 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 11 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 64 PIC X(01) FROM     " ".

       01  ENTR-01    AUTO.
           03  SUF-E02 LINE 10 COLUMN 36 PIC ZZ9   USING SUF-DSP.
           03  PSQ-E02 LINE 12 COLUMN 34 PIC X(25) USING NOM-PSQ.

       01  ENTR-02 BCK-CLR  02 FOR-CLR   15.
           03  PRX-T02 LINE 10 COLUMN 34 PIC 9(01) FROM  PRX-DSP.
           03  ATV-T02 LINE 10 COLUMN 44 PIC X(18) FROM  ATV-TAB
              (PRX-DSP).
           03  SUF-T02 LINE 10 COLUMN 36 PIC 9(03) FROM  SUF-DSP.
           03  DES-T02 LINE 12 COLUMN 34 PIC X(25) FROM  DES-DSP.
           03  PSQ-T02 LINE 12 COLUMN 34 PIC X(25) FROM  NOM-PSQ.

       01  TELA-03.
           03  BCK-CLR 06 FOR-CLR 07.
               05  LINE 14 COLUMN 17 PIC X(47) FROM ALL "ﬂ".
               05  LINE 18 COLUMN 17 PIC X(01) FROM     "‹".
           03  BCK-CLR 06 FOR-CLR 14.
               05  LINE 15 COLUMN 17 PIC X(47) FROM
                   "  Hist¢rico ...:".
               05  LINE 16 COLUMN 17 PIC X(47) FROM SPACES.
               05  LINE 17 COLUMN 17 PIC X(47) FROM
                   "  Vencimento ..:              R$".
           03  BCK-CLR 00 FOR-CLR 06.
               05  LINE 18 COLUMN 18 PIC X(46) FROM ALL "ﬂ".
               05  LINE 15 COLUMN 64 PIC X(01) FROM " ".
               05  LINE 16 COLUMN 64 PIC X(01) FROM " ".
               05  LINE 17 COLUMN 64 PIC X(01) FROM " ".
               05  LINE 18 COLUMN 64 PIC X(01) FROM " ".

       01  ENTR-03     AUTO.
           03  HST-E03 LINE 15 COLUMN 34 PIC X(25)        USING HST-LDP.
           03  VAL-E03 LINE 17 COLUMN 50 PIC Z.ZZZ.ZZ9,99 USING VAL-LDP.

       01  ENTR-04    BCK-CLR 06 FOR-CLR 15.
           03  HST-T03 LINE 15 COLUMN 34 PIC X(25)         FROM HST-LDP.
           03  VAL-T03 LINE 17 COLUMN 50 PIC Z.ZZZ.ZZ9,99  FROM VAL-LDP.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           OPEN     I-O      CADLDP
                    INPUT    CADCTR   CADDSP
           READ     CADCTR.

       ROT-CODE-00.
           DISPLAY  TELA-02      PERFORM  ROT-PESQ-00
           IF  IND-PSQ   <    1  GO       ROT-EXIT-00.
           MOVE     IND-PSQ  TO  PRX-DSP  PRX-AUX
           DISPLAY  PRX-T02      ATV-T02
           MOVE     1   TO  IND2 PERFORM  ROT-GTLA-00.

       ROT-CODE-10.
           MOVE     ZEROS    TO  SUF-DSP
           ACCEPT   SUF-E02      DISPLAY  SUF-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           IF  SUF-DSP   <   1   GO       ROT-PESQ-10.
           MOVE     PRX-AUX  TO  PRX-DSP
           READ     CADDSP       MOVE 21  TO  IND-MSG
           IF  FST-DSP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-10.
       ROT-EXBE-00.
           DISPLAY  SUF-T02      DES-T02.

       ROT-EXBE-10.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-EXBE-10.
           IF  TCL-MSG   =   05  GO       ROT-EXBE-20.
           IF  TCL-MSG   =   06  GO       ROT-EXBE-30.
           IF  TCL-MSG   >   00  GO       ROT-EXBE-10.
           IF  HEX-MSG   =   13  GO       ROT-ENTR-00.
           PERFORM  ROT-RTLA-00  GO       ROT-CODE-10.

       ROT-EXBE-20.
           START    CADDSP       KEY  <   CHV-DSP
           IF  FST-DSP   =  "23" GO       ROT-EXBE-10.
           READ     CADDSP       PREVIOUS
           IF  PRX-DSP       =   PRX-AUX
                                 GO       ROT-EXBE-00.
           READ     CADDSP  NEXT GO       ROT-EXBE-10.

       ROT-EXBE-30.
           START    CADDSP       KEY  >   CHV-DSP
           IF  FST-DSP   =  "23" GO       ROT-EXBE-10.
           READ     CADDSP  NEXT
           IF  PRX-DSP       =   PRX-AUX
                                 GO       ROT-EXBE-00.
           READ     CADDSP       PREVIOUS
           GO       ROT-EXBE-10.

       ROT-ENTR-00.
           MOVE     2        TO  IND2
           MOVE     SPACES   TO  HST-LDP
           MOVE     ZEROS    TO  DTA-DTA  VAL-LDP
           DISPLAY  TELA-03      PERFORM  ROT-GTLA-00.

       ROT-ENTR-10.
           ACCEPT   HST-E03      DISPLAY  HST-T03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  MOVE     1  TO  IND2
                                 PERFORM  ROT-RTLA-00
                                 GO       ROT-CODE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
           IF  HST-LDP   =       SPACES
                                 GO       ROT-ENTR-10.
       ROT-ENTR-20.
           PERFORM  ROT-DATA-00  MOVE 18  TO  IND-MSG
           IF  FLG-DTA   =    0  GO       ROT-ENTR-10.
           IF  DTA-DTA   <       DTI-CTR
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-20.
       ROT-ENTR-30.
           ACCEPT   VAL-E03      DISPLAY  VAL-T03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-30.
           IF  VAL-LDP   =       ZEROS
                                 GO       ROT-ENTR-30.
       ROT-ENTR-40.
           MOVE 10  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-40.
           IF  TCL-MSG   >   00  GO       ROT-ENTR-40.
           IF  HEX-MSG   =   27  GO       ROT-ENTR-30.
           MOVE     CHV-DSP  TO  DSP-LDP
           MOVE     DTA-DTA  TO  DTA-LDP
           MOVE     "P"      TO  FLG-LDP
           MOVE     1        TO  LCT-LDP.

       ROT-ENTR-50.
           WRITE    REG-LDP      ADD  1   TO  LCT-LDP
           IF  FST-LDP   =  "22" GO       ROT-ENTR-50.
           OPEN     I-O          CADSDO
           MOVE     2        TO  PRX-SDO
           MOVE     PRX-LDP  TO  SUF-SDO.

       ROT-ENTR-60.
           READ     CADSDO       MOVE 20  TO  IND-MSG
           IF  FST-SDO   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-60.
           ADD      VAL-LDP  TO  ENT-SDO
           REWRITE  REG-SDO      CLOSE    CADSDO
           PERFORM  ROT-RTLA-00  GO       ROT-ENTR-00.

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
           SUBTRACT  1      FROM IND1
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
           PERFORM  ROT-EXBE-00  GO       ROT-PESQ-70.

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
           PERFORM  ROT-EXBE-00  GO       ROT-PESQ-70.

       ROT-PESQ-60.
           PERFORM  ROT-RTLA-00
           MOVE     0        TO  ACM-PSQ
           DISPLAY  SUF-T02      PSQ-T02.

       ROT-PESQ-70.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-PESQ-70.
           IF  TCL-MSG   =   05  PERFORM  ROT-PESQ-60
                                 GO       ROT-PESQ-50.
           IF  TCL-MSG   =   06  PERFORM  ROT-PESQ-60
                                 GO       ROT-PESQ-40.
           IF  TCL-MSG   >   00  GO       ROT-PESQ-70.
           IF  HEX-MSG   =   13  GO       ROT-ENTR-00.
           PERFORM  ROT-RTLA-00  GO       ROT-PESQ-10.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-DATA-00.
           CALL    "DYDATA"  USING  PRM-DTA
                                    PRM-HLP.
       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR (IND2)
           CALL    "DYTELA"  USING  PRM-SCR (IND2).

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR (IND2)
           CALL    "DYTELA"  USING  PRM-SCR (IND2).

       ROT-EXIT-00.
           CLOSE    CADCTR   CADDSP  CADLDP  GOBACK.
