       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1352.
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

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCTR.CPY.
       COPY  CADDSP.CPY.
       COPY  CADLDP.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CTR          PIC  X(02).
           03  FST-DSP          PIC  X(02).
           03  FST-LDP          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  IND1             PIC  9(02).
           03  PRX-AUX          PIC  9(01).
           03  ACM-PSQ          PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  ACM-PDT          PIC  9(08)V99.
           03  ACM-LIQ          PIC S9(08)V99.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  DTA-LIM.
               05  ANO-LIM      PIC  9(02).
               05  MES-LIM      PIC  9(02).
               05  DIA-LIM      PIC  9(02).

           03  TAB-ATV.
               05  FILLER       PIC  X(18)  VALUE  "OBRIG. TRABALHISTA".
               05  FILLER       PIC  X(18)  VALUE  "OBRIGACOES FISCAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESP. OPERACIONAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESPESA DE CAPITAL".
               05  FILLER       PIC  X(18)  VALUE  "CREDITOS DE SOCIOS".
               05  FILLER       PIC  X(18)  VALUE  "COMPRA DE PRODUTOS".
           03  RED-ATV          REDEFINES   TAB-ATV.
               05  ATV-TAB      PIC  X(18)  OCCURS  6.

           03  NOM-PSQ          PIC  X(25).
           03  RED-NOM          REDEFINES   NOM-PSQ.
               05  LET-PSQ      PIC  X(01)  OCCURS  25.

       01  CAB-001.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(19)  VALUE  "EXTRATO DE CONTA :".
           03  PRX-CB2          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-CB2          PIC  9(03).
           03  FILLER           PIC  X(05)  VALUE  "  -".
           03  DES-CB2          PIC  X(39).
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "  DATA      L/P    HISTORICO DO MOVIMENTO           VALOR PA
      -    "GO     VALOR A PAGAR".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  DIA-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DT1          PIC  9(02).
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  FLG-DT1          PIC  X(06).
           03  HST-DT1          PIC  X(30).
           03  LIQ-DT1          PIC  --.---.--9,99 BLANK WHEN ZEROS.
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  PDT-DT1          PIC  ZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.

       01  TOT-001.
           03  FILLER           PIC  X(49)  VALUE
               "                   TOTAL DA CONTA ..........".
           03  LIQ-TT1          PIC  --.---.--9,99.
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  PDT-TT1          PIC  ZZ.ZZZ.ZZ9,99.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  27.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1352".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

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

       01  PRM-IMP.
           03  ARQ-IMP          PIC  9(06).
           03  PAG-IMP          PIC  9(03).
           03  FLG-IMP          PIC  9(01)  VALUE  1.
           03  LIN-IMP          PIC  9(02)  VALUE  17.
           03  CL1-IMP          PIC  9(02)  VALUE  49.
           03  CL2-IMP          PIC  9(02)  VALUE  47.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "€" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 06 COLUMN 13 VALUE " Administraá∆o Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 07 COLUMN 15 VALUE " Controle de Despesas ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 08 COLUMN 17 VALUE " Emiss∆o de Relat¢rio ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 06 COLUMN 35 VALUE "ﬂ".
               05  LINE 07 COLUMN 37 VALUE "ﬂ".
               05  LINE 08 COLUMN 39 VALUE "ﬂ".
               05  LINE 07 COLUMN 14 VALUE "‹".
               05  LINE 08 COLUMN 16 VALUE "‹".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 09 COLUMN 13 VALUE " …ÕÕ
      -            " ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª ".
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
               05  LINE 18 COLUMN 13 VALUE " »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -            "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 09 COLUMN 19 VALUE "  Extrato de C¢digos  ".
           03  BCK-CLR 00 FOR-CLR 08.
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
               05  LINE 10 COLUMN 17 PIC X(47) FROM ALL "ﬂ".
               05  LINE 14 COLUMN 17 PIC X(01) FROM     "‹".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 11 COLUMN 17 PIC X(47) FROM
                   "  C¢digo Conta :  -     -".
               05  LINE 12 COLUMN 17 PIC X(47) FROM SPACES.
               05  LINE 13 COLUMN 17 PIC X(47) FROM "  Descriá∆o ...:".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 14 COLUMN 18 PIC X(46) FROM ALL "ﬂ".
               05  LINE 11 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 64 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 64 PIC X(01) FROM     " ".

       01  ENTR-01     AUTO.
           03  SUF-E02 LINE 11 COLUMN 36 PIC ZZ9   USING SUF-DSP.
           03  PSQ-E02 LINE 13 COLUMN 34 PIC X(25) USING NOM-PSQ.

       01  ENTR-02 BCK-CLR  02 FOR-CLR   15.
           03  PRX-T02 LINE 11 COLUMN 34 PIC 9(01) FROM  PRX-DSP.
           03  ATV-T02 LINE 11 COLUMN 44 PIC X(18) FROM  ATV-TAB
              (PRX-DSP).
           03  SUF-T02 LINE 11 COLUMN 36 PIC 9(03) FROM  SUF-DSP.
           03  DES-T02 LINE 13 COLUMN 34 PIC X(25) FROM  DES-DSP.
           03  PSQ-T02 LINE 13 COLUMN 34 PIC X(25) FROM  NOM-PSQ.

       01  TELA-03 BCK-CLR 01.
           03  FOR-CLR  07.
               05  LINE 15 COLUMN 26 PIC X(30) FROM ALL "ﬂ".
               05  LINE 17 COLUMN 26 PIC X(01) FROM ALL "‹".
           03  FOR-CLR  15.
               05  LINE 16 COLUMN 26 PIC X(30) FROM
                   "   P†ginas Montadas ƒ".
               05  PAG-T04 LINE 16 COLUMN 50 PIC ZZ9 FROM PAG-CB1.
           03  FOR-CLR  00.
               05  LINE 16 COLUMN 56 PIC X(01) FROM     "€".
               05  LINE 17 COLUMN 56 PIC X(01) FROM     "€".
               05  LINE 17 COLUMN 27 PIC X(29) FROM ALL "‹".

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           ACCEPT   ARQ-IMP   FROM    TIME
           OPEN     INPUT    CADCTR   CADDSP   CADLDP
                    OUTPUT   CADTMP   READ     CADCTR.

       ROT-CODE-00.
           DISPLAY  TELA-02      PERFORM  ROT-PESQ-00
           IF  IND-PSQ   <    1  GO       ROT-EXIT-00.
           MOVE     IND-PSQ  TO  PRX-DSP  PRX-AUX
           DISPLAY  PRX-T02      ATV-T02
           PERFORM  ROT-GTLA-00.

       ROT-CODE-10.
           MOVE     ZEROS    TO  SUF-DSP
           ACCEPT   SUF-E02      DISPLAY  SUF-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
           IF  SUF-DSP   <    1  GO       ROT-PSQD-00.
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
           IF  HEX-MSG   =   13  GO       ROT-TEST-00.
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

       ROT-TEST-00.
           MOVE     SPACES   TO  CHV-LDP
           MOVE     CHV-DSP  TO  DSP-LDP
           MOVE     DTI-CTR  TO  DTA-LIM
           MOVE     31       TO  DIA-LIM.

       ROT-TEST-10.
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       ROT-ERRO-00.
           READ     CADLDP  NEXT
           IF  DSP-LDP       >   CHV-DSP
                                 GO       ROT-ERRO-00.
           IF  FLG-LDP   =  "E"  OR
              (FLG-LDP   =  "L"  AND      DTA-LDP  >
               DTA-LIM)          GO       ROT-TEST-10.

       ROT-IMPR-00.
           MOVE     ZEROS    TO  PAG-CB1  CHV-TMP
                                 ACM-PDT  ACM-LIQ
           PERFORM  ROT-NOME-00  DISPLAY  TELA-03
           MOVE     PRX-DSP  TO  PRX-CB2
           MOVE     SUF-DSP  TO  SUF-CB2
           MOVE     DES-DSP  TO  DES-CB2
           MOVE     SPACES   TO  CHV-LDP
           MOVE     CHV-DSP  TO  DSP-LDP.

       ROT-IMPR-10.
           DISPLAY  PAG-T04
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
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       ROT-IMPR-40.
           READ     CADLDP  NEXT
           IF  DSP-LDP   >       CHV-DSP
                                 GO       ROT-IMPR-40.
           IF  FLG-LDP   =  "E"  GO       ROT-IMPR-20.
           IF  FLG-LDP  NOT  =   "P" AND  DTA-LDP  >   DTA-LIM
                                 GO       ROT-IMPR-20.
           IF  LIN-TMP   <   60  GO       ROT-IMPR-30.
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
           WRITE    REG-TMP      PERFORM  ROT-IMPR-10.

       ROT-IMPR-30.
           MOVE     DTA-LDP  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     ANO-AUX  TO  ANO-DT1
           MOVE     FLG-LDP  TO  FLG-DT1
           MOVE     HST-LDP  TO  HST-DT1
           IF  FLG-LDP   =  "P"  MOVE     VAL-LDP  TO  PDT-DT1
                                 ADD      VAL-LDP  TO  ACM-PDT
                                 MOVE     ZEROS    TO  LIQ-DT1
           ELSE
               MOVE  ZEROS   TO  PDT-DT1
               IF  FLG-LDP   =   "L"
                                 MOVE     VAL-LDP  TO  LIQ-DT1
                                 ADD      VAL-LDP  TO  ACM-LIQ
               ELSE
                                 COMPUTE  LIQ-DT1  =   VAL-LDP
                                             -  2  *   VAL-LDP
                                 SUBTRACT VAL-LDP FROM ACM-LIQ.
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-20.

       ROT-IMPR-40.
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           MOVE     ACM-PDT  TO  PDT-TT1
           MOVE     ACM-LIQ  TO  LIQ-TT1
           ADD      1        TO  LIN-TMP
           ADD      1        TO  CHV-TMP
           MOVE     TOT-001  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     PAG-CB1  TO  PAG-IMP
           DISPLAY  PAG-T04      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           DELETE     FILE       CADTMP
           OPEN      OUTPUT      CADTMP
           PERFORM  ROT-RTLA-00  GO       ROT-CODE-10.

       ROT-PSQD-00.
           MOVE     SPACES   TO  NOM-PSQ
           MOVE     ZEROS    TO  SUF-DSP
           DISPLAY  SUF-T02.

       ROT-PSQD-10.
           ACCEPT   PSQ-E02      DISPLAY  PSQ-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-CODE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-PSQD-10.
           IF  NOM-PSQ   =       SPACES
                                 GO       ROT-PSQD-10.
           MOVE     26  TO  IND1 PERFORM  ROT-PSQD-50.

       ROT-PSQD-20.
           SUBTRACT  1  FROM  IND1
           IF  LET-PSQ (IND1) =  " "  GO  ROT-PSQD-20.
           MOVE     PRX-AUX  TO  PRX-DSP
           MOVE     ZEROS    TO  ACM-PSQ.

       ROT-PSQD-30.
           MOVE     22       TO  IND-MSG
           START    CADDSP       KEY  >   CHV-DSP
           IF  FST-DSP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PSQD-00.
           READ     CADDSP  NEXT
           IF  PRX-DSP  NOT  =   PRX-AUX
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-PSQD-00.
           DISPLAY  SUF-T02
           INSPECT  DES-DSP      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PSQD-30.
           PERFORM  ROT-EXBE-00  GO       ROT-PSQD-60.

       ROT-PSQD-40.
           MOVE     22       TO  IND-MSG
           START    CADDSP       KEY  <   CHV-DSP
           IF  FST-DSP   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-PSQD-00.
           READ     CADDSP       PREVIOUS
           IF  PRX-DSP  NOT  =   PRX-AUX
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-PSQD-00.
           DISPLAY  SUF-T02
           INSPECT  DES-DSP      TALLYING ACM-PSQ
                                 FOR  ALL NOM-PSQ (1: IND1)
           IF  ACM-PSQ   =   00  GO       ROT-PSQD-40.
           PERFORM  ROT-EXBE-00  GO       ROT-PSQD-60.

       ROT-PSQD-50.
           PERFORM  ROT-RTLA-00
           MOVE     0        TO  ACM-PSQ
           DISPLAY  SUF-T02      PSQ-T02.

       ROT-PSQD-60.
           MOVE  8  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-PSQD-60.
           IF  TCL-MSG   =   05  PERFORM  ROT-PSQD-50
                                 GO       ROT-PSQD-40.
           IF  TCL-MSG   =   06  PERFORM  ROT-PSQD-50
                                 GO       ROT-PSQD-30.
           IF  TCL-MSG   >   00  GO       ROT-PSQD-60.
           IF  HEX-MSG   =   13  GO       ROT-TEST-00.
           PERFORM  ROT-RTLA-00  GO       ROT-PSQD-00.

       ROT-ERRO-00.
           MOVE 31  TO  IND-MSG  PERFORM  ROT-MSGM-00
           PERFORM  ROT-RTLA-00  GO       ROT-CODE-10.

       ROT-PESQ-00.
           CALL   "DYPESQ"  USING  PRM-PSQ
                                   PRM-HLP.
       ROT-MSGM-00.
           CALL   "DYMSGM"  USING  PRM-MSG.

       ROT-HELP-00.
           CALL   "DYHELP"  USING  PRM-HLP.

       ROT-GTLA-00.
           MOVE   "G"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE   "R"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

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
           CLOSE    CADCTR  CADDSP  CADLDP  CADTMP
           DELETE   FILE    CADTMP  GOBACK.
