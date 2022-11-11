       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1553.
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

           SELECT      CADNTA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTA
                       FILE          STATUS  IS  FST-NTA.

           SELECT      CADSRT        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-SRT.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCRD.CPY.
       COPY  CADNTA.CPY.
       COPY  CADTMP.CPY.

       SD  CADSRT.

       01  REG-SRT.
           03  DTA-SRT          PIC  9(06).
           03  CLI-SRT          PIC  9(05).
           03  NUM-SRT          PIC  9(06).
           03  DES-SRT          PIC  X(18).
           03  PCA-SRT          PIC  X(07).
           03  VAL-SRT          PIC  9(06)V99.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CRD          PIC  X(02).
           03  FST-NTA          PIC  X(02).
           03  FST-SRT          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  NUM-OPC          PIC  9(01).
           03  DTA-INI          PIC  9(06).
           03  DTA-FIN          PIC  9(06).
           03  ACM-AUX          PIC  9(07)V99.
           03  TOT-AUX          PIC  9(07)V99.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  DTA-DET.
               05  DIA-DET      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-DET      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-DET      PIC  9(02).

           03  PCA-AUX.
               05  PC1-AUX      PIC  X(03).
               05  PC2-AUX      PIC  9(04).

           03  TAB-CRD.
               05  FILLER       PIC  X(20) VALUE "REQUISICOES EMPRESAS".
               05  FILLER       PIC  X(20) VALUE "CHEQUES  PRE-DATADOS".
               05  FILLER       PIC  X(20) VALUE "AMERICA EXPRESS CARD".
               05  FILLER       PIC  X(20) VALUE "MASTERCARD E DINER'S".
               05  FILLER       PIC  X(20) VALUE "CARTAO VISA E OUTROS".
               05  FILLER       PIC  X(20) VALUE "VALES FRETE DIVERSOS".
           03  RED-CRD          REDEFINES  TAB-CRD.
               05  CRD-TAB      PIC  X(20) OCCURS  6.

       01  CAB-001.
           03  FILLER           PIC  X(19)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(09)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(11)  VALUE  "RELACAO DE".
           03  TIP-CB2          PIC  X(57).
           03  DTA-CB2          PIC  X(12).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "  DATA   EMPRESA NR.NOTA  DESCRICAO DA NOTA     PLACA   VALO
      -    "R NOTA  TOTAL DO DIA".

       01  CAB-005              PIC  X(80)  VALUE
           "  DATA       NUMERO     NOME / DESCRICAO                   V
      -    "ALOR    TOTAL DO DIA".

       01  CAB-006              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  DTA-DT1          PIC  X(08).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  CLI-DT1          PIC  9(05).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  NUM-DT1          PIC  999.999.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  DES-DT1          PIC  X(20).
           03  PC1-DT1          PIC  X(03).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  PC2-DT1          PIC  9(04).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  VAL-DT1          PIC  ZZZ.ZZ9,99.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  TOT-DT1          PIC  Z.ZZZ.ZZ9,99  BLANK WHEN ZEROS.

       01  DET-002.
           03  DTA-DT2          PIC  X(08).
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  NUM-DT2          PIC  9(06).
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  DES-DT2          PIC  X(30).
           03  VAL-DT2          PIC  ZZZ.ZZ9,99.
           03  FILLER           PIC  X(04).
           03  TOT-DT2          PIC  Z.ZZZ.ZZ9,99  BLANK WHEN ZEROS.

       01  TOT-001.
           03  FILLER           PIC  X(24)  VALUE  SPACES.
           03  FILLER           PIC  X(44)  VALUE
               "- TOTAL GERAL ............. R$".
           03  VAL-TT1          PIC  Z.ZZZ.ZZ9,99.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  50.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  31.
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1553".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-DTA.
           03  DTA-DTA.
               05  ANO-DTA      PIC  9(02).
               05  MES-DTA      PIC  9(02).
               05  DIA-DTA      PIC  9(02).
           03  LIN-DTA          PIC  9(02)  VALUE  13.
           03  COL-DTA          PIC  9(02).
           03  BCK-DTA          PIC  9(01)  VALUE  2.
           03  FOR-DTA          PIC  9(01)  VALUE  7.
           03  FLG-DTA          PIC  9(01).

       01  PRM-PSQ.
           03  OPC-PSQ          PIC  9(01)  VALUE  4.
           03  IND-PSQ          PIC  X(01)  VALUE  "1".
           03  LIN-PSQ          PIC  9(02)  VALUE  13.
           03  COL-PSQ          PIC  9(02)  VALUE  37.
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
           03  CL1-IMP          PIC  9(02)  VALUE  41.
           03  CL2-IMP          PIC  9(02)  VALUE  46.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 07 COLUMN 19 VALUE " Administra‡Æo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 08 COLUMN 21 VALUE " Controle de Clientes ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 09 COLUMN 23 VALUE " EmissÆo de Relat¢rio ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 07 COLUMN 41 VALUE "ß".
               05  LINE 08 COLUMN 43 VALUE "ß".
               05  LINE 09 COLUMN 45 VALUE "ß".
               05  LINE 08 COLUMN 20 VALUE "Ü".
               05  LINE 09 COLUMN 22 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 10 COLUMN 19 VALUE
                   " ÉÍÍ                        ÍÍÍÍÍÍÍÍÍÍÍ» ".
               05  LINE 11 COLUMN 19 VALUE
                   " º                                       º ".
               05  LINE 12 COLUMN 19 VALUE
                   " º                                       º ".
               05  LINE 13 COLUMN 19 VALUE
                   " º                                       º ".
               05  LINE 14 COLUMN 19 VALUE
                   " º                                       º ".
               05  LINE 15 COLUMN 19 VALUE
                   " º                                       º ".
               05  LINE 16 COLUMN 19 VALUE
                   " º                                       º ".
               05  LINE 17 COLUMN 19 VALUE
                   " º                                       º ".
               05  LINE 18 COLUMN 19 VALUE
                   " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 10 COLUMN 25 VALUE " Rela‡Æo dos Cr‚ditos ".
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 11 COLUMN 23 PIC X(34) FROM ALL "ß".
               05  LINE 14 COLUMN 23 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 12 COLUMN 23 PIC X(34) FROM "  Cr‚dito :".
               05  LINE 13 COLUMN 23 PIC X(34) FROM
                   "  Per¡odo :          …".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 12 COLUMN 57 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 57 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 57 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 24 PIC X(33) FROM ALL "ß".
               05  LINE 11 COLUMN 62 VALUE "  ".
               05  LINE 12 COLUMN 62 VALUE "  ".
               05  LINE 13 COLUMN 62 VALUE "  ".
               05  LINE 14 COLUMN 62 VALUE "  ".
               05  LINE 15 COLUMN 62 VALUE "  ".
               05  LINE 16 COLUMN 62 VALUE "  ".
               05  LINE 17 COLUMN 62 VALUE "  ".
               05  LINE 18 COLUMN 62 VALUE "  ".
               05  LINE 19 COLUMN 21 PIC X(43) FROM SPACES.

       01  TELA-02 BCK-CLR 01.
           03  LINE 15 COLUMN 23 PIC X(34) FROM ALL "ß" FOR-CLR 07.
           03  LINE 17 COLUMN 23 PIC X(01) FROM     "Ü" FOR-CLR 07.
           03  LINE 16 COLUMN 23 PIC X(34) FROM
                       "     P ginas Montadas Ä      " FOR-CLR 15.
           03  PAG-T02 LINE 16 COLUMN 49 PIC ZZ9
                                           FROM PAG-CB1 FOR-CLR 15.
           03  LINE 16 COLUMN 57 PIC X(01) FROM     "Û" FOR-CLR 00.
           03  LINE 17 COLUMN 57 PIC X(01) FROM     "Û" FOR-CLR 00.
           03  LINE 17 COLUMN 24 PIC X(33) FROM ALL "Ü" FOR-CLR 00.

       01  TELA-03 BCK-CLR 05.
           03  LINE 15 COLUMN 23 PIC X(34) FROM ALL "ß" FOR-CLR 07.
           03  LINE 17 COLUMN 23 PIC X(01) FROM     "Ü" FOR-CLR 07.
           03  LINE 16 COLUMN 23 PIC X(34) FROM
                   "  Aguarde, Sorteando Arquivo ...  " FOR-CLR 15.
           03  LINE 16 COLUMN 57 PIC X(01) FROM     "Û" FOR-CLR 00.
           03  LINE 17 COLUMN 57 PIC X(01) FROM     "Û" FOR-CLR 00.
           03  LINE 17 COLUMN 24 PIC X(33) FROM ALL "Ü" FOR-CLR 00.

       01  ENTR-01 BCK-CLR 02 FOR-CLR 15
                   LINE 12 COLUMN 35 PIC X(20) FROM CRD-TAB (NUM-OPC).

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.
           ACCEPT   ARQ-IMP   FROM    TIME
           ACCEPT   DTA-SYS   FROM    DATE
           OPEN     OUTPUT   CADTMP
                    INPUT    CADCRD   CADNTA.

       ROT-MENU-00.
           DISPLAY  TELA-01      PERFORM  ROT-PESQ-00
           MOVE     IND-PSQ  TO  NUM-OPC
           IF  NUM-OPC   <    1  GO       ROT-EXIT-00.
           DISPLAY  ENTR-01
           MOVE     CRD-TAB     (NUM-OPC) TO  TIP-CB2
           MOVE     DTA-SYS  TO  DTA-INI.

       ROT-MENU-10.
           MOVE     DTA-INI  TO  DTA-DTA
           MOVE 35  TO  COL-DTA  PERFORM  ROT-DATA-00
           IF  FLG-DTA   =    0  GO       ROT-MENU-00.
           MOVE     DTA-DTA  TO  DTA-INI
           MOVE 46  TO  COL-DTA  PERFORM  ROT-DATA-00
           IF  FLG-DTA   =    0  GO       ROT-MENU-10.
           MOVE     DTA-DTA  TO  DTA-FIN
           MOVE     ZEROS    TO  PAG-CB1  CHV-TMP
           IF  NUM-OPC   >    1  GO       ROT-CRED-00.

       ROT-NOTA-00.
           MOVE     SPACES   TO  CHV-NTA
           START    CADNTA       KEY  >   CHV-NTA
           IF  FST-NTA   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-MENU-00.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-03
           MOVE     SPACES   TO  CHV-NTA
           SORT     CADSRT   ON  ASCENDING    KEY  DTA-SRT
                                                   CLI-SRT
                                                   NUM-SRT
                    INPUT        PROCEDURE    IS   ROT-NOTA-05
                    OUTPUT       PROCEDURE    IS   ROT-NOTA-20
           GO       ROT-MENU-10.

       ROT-NOTA-05  SECTION.

       ROT-NOTA-10.
           START    CADNTA       KEY  >   CHV-NTA
           IF  FST-NTA   =  "23" GO       ROT-NOTA-15.
           READ     CADNTA  NEXT
           IF  DTA-NTA       <   DTA-INI
               OR    >   DTA-FIN GO       ROT-NOTA-10.
           MOVE     DTA-NTA  TO  DTA-SRT
           MOVE     CLI-NTA  TO  CLI-SRT
           MOVE     NUM-NTA  TO  NUM-SRT
           MOVE     DES-NTA  TO  DES-SRT
           MOVE     PCA-NTA  TO  PCA-SRT
           MOVE     VAL-NTA  TO  VAL-SRT
           RELEASE  REG-SRT      GO       ROT-NOTA-10.

       ROT-NOTA-15.
           EXIT.

       ROT-NOTA-20  SECTION.

       ROT-NOTA-25.
           RETURN   CADSRT       PERFORM  ROT-RTLA-00
           IF  FST-SRT   =  "10" PERFORM  ROT-MSGM-00
                                 GO       ROT-NOTA-50.
           DISPLAY  TELA-02
           MOVE     DTA-SRT  TO  DTA-AUX
           PERFORM  ROT-NOME-00  PERFORM  ROT-CAB1-00.

       ROT-NOTA-30.
           IF  LIN-TMP   >   59  PERFORM  ROT-CAB2-00
                                 PERFORM  ROT-CAB1-00.
       ROT-NOTA-35.
           MOVE     CLI-SRT  TO  CLI-DT1
           MOVE     NUM-SRT  TO  NUM-DT1
           MOVE     DES-SRT  TO  DES-DT1
           MOVE     PCA-SRT  TO  PCA-AUX
           MOVE     PC1-AUX  TO  PC1-DT1
           MOVE     PC2-AUX  TO  PC2-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      VAL-SRT  TO  ACM-AUX
           RETURN   CADSRT
           IF  FST-SRT   =  "10" GO       ROT-NOTA-40.
           IF  DTA-SRT  NOT   =  DTA-AUX
                                 PERFORM  ROT-NOTA-40
                                 GO       ROT-NOTA-30.
           ADD      1        TO  LIN-TMP  CHV-TMP
           MOVE     DET-001  TO  DET-TMP
           MOVE     SPACES   TO  DTA-DT1
           WRITE    REG-TMP      GO       ROT-NOTA-30.

       ROT-NOTA-40.
           ADD      1        TO  LIN-TMP  CHV-TMP
           MOVE     ACM-AUX  TO  TOT-DT1
           MOVE     DET-001  TO  DET-TMP  WRITE    REG-TMP
           ADD      ACM-AUX  TO  TOT-AUX
           MOVE     ZEROS    TO  ACM-AUX  TOT-DT1
           MOVE     DTA-SRT  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DET
           MOVE     MES-AUX  TO  MES-DET
           MOVE     ANO-AUX  TO  ANO-DET
           MOVE     DTA-DET  TO  DTA-DT1
           ADD      1        TO  LIN-TMP  CHV-TMP
           MOVE     SPACES   TO  DET-TMP  WRITE    REG-TMP.

       ROT-NOTA-45.
           PERFORM  ROT-TOTA-00  PERFORM  ROT-RTLA-00.

       ROT-NOTA-50.
           EXIT.

       ROT-NOTA-55  SECTION.

       ROT-CRED-00.
           MOVE     SPACES   TO  CHV-CRD
           MOVE     IND-PSQ  TO  PRX-CRD
           MOVE     DTA-INI  TO  DTA-CRD
           START    CADCRD       KEY  >   CHV-CRD
           IF  FST-CRD   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-MENU-10.
           READ     CADCRD  NEXT
           IF  PRX-CRD  NOT   =  IND-PSQ  OR  DTA-CRD
               >    DTA-FIN      PERFORM  ROT-MSGM-00
                                 GO       ROT-MENU-10.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-02
           MOVE     DTA-CRD  TO  DTA-AUX
           PERFORM  ROT-NOME-00  PERFORM  ROT-CAB1-00.

       ROT-CRED-10.
           IF  LIN-TMP   >   59  PERFORM  ROT-CAB2-00
                                 PERFORM  ROT-CAB1-00.
       ROT-CRED-20.
           MOVE     NUM-CRD  TO  NUM-DT2
           MOVE     NOM-CRD  TO  DES-DT2
           MOVE     VAL-CRD  TO  VAL-DT2
           ADD      VAL-CRD  TO  ACM-AUX
           START    CADCRD       KEY  >   CHV-CRD
           IF  FST-CRD   =  "23" GO       ROT-CRED-30.
           READ     CADCRD  NEXT
           IF  PRX-CRD  NOT   =  IND-PSQ  OR  DTA-CRD
               >    DTA-FIN      GO       ROT-CRED-30.
           IF  DTA-CRD  NOT   =  DTA-AUX
                                 PERFORM  ROT-CRED-30
                                 GO       ROT-CRED-10.
           ADD      1        TO  LIN-TMP  CHV-TMP
           MOVE     DET-002  TO  DET-TMP
           MOVE     SPACES   TO  DTA-DT2
           WRITE    REG-TMP      GO       ROT-CRED-10.

       ROT-CRED-30.
           ADD      1        TO  LIN-TMP  CHV-TMP
           MOVE     ACM-AUX  TO  TOT-DT2
           MOVE     DET-002  TO  DET-TMP  WRITE    REG-TMP
           ADD      ACM-AUX  TO  TOT-AUX
           MOVE     ZEROS    TO  ACM-AUX  TOT-DT2
           MOVE     DTA-CRD  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DET
           MOVE     MES-AUX  TO  MES-DET
           MOVE     ANO-AUX  TO  ANO-DET
           MOVE     DTA-DET  TO  DTA-DT2
           ADD      1        TO  LIN-TMP  CHV-TMP
           MOVE     SPACES   TO  DET-TMP  WRITE    REG-TMP.

       ROT-CRED-40.
           PERFORM  ROT-TOTA-00
           PERFORM  ROT-RTLA-00  GO       ROT-MENU-10.

       ROT-CAB1-00.
           DISPLAY  PAG-T02
           ADD      1        TO  PAG-CB1  CHV-TMP
           MOVE     PAG-CB1  TO  PAG-TMP
           MOVE     1        TO  LIN-TMP
           MOVE     CAB-001  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-002  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-003  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           IF  NUM-OPC   =    1
               MOVE CAB-004  TO  DET-TMP  WRITE    REG-TMP
           ELSE
               MOVE CAB-005  TO  DET-TMP  WRITE    REG-TMP.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-003  TO  DET-TMP  WRITE    REG-TMP
           MOVE     DIA-AUX  TO  DIA-DET
           MOVE     MES-AUX  TO  MES-DET
           MOVE     ANO-AUX  TO  ANO-DET
           MOVE     DTA-DET  TO  DTA-DT2  DTA-DT1.

       ROT-CAB2-00.
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     92       TO  LIN-TMP
           MOVE     CAB-006  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     93       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP  WRITE    REG-TMP.

       ROT-TOTA-00.
           MOVE     TOT-AUX  TO  VAL-TT1
           ADD      1        TO  LIN-TMP  CHV-TMP
           MOVE     TOT-001  TO  DET-TMP  WRITE    REG-TMP
           MOVE     PAG-CB1  TO  PAG-IMP
           DISPLAY  PAG-T02      CLOSE    CADTMP
           CALL     "DYIMPR"     USING    PRM-IMP
           OPEN     OUTPUT       CADTMP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-PESQ-00.
           CALL    "DYPESQ"  USING  PRM-PSQ
                                    PRM-HLP.
       ROT-DATA-00.
           CALL    "DYDATA"  USING  PRM-DTA
                                    PRM-HLP.
       ROT-NOME-00.
           MOVE     DIA-SYS   TO    DIA-CAB
           MOVE     MES-SYS   TO    MES-CAB
           MOVE     ANO-SYS   TO    ANO-CAB
           CALL    "DYNOME"  USING  PRM-NOM
                                    PRM-CAB
           MOVE     CLI-NOM   TO    CLI-CB1
           MOVE     PRM-CAB   TO    DTA-CB2
           MOVE     ZEROS     TO    ACM-AUX  TOT-AUX.

       ROT-GTLA-00.
           MOVE    "G"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE    "R"        TO    FLG-SCR
           CALL    "DYTELA"  USING  PRM-SCR.

       ROT-EXIT-00.
           CLOSE    CADCRD  CADNTA  CADTMP
           DELETE    FILE   CADTMP  GOBACK.
