       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1230.
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

           SELECT      CADCXA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CXA
                       FILE          STATUS  IS  FST-CXA.

           SELECT      CADECR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ECR
                       FILE          STATUS  IS  FST-ECR.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST
                       FILE          STATUS  IS  FST-EST.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC
                       FILE          STATUS  IS  FST-LBC.

           SELECT      CADLCP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCP
                       FILE          STATUS  IS  FST-LCP.

           SELECT      CADLCX        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCX
                       FILE          STATUS  IS  FST-LCX.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP
                       FILE          STATUS  IS  FST-LDP.

           SELECT      CADLES        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LES
                       FILE          STATUS  IS  FST-LES.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS
                       FILE          STATUS  IS  FST-PRS.

           SELECT      CADSDO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SDO
                       FILE          STATUS  IS  FST-SDO.

           SELECT      CADSPL        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SPL
                       FILE          STATUS  IS  FST-SPL.

           SELECT      CADSRT        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-SRT.

           SELECT      CADTNQ        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TNQ
                       FILE          STATUS  IS  FST-TNQ.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADBCO.CPY.
       COPY  CADCTR.CPY.
       COPY  CADCXA.CPY.
       COPY  CADECR.CPY.
       COPY  CADEST.CPY.
       COPY  CADLBC.CPY.
       COPY  CADLCP.CPY.
       COPY  CADLCX.CPY.
       COPY  CADLDP.CPY.
       COPY  CADLES.CPY.
       COPY  CADPRS.CPY.
       COPY  CADSDO.CPY.
       COPY  CADSPL.CPY.
       COPY  CADTNQ.CPY.

       SD  CADSRT.

       01  REG-SRT              PIC  X(54).
       01  RED-BCO              REDEFINES   REG-SRT.
           03  BCO-SRT.
               05  CH1-SRT      PIC  X(12).
               05  FILLER       PIC  X(42).
       01  RED-DSP              REDEFINES   REG-SRT.
           03  DSP-SRT.
               05  CH2-SRT      PIC  X(13).
               05  FILLER       PIC  X(35).
           03  FILLER           PIC  X(06).

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-BCO          PIC  X(02).
           03  FST-CTR          PIC  X(02).
           03  FST-CXA          PIC  X(02).
           03  FST-ECR          PIC  X(02).
           03  FST-EST          PIC  X(02).
           03  FST-LBC          PIC  X(02).
           03  FST-LCP          PIC  X(02).
           03  FST-LCX          PIC  X(02).
           03  FST-LDP          PIC  X(02).
           03  FST-LES          PIC  X(02).
           03  FST-PRS          PIC  X(02).
           03  FST-SDO          PIC  X(02).
           03  FST-SPL          PIC  X(02).
           03  FST-SRT          PIC  X(02).
           03  FST-TNQ          PIC  X(02).

           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  COL-AUX          PIC  9(02).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  15.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1230".
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
           03  LIN-DTA          PIC  9(02)  VALUE  12.
           03  COL-DTA          PIC  9(02)  VALUE  49.
           03  BCK-DTA          PIC  9(01)  VALUE  2.
           03  FOR-DTA          PIC  9(01)  VALUE  7.
           03  FLG-DTA          PIC  9(01).

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01  FOR-CLR 8
                    LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û".
       01  TELA-02.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 08 COLUMN 16 VALUE " Administra‡Æo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 09 COLUMN 18 VALUE " Controle de Receitas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 08 COLUMN 38 VALUE "ß".
               05  LINE 09 COLUMN 40 VALUE "ß".
               05  LINE 09 COLUMN 17 VALUE "Ü".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 10 COLUMN 16 VALUE
                   " É                        ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ» ".
               05  LINE 11 COLUMN 16 VALUE
                   " º                                            º ".
               05  LINE 12 COLUMN 16 VALUE
                   " º                                            º ".
               05  LINE 13 COLUMN 16 VALUE
                   " º                                            º ".
               05  LINE 14 COLUMN 16 VALUE
                   " º                                            º ".
               05  LINE 15 COLUMN 16 VALUE
                   " º                                            º ".
               05  LINE 16 COLUMN 16 VALUE
                   " º                                            º ".
               05  LINE 17 COLUMN 16 VALUE
                   " º                                            º ".
               05  LINE 18 COLUMN 16 VALUE
                   " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 10 COLUMN 20 VALUE " Zeramento do Per¡odo ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 11 COLUMN 64 VALUE  "  ".
               05  LINE 12 COLUMN 64 VALUE  "  ".
               05  LINE 13 COLUMN 64 VALUE  "  ".
               05  LINE 14 COLUMN 64 VALUE  "  ".
               05  LINE 15 COLUMN 64 VALUE  "  ".
               05  LINE 16 COLUMN 64 VALUE  "  ".
               05  LINE 17 COLUMN 64 VALUE  "  ".
               05  LINE 18 COLUMN 64 VALUE  "  ".
               05  LINE 19 COLUMN 18 PIC X(48) FROM SPACES.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 11 COLUMN 20 PIC X(39) FROM ALL "ß".
               05  LINE 13 COLUMN 20 PIC X(01) FROM     "Ü".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 12 COLUMN 20 PIC X(39) FROM
                   "  Data do Per¡odo … Iniciar:".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 12 COLUMN 59 VALUE " ".
               05  LINE 13 COLUMN 59 VALUE " ".
               05  LINE 13 COLUMN 21 PIC X(38) FROM ALL "ß".

       01  TELA-03  BCK-CLR 04.
           03  LINE 14 COLUMN 20 PIC X(39) FROM ALL "ß" FOR-CLR 07.
           03  LINE 16 COLUMN 20 PIC X(39) FROM ALL " " FOR-CLR 07.
           03  LINE 17 COLUMN 20 PIC X(01) FROM     "Ü" FOR-CLR 07.
           03  LINE 15 COLUMN 20 PIC X(39) FROM
               "  DESEJA CONFIRMAR O ZERAMENTO ?  [ ]"  FOR-CLR 15.
           03  LINE 16 COLUMN 20 PIC X(39) FROM
               "  "  FOR-CLR 15.
           03  LINE 15 COLUMN 59 PIC X(01) FROM     "Û" FOR-CLR 00.
           03  LINE 16 COLUMN 59 PIC X(01) FROM     "Û" FOR-CLR 00.
           03  LINE 17 COLUMN 59 PIC X(01) FROM     "Û" FOR-CLR 00.
           03  LINE 17 COLUMN 21 PIC X(38) FROM ALL "Ü" FOR-CLR 00.

       01  TELA-04  BCK-CLR 04.
           03  LINE 14 COLUMN 20 PIC X(39) FROM ALL "ß" FOR-CLR 07.
           03  LINE 16 COLUMN 20 PIC X(39) FROM ALL " " FOR-CLR 07.
           03  LINE 17 COLUMN 20 PIC X(01) FROM     "Ü" FOR-CLR 07.
           03  LINE 15 COLUMN 20 PIC X(39) FROM
               "  0%.............50%.............100%"  FOR-CLR 14.
           03  LINE 16 COLUMN 22 PIC X(35) FROM SPACES  BCK-CLR 00.
           03  LINE 15 COLUMN 59 PIC X(01) FROM     "Û" FOR-CLR 00.
           03  LINE 16 COLUMN 59 PIC X(01) FROM     "Û" FOR-CLR 00.
           03  LINE 17 COLUMN 59 PIC X(01) FROM     "Û" FOR-CLR 00.
           03  LINE 17 COLUMN 21 PIC X(38) FROM ALL "Ü" FOR-CLR 00.

       01  ENTR-01 LINE 15 COLUMN 55 PIC X(01) USING RSP-OPC AUTO
                   BCK-CLR 04 FOR-CLR 15.

       01  ENTR-02 LINE 16 COLUMN COL-AUX PIC X(07) FROM ALL "Û"
                   FOR-CLR 15.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-TEST-00.
           CALL    "DYSPOL"      USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL   =    1  GOBACK.
           DISPLAY  TELA-01      OPEN     I-O      CADCTR.

       ROT-TEST-10.
           READ     CADCTR       MOVE 20  TO  IND-MSG
           IF  FST-CTR   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-TEST-10.
           MOVE     DTI-CTR  TO  DTA-DTA
           IF  MES-DTA   =   12  ADD   1  TO  ANO-DTA
                                 MOVE  1  TO  MES-DTA
           ELSE
                                 ADD   1  TO  MES-DTA.
       ROT-ENTR-00.
           DISPLAY  TELA-02      PERFORM  ROT-DATA-00
           IF  FLG-DTA   =    0  GO       ROT-EXIT-00.

       ROT-ENTR-10.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  TELA-03      ACCEPT   ENTR-01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
           IF  RSP-OPC  NOT   =  "S"
                                 GO       ROT-ENTR-10.
       ROT-ZERA-00.
           DISPLAY  TELA-04
           MOVE     DTA-DTA  TO  DTI-CTR
           MOVE     ZEROS    TO  DIA-DTA  DIA-CTR
           MOVE     DTA-DTA  TO  DTD-CTR
           IF  LMC-CTR   >       160      MOVE  1  TO  LMC-CTR.
           REWRITE  REG-CTR      CLOSE    CADCTR
           OPEN     OUTPUT       CADLCP   CADLCX
                                 CADLES   CADSPL
           CLOSE                 CADLCP   CADLCX
                                 CADLES   CADSPL
           OPEN     I-O          CADEST
                                 CADECR   CADTNQ
           MOVE     ZEROS    TO  CHV-EST
                                 CHV-ECR  CHV-TNQ
           MOVE 22  TO  COL-AUX  DISPLAY  ENTR-02
           PERFORM  ROT-ZERA-05  UNTIL    FST-EST    =    "23"
           PERFORM  ROT-ZERA-15  UNTIL    FST-ECR    =    "23"
           PERFORM  ROT-ZERA-25  UNTIL    FST-TNQ    =    "23"
           MOVE 29  TO  COL-AUX  DISPLAY  ENTR-02
           CLOSE    CADEST       CADECR   CADTNQ
           OPEN     I-O          CADCXA
                                 CADPRS   CADSDO
           MOVE     ZEROS    TO  CHV-CXA
                                 CHV-PRS  CHV-SDO
           PERFORM  ROT-ZERA-35  UNTIL    FST-CXA    =    "23"
           PERFORM  ROT-ZERA-45  UNTIL    FST-PRS    =    "23"
           PERFORM  ROT-ZERA-55  UNTIL    FST-SDO    =    "23"
           CLOSE    CADCXA       CADPRS   CADSDO
           MOVE 36  TO  COL-AUX  DISPLAY  ENTR-02
           OPEN     I-O          CADBCO
                    INPUT        CADLBC
           MOVE     ZEROS    TO  CHV-BCO
           SORT     CADSRT       ON       ASCENDING  KEY  CH1-SRT
                                 INPUT    PROCEDURE  IS   ROT-ZBCO-00
                                 OUTPUT   PROCEDURE  IS   ROT-ZBCO-60
           MOVE 43  TO  COL-AUX  DISPLAY  ENTR-02
           OPEN     INPUT        CADLDP
           MOVE     SPACES   TO  CHV-LDP
           SORT     CADSRT       ON       ASCENDING  KEY  CH2-SRT
                                 INPUT    PROCEDURE  IS   ROT-ZDSP-00
                                 OUTPUT   PROCEDURE  IS   ROT-ZDSP-30.
       ROT-ZERA-05.
           PERFORM  ROT-ZERA-10  REWRITE  REG-EST
           START    CADEST       KEY  >   CHV-EST.

       ROT-ZERA-10.
           READ     CADEST  NEXT MOVE 20  TO  IND-MSG
           IF  FST-EST  =   "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-ZERA-10.
           IF  PRX-EST   =    1  COMPUTE  MIN-EST   =
                                          QVM-EST   /  15
           ELSE
                                 COMPUTE  MIN-EST   =
                                          QVM-EST   /   2.
           MOVE     ZEROS    TO  QVM-EST
           IF  MES-DTA   =   01  MOVE  0  TO  QVA-EST.

       ROT-ZERA-15.
           PERFORM  ROT-ZERA-20  REWRITE  REG-ECR
           START    CADECR       KEY  >   CHV-ECR.

       ROT-ZERA-20.
           READ     CADECR  NEXT MOVE 20  TO  IND-MSG
           IF  FST-ECR  =   "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-ZERA-20.
           MOVE     IDL-ECR  TO  ICL-ECR  FCL-ECR
           MOVE     ZEROS    TO  AFC-ECR  AFD-ECR
                                 VDC-ECR  VDD-ECR.
       ROT-ZERA-25.
           PERFORM  ROT-ZERA-30  REWRITE  REG-TNQ
           START    CADTNQ       KEY  >   CHV-TNQ.

       ROT-ZERA-30.
           READ     CADTNQ  NEXT MOVE 20  TO  IND-MSG
           IF  FST-TNQ  =   "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-ZERA-30.
           MOVE     ZEROS    TO  SDF-TNQ.

       ROT-ZERA-35.
           PERFORM  ROT-ZERA-40  REWRITE  REG-CXA
           START    CADCXA       KEY  >   CHV-CXA.

       ROT-ZERA-40.
           READ     CADCXA  NEXT MOVE 20  TO  IND-MSG
           IF  FST-CXA  =   "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-ZERA-40.
           MOVE     1        TO  FLG-CXA
           MOVE     ZEROS    TO  DTA-CXA.

       ROT-ZERA-45.
           PERFORM  ROT-ZERA-50  REWRITE  REG-PRS
           START    CADPRS       KEY  >   CHV-PRS.

       ROT-ZERA-50.
           READ     CADPRS  NEXT MOVE 20  TO  IND-MSG
           IF  FST-PRS  =   "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-ZERA-50.
           MOVE     ZEROS    TO  QTD-PRS  CTD-PRS
                                 VDD-PRS  QTM-PRS
                                 CTM-PRS  VDM-PRS.
       ROT-ZERA-55.
           PERFORM  ROT-ZERA-60  REWRITE  REG-SDO
           START    CADSDO       KEY  >   CHV-SDO.

       ROT-ZERA-60.
           READ     CADSDO  NEXT MOVE 20  TO  IND-MSG
           IF  FST-SDO  =   "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-ZERA-60.
           COMPUTE  SDI-SDO   =  SDI-SDO  +   ENT-SDO
                              -  SAI-SDO  +   COR-SDO
           MOVE     ZEROS    TO  ENT-SDO
                                 SAI-SDO      COR-SDO.
       ROT-ZBCO-00  SECTION.

       ROT-ZBCO-10.
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-ZBCO-50.

       ROT-ZBCO-20.
           READ     CADBCO  NEXT MOVE 20  TO  IND-MSG
           IF  FST-BCO   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-ZBCO-20.
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC.

       ROT-ZBCO-30.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-ZBCO-40.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO
                                 GO       ROT-ZBCO-40.
           IF  DTA-LBC  NOT   <  DTI-CTR
                                 MOVE     REG-LBC  TO  BCO-SRT
                                 RELEASE  REG-SRT
           ELSE
               IF  COD-LBC  <  4 COMPUTE  SDO-BCO   =
                                          SDO-BCO   +  VAL-LBC
               ELSE
                                 COMPUTE  SDO-BCO   =
                                          SDO-BCO   -  VAL-LBC.
           GO       ROT-ZBCO-30.

       ROT-ZBCO-40.
           REWRITE  REG-BCO      GO       ROT-ZBCO-10.

       ROT-ZBCO-50.
           CLOSE    CADLBC       OPEN  OUTPUT  CADLBC.

       ROT-ZBCO-60  SECTION.

       ROT-ZBCO-70.
           RETURN   CADSRT
           IF  FST-SRT  =   "10" GO       ROT-ZBCO-80.
           MOVE     BCO-SRT  TO  REG-LBC
           WRITE    REG-LBC      GO       ROT-ZBCO-70.

       ROT-ZBCO-80.
           CLOSE    CADBCO       CADLBC.

       ROT-ZDSP-00  SECTION.

       ROT-ZDSP-10.
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       ROT-ZDSP-20.
           READ     CADLDP  NEXT
           IF  FLG-LDP  NOT  =   "P" AND  DTA-LDP  <   DTI-CTR
                                 GO       ROT-ZDSP-10.
           MOVE     REG-LDP  TO  DSP-SRT
           RELEASE  REG-SRT      GO       ROT-ZDSP-10.

       ROT-ZDSP-20.
           CLOSE    CADLDP       OPEN  OUTPUT  CADLDP.

       ROT-ZDSP-30  SECTION.

       ROT-ZDSP-40.
           RETURN   CADSRT
           IF  FST-SRT  =   "10" GO       ROT-ZDSP-50.
           MOVE     DSP-SRT  TO  REG-LDP
           WRITE    REG-LDP      GO       ROT-ZDSP-40.

       ROT-ZDSP-50.
           MOVE 50  TO  COL-AUX  DISPLAY  ENTR-02
           CLOSE    CADLDP       GOBACK.

       ROT-HELP-00.
           CALL     "DYHELP"     USING    PRM-HLP.

       ROT-MSGM-00.
           CALL     "DYMSGM"     USING    PRM-MSG.

       ROT-DATA-00.
           CALL     "DYDATA"     USING    PRM-DTA  PRM-HLP.

       ROT-EXIT-00.
           CLOSE     CADCTR      GOBACK.
