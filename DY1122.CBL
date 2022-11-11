       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1122.
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

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST
                       FILE          STATUS  IS  FST-EST.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADATV.CPY.
       COPY  CADEST.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-ATV          PIC  X(02).
           03  FST-EST          PIC  X(02).

           03  TECLADO          PIC  9(02).
           03  SUF-INI          PIC  9(03).
           03  SUF-FIN          PIC  9(03).
           03  PCT-AUX          PIC S9(02)V99.
           03  FTR-AUX          PIC  9(03)V99.

           03  VAL-AUX          PIC  9(10)V9999.
           03  RED-VAL          REDEFINES   VAL-AUX.
               05  INT-AUX      PIC  9(10).
               05  DEC-AUX      PIC  9(04).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  03.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1122".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
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

       01  PRM-PSQ.
           03  OPC-PSQ          PIC  9(01)  VALUE  1.
           03  IND-PSQ          PIC  X(01)  VALUE  "1".
           03  LIN-PSQ          PIC  9(02)  VALUE  11.
           03  COL-PSQ          PIC  9(02)  VALUE  46.
           03  SB1-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB2-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB3-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB4-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB5-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB6-PSQ          PIC  X(02)  VALUE  "  ".
           03  SB7-PSQ          PIC  X(26)  VALUE  "           ∫".

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "€" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 06 COLUMN 20 VALUE " Administraá∆o Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 07 COLUMN 22 VALUE " Controle de Estoques ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 08 COLUMN 24 VALUE " Movimentos e Acertos ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 06 COLUMN 42 VALUE "ﬂ".
               05  LINE 07 COLUMN 44 VALUE "ﬂ".
               05  LINE 08 COLUMN 46 VALUE "ﬂ".
               05  LINE 07 COLUMN 21 VALUE "‹".
               05  LINE 08 COLUMN 23 VALUE "‹".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 09 COLUMN 20 VALUE
                   " …ÕÕ                        ÕÕÕÕÕÕÕÕÕª ".
               05  LINE 10 COLUMN 20 VALUE
                   " ∫                                     ∫ ".
               05  LINE 11 COLUMN 20 VALUE
                   " ∫                                     ∫ ".
               05  LINE 12 COLUMN 20 VALUE
                   " ∫                                     ∫ ".
               05  LINE 13 COLUMN 20 VALUE
                   " ∫                                     ∫ ".
               05  LINE 14 COLUMN 20 VALUE
                   " ∫                                     ∫ ".
               05  LINE 15 COLUMN 20 VALUE
                   " ∫                                     ∫ ".
               05  LINE 16 COLUMN 20 VALUE
                   " ∫                                     ∫ ".
               05  LINE 17 COLUMN 20 VALUE
                   " ∫                                     ∫ ".
               05  LINE 18 COLUMN 20 VALUE
                   " ∫                                     ∫ ".
               05  LINE 19 COLUMN 20 VALUE
                   " »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 09 COLUMN 26 VALUE "  Reajuste de Preáos  ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 10 COLUMN 61 VALUE "  ".
               05  LINE 11 COLUMN 61 VALUE "  ".
               05  LINE 12 COLUMN 61 VALUE "  ".
               05  LINE 13 COLUMN 61 VALUE "  ".
               05  LINE 14 COLUMN 61 VALUE "  ".
               05  LINE 15 COLUMN 61 VALUE "  ".
               05  LINE 16 COLUMN 61 VALUE "  ".
               05  LINE 17 COLUMN 61 VALUE "  ".
               05  LINE 18 COLUMN 61 VALUE "  ".
               05  LINE 19 COLUMN 61 VALUE "  ".
               05  LINE 20 COLUMN 22 PIC X(41) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 10 COLUMN 24 PIC X(32) FROM ALL "ﬂ".
               05  LINE 14 COLUMN 24 PIC X(01) FROM     "‹".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 11 COLUMN 24 PIC X(32) FROM
                   "  C¢digo Inicial ...:  -".
               05  LINE 12 COLUMN 24 PIC X(32) FROM SPACES.
               05  LINE 13 COLUMN 24 PIC X(32) FROM
                   "  C¢digo Final .....:  -".
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 11 COLUMN 56 PIC X(01) FROM " ".
               05  LINE 12 COLUMN 56 PIC X(01) FROM " ".
               05  LINE 13 COLUMN 56 PIC X(01) FROM " ".
               05  LINE 14 COLUMN 56 PIC X(01) FROM " ".
               05  LINE 14 COLUMN 25 PIC X(31) FROM ALL "ﬂ".

       01  TELA-03.
           03  BCK-CLR 06 FOR-CLR 07.
               05  LINE 15 COLUMN 24 PIC X(32) FROM ALL "ﬂ".
               05  LINE 18 COLUMN 24 PIC X(01) FROM     "‹".
           03  BCK-CLR 06 FOR-CLR 14.
               05  LINE 16 COLUMN 24 PIC X(32) FROM
                   "  Percentual (+/-) .:        %".
               05  LINE 17 COLUMN 24 PIC X(32) FROM
                   "  Arredondamento ...:".
           03  BCK-CLR 00 FOR-CLR 06.
               05  LINE 16 COLUMN 56 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 56 PIC X(01) FROM     " ".
               05  LINE 18 COLUMN 56 PIC X(01) FROM     " ".
               05  LINE 18 COLUMN 25 PIC X(31) FROM ALL "ﬂ".

       01  ENTR-01     AUTO.
           03  SFI-E01 LINE 11 COLUMN 48 PIC ZZ9    USING SUF-INI.
           03  SFF-E01 LINE 13 COLUMN 48 PIC ZZ9    USING SUF-FIN.
           03  PCT-E01 LINE 16 COLUMN 46 PIC --9,99 USING PCT-AUX.
           03  FTR-E01 LINE 17 COLUMN 46 PIC Z9,99  USING FTR-AUX.

       01  ENTR-02 BCK-CLR 02 FOR-CLR 15.
           03  AT1-T02 LINE 11 COLUMN 46 PIC X(01)  FROM CHV-ATV.
           03  AT2-T02 LINE 13 COLUMN 46 PIC X(01)  FROM CHV-ATV.
           03  SFI-T02 LINE 11 COLUMN 48 PIC 9(03)  FROM SUF-INI.
           03  SFF-T02 LINE 13 COLUMN 48 PIC 9(03)  FROM SUF-FIN.

       01  ENTR-03 BCK-CLR 06 FOR-CLR 15.
           03  PCT-T03 LINE 16 COLUMN 46 PIC --9,99 FROM PCT-AUX.
           03  FTR-T03 LINE 17 COLUMN 46 PIC Z9,99  FROM FTR-AUX.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           OPEN    INPUT     CADATV   I-O      CADEST.

       ROT-ENTR-00.
           DISPLAY  TELA-02      PERFORM  ROT-PESQ-00
           IF  IND-PSQ   <    1  GO       ROT-EXIT-00.
           MOVE     IND-PSQ  TO  CHV-ATV
           READ     CADATV
           DISPLAY  AT1-T02      AT2-T02
           MOVE     ZEROS    TO  SUF-INI  SUF-FIN
                                 FTR-AUX  PCT-AUX.
       ROT-ENTR-10.
           ACCEPT   SFI-E01      DISPLAY  SFI-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-10.
           IF  SUF-INI   =   0   GO       ROT-ENTR-10.

       ROT-ENTR-20.
           ACCEPT   SFF-E01      DISPLAY  SFF-T02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-20.
           IF  SUF-FIN   =   0   GO       ROT-ENTR-20.
           IF  SUF-FIN   <       SUF-INI
                                 MOVE 41  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-20.
           MOVE     32       TO  IND-MSG
           MOVE     CHV-ATV  TO  PRX-EST
           COMPUTE  SUF-EST  =   SUF-INI  -  1
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-20.
           READ     CADEST  NEXT
           IF  PRX-EST  NOT  =   CHV-ATV  OR  SUF-EST
                   >    SUF-FIN  PERFORM  ROT-MSGM-00
                                 GO       ROT-ENTR-20.
       ROT-ENTR-30.
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-03.

       ROT-ENTR-40.
           ACCEPT   PCT-E01      DISPLAY  PCT-T03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-ENTR-20.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-40.
           IF  PCT-AUX   =  0,00 GO       ROT-ENTR-40.

       ROT-ENTR-50.
           ACCEPT   FTR-E01      DISPLAY  FTR-T03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-ENTR-40.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-ENTR-50.
       ROT-ATUA-00.
           MOVE 10  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-ATUA-00.
           IF  TCL-MSG   >   00  GO       ROT-ATUA-00.
           IF  HEX-MSG   =   27  GO       ROT-ENTR-50.

       ROT-ATUA-10.
           COMPUTE  VDA-EST  =   VDA-EST  *   PCT-AUX
                                 /   100  +   VDA-EST
           COMPUTE  VAL-AUX  =   VDA-EST  /   FTR-AUX
           IF  FTR-AUX   =   0   OR
               DEC-AUX   =   0   GO       ROT-ATUA-20.
           COMPUTE  VDA-EST  =   INT-AUX  *   FTR-AUX  +  FTR-AUX.

       ROT-ATUA-20.
           COMPUTE  PCT-EST  =  (VDA-EST  -   CTM-EST)
                             *   100      /   VDA-EST.
       ROT-ATUA-30.
           REWRITE  REG-EST      MOVE 20  TO  IND-MSG
           IF  FST-EST   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-ATUA-30.
       ROT-ATUA-40.
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" PERFORM  ROT-RTLA-00
                                 GO       ROT-ENTR-00.
           READ     CADEST  NEXT
           IF  FST-EST   =  "10" OR
               PRX-EST  NOT  =   CHV-ATV  OR  SUF-EST
               >    SUF-FIN      PERFORM  ROT-RTLA-00
                                 GO       ROT-ENTR-00.
           GO       ROT-ATUA-10.

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

       ROT-PESQ-00.
           CALL    "DYPESQ"  USING  PRM-PSQ
                                    PRM-HLP.
       ROT-EXIT-00.
           CLOSE    CADATV  CADEST  GOBACK.
