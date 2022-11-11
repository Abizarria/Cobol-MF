       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY4100.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADOPR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-OPR
                       FILE          STATUS  IS  FST-OPR.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADOPR.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  TECLADO          PIC  9(02).
           03  FST-OPR          PIC  X(02).
           03  COD-001          PIC  X(10).
           03  COD-002          PIC  X(10).
           03  COD-003          PIC  X(10).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "4100".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  60.

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

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "€" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 08 COLUMN 21 VALUE " Manutená∆o de Senhas ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 08 COLUMN 43 VALUE "ﬂ".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 09 COLUMN 21 VALUE
                   " …                        ÕÕÕÕÕÕÕÕÕª ".
               05  LINE 10 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 11 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 12 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 13 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 14 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 15 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 16 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 17 COLUMN 21 VALUE
                   " ∫                                   ∫ ".
               05  LINE 18 COLUMN 21 VALUE
                   " »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 09 COLUMN 25 VALUE " Alteraá∆o das Senhas ".
           03 BCK-CLR 00.
               05  LINE 10 COLUMN 60 VALUE "  ".
               05  LINE 11 COLUMN 60 VALUE "  ".
               05  LINE 12 COLUMN 60 VALUE "  ".
               05  LINE 13 COLUMN 60 VALUE "  ".
               05  LINE 14 COLUMN 60 VALUE "  ".
               05  LINE 15 COLUMN 60 VALUE "  ".
               05  LINE 16 COLUMN 60 VALUE "  ".
               05  LINE 17 COLUMN 60 VALUE "  ".
               05  LINE 18 COLUMN 60 VALUE "  ".
               05  LINE 19 COLUMN 23 PIC X(39) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 10 COLUMN 25 PIC X(30) FROM ALL "ﬂ".
               05  LINE 13 COLUMN 25 PIC X(01) FROM     "‹".
           03  BCK-CLR 02 FOR-CLR 14.
               05  LINE 11 COLUMN 25 PIC X(30) FROM "  Nome Operador :".
               05  LINE 12 COLUMN 25 PIC X(30) FROM "  Senha Atual ..:".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 11 COLUMN 43 PIC X(10) FROM PRM-OPR.
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 11 COLUMN 55 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 55 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 55 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "ﬂ".

       01  ENTR-01  BCK-CLR 02 FOR-CLR 15 NO-ECHO
                    LINE 12 COLUMN 43 PIC X(10) USING COD-001.

       01  ENTR-02  BCK-CLR 02 FOR-CLR 15
                    LINE 12 COLUMN 43 PIC X(10) FROM  ALL "*".

       01  TELA-03.
           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 14 COLUMN 25 PIC X(30) FROM ALL "ﬂ".
               05  LINE 17 COLUMN 25 PIC X(01) FROM     "‹".
           03  BCK-CLR 05 FOR-CLR 11.
               05  LINE 15 COLUMN 25 PIC X(30) FROM "  Nova Senha ...:".
               05  LINE 16 COLUMN 25 PIC X(30) FROM "  Confirmaá∆o ..:".
           03  BCK-CLR 00 FOR-CLR 05.
               05  LINE 15 COLUMN 55 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 55 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 55 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 26 PIC X(29) FROM ALL "ﬂ".

       01  ENTR-03  BCK-CLR 05 FOR-CLR 15 NO-ECHO.
           03  SN1-E03 LINE 15 COLUMN 43 PIC X(10) USING COD-002.
           03  SN2-E03 LINE 16 COLUMN 43 PIC X(10) USING COD-003.

       01  ENTR-04  BCK-CLR 05 FOR-CLR 15.
           03  SN1-T03 LINE 15 COLUMN 43 PIC X(10) FROM  ALL "*".
           03  SN2-T03 LINE 16 COLUMN 43 PIC X(10) FROM  ALL "*".

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-000-000.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           OPEN     I-O      CADOPR.

       ROT-OPER-00.
           MOVE     PRM-OPR  TO  CHV-OPR
           READ     CADOPR
           IF  FST-OPR   =  "9D" MOVE 20  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-OPER-00.
           DISPLAY  TELA-02      PERFORM  ROT-GTLA-00.

       ROT-CODE-00.
           MOVE     SPACES   TO  COD-001
           ACCEPT   ENTR-01      DISPLAY  ENTR-02
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-EXIT-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-00.
           IF  COD-001  NOT   =  COD-OPR
                                 MOVE 25  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-00.
       ROT-CODE-10.
           DISPLAY  TELA-03
           MOVE     SPACES   TO  COD-002
           ACCEPT   SN1-E03      DISPLAY  SN1-T03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  PERFORM  ROT-RTLA-00
                                 GO       ROT-CODE-00.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-10.
       ROT-CODE-20.
           MOVE     SPACES   TO  COD-003
           ACCEPT   SN2-E03      DISPLAY  SN2-T03
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GO       ROT-CODE-10.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-CODE-20.
           IF  COD-002  NOT  =   COD-003
                                 MOVE 26  TO  IND-MSG
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-CODE-20.
           MOVE     COD-002  TO  COD-OPR
           REWRITE  REG-OPR      GO       ROT-EXIT-00.

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
           CLOSE  CADOPR  GOBACK.
