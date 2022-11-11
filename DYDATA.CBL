       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYDATA.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.

       DATA            DIVISION.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  TECLADO          PIC  9(02).
           03  IND-MES          PIC  9(02).
           03  TOT-ANO          PIC  9(04).

           03  DTA-AUX.
               05  DIA-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-AUX      PIC  9(02).

           03  TOT-DIA          PIC  9(06)V9999.
           03  RED-TOT          REDEFINES  TOT-DIA.
               05  INT-DIA      PIC  9(06).
               05  DEC-DIA      PIC  9(04).

           03  TOT-BIS          PIC  9(03)V99.
           03  RED-BIS          REDEFINES  TOT-BIS.
               05  INT-BIS      PIC  9(03).
               05  DEC-BIS      PIC  9(02).

           03  TAB-DIA.
               05  FILLER       PIC  9(02)  VALUE  31.
               05  FILLER       PIC  9(02)  VALUE  00.
               05  FILLER       PIC  9(02)  VALUE  31.
               05  FILLER       PIC  9(02)  VALUE  30.
               05  FILLER       PIC  9(02)  VALUE  31.
               05  FILLER       PIC  9(02)  VALUE  30.
               05  FILLER       PIC  9(02)  VALUE  31.
               05  FILLER       PIC  9(02)  VALUE  31.
               05  FILLER       PIC  9(02)  VALUE  30.
               05  FILLER       PIC  9(02)  VALUE  31.
               05  FILLER       PIC  9(02)  VALUE  30.
               05  FILLER       PIC  9(02)  VALUE  31.
           03  RED-DIA          REDEFINES   TAB-DIA.
               05  DIA-TAB      PIC  9(02)  OCCURS 12.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  15.
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       LINKAGE         SECTION.

       01  PRM-DTA.
           03  DTA-DTA.
               05  ANO-DTA      PIC  9(02).
               05  MES-DTA      PIC  9(02).
               05  DIA-DTA      PIC  9(02).
           03  LIN-DTA          PIC  9(02).
           03  COL-DTA          PIC  9(02).
           03  BCK-DTA          PIC  9(01).
           03  FOR-DTA          PIC  9(01).
           03  FLG-DTA          PIC  9(01).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04).
           03  BCK-HLP          PIC  9(01).
           03  FOR-HLP          PIC  9(01).
           03  LET-HLP          PIC  9(01).

       SCREEN          SECTION.

       01  ENTR-01  LINE  LIN-DTA  COLUMN   COL-DTA  PIC  99/99/99
                    USING DTA-AUX  AUTO.

       01  TELA-01  LINE  LIN-DTA  COLUMN   COL-DTA  PIC  99/99/99
                    FROM  DTA-AUX  BCK-CLR  BCK-DTA
                                   FOR-CLR  FOR-DTA  HIGHLIGHT.

       PROCEDURE       DIVISION  USING  PRM-DTA  PRM-HLP.

       ROT-0000-00.
           MOVE     DIA-DTA  TO  DIA-AUX
           MOVE     MES-DTA  TO  MES-AUX
           MOVE     ANO-DTA  TO  ANO-AUX
           MOVE     0        TO  FLG-DTA.

       ROT-DATA-00.
           ACCEPT   ENTR-01      DISPLAY  TELA-01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GOBACK.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-DATA-00.
       ROT-DATA-10.
           COMPUTE  TOT-BIS  =   ANO-AUX  /   4
           IF  DEC-BIS   >   0
               MOVE     28   TO  DIA-TAB (02)
           ELSE
               MOVE     29   TO  DIA-TAB (02).
           IF (MES-AUX   <   01  OR    >  12)  OR
              (DIA-AUX   <   01  OR    >  DIA-TAB (MES-AUX))
                                 PERFORM  ROT-MSGM-00
                                 GO       ROT-0000-00.
           COMPUTE  TOT-ANO  =   ANO-AUX  +   1999
           COMPUTE  TOT-DIA  =   DIA-AUX
                             +  (TOT-ANO  /   4)
                             +  (TOT-ANO  *   365)
           MOVE     ZEROS    TO  IND-MES  DEC-DIA.

       ROT-DATA-20.
           ADD      1        TO  IND-MES
           IF  IND-MES   <       MES-AUX
               ADD      DIA-TAB (IND-MES) TO  TOT-DIA
                                 GO       ROT-DATA-20.
           COMPUTE  TOT-DIA  =   TOT-DIA  /   7
           MOVE     ZEROS    TO  INT-DIA
           COMPUTE  FLG-DTA      ROUNDED
                             =   TOT-DIA  *   7
           IF  FLG-DTA   =   0
               MOVE      7   TO  FLG-DTA.
           MOVE     DIA-AUX  TO  DIA-DTA
           MOVE     MES-AUX  TO  MES-DTA
           MOVE     ANO-AUX  TO  ANO-DTA
           GOBACK.

       ROT-HELP-00.
           CALL   "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL   "DYMSGM"  USING  PRM-MSG.
