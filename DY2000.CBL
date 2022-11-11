       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY2000.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

       DATA            DIVISION.
       FILE            SECTION.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.

           03  TECLADO          PIC  9(02).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  54.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "2000".
           03  BCK-HLP          PIC  9(01)  VALUE  7.
           03  FOR-HLP          PIC  9(01)  VALUE  3.
           03  LET-HLP          PIC  9(01)  VALUE  1.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  45.
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.

       ROT-ENTR-00.
           PERFORM  ROT-MSGM-00       GO       ROT-EXIT-00.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-EXIT-00.
           GOBACK.
