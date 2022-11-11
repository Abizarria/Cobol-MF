       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYTELA.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT  IS  COMMA.

       DATA            DIVISION.

       WORKING-STORAGE SECTION.

       LINKAGE         SECTION.

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

       PROCEDURE       DIVISION  USING  PRM-SCR.

       ROT-0000-00.
           MOVE    03      TO    LIN-SCR
           MOVE    00      TO    COL-SCR
           MOVE    1600    TO    P05-SCR
           EVALUATE   FLG-SCR
               WHEN   "G"        CALL  "CBL_READ_SCR_CHATTRS"
                                 USING  POS-SCR  P03-SCR
                                        P04-SCR  P05-SCR  P02-SCR
               WHEN   "R"        CALL  "CBL_WRITE_SCR_CHATTRS"
                                 USING  POS-SCR  P03-SCR
                                        P04-SCR  P05-SCR  P02-SCR
           END-EVALUATE.         GOBACK.
