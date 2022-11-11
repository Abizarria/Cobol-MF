       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYSPOL.
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

           SELECT      CADSPL        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SPL
                       FILE          STATUS  IS  FST-SPL.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADOPR.CPY.
       COPY  CADSPL.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-OPR          PIC  X(02).
           03  FST-SPL          PIC  X(02).

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  27.
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01).
           03  PRG-SPL          PIC  9(02).

       SCREEN          SECTION.

       PROCEDURE       DIVISION  USING  PRM-OPR  PRM-SPL.

       ROT-0000-00.
           OPEN     I-O           CADSPL
           OPEN     INPUT         CADOPR
           MOVE     PRM-OPR  TO   CHV-OPR
           READ     CADOPR
           IF  FST-OPR   =  "23"      OR
               PRG-OPR (PRG-SPL)  =   "N"   GO   ROT-MSGM-00.

       ROT-SPOL-00.
           ACCEPT   DTA-SPL  FROM  DATE
           MOVE     PRM-OPR   TO   OPR-SPL
           MOVE     PRG-SPL   TO   IND-SPL.

       ROT-SPOL-10.
           ACCEPT   HRA-SPL  FROM  TIME
           WRITE    REG-SPL
           IF  FST-SPL   >   "00"  GO   ROT-SPOL-10.

       ROT-EXIT-00.
           CLOSE    CADOPR  CADSPL  GOBACK.

       ROT-MSGM-00.
           MOVE     1         TO    LCK-SPL
           CALL    "DYMSGM"  USING  PRM-MSG  GO  ROT-EXIT-00.
