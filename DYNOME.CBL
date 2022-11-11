       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYNOME.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  ALPHABET  XX  EBCDIC
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADPRM        ASSIGN  TO  DISK.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADPRM.CPY.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND-SEM          PIC  9(01).
           03  IND-MES          PIC  9(02).
           03  TOT-ANO          PIC  9(04).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

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

           03  TAB-SEM.
               05  FILLER       PIC  X(03)  VALUE  "DOM".
               05  FILLER       PIC  X(03)  VALUE  "SEG".
               05  FILLER       PIC  X(03)  VALUE  "TER".
               05  FILLER       PIC  X(03)  VALUE  "QUA".
               05  FILLER       PIC  X(03)  VALUE  "QUI".
               05  FILLER       PIC  X(03)  VALUE  "SEX".
               05  FILLER       PIC  X(03)  VALUE  "SAB".
           03  RED-SEM          REDEFINES   TAB-SEM.
               05  SEM-TAB      PIC  X(03)  OCCURS  07.

       LINKAGE         SECTION.

       01  PRM-NOM.
           03  COD-PRM          PIC  9(06).
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

       PROCEDURE       DIVISION  USING  PRM-NOM  PRM-CAB.

       ROT-DATA-00.
           OPEN     INPUT        CADPRM
           READ     CADPRM
           MOVE     REG-PRM  TO  PRM-NOM
           COMPUTE  TOT-BIS  =   ANO-CAB  /   4
           IF  DEC-BIS   >   0
               MOVE     28   TO  DIA-TAB (02)
           ELSE
               MOVE     29   TO  DIA-TAB (02).
           COMPUTE  TOT-ANO  =   ANO-CAB  +   1999
           COMPUTE  TOT-DIA  =   DIA-CAB
                             +  (TOT-ANO  /   4)
                             +  (TOT-ANO  *   365)
           MOVE     ZEROS    TO  IND-MES  DEC-DIA.

       ROT-DATA-10.
           ADD      1        TO  IND-MES
           IF  IND-MES   <       MES-CAB
               ADD      DIA-TAB (IND-MES) TO  TOT-DIA
                                 GO       ROT-DATA-10.
           COMPUTE  TOT-DIA  =   TOT-DIA  /   7
           MOVE     ZEROS    TO  INT-DIA
           COMPUTE  IND-SEM      ROUNDED
                             =   TOT-DIA  *   7
           IF  IND-SEM   =   0
               MOVE      7   TO  IND-SEM.
           MOVE     SEM-TAB     (IND-SEM) TO  SEM-CAB
           MOVE     "/"      TO  B01-CAB      B02-CAB
           CLOSE    CADPRM       GOBACK.
