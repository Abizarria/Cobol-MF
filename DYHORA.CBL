       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYHORA.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT  IS  COMMA.

       DATA            DIVISION.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.

           03  IND-SEM          PIC  9(01).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-AUX.
               05  SEM-AUX      PIC  X(04).
               05  DIA-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-AUX      PIC  9(02).

           03  HRA-SYS.
               05  HOR-SYS      PIC  9(02).
               05  MIN-SYS      PIC  9(02).
               05  SEG-SYS      PIC  9(02).

           03  HRA-AUX.
               05  HOR-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  ":".
               05  MIN-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  ":".
               05  SEG-AUX      PIC  9(02).

           03  TAB-SEM.
               05  FILLER       PIC  X(03)  VALUE  "SEG".
               05  FILLER       PIC  X(03)  VALUE  "TER".
               05  FILLER       PIC  X(03)  VALUE  "QUA".
               05  FILLER       PIC  X(03)  VALUE  "QUI".
               05  FILLER       PIC  X(03)  VALUE  "SEX".
               05  FILLER       PIC  X(03)  VALUE  "SAB".
               05  FILLER       PIC  X(03)  VALUE  "DOM".
           03  RED-SEM          REDEFINES   TAB-SEM.
               05  SEM-TAB      PIC  X(03)  OCCURS  07.

       SCREEN          SECTION.

       01  TELA-01  BCK-CLR 02 FOR-CLR 00.
           03  LINE 03 COLUMN 47 PIC X(08) USING HRA-AUX.
           03  LINE 03 COLUMN 68 PIC X(12) USING DTA-AUX.

       PROCEDURE       DIVISION.

       ROT-0000-00.
           ACCEPT   DTA-SYS FROM DATE
           ACCEPT   HRA-SYS FROM TIME
           ACCEPT   IND-SEM FROM DAY-OF-WEEK
           MOVE     DIA-SYS  TO  DIA-AUX
           MOVE     MES-SYS  TO  MES-AUX
           MOVE     ANO-SYS  TO  ANO-AUX
           MOVE     HOR-SYS  TO  HOR-AUX
           MOVE     MIN-SYS  TO  MIN-AUX
           MOVE     SEG-SYS  TO  SEG-AUX
           MOVE     SEM-TAB     (IND-SEM) TO  SEM-AUX
           DISPLAY  TELA-01      GOBACK.
