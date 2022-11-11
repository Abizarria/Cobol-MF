       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY5100.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  ALPHABET  XX  EBCDIC
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      BACKUP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  LINE  SEQUENTIAL
                       FILE          STATUS  IS  FST-BCK.

       DATA            DIVISION.
       FILE            SECTION.

       FD  BACKUP    VALUE  OF  FILE-ID  "BACKUP.TXT".

       01  REG-BCK              PIC  X(50).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  FST-BCK          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  RSP-OPC          PIC  X(01).

           03  STA-AUX          PIC  9(02)V9.
           03  RED-STA   REDEFINES   STA-AUX.
               05  INT-AUX      PIC  9(02).
               05  DEC-AUX      PIC  9(01).

           03  TAB-ARQ.
               05  ARQ-01       PIC  X(10)  VALUE  "CADATV.DAT".
               05  ARQ-02       PIC  X(10)  VALUE  "CADATV.IDX".
               05  ARQ-03       PIC  X(10)  VALUE  "CADBCO.DAT".
               05  ARQ-04       PIC  X(10)  VALUE  "CADBCO.IDX".
               05  ARQ-05       PIC  X(10)  VALUE  "CADCDV.DAT".
               05  ARQ-06       PIC  X(10)  VALUE  "CADCDV.IDX".
               05  ARQ-07       PIC  X(10)  VALUE  "CADCLI.DAT".
               05  ARQ-08       PIC  X(10)  VALUE  "CADCLI.IDX".
               05  ARQ-09       PIC  X(10)  VALUE  "CADCMB.DAT".
               05  ARQ-10       PIC  X(10)  VALUE  "CADCMB.IDX".
               05  ARQ-11       PIC  X(10)  VALUE  "CADCRD.DAT".
               05  ARQ-12       PIC  X(10)  VALUE  "CADCRD.IDX".
               05  ARQ-13       PIC  X(10)  VALUE  "CADCTR.DAT".
               05  ARQ-14       PIC  X(10)  VALUE  "CADCXA.DAT".
               05  ARQ-15       PIC  X(10)  VALUE  "CADCXA.IDX".
               05  ARQ-16       PIC  X(10)  VALUE  "CADDSP.DAT".
               05  ARQ-17       PIC  X(10)  VALUE  "CADDSP.IDX".
               05  ARQ-18       PIC  X(10)  VALUE  "CADECR.DAT".
               05  ARQ-19       PIC  X(10)  VALUE  "CADECR.IDX".
               05  ARQ-20       PIC  X(10)  VALUE  "CADEST.DAT".
               05  ARQ-21       PIC  X(10)  VALUE  "CADEST.IDX".
               05  ARQ-22       PIC  X(10)  VALUE  "CADFAT.DAT".
               05  ARQ-23       PIC  X(10)  VALUE  "CADFAT.IDX".
               05  ARQ-24       PIC  X(10)  VALUE  "CADHLP.DAT".
               05  ARQ-25       PIC  X(10)  VALUE  "CADHLP.IDX".
               05  ARQ-26       PIC  X(10)  VALUE  "CADLBC.DAT".
               05  ARQ-27       PIC  X(10)  VALUE  "CADLBC.IDX".
               05  ARQ-28       PIC  X(10)  VALUE  "CADLCP.DAT".
               05  ARQ-29       PIC  X(10)  VALUE  "CADLCP.IDX".
               05  ARQ-30       PIC  X(10)  VALUE  "CADLCR.DAT".
               05  ARQ-31       PIC  X(10)  VALUE  "CADLCR.IDX".
               05  ARQ-32       PIC  X(10)  VALUE  "CADLCX.DAT".
               05  ARQ-33       PIC  X(10)  VALUE  "CADLCX.IDX".
               05  ARQ-34       PIC  X(10)  VALUE  "CADLDP.DAT".
               05  ARQ-35       PIC  X(10)  VALUE  "CADLDP.IDX".
               05  ARQ-36       PIC  X(10)  VALUE  "CADLES.DAT".
               05  ARQ-37       PIC  X(10)  VALUE  "CADLES.IDX".
               05  ARQ-38       PIC  X(10)  VALUE  "CADMDR.DAT".
               05  ARQ-39       PIC  X(10)  VALUE  "CADMDR.IDX".
               05  ARQ-40       PIC  X(10)  VALUE  "CADNTA.DAT".
               05  ARQ-41       PIC  X(10)  VALUE  "CADNTA.IDX".
               05  ARQ-42       PIC  X(10)  VALUE  "CADOPR.DAT".
               05  ARQ-43       PIC  X(10)  VALUE  "CADOPR.IDX".
               05  ARQ-44       PIC  X(10)  VALUE  "CADPRM.DAT".
               05  ARQ-45       PIC  X(10)  VALUE  "CADPRS.DAT".
               05  ARQ-46       PIC  X(10)  VALUE  "CADPRS.IDX".
               05  ARQ-47       PIC  X(10)  VALUE  "CADSDO.DAT".
               05  ARQ-48       PIC  X(10)  VALUE  "CADSDO.IDX".
               05  ARQ-49       PIC  X(10)  VALUE  "CADSPL.DAT".
               05  ARQ-50       PIC  X(10)  VALUE  "CADSPL.IDX".
               05  ARQ-51       PIC  X(10)  VALUE  "CADTNQ.DAT".
               05  ARQ-52       PIC  X(10)  VALUE  "CADTNQ.IDX".
           03  RED-ARQ          REDEFINES   TAB-ARQ.
               05  ARQ-TAB      PIC  X(10)  OCCURS  52.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "5100".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  65.

       01  PRM-ADS              PIC  9(02)  COMP-X.
       01  PRM-FNC              PIC  9(02)  COMP-X  VALUE  35.

       01  PRM-CMD.
           03  SIZ-CMD          PIC  9(02)  COMP-X.
           03  DES-CMD.
               05  FILLER       PIC  X(05)  VALUE  "COPY".
               05  ARQ-CMD      PIC  X(11).
               05  LOC-CMD      PIC  X(50).
               05  FILLER       PIC  X(06)  VALUE  " > NUL".

       LINKAGE         SECTION.

       01  PRM-OPR          PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "Û" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 08 COLUMN 22 VALUE " Back-up de Seguran‡a ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 08 COLUMN 44 VALUE "ß".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 09 COLUMN 22 VALUE
                   " É                       ÍÍÍÍÍÍÍÍÍ» ".
               05  LINE 10 COLUMN 22 VALUE
                   " º                                  º ".
               05  LINE 11 COLUMN 22 VALUE
                   " º                                  º ".
               05  LINE 12 COLUMN 22 VALUE
                   " º                                  º ".
               05  LINE 13 COLUMN 22 VALUE
                   " º                                  º ".
               05  LINE 14 COLUMN 22 VALUE
                   " º                                  º ".
               05  LINE 15 COLUMN 22 VALUE
                   " º                                  º ".
               05  LINE 16 COLUMN 22 VALUE
                   " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 09 COLUMN 26 VALUE "  Atualizar Back-up  ".
           03  BCK-CLR 00.
               05  LINE 10 COLUMN 60 VALUE "  ".
               05  LINE 11 COLUMN 60 VALUE "  ".
               05  LINE 12 COLUMN 60 VALUE "  ".
               05  LINE 13 COLUMN 60 VALUE "  ".
               05  LINE 14 COLUMN 60 VALUE "  ".
               05  LINE 15 COLUMN 60 VALUE "  ".
               05  LINE 16 COLUMN 60 VALUE "  ".
               05  LINE 17 COLUMN 24 PIC X(38) FROM SPACES.

           03  BCK-CLR 05 FOR-CLR 07.
               05  LINE 10 COLUMN 26 PIC X(29) FROM ALL "ß".
               05  LINE 12 COLUMN 26 PIC X(01) FROM     "Ü".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 11 COLUMN 26 VALUE
                   "  Confirma Atualiza‡Æo? [ ]  ".
           03  BCK-CLR 05 FOR-CLR 00.
               05  LINE 11 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 12 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 12 COLUMN 27 PIC X(28) FROM ALL "Ü".

           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "ß".
               05  LINE 14 COLUMN 26 PIC X(29) FROM ALL " ".
               05  LINE 15 COLUMN 26 PIC X(01) FROM     "Ü".
           03  BCK-CLR 00.
               05  LINE 14 COLUMN 28 PIC X(25) FROM SPACES.
           03  BCK-CLR 02 FOR-CLR 00.
               05  LINE 14 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 15 COLUMN 55 PIC X(01) FROM     "Û".
               05  LINE 15 COLUMN 27 PIC X(28) FROM ALL "Ü".

       01  TELA-02  BCK-CLR 05 FOR-CLR 15.
           03  LINE 11 COLUMN 26 VALUE "  Atualizando .: ".
           03  LINE 11 COLUMN 43 PIC X(10) FROM ARQ-TAB (IND1).

       01  TELA-03  LINE 14 COLUMN IND2 VALUE "Û" BCK-CLR 00 FOR-CLR 12.

       01  ENTR-01  BCK-CLR 05 FOR-CLR 15
                    LINE 11 COLUMN 51 PIC X(01) USING RSP-OPC AUTO.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01

           OPEN     INPUT        BACKUP
           READ     BACKUP
           MOVE     REG-BCK  TO  LOC-CMD
           CLOSE    BACKUP.

       ROT-BACK-00.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   ENTR-01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  GOBACK.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-BACK-00.
           IF  RSP-OPC  NOT   =  "S"
                                 GO       ROT-BACK-00.
           MOVE     27  TO  IND2 MOVE     1  TO  IND1.

       ROT-BACK-10.
           MOVE     ARQ-TAB     (IND1)    TO  ARQ-CMD
           DISPLAY  DES-CMD      UPON    COMMAND-LINE
           PERFORM  ROT-COPY-00  DISPLAY  TELA-02
           COMPUTE  STA-AUX   =  IND1      /    2
           IF  DEC-AUX   =    0  ADD  1   TO IND2
                                 DISPLAY  TELA-03.
           IF  IND1      >   51  GOBACK.
           ADD      1   TO  IND1 GO       ROT-BACK-10.

       ROT-COPY-00.
           CALL  X"91"    USING  PRM-ADS
                                 PRM-FNC  PRM-CMD.
       ROT-HELP-00.
           CALL  "DYHELP" USING  PRM-HLP.
