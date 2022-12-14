       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1130.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCMB        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CMB
                       FILE          STATUS  IS  FST-CMB.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST
                       FILE          STATUS  IS  FST-EST.

           SELECT      CADTNQ        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TNQ
                       FILE          STATUS  IS  FST-TNQ.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCMB.CPY.
       COPY  CADEST.CPY.
       COPY  CADTNQ.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CMB          PIC  X(02).
           03  FST-EST          PIC  X(02).
           03  FST-TNQ          PIC  X(02).

           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  IND3             PIC  9(02).
           03  IND4             PIC  9(02).
           03  IND5             PIC  9(02).
           03  IND6             PIC  9(02).
           03  IND7             PIC  9(02).
           03  IND8             PIC  9(02).
           03  TECLADO          PIC  9(02).

           03  CMB-AUX.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  DES-AUX      PIC  X(16)  VALUE  SPACES.

           03  TNQ-AUX.
               05  FILLER       PIC  X(01)  VALUE  SPACES.
               05  NUM-AUX      PIC  9(02).
               05  FILLER       PIC  X(03)  VALUE  " -".
               05  SDO-AUX      PIC  ZZ.ZZ9,9.
               05  FILLER       PIC  X(01)  VALUE  SPACES.

           03  TAB-CMB          PIC  X(17)  OCCURS  15.
           03  TAB-TNQ          PIC  X(15)  OCCURS  15.

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  04.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "1130".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "?" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 07 COLUMN 17 VALUE " Administra??o Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 08 COLUMN 19 VALUE " Controle de Estoques ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 07 COLUMN 39 VALUE "?".
               05  LINE 08 COLUMN 41 VALUE "?".
               05  LINE 08 COLUMN 18 VALUE "?".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 09 COLUMN 17 VALUE " ?                        
      -            "??????????????????? ".
               05  LINE 10 COLUMN 17 VALUE " ?
      -            "                  ? ".
               05  LINE 11 COLUMN 17 VALUE " ?
      -            "                  ? ".
               05  LINE 12 COLUMN 17 VALUE " ?
      -            "                  ? ".
               05  LINE 13 COLUMN 17 VALUE " ?
      -            "                  ? ".
               05  LINE 14 COLUMN 17 VALUE " ?
      -            "                  ? ".
               05  LINE 15 COLUMN 17 VALUE " ?
      -            "                  ? ".
               05  LINE 16 COLUMN 17 VALUE " ?
      -            "                  ? ".
               05  LINE 17 COLUMN 17 VALUE " ?
      -            "                  ? ".
               05  LINE 18 COLUMN 17 VALUE " ???????????????????????????
      -            "??????????????????? ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 09 COLUMN 21 VALUE " Controle dos Tanques ".
           03  BCK-CLR 00.
               05  LINE 10 COLUMN 65 VALUE "  ".
               05  LINE 11 COLUMN 65 VALUE "  ".
               05  LINE 12 COLUMN 65 VALUE "  ".
               05  LINE 13 COLUMN 65 VALUE "  ".
               05  LINE 14 COLUMN 65 VALUE "  ".
               05  LINE 15 COLUMN 65 VALUE "  ".
               05  LINE 16 COLUMN 65 VALUE "  ".
               05  LINE 17 COLUMN 65 VALUE "  ".
               05  LINE 18 COLUMN 65 VALUE "  ".
               05  LINE 19 COLUMN 19 PIC X(48) FROM SPACES.

       01  TELA-02.
           03  BCK-CLR 02 FOR-CLR 07.
               05  LINE 10 COLUMN 21 PIC X(19) FROM ALL "?".
               05  LINE 17 COLUMN 21 PIC X(01) FROM     "?".
           03  BCK-CLR 02 FOR-CLR 15.
               05  LINE 11 COLUMN 21 PIC X(19) FROM SPACES.
               05  LINE 11 COLUMN 22 PIC X(17) FROM TAB-CMB (IND2).
               05  LINE 12 COLUMN 21 PIC X(19) FROM SPACES.
               05  LINE 12 COLUMN 22 PIC X(17) FROM TAB-CMB (IND2 + 1).
               05  LINE 13 COLUMN 21 PIC X(19) FROM SPACES.
               05  LINE 13 COLUMN 22 PIC X(17) FROM TAB-CMB (IND2 + 2).
               05  LINE 14 COLUMN 21 PIC X(19) FROM SPACES.
               05  LINE 14 COLUMN 22 PIC X(17) FROM TAB-CMB (IND2 + 3).
               05  LINE 15 COLUMN 21 PIC X(19) FROM SPACES.
               05  LINE 15 COLUMN 22 PIC X(17) FROM TAB-CMB (IND2 + 4).
               05  LINE 16 COLUMN 21 PIC X(19) FROM SPACES.
               05  LINE 16 COLUMN 22 PIC X(17) FROM TAB-CMB (IND2 + 5).
           03  BCK-CLR 00 FOR-CLR 02.
               05  LINE 11 COLUMN 40 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 40 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 40 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 40 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 40 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 40 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 40 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 22 PIC X(18) FROM ALL "?".

       01  MENU-01  BCK-CLR 02 FOR-CLR 15.
           03  LINE IND4 COLUMN 22 PIC X(17) FROM TAB-CMB (IND3).

       01  SETA-01  BCK-CLR 07 FOR-CLR 12.
           03  LINE IND4 COLUMN 22 PIC X(17) FROM TAB-CMB (IND3).

       01  PREV-01  BCK-CLR 02.
           03  TR1-ACM LINE 11 COLUMN 39 VALUE " ".
           03  TR1-ABX LINE 16 COLUMN 39 VALUE " ".
           03  ST1-ACM LINE 11 COLUMN 39 VALUE "" FOR-CLR 14.
           03  ST1-ABX LINE 16 COLUMN 39 VALUE "" FOR-CLR 14.

       01  TELA-03.
           03  BCK-CLR 03 FOR-CLR 07.
               05  LINE 10 COLUMN 43 PIC X(17) FROM ALL "?".
               05  LINE 17 COLUMN 43 PIC X(01) FROM     "?".
           03  BCK-CLR 03 FOR-CLR 15.
               05  LINE 11 COLUMN 43 PIC X(17) FROM SPACES.
               05  LINE 11 COLUMN 44 PIC X(15) FROM TAB-TNQ (IND6).
               05  LINE 12 COLUMN 43 PIC X(17) FROM SPACES.
               05  LINE 12 COLUMN 44 PIC X(15) FROM TAB-TNQ (IND6 + 1).
               05  LINE 13 COLUMN 43 PIC X(17) FROM SPACES.
               05  LINE 13 COLUMN 44 PIC X(15) FROM TAB-TNQ (IND6 + 2).
               05  LINE 14 COLUMN 43 PIC X(17) FROM SPACES.
               05  LINE 14 COLUMN 44 PIC X(15) FROM TAB-TNQ (IND6 + 3).
               05  LINE 15 COLUMN 43 PIC X(17) FROM SPACES.
               05  LINE 15 COLUMN 44 PIC X(15) FROM TAB-TNQ (IND6 + 4).
               05  LINE 16 COLUMN 43 PIC X(17) FROM SPACES.
               05  LINE 16 COLUMN 44 PIC X(15) FROM TAB-TNQ (IND6 + 5).
           03  BCK-CLR 00 FOR-CLR 03.
               05  LINE 11 COLUMN 60 PIC X(01) FROM     " ".
               05  LINE 12 COLUMN 60 PIC X(01) FROM     " ".
               05  LINE 13 COLUMN 60 PIC X(01) FROM     " ".
               05  LINE 14 COLUMN 60 PIC X(01) FROM     " ".
               05  LINE 15 COLUMN 60 PIC X(01) FROM     " ".
               05  LINE 16 COLUMN 60 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 60 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 44 PIC X(16) FROM ALL "?".

       01  MENU-02  BCK-CLR 03 FOR-CLR 15.
           03  LINE IND8 COLUMN 44 PIC X(15) FROM TAB-TNQ (IND7).

       01  PREV-02  BCK-CLR 03.
           03  TR2-ACM LINE 11 COLUMN 59 VALUE " ".
           03  TR2-ABX LINE 16 COLUMN 59 VALUE " ".
           03  ST2-ACM LINE 11 COLUMN 59 VALUE "" FOR-CLR 14.
           03  ST2-ABX LINE 16 COLUMN 59 VALUE "" FOR-CLR 14.

       01  ENTR-01 LINE IND8 COLUMN 50 PIC ZZ.ZZ9,9 USING SDI-TNQ.

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL"  USING    PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1   GOBACK.  DISPLAY  TELA-01
           OPEN     INPUT    CADCMB
                    I-O      CADEST   CADTNQ
           MOVE     1          TO      IND1.

       ROT-PESQ-00.
           MOVE     SPACES   TO  TAB-CMB (IND1)
           ADD      1        TO  IND1
           IF  IND1    <     16  GO       ROT-PESQ-00.
           MOVE     11       TO  IND4
           MOVE     01       TO  IND2     IND3
           MOVE     ZEROS    TO  CHV-CMB  IND1.

       ROT-PESQ-10.
           START    CADCMB       KEY  >   CHV-CMB
           IF  FST-CMB   =  "23" GO       ROT-PESQ-20.
           READ     CADCMB  NEXT ADD      1  TO  IND1
           MOVE     DES-CMB  TO  DES-AUX
           MOVE     CMB-AUX  TO  TAB-CMB  (IND1)
           GO       ROT-PESQ-10.

       ROT-PESQ-20.
           DISPLAY  TELA-02      SETA-01
           IF (IND2 + 05) < IND1 DISPLAY  ST1-ABX
               IF  IND2     >  1 DISPLAY  ST1-ACM
                   ELSE          DISPLAY  TR1-ACM
           ELSE    IF  IND2 >  1 DISPLAY  ST1-ACM  TR1-ABX
                       ELSE      DISPLAY  TR1-ACM.
       ROT-PESQ-30.
           MOVE     1        TO  IND5.

       ROT-PESQ-40.
           MOVE     SPACES   TO  TAB-TNQ (IND5)
           ADD      1        TO  IND5
           IF  IND5    <     16  GO       ROT-PESQ-40.
           MOVE     11       TO  IND8
           MOVE     01       TO  IND6     IND7
           MOVE     ZEROS    TO  CHV-TNQ  IND5.

       ROT-PESQ-50.
           START    CADTNQ       KEY  >   CHV-TNQ
           IF  FST-TNQ   =  "23" GO       ROT-PESQ-60.
           READ     CADTNQ  NEXT
           IF  CMB-TNQ  NOT   =  IND3
                                 GO       ROT-PESQ-50.
           ADD      1        TO  IND5
           MOVE     CHV-TNQ  TO  NUM-AUX
           MOVE     SDI-TNQ  TO  SDO-AUX
           MOVE     TNQ-AUX  TO  TAB-TNQ  (IND5)
           GO       ROT-PESQ-50.

       ROT-PESQ-60.
           DISPLAY  TELA-03
           IF (IND6 + 05) < IND5 DISPLAY  ST2-ABX
               IF  IND6     >  1 DISPLAY  ST2-ACM
                   ELSE          DISPLAY  TR2-ACM
           ELSE    IF  IND6 >  1 DISPLAY  ST2-ACM  TR2-ABX
                       ELSE      DISPLAY  TR2-ACM.

       ROT-COMB-00.
           MOVE  2  TO  IND-MSG  PERFORM  ROT-MSGM-00
           IF  TCL-MSG   =   01  PERFORM  ROT-HELP-00
                                 GO       ROT-COMB-00.
           IF  TCL-MSG   =   05  GO       ROT-COMB-50.
           IF  TCL-MSG   =   06  GO       ROT-COMB-60.
           IF  TCL-MSG   =   53  GO       ROT-COMB-70.
           IF  TCL-MSG   =   54  GO       ROT-COMB-80.
           IF  TCL-MSG   >   00  GO       ROT-COMB-00.
           IF  HEX-MSG   =   13  GO       ROT-TANQ-00.
           MOVE     SPACES   TO  CHV-EST.

       ROT-COMB-10.
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-EXIT-00.

       ROT-COMB-20.
           READ     CADEST  NEXT MOVE 20  TO  IND-MSG
           IF  FST-EST   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-COMB-20.
           IF  PRX-EST   >   "1" GO       ROT-EXIT-00.
           MOVE     ZEROS    TO  CHV-TNQ  SDO-EST.

       ROT-COMB-30.
           START    CADTNQ       KEY  >   CHV-TNQ
           IF  FST-TNQ   =  "23" GO       ROT-COMB-40.
           READ     CADTNQ  NEXT
           IF  CMB-TNQ  NOT   =  SUF-EST
                                 GO       ROT-COMB-30.
           ADD      SDI-TNQ  TO  SDO-EST
           GO       ROT-COMB-30.

       ROT-COMB-40.
           REWRITE  REG-EST      GO       ROT-COMB-10.

       ROT-COMB-50.
           IF  IND3      =   01  GO       ROT-COMB-00.
           IF  IND4      =   11  COMPUTE  IND2 = IND2 - 1
                                 COMPUTE  IND3 = IND3 - 1
                                 GO       ROT-PESQ-20.
           DISPLAY  MENU-01      COMPUTE  IND3 = IND3 - 1
                                 COMPUTE  IND4 = IND4 - 1
           DISPLAY  SETA-01      GO       ROT-PESQ-30.

       ROT-COMB-60.
           IF  IND3      =  IND1 GO       ROT-COMB-00.
           IF  IND4      =   16  COMPUTE  IND2 = IND2 + 1
                                 COMPUTE  IND3 = IND3 + 1
                                 GO       ROT-PESQ-20.
           DISPLAY  MENU-01      COMPUTE  IND3 = IND3 + 1
                                 COMPUTE  IND4 = IND4 + 1
           DISPLAY  SETA-01      GO       ROT-PESQ-30.

       ROT-COMB-70.
           IF  IND2      =   01  GO       ROT-COMB-00.
           MOVE     11       TO  IND4
           IF  IND2      >    6  COMPUTE  IND3 = IND2 - 6
                                 COMPUTE  IND2 = IND2 - 6
           ELSE
                                 MOVE  1  TO   IND2  IND3.
           GO      ROT-PESQ-20.

       ROT-COMB-80.
           IF  IND1      <      (IND2  +  6)
                                 GO       ROT-COMB-00.
           COMPUTE  IND3     =   IND2  +  6
           COMPUTE  IND2     =   IND2  +  6
           MOVE     11  TO  IND4 GO       ROT-PESQ-20.

       ROT-TANQ-00.
           MOVE     TAB-TNQ      (IND7)   TO  TNQ-AUX
           MOVE     NUM-AUX  TO  CHV-TNQ
           READ     CADTNQ       MOVE 20  TO  IND-MSG
           IF  FST-TNQ   =  "9D" PERFORM  ROT-MSGM-00
                                 GO       ROT-TANQ-00.
       ROT-TANQ-10.
           ACCEPT   ENTR-01
           ACCEPT   TECLADO FROM ESCAPE   KEY
           IF  TECLADO   =   01  DISPLAY  MENU-02
                                 GO       ROT-PESQ-30.
           IF  TECLADO   =   02  PERFORM  ROT-HELP-00
                                 GO       ROT-TANQ-10.
           MOVE     CHV-TNQ  TO  NUM-AUX
           MOVE     SDI-TNQ  TO  SDO-AUX
           MOVE     TNQ-AUX  TO  TAB-TNQ  (IND7)
           REWRITE  REG-TNQ      DISPLAY  MENU-02
           IF  IND7  =  IND5     GO       ROT-PESQ-30.
           IF  IND8  =   16      ADD      1  TO  IND6
                                 ADD      1  TO  IND7
                                 PERFORM  ROT-PESQ-60
           ELSE
                                 ADD      1  TO  IND7
                                 ADD      1  TO  IND8.
           GO       ROT-TANQ-00.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-MSGM-00.
           CALL    "DYMSGM"  USING  PRM-MSG.

       ROT-EXIT-00.
           CLOSE    CADCMB   CADEST  CADTNQ  GOBACK.
