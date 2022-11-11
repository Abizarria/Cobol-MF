       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DY1255.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADBCO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-BCO
                       FILE          STATUS  IS  FST-BCO.

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST
                       FILE          STATUS  IS  FST-EST.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC
                       FILE          STATUS  IS  FST-LBC.

           SELECT      CADLCP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCP
                       FILE          STATUS  IS  FST-LCP.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP
                       FILE          STATUS  IS  FST-LDP.

           SELECT      CADLES        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LES
                       FILE          STATUS  IS  FST-LES.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS
                       FILE          STATUS  IS  FST-PRS.

           SELECT      CADSDO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-SDO
                       FILE          STATUS  IS  FST-SDO.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADBCO.CPY.
       COPY  CADCTR.CPY.
       COPY  CADEST.CPY.
       COPY  CADLBC.CPY.
       COPY  CADLCP.CPY.
       COPY  CADLDP.CPY.
       COPY  CADLES.CPY.
       COPY  CADPRS.CPY.
       COPY  CADSDO.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-BCO          PIC  X(02).
           03  FST-CTR          PIC  X(02).
           03  FST-EST          PIC  X(02).
           03  FST-LBC          PIC  X(02).
           03  FST-LCP          PIC  X(02).
           03  FST-LDP          PIC  X(02).
           03  FST-LES          PIC  X(02).
           03  FST-PRS          PIC  X(02).
           03  FST-SDO          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  IND1             PIC  9(01)     VALUE  1.
           03  IND2             PIC  9(02)     VALUE  1.

           03  QTD-AUX          PIC S9(07)V9   VALUE  ZEROS.
           03  VAL-AUX          PIC S9(08)V99  VALUE  ZEROS.
           03  TOT-CST          PIC  9(09)V99  VALUE  ZEROS.
           03  TOT-VDA          PIC  9(09)V99  VALUE  ZEROS.
           03  TOT-LUC          PIC S9(09)V99  VALUE  ZEROS.
           03  TOT-GER          PIC S9(09)V99  VALUE  ZEROS.

           03  ACM-SDI          PIC S9(08)V99  VALUE  ZEROS.
           03  TOT-SDI          PIC S9(08)V99  VALUE  ZEROS.
           03  ACM-ENT          PIC  9(08)V99  VALUE  ZEROS.
           03  TOT-ENT          PIC  9(08)V99  VALUE  ZEROS.
           03  ACM-SAI          PIC  9(08)V99  VALUE  ZEROS.
           03  TOT-SAI          PIC  9(08)V99  VALUE  ZEROS.
           03  ACM-COR          PIC S9(08)V99  VALUE  ZEROS.
           03  TOT-COR          PIC S9(08)V99  VALUE  ZEROS.
           03  TOT-SDA          PIC  9(08)V99  VALUE  ZEROS.

           03  DTA-SYS.                             
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-LIM.
               05  ANO-LIM      PIC  9(02).
               05  MES-LIM      PIC  9(02).
               05  DIA-LIM      PIC  9(02).

           03  TAB-IND   VALUE  "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".
               05  IND-TAB      PIC  X(01)  OCCURS  35  INDEXED BY I.

           03  TAB-001.
               05  FILLER       PIC X(1725) VALUE  "TOTAL COMBUSTIVEL".
               05  FILLER       PIC X(1725) VALUE  "TOTAL DE PRODUTOS".
               05  FILLER       PIC X(1725) VALUE  "TOTAL DE SERVICOS".
               05  FILLER       PIC X(1725) VALUE  "TOTAL FINANCEIRO ".
           03  RED-001          REDEFINES   TAB-001  OCCURS  04.
               05  DES-T01      PIC  X(17).
               05  QTD-T01      PIC  9(07)V9.
               05  CST-T01      PIC  9(08)V99.
               05  VDA-T01      PIC  9(08)V99.
               05  LIN-T01      OCCURS     35.
                   07  DES-L01  PIC  X(20).
                   07  QTD-L01  PIC  9(07)V9.
                   07  CST-L01  PIC  9(08)V99.
                   07  VDA-L01  PIC  9(08)V99.

           03  TAB-002.
               05  FILLER       PIC  X(74)  VALUE
                   "OBRIG. TRABALHISTA(+) TOTAL COMBUSTIVEL".
               05  FILLER       PIC  X(74)  VALUE
                   "OBRIGACOES FISCAIS(+) TOTAL DE PRODUTOS".
               05  FILLER       PIC  X(74)  VALUE
                   "DESP. OPERACIONAIS(+) TOTAL DE SERVICOS".
               05  FILLER       PIC  X(74)  VALUE
                   "DESPESA DE CAPITAL(+) TOTAL FINANCEIRO ".
               05  FILLER       PIC  X(74)  VALUE
                   "CREDITOS DE SOCIOS(-) TOTAL DE DESPESAS".
               05  FILLER       PIC  X(74)  VALUE
                   "CHEQUES DEVOLVIDOS(=) LUCRO LIQUIDO    ".
           03  RED-002          REDEFINES  TAB-002  OCCURS  06.
               05  DSD-T02      PIC  X(18).
               05  DSR-T02      PIC  X(21).
               05  QTD-T02      PIC  9(04).
               05  VLD-T02      PIC S9(08)V99.
               05  VLP-T02      PIC  9(08)V99.
               05  VLR-T02      PIC S9(09)V99.

           03  TAB-003          OCCURS    02.
               05  QCP-T03      PIC S9(07)V9.
               05  QVD-T03      PIC  9(07)V9.
               05  QES-T03      PIC S9(06)V9.
               05  SDA-T03      PIC S9(06)V9.
               05  VCP-T03      PIC S9(08)V99.
               05  VVD-T03      PIC  9(08)V99.
               05  VES-T03      PIC S9(07)V99.
               05  VLA-T03      PIC S9(07)V99.
               05  LIN-T03      OCCURS     35.
                   07  QCP-L03  PIC S9(07)V9.
                   07  QVD-L03  PIC  9(07)V9.
                   07  QES-L03  PIC S9(06)V9.
                   07  SDA-L03  PIC S9(06)V9.
                   07  VCP-L03  PIC S9(08)V99.
                   07  VVD-L03  PIC  9(08)V99.
                   07  VES-L03  PIC S9(07)V99.
                   07  VLA-L03  PIC S9(07)V99.

           03  TAB-004          OCCURS     18.
               05  DES-T04      PIC  X(15).
               05  SDI-T04      PIC S9(07)V99.
               05  ENT-T04      PIC  9(07)V99.
               05  SAI-T04      PIC  9(07)V99.
               05  COR-T04      PIC S9(07)V99.

       01  CAB-001.
           03  FILLER           PIC  X(47)  VALUE  "-DYNAMIC-".
           03  CLI-CB1          PIC  X(40).
           03  FILLER           PIC  X(38)  VALUE  SPACES.
           03  FILLER           PIC  X(09)  VALUE  "FOLHA .:".
           03  PAG-CB1          PIC  9(03)  VALUE  ZEROS.

       01  CAB-002.
           03  FILLER           PIC  X(23)  VALUE
               "ANALISE OPERACIONAL DE".
           03  DDI-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MMI-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AAI-CB2          PIC  9(02).
           03  FILLER           PIC  X(03)  VALUE  " A".
           03  DDF-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MMF-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AAF-CB2          PIC  9(02).
           03  FILLER           PIC  X(83)  VALUE  SPACES.
           03  DTA-CB2          PIC  X(12).

       01  CAB-003.
           03  FILLER           PIC  X(68)  VALUE  "зддддддддддддддддддд
      -        "ддддддддбдддддддддддддддддддбддддддддддддддддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддбдддддддддддддддд
      -        "ддддддбдддддддддддддддддддддддбддддддддддддддддд©".

       01  CAB-004.
           03  FILLER           PIC  X(68)  VALUE  "Ё    O R I G E M
      -        "        Ё    QUANTIDADES    Ё  VALOR DAS RECEITA".
           03  FILLER           PIC  X(69)  VALUE  "S  Ё     CUSTO  MEDI
      -        "O     Ё      LUCRO BRUTO      Ё    MARGEM  %    Ё".

       01  CAB-005.
           03  FILLER           PIC  X(68)  VALUE  "цддддддддддддддддддд
      -        "ддддддддедддддддддддддддддддеддддддддддддддддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддедддддддддддддддд
      -        "ддддддедддддддддддддддддддддддеддддддддддддддддд╢".

       01  CAB-006.
           03  FILLER           PIC  X(68)  VALUE  "юддддддддддддддддддд
      -        "ддддддддадддддддддддддддддддаддддддддддддддддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддадддддддддддддддд
      -        "ддддддадддддддддддддддддддддддаддддддддддддддддды".

       01  CAB-007.
           03  FILLER           PIC  X(68)  VALUE  "зддддддддддддддддддд
      -        "дддддбддддддддддбдддддддддддддддддддбддддддддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддбдддддддддддддддд
      -        "ддддддддддддбддддддддддддддддддддбдддддддддддддд©".

       01  CAB-008.
           03  FILLER           PIC  X(68)  VALUE  "Ё   D E S P E S A S
      -        "     Ё  QUANT.  Ё  VALOR LIQUIDADO  Ё   MARGEM %".
           03  FILLER           PIC  X(69)  VALUE  "   Ё   R E S U L T A
      -        " D O S      Ё    VALOR APURADO   Ё   MARGEM %   Ё".

       01  CAB-009.
           03  FILLER           PIC  X(68)  VALUE  "цддддддддддддддддддд
      -        "дддддеддддддддддедддддддддддддддддддеддддддддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддедддддддддддддддд
      -        "ддддддддддддеддддддддддддддддддддедддддддддддддд╢".

       01  CAB-010.
           03  FILLER           PIC  X(68)  VALUE  "юддддддддддддддддддд
      -        "дддддаддддддддддадддддддддддддддддддаддддддддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддадддддддддддддддд
      -        "ддддддддддддаддддддддддддддддддддадддддддддддддды".

       01  CAB-011              PIC X(137)  VALUE  ALL  "- ".

       01  CAB-012.
           03  FILLER           PIC  X(68)  VALUE  "зддддддддддддддддддд
      -        "дддбддддддддддддддбддддддддддддддддбдддддддддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддбдддддддддддддддд
      -        "бддддддддддддддбддддддддддддддбддддддддддддддддд©".

       01  CAB-013.
           03  FILLER           PIC  X(68)  VALUE  "Ё  D E S C R I C A O
      -        "   Ё    INICIAL   Ё     COMPRAS    Ё     VENDAS ".
           03  FILLER           PIC  X(69)  VALUE  "   Ё    CORRECOES
      -        "Ё     ATUAL    Ё   VARIACAO   Ё  VARIACAO - R$  Ё".

       01  CAB-014.
           03  FILLER           PIC  X(68)  VALUE  "цддддддддддддддддддд
      -        "дддеддддддддддддддеддддддддддддддддедддддддддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддедддддддддддддддд
      -        "еддддддддддддддеддддддддддддддеддддддддддддддддд╢".

       01  CAB-015.
           03  FILLER           PIC  X(68)  VALUE  "юддддддддддддддддддд
      -        "дддаддддддддддддддаддддддддддддддддадддддддддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддадддддддддддддддд
      -        "аддддддддддддддаддддддддддддддаддддддддддддддддды".

       01  CAB-016.
           03  FILLER           PIC  X(68)  VALUE  "зддддддддддддддддддд
      -        "ддддбддддддддддддддддддбдддддддддддддддддбдддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддддддддддбдддддддд
      -        "ддддддддддбддддддддддддддддддбдддддддддддддддддд©".

       01  CAB-017.
           03  FILLER           PIC  X(68)  VALUE  "Ё  D E S C R I C A O
      -        "    Ё      INICIAL     Ё     ENTRADAS    Ё      ".
           03  FILLER           PIC  X(69)  VALUE  "SAIDAS     Ё     COR
      -        "RECOES    Ё       ATUAL      Ё     VARIACAO     Ё".

       01  CAB-018.
           03  FILLER           PIC  X(68)  VALUE  "цддддддддддддддддддд
      -        "ддддеддддддддддддддддддедддддддддддддддддедддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддддддддддедддддддд
      -        "ддддддддддеддддддддддддддддддедддддддддддддддддд╢".

       01  CAB-019.
           03  FILLER           PIC  X(68)  VALUE  "юддддддддддддддддддд
      -        "ддддаддддддддддддддддддадддддддддддддддддадддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддддддддддадддддддд
      -        "ддддддддддаддддддддддддддддддадддддддддддддддддды".

       01  CAB-020.
           03  FILLER           PIC  X(68)  VALUE  "зддддддддддддддддддд
      -        "дддддддддддддддддддддддддддддддддддддддддддддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддддддддддддддддддд
      -        "дддддддддддддддддддддддддддддддддддддддддддддддд©".

       01  CAB-021.
           03  FILLER           PIC  X(68)  VALUE  "юддддддддддддддддддд
      -        "дддддддддддддддддддддддддддддддддддддддддддддддд".
           03  FILLER           PIC  X(69)  VALUE  "дддддддддддддддддддд
      -        "дддддддддддддддддддддддддддддддддддддддддддддддды".

       01  DET-001.
           03  FILLER           PIC  X(05)  VALUE  "Ё".
           03  DES-DT1          PIC  X(20).
           03  FILLER           PIC  X(08)  VALUE  "   Ё".
           03  QTD-DT1          PIC  Z.ZZZ.ZZ9,9.
           03  FILLER           PIC  X(09)  VALUE  "    Ё".
           03  VDA-DT1          PIC  ZZZ.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(09)  VALUE  "    Ё".
           03  CST-DT1          PIC  ZZZ.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(09)  VALUE  "    Ё".
           03  LUC-DT1          PIC  ----.---.--9,99.
           03  FILLER           PIC  X(09)  VALUE  "    Ё".
           03  PCT-DT1          PIC  ---9,99.
           03  FILLER           PIC  X(07)  VALUE  " %    Ё".

       01  DET-002.
           03  FILLER           PIC  X(04)  VALUE  "Ё".
           03  DSD-DT2          PIC  X(18).
           03  FILLER           PIC  X(06)  VALUE  "   Ё".
           03  QTD-DT2          PIC  Z.ZZ9.
           03  FILLER           PIC  X(06)  VALUE  "   Ё".
           03  VLD-DT2          PIC  ---.---.--9,99.
           03  FILLER           PIC  X(07)  VALUE  "   Ё".
           03  PCD-DT2          PIC  ZZ9,99.
           03  FILLER           PIC  X(09)  VALUE  " %   Ё".
           03  DSR-DT2          PIC  X(22).
           03  FILLER           PIC  X(06)  VALUE  "   Ё".
           03  VLR-DT2          PIC  ----.---.--9,99.
           03  FILLER           PIC  X(06)  VALUE  "   Ё".
           03  PCR-DT2          PIC  ---9,99.
           03  FILLER           PIC  X(06)  VALUE  " %   Ё".

       01  DET-003.
           03  FILLER           PIC  X(03)  VALUE  "Ё".
           03  DES-DT3          PIC  X(18).
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  SDI-DT3          PIC  ----.--9,9.
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  QCP-DT3          PIC  --.---.--9,9.
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  QVD-DT3          PIC  Z.ZZZ.ZZ9,9.
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  QES-DT3          PIC  --.---.--9,9.
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  SDA-DT3          PIC  ----.--9,9.
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  DIF-DT3          PIC  ----.--9,9.
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  VAL-DT3          PIC  --.---.--9,99.
           03  FILLER           PIC  X(03)  VALUE  "  Ё".

       01  DET-004.
           03  FILLER           PIC  X(03)  VALUE  "Ё".
           03  DES-DT4          PIC  X(19).
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  SDI-DT4          PIC  ---.---.--9,99.
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  ENT-DT4          PIC  ZZ.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  SAI-DT4          PIC  ZZ.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  COR-DT4          PIC  ---.---.--9,99.
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  SDA-DT4          PIC  ---.---.--9,99.
           03  FILLER           PIC  X(05)  VALUE  "  Ё".
           03  VAR-DT4          PIC  ---.---.--9,99.
           03  FILLER           PIC  X(03)  VALUE  "  Ё".

       01  DET-005.
           03  FILLER           PIC  X(70)  VALUE  "Ё  R E S U L T A D O
      -        "    O P E R A C I O N A L    N O    P E R I O D O".
           03  FILLER           PIC  X(49)  VALUE
               "............................................. R$".
           03  VAL-DT5          PIC  ----.---.--9,99.
           03  FILLER           PIC  X(03)  VALUE  "  Ё".

       01  PRM-SPL.
           03  LCK-SPL          PIC  9(01)  VALUE  0.
           03  PRG-SPL          PIC  9(02)  VALUE  21.

       01  PRM-NOM.
           03  COD-NOM          PIC  9(06).
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

       01  PRM-IMP.
           03  ARQ-IMP          PIC  9(06).
           03  PAG-IMP          PIC  9(03).
           03  FLG-IMP          PIC  9(01)  VALUE  2.
           03  LIN-IMP          PIC  9(02)  VALUE  16.
           03  CL1-IMP          PIC  9(02)  VALUE  40.
           03  CL2-IMP          PIC  9(02)  VALUE  46.

       LINKAGE         SECTION.

       01  PRM-OPR              PIC  X(10).

       SCREEN          SECTION.

       01  TELA-01.
           03  LINE 05 COLUMN 01 PIC X(1360) FROM ALL "ш" FOR-CLR 8.
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 07 COLUMN 21 VALUE " Administra┤фo Global ".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 08 COLUMN 23 VALUE " Controle de Receitas ".
           03  BCK-CLR 05 FOR-CLR 15.
               05  LINE 09 COLUMN 25 VALUE " Emissфo de Relat╒rio ".
           03  BCK-CLR 00 FOR-CLR 08.
               05  LINE 07 COLUMN 43 VALUE "ъ".
               05  LINE 08 COLUMN 45 VALUE "ъ".
               05  LINE 09 COLUMN 47 VALUE "ъ".
               05  LINE 08 COLUMN 22 VALUE "э".
               05  LINE 09 COLUMN 24 VALUE "э".
           03  BCK-CLR 07 FOR-CLR 14.
               05  LINE 10 COLUMN 21 VALUE
                   " имм                        ммммммм╩ ".
               05  LINE 11 COLUMN 21 VALUE
                   " ╨                                   ╨ ".
               05  LINE 12 COLUMN 21 VALUE
                   " ╨                                   ╨ ".
               05  LINE 13 COLUMN 21 VALUE
                   " ╨                                   ╨ ".
               05  LINE 14 COLUMN 21 VALUE
                   " ╨                                   ╨ ".
               05  LINE 15 COLUMN 21 VALUE
                   " ╨                                   ╨ ".
               05  LINE 16 COLUMN 21 VALUE
                   " ╨                                   ╨ ".
               05  LINE 17 COLUMN 21 VALUE
                   " хммммммммммммммммммммммммммммммммммм╪ ".
           03  BCK-CLR 06 FOR-CLR 15.
               05  LINE 10 COLUMN 27 VALUE " An═lise  Operacional ".
           03  BCK-CLR 00.
               05  LINE 11 COLUMN 60 VALUE "  ".
               05  LINE 12 COLUMN 60 VALUE "  ".
               05  LINE 13 COLUMN 60 VALUE "  ".
               05  LINE 14 COLUMN 60 VALUE "  ".
               05  LINE 15 COLUMN 60 VALUE "  ".
               05  LINE 16 COLUMN 60 VALUE "  ".
               05  LINE 17 COLUMN 60 VALUE "  ".
               05  LINE 18 COLUMN 23 PIC X(39) FROM SPACES.

           03  BCK-CLR 04 FOR-CLR 07.
               05  LINE 11 COLUMN 25 PIC X(30) FROM ALL "ъ".
               05  LINE 13 COLUMN 25 PIC X(01) FROM     "э".
           03  BCK-CLR 04 FOR-CLR 15.
               05  LINE 12 COLUMN 25 PIC X(30) FROM
                   "       RELA─гO COMPLETA       ".
           03  BCK-CLR 04 FOR-CLR 00.
               05  LINE 12 COLUMN 55 PIC X(01) FROM     "ш".
               05  LINE 13 COLUMN 55 PIC X(01) FROM     "ш".
               05  LINE 13 COLUMN 26 PIC X(29) FROM ALL "э".

           03  BCK-CLR 01 FOR-CLR 07.
               05  LINE 14 COLUMN 25 PIC X(30) FROM ALL "ъ".
               05  LINE 16 COLUMN 25 PIC X(01) FROM     "э".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 15 COLUMN 25 PIC X(30) FROM
                   "   P═ginas Montadas д    ".
               05  PAG-T01 LINE 15 COLUMN 49 PIC ZZ9 FROM PAG-CB1.
           03  BCK-CLR 01 FOR-CLR 00.
               05  LINE 15 COLUMN 55 PIC X(01) FROM     "ш".
               05  LINE 16 COLUMN 55 PIC X(01) FROM     "ш".
               05  LINE 16 COLUMN 26 PIC X(29) FROM ALL "э".

       PROCEDURE       DIVISION  USING  PRM-OPR.

       ROT-0000-00.
           CALL    "DYSPOL" USING   PRM-OPR  PRM-SPL
           IF  LCK-SPL = 1  GOBACK. DISPLAY  TELA-01.

       ROT-ZERA-00.
           IF  IND1    =     01  AND      IND2  <  19
               MOVE  SPACES  TO  DES-T04 (IND2)
               MOVE  ZEROS   TO  SDI-T04 (IND2)
                                 ENT-T04 (IND2)
                                 SAI-T04 (IND2)
                                 COR-T04 (IND2).
           IF  IND1    =     01  AND      IND2  <  07
               MOVE  ZEROS   TO  QTD-T02 (IND2)
                                 VLD-T02 (IND2)
                                 VLP-T02 (IND2)
                                 VLR-T02 (IND2).
           IF  IND1    <     03
               MOVE  ZEROS   TO  QCP-L03 (IND1, IND2)
                                 QVD-L03 (IND1, IND2)
                                 QES-L03 (IND1, IND2)
                                 SDA-L03 (IND1, IND2)
                                 VCP-L03 (IND1, IND2)
                                 VVD-L03 (IND1, IND2)
                                 VES-L03 (IND1, IND2)
                                 VLA-L03 (IND1, IND2).
           IF  IND1    <     05
               MOVE  SPACES  TO  DES-L01 (IND1, IND2)
               MOVE  ZEROS   TO  QTD-L01 (IND1, IND2)
                                 CST-L01 (IND1, IND2)
                                 VDA-L01 (IND1, IND2).
           ADD      1        TO  IND2
           IF  IND2    <     36  GO       ROT-ZERA-00.
           IF  IND1    <     03
               MOVE  ZEROS   TO  QCP-T03 (IND1)  VCP-T03 (IND1)
                                 QVD-T03 (IND1)  VVD-T03 (IND1)
                                 QES-T03 (IND1)  VES-T03 (IND1)
                                 SDA-T03 (IND1)  VLA-T03 (IND1).
           MOVE      ZEROS   TO  QTD-T01 (IND1)
                                 CST-T01 (IND1)
                                 VDA-T01 (IND1).
           ADD       1       TO  IND1
           MOVE      1       TO  IND2
           IF  IND1    <     05  GO       ROT-ZERA-00.
           MOVE     "GANHO COM CAPITAL"   TO  DES-L01 (4, 27).

       ROT-LER1-00.
           OPEN     INPUT   CADCTR  CADPRS  CADSDO
                            CADBCO  CADLBC  CADLDP
           READ     CADCTR
           ACCEPT   DTA-SYS  FROM   DATE
           ACCEPT   ARQ-IMP  FROM   TIME
           MOVE     DTI-CTR   TO    DTA-LIM
           MOVE     31        TO    DIA-LIM
           MOVE     ZEROS     TO    CHV-BCO
           MOVE     SPACES    TO    CHV-LDP
                                    CHV-PRS  CHV-SDO.
       ROT-LER1-10.
           START    CADSDO       KEY  >   CHV-SDO
           IF  FST-SDO   =  "23" GO       ROT-LER1-20.
           READ     CADSDO  NEXT
           IF  PRX-SDO   >    1  OR   SUF-SDO  >  "6"
                                 GO       ROT-LER1-20.
           MOVE     SUF-SDO  TO  IND1
           MOVE     SDI-SDO  TO  SDI-T04 (IND1)
           MOVE     ENT-SDO  TO  ENT-T04 (IND1)
           MOVE     SAI-SDO  TO  SAI-T04 (IND1)
           MOVE     COR-SDO  TO  COR-T04 (IND1)
           GO       ROT-LER1-10.

       ROT-LER1-20.
           START    CADPRS       KEY  >   CHV-PRS
           IF  FST-PRS   =  "23" GO       ROT-LER1-40.
           READ     CADPRS  NEXT
           IF  ATV-PRS   >    5  GO       ROT-LER1-20.
           IF  ATV-PRS   =    4  AND  PRX-PRS  <  "S"
                                 GO       ROT-LER1-20.
           SET      I   TO    1
           SEARCH   IND-TAB      WHEN     PRX-PRS  =
                                          IND-TAB (I)
           SET      IND2     TO  I
           IF  ATV-PRS   =    5  GO       ROT-LER1-30.
           MOVE     ATV-PRS  TO  IND1
           MOVE     DES-PRS  TO  DES-L01 (IND1, IND2)
           ADD      QTM-PRS  TO  QTD-T01 (IND1)
                                 QTD-L01 (IND1, IND2)
           ADD      CTM-PRS  TO  CST-T01 (IND1)
                                 CST-L01 (IND1, IND2)
           ADD      VDM-PRS  TO  VDA-T01 (IND1)
                                 VDA-L01 (IND1, IND2)
           GO       ROT-LER1-20.

       ROT-LER1-30.
           IF  PRX-PRS   >  "I"  GO      ROT-LER1-20.
           MOVE     DES-PRS  TO  DES-T04 (IND2)
           IF  PRX-PRS   <  "A"  GO      ROT-LER1-20.
           MOVE     1        TO  PRX-SDO
           MOVE     PRX-PRS  TO  SUF-SDO
           READ     CADSDO
           MOVE     SDI-SDO  TO  SDI-T04 (IND2)
           MOVE     ENT-SDO  TO  ENT-T04 (IND2)
           MOVE     SAI-SDO  TO  SAI-T04 (IND2)
           MOVE     COR-SDO  TO  COR-T04 (IND2)
           GO       ROT-LER1-20.

       ROT-LER1-40.
           START    CADLDP       KEY  >   CHV-LDP
           IF  FST-LDP   =  "23" GO       ROT-LER1-50.
           READ     CADLDP  NEXT
           IF  FLG-LDP    =  "E" GO       ROT-LER1-40.
           IF  FLG-LDP    =  "P"
               ADD  VAL-LDP  TO  VLP-T02 (PRX-LDP)
                                 GO       ROT-LER1-40.
           IF  PRX-LDP  >  5 OR  DTA-LDP  >   DTA-LIM
                                 GO       ROT-LER1-40.
           ADD      1        TO  QTD-T02 (PRX-LDP)
           EVALUATE FLG-LDP
               WHEN   "L"
                                 ADD      VAL-LDP  TO  VLR-T02 (5)
                                 ADD      VAL-LDP  TO  VLD-T02 (PRX-LDP)
               WHEN   "R"
                                 SUBTRACT VAL-LDP FROM VLR-T02 (5)
                                 SUBTRACT VAL-LDP FROM VLD-T02 (PRX-LDP)
           END-EVALUATE
           GO       ROT-LER1-40.

       ROT-LER1-50.
           START    CADBCO       KEY  >   CHV-BCO
           IF  FST-BCO   =  "23" GO       ROT-LER1-70.
           READ     CADBCO  NEXT
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           ADD      SDO-BCO  TO  ACM-SDI.

       ROT-LER1-60.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-LER1-50.
           READ     CADLBC  NEXT
           IF  BCO-LBC  NOT   =  CHV-BCO  OR  DTA-LBC
               >    DTA-LIM      GO       ROT-LER1-50.
           IF  COD-LBC   =    3  ADD      1        TO  QTD-T01 (4)
                                                       QTD-L01 (4, 27)
                                 ADD      VAL-LBC  TO  VDA-T01 (4)
                                                       VDA-L01 (4, 27).
           IF  COD-LBC    =   4  ADD      1        TO  QTD-T02 (6)
                                 ADD      VAL-LBC  TO  VLD-T02 (6)
                                                       VLR-T02 (5).
           IF  COD-LBC   <    4  ADD      VAL-LBC  TO  ACM-ENT.
           IF  COD-LBC   >    3  ADD      VAL-LBC  TO  ACM-SAI.
           IF  COD-LBC   >    0  AND  <   6
                                 GO       ROT-LER1-60.
           START    CADLBC       KEY  >   CHV-LBC
           IF  FST-LBC   =  "23" GO       ROT-LER1-50.
           READ     CADLBC  NEXT
           IF  COD-LBC   =    0  COMPUTE  ACM-COR   =
                                          ACM-COR   +  VAL-LBC
           ELSE
                                 COMPUTE  ACM-COR   =
                                          ACM-COR   -  VAL-LBC.
           GO       ROT-LER1-60.

       ROT-LER1-70.
           CLOSE    CADBCO       CADLBC
                    CADLDP       CADPRS
           MOVE     1        TO  PAG-CB1
           DISPLAY  PAG-T01
           OPEN     INPUT        CADEST
                    CADLCP       CADLES
           MOVE     SPACES   TO  CHV-EST
                                 CHV-LCP  CHV-LES.
       ROT-LER2-00.
           START    CADEST       KEY  >   CHV-EST
           IF  FST-EST   =  "23" GO       ROT-LER2-30.
           READ     CADEST  NEXT
           IF  PRX-EST   >    1  GO       ROT-LER2-10.
           MOVE     1        TO  IND1
           MOVE     SUF-EST  TO  IND2
           PERFORM  ROT-LER2-20  GO       ROT-LER2-00.

       ROT-LER2-10.
           SET      I   TO    1
           SEARCH   IND-TAB      WHEN     PRX-EST  =
                                          IND-TAB (I)
           SET      IND2     TO  I
           MOVE     2        TO  IND1
           PERFORM  ROT-LER2-20  GO       ROT-LER2-00.

       ROT-LER2-20.
           ADD      SDO-EST  TO  SDA-T03 (IND1)
                                 SDA-L03 (IND1, IND2)
           ADD      QVM-EST  TO  QVD-T03 (IND1)
                                 QVD-L03 (IND1, IND2)
           COMPUTE  VAL-AUX   =  SDO-EST  *   CST-EST
           ADD      VAL-AUX  TO  VLA-T03 (IND1)
                                 VLA-L03 (IND1, IND2)
           COMPUTE  VAL-AUX   =  QVM-EST  *   CST-EST
           ADD      VAL-AUX  TO  VVD-T03 (IND1)
                                 VVD-L03 (IND1, IND2).
       ROT-LER2-30.
           START    CADLCP       KEY  >   CHV-LCP
           IF  FST-LCP   =  "23" GO       ROT-LER2-60.
           READ     CADLCP  NEXT
           IF  DTA-LCP   >       DTD-CTR
                                 ADD   1  TO  SUF-LCP
                                 MOVE  0  TO  DTA-LCP
                                 GO       ROT-LER2-30.
           MOVE     PRX-LCP  TO  PRX-EST
           MOVE     SUF-LCP  TO  SUF-EST
           READ     CADEST
           IF  PRX-LCP   >    1  GO       ROT-LER2-40.
           MOVE     1        TO  IND1
           MOVE     SUF-LCP  TO  IND2
           PERFORM  ROT-LER2-50  GO       ROT-LER2-30.

       ROT-LER2-40.
           SET      I   TO    1
           SEARCH   IND-TAB      WHEN     PRX-LCP  =
                                          IND-TAB (I)
           SET      IND2     TO  I
           MOVE     2        TO  IND1
           PERFORM  ROT-LER2-50  GO       ROT-LER2-30.

       ROT-LER2-50.
           COMPUTE  VAL-AUX   =  QTD-LCP  *   CST-EST
           ADD      QTD-LCP  TO  QCP-T03 (IND1)
                                 QCP-L03 (IND1, IND2)
           ADD      VAL-AUX  TO  VCP-T03 (IND1)
                                 VCP-L03 (IND1, IND2).
       ROT-LER2-60.
           START    CADLES       KEY  >   CHV-LES
           IF  FST-LES   =  "23" GO       ROT-LER2-90.
           READ     CADLES  NEXT
           MOVE     PRX-LES  TO  PRX-EST
           MOVE     SUF-LES  TO  SUF-EST
           READ     CADEST
           IF  PRX-LES   >    1  GO       ROT-LER2-70.
           MOVE     1        TO  IND1
           MOVE     SUF-LES  TO  IND2
           PERFORM  ROT-LER2-80  GO       ROT-LER2-60.

       ROT-LER2-70.
           SET      I   TO    1
           SEARCH   IND-TAB      WHEN     PRX-LES  =
                                          IND-TAB (I)
           SET      IND2     TO  I
           MOVE     2        TO  IND1
           PERFORM  ROT-LER2-80  GO       ROT-LER2-60.

       ROT-LER2-80.
           COMPUTE  QTD-AUX   =  SDC-LES  -   SDA-LES
           COMPUTE  VAL-AUX   =  QTD-AUX  *   CST-EST
           ADD      QTD-AUX  TO  QES-T03 (IND1)
                                 QES-L03 (IND1, IND2)
           ADD      VAL-AUX  TO  VES-T03 (IND1)
                                 VES-L03 (IND1, IND2).
       ROT-LER2-90.
           CLOSE    CADCTR       CADEST
                    CADLCP       CADLES
           OPEN     OUTPUT       CADTMP
           MOVE     DIA-SYS  TO  DIA-CAB
           MOVE     MES-SYS  TO  MES-CAB
           MOVE     ANO-SYS  TO  ANO-CAB
           CALL    "DYNOME"      USING    PRM-NOM
                                          PRM-CAB
           MOVE     CLI-NOM  TO  CLI-CB1
           MOVE     DTI-CTR  TO  DTA-SYS
           MOVE     DIA-SYS  TO  DDI-CB2
           MOVE     MES-SYS  TO  MMI-CB2
           MOVE     ANO-SYS  TO  AAI-CB2
           MOVE     DTD-CTR  TO  DTA-SYS
           MOVE     DIA-SYS  TO  DDF-CB2
           MOVE     MES-SYS  TO  MMF-CB2
           MOVE     ANO-SYS  TO  AAF-CB2
           MOVE     PRM-CAB  TO  DTA-CB2.

       ROT-IMP1-00.
           MOVE     1        TO  CHV-TMP  PAG-TMP  LIN-TMP
           MOVE     CAB-001  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-002  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-003  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-004  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-005  TO  DET-TMP  WRITE    REG-TMP
           MOVE     1        TO  IND1     IND2.

       ROT-IMP1-10.
           MOVE     DES-L01 (IND1, IND2)  TO  DES-DT1
           MOVE     QTD-L01 (IND1, IND2)  TO  QTD-DT1
           MOVE     CST-L01 (IND1, IND2)  TO  CST-DT1
           MOVE     VDA-L01 (IND1, IND2)  TO  VDA-DT1
           COMPUTE  LUC-DT1   =  VDA-L01 (IND1, IND2)
                              -  CST-L01 (IND1, IND2)
           COMPUTE  PCT-DT1   = (VDA-L01 (IND1, IND2)
                              -  CST-L01 (IND1, IND2))
                    *   100   /  VDA-L01 (IND1, IND2)
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP      ADD   1  TO    IND2
           IF  IND2  <  36  AND  DES-L01 (IND1, IND2)
               NOT   =   SPACES  GO       ROT-IMP1-10.
           MOVE     DES-T01 (IND1)   TO   DES-DT1
           MOVE     QTD-T01 (IND1)   TO   QTD-DT1
           MOVE     CST-T01 (IND1)   TO   CST-DT1
           MOVE     VDA-T01 (IND1)   TO   VDA-DT1
           COMPUTE  LUC-DT1   =  VDA-T01  (IND1)
                              -  CST-T01  (IND1)
           COMPUTE  PCT-DT1   = (VDA-T01  (IND1)
                              -  CST-T01  (IND1))
                    *   100   /  VDA-T01  (IND1)
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-005  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-005  TO  DET-TMP  WRITE    REG-TMP.

       ROT-IMP1-20.
           IF       IND1  >   3  MOVE     1  TO  IND1
                                 GO       ROT-IMP1-40.
           ADD      1        TO  IND1
           MOVE     1        TO  IND2.

       ROT-IMP1-30.
           IF  DES-L01 (IND1, IND2)  NOT   =   SPACES
                                     GO   ROT-IMP1-10.
           IF  IND2     >    35      GO   ROT-IMP1-20.
           ADD    1     TO  IND2     GO   ROT-IMP1-30.

       ROT-IMP1-40.
           COMPUTE  TOT-CST  =   TOT-CST   +  CST-T01 (IND1)
           COMPUTE  TOT-VDA  =   TOT-VDA   +  VDA-T01 (IND1)
           COMPUTE  VLR-T02      (IND1)    =  VDA-T01 (IND1)
                                           -  CST-T01 (IND1)
           ADD      1        TO  IND1
           IF  IND1     <    05  GO       ROT-IMP1-40.
           COMPUTE  TOT-LUC  =   TOT-VDA   -  TOT-CST
           COMPUTE  VLR-T02      (6)  =   TOT-LUC
                                      -   VLR-T02 (5)
           MOVE     "TOTAL GERAL"         TO  DES-DT1
           MOVE     ZEROS    TO  QTD-DT1
           MOVE     TOT-CST  TO  CST-DT1
           MOVE     TOT-VDA  TO  VDA-DT1
           MOVE     TOT-LUC  TO  LUC-DT1
           COMPUTE  PCT-DT1  =  (TOT-VDA     -     TOT-CST)
                                 *   100     /     TOT-VDA
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-006  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-007  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-008  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-009  TO  DET-TMP  WRITE    REG-TMP
           MOVE      1       TO  IND1.

       ROT-IMP1-50.
           MOVE     DSD-T02 (IND1)   TO   DSD-DT2
           MOVE     QTD-T02 (IND1)   TO   QTD-DT2
           MOVE     VLD-T02 (IND1)   TO   VLD-DT2
           COMPUTE  PCD-DT2     =         100   *
                    VLD-T02 (IND1)   /    TOT-LUC
           MOVE     DSR-T02 (IND1)   TO   DSR-DT2
           MOVE     VLR-T02 (IND1)   TO   VLR-DT2
           COMPUTE  PCR-DT2     =         100   *
                    VLR-T02 (IND1)   /    TOT-LUC
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-002  TO  DET-TMP
           WRITE    REG-TMP      ADD   1  TO  IND1
           IF  IND1    <     07  GO       ROT-IMP1-50.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-010  TO  DET-TMP  WRITE    REG-TMP.

       ROT-IMP2-00.
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-011  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP  WRITE    REG-TMP
           MOVE     1        TO  LIN-TMP
           ADD      1        TO  PAG-CB1  CHV-TMP
           MOVE     PAG-CB1  TO  PAG-TMP
           MOVE     CAB-001  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-002  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-012  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-013  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-014  TO  DET-TMP  WRITE    REG-TMP
           MOVE     1        TO  IND1     IND2.

       ROT-IMP2-10.
           MOVE     DES-L01 (IND1, IND2)  TO  DES-DT3
           MOVE     QCP-L03 (IND1, IND2)  TO  QCP-DT3
           MOVE     QVD-L03 (IND1, IND2)  TO  QVD-DT3
           MOVE     QES-L03 (IND1, IND2)  TO  QES-DT3
           MOVE     SDA-L03 (IND1, IND2)  TO  SDA-DT3
           COMPUTE  QTD-AUX   =  SDA-L03 (IND1, IND2)
                              -  QCP-L03 (IND1, IND2)
                              +  QVD-L03 (IND1, IND2)
                              -  QES-L03 (IND1, IND2)
           MOVE     QTD-AUX  TO  SDI-DT3
           COMPUTE  DIF-DT3   =  SDA-L03 (IND1, IND2)
                              -  QTD-AUX
           COMPUTE  VAL-AUX   =  VLA-L03 (IND1, IND2)
                              -  VCP-L03 (IND1, IND2)
                              +  VVD-L03 (IND1, IND2)
                              -  VES-L03 (IND1, IND2)
           COMPUTE  VAL-DT3   =  VLA-L03 (IND1, IND2)
                              -  VAL-AUX
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-003  TO  DET-TMP
           WRITE    REG-TMP      ADD      1  TO  IND2
           IF  IND2  <  36  AND  DES-L01 (IND1, IND2)
               NOT   =   SPACES  GO       ROT-IMP2-10.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-014  TO  DET-TMP  WRITE    REG-TMP
           MOVE     DES-T01 (IND1)   TO   DES-DT3
           MOVE     QCP-T03 (IND1)   TO   QCP-DT3
           MOVE     QVD-T03 (IND1)   TO   QVD-DT3
           MOVE     QES-T03 (IND1)   TO   QES-DT3
           MOVE     SDA-T03 (IND1)   TO   SDA-DT3
           COMPUTE  QTD-AUX   =  SDA-T03 (IND1) -  QCP-T03 (IND1)
                              +  QVD-T03 (IND1) -  QES-T03 (IND1)
           MOVE     QTD-AUX  TO  SDI-DT3
           COMPUTE  DIF-DT3   =  SDA-T03 (IND1) -  QTD-AUX
           COMPUTE  VAL-AUX   =  VLA-T03 (IND1) -  VCP-T03 (IND1)
                              +  VVD-T03 (IND1) -  VES-T03 (IND1)
           COMPUTE  VAL-DT3   =  VLA-T03 (IND1) -  VAL-AUX
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-003  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-014  TO  DET-TMP
           WRITE    REG-TMP      MOVE  2  TO  IND2
           IF  IND1      <   02  ADD   1  TO  IND1
                                 GO       ROT-IMP2-10.
           MOVE     "TOTAL DO ESTOQUE"    TO  DES-DT3
           COMPUTE  QCP-DT3   =  QCP-T03  (1)  +   QCP-T03 (2)
           COMPUTE  QVD-DT3   =  QVD-T03  (1)  +   QVD-T03 (2)
           COMPUTE  QES-DT3   =  QES-T03  (1)  +   QES-T03 (2)
           COMPUTE  SDA-DT3   =  SDA-T03  (1)  +   SDA-T03 (2)
           COMPUTE  QTD-AUX   = (SDA-T03  (1)  +   SDA-T03 (2))
                              - (QCP-T03  (1)  +   QCP-T03 (2))
                              + (QVD-T03  (1)  +   QVD-T03 (2))
                              - (QES-T03  (1)  +   QES-T03 (2))
           MOVE     QTD-AUX  TO  SDI-DT3
           COMPUTE  DIF-DT3   =  SDA-T03  (1)
                              +  SDA-T03  (2)  -   QTD-AUX
           COMPUTE  VAL-AUX   = (VLA-T03  (1)  +   VLA-T03 (2))
                              - (VCP-T03  (1)  +   VCP-T03 (2))
                              + (VVD-T03  (1)  +   VVD-T03 (2))
                              - (VES-T03  (1)  +   VES-T03 (2))
           COMPUTE  TOT-GER   =  VLA-T03  (1)
                              +  VLA-T03  (2)  -   VAL-AUX
           MOVE     TOT-GER  TO  VAL-DT3
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-003  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-015  TO  DET-TMP  WRITE    REG-TMP.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-016  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-017  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-018  TO  DET-TMP  WRITE    REG-TMP
           MOVE     0        TO  IND2.

       ROT-IMP2-20.
           ADD      1        TO  IND2
           IF  IND2    >     18  GO       ROT-IMP2-30.
           IF  DES-T04 (IND2)    =        SPACES
                                 GO       ROT-IMP2-20.
           MOVE     DES-T04 (IND2)   TO   DES-DT4
           MOVE     SDI-T04 (IND2)   TO   SDI-DT4
           MOVE     ENT-T04 (IND2)   TO   ENT-DT4
           MOVE     SAI-T04 (IND2)   TO   SAI-DT4
           MOVE     COR-T04 (IND2)   TO   COR-DT4
           COMPUTE  SDA-DT4   =  SDI-T04  (IND2)  +  ENT-T04 (IND2)
                              -  SAI-T04  (IND2)  +  COR-T04 (IND2)
           COMPUTE  VAR-DT4   =  SDI-T04  (IND2)
                              +  ENT-T04  (IND2)  -  SAI-T04 (IND2)
                              +  COR-T04  (IND2)  -  SDI-T04 (IND2)
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-004  TO  DET-TMP  WRITE      REG-TMP
           ADD      SDI-T04 (IND2)   TO   TOT-SDI
           ADD      ENT-T04 (IND2)   TO   TOT-ENT
           ADD      SAI-T04 (IND2)   TO   TOT-SAI
           ADD      COR-T04 (IND2)   TO   TOT-COR
           GO       ROT-IMP2-20.

       ROT-IMP2-30.
           MOVE     "TOTAL A RECEBER"     TO  DES-DT4
           MOVE     TOT-SDI  TO  SDI-DT4
           MOVE     TOT-ENT  TO  ENT-DT4
           MOVE     TOT-SAI  TO  SAI-DT4
           MOVE     TOT-COR  TO  COR-DT4
           COMPUTE  SDA-DT4   =  TOT-SDI  +   TOT-ENT
                              -  TOT-SAI  +   TOT-COR
           COMPUTE  VAR-DT4   =  TOT-SDI  +   TOT-ENT
                              -  TOT-SAI  +   TOT-COR  -  TOT-SDI
           COMPUTE  TOT-GER   =  TOT-GER  +   TOT-SDI  +  TOT-ENT
                              -  TOT-SAI  +   TOT-COR  -  TOT-SDI
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-018  TO  DET-TMP  WRITE      REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-004  TO  DET-TMP  WRITE      REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-018  TO  DET-TMP  WRITE      REG-TMP
           MOVE     "COMPRA DE PRODUTOS"  TO  DSD-T02  (6)
           MOVE     2        TO  PRX-SDO
           MOVE     ZEROS    TO  TOT-SDI  TOT-ENT  TOT-SAI
                                 TOT-COR  TOT-SDA  SUF-SDO.
       ROT-IMP2-40.
           START    CADSDO       KEY  >   CHV-SDO
           IF  FST-SDO   =  "23" GO       ROT-IMP2-50.
           READ     CADSDO  NEXT
           MOVE     SUF-SDO  TO  IND2
           MOVE     DSD-T02     (IND2)    TO  DES-DT4
           MOVE     VLP-T02     (IND2)    TO  SDA-DT4
           MOVE     SDI-SDO  TO  SDI-DT4
           MOVE     ENT-SDO  TO  ENT-DT4
           MOVE     SAI-SDO  TO  SAI-DT4

           COMPUTE  COR-DT4   =  VLP-T02 (IND2)
                 - (SDI-SDO   +  ENT-SDO  -   SAI-SDO)
           COMPUTE  VAR-DT4   =  VLP-T02 (IND2)
                                          -   SDI-SDO
           ADD      1        TO  CHV-TMP      LIN-TMP
           MOVE     DET-004  TO  DET-TMP
           ADD      SDI-SDO  TO  TOT-SDI
           ADD      ENT-SDO  TO  TOT-ENT
           ADD      SAI-SDO  TO  TOT-SAI
           ADD      VLP-T02      (IND2)   TO  TOT-SDA
           WRITE    REG-TMP      GO       ROT-IMP2-40.

       ROT-IMP2-50.
           MOVE     "TOTAL A PAGAR  "     TO  DES-DT4
           MOVE     TOT-SDI  TO  SDI-DT4
           MOVE     TOT-ENT  TO  ENT-DT4
           MOVE     TOT-SAI  TO  SAI-DT4
           MOVE     TOT-SDA  TO  SDA-DT4
           COMPUTE  COR-DT4   =  TOT-SDA  -  (TOT-SDI
                              +  TOT-ENT  -   TOT-SAI)
           COMPUTE  VAR-DT4   =  TOT-SDA  -   TOT-SDI
           COMPUTE  TOT-GER   =  TOT-GER
                              - (TOT-SDA  -   TOT-SDI)
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-018  TO  DET-TMP  WRITE      REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-004  TO  DET-TMP  WRITE      REG-TMP
           MOVE     "SALDOS BANCARIOS"    TO  DES-DT4
           MOVE     ACM-SDI  TO  SDI-DT4
           MOVE     ACM-ENT  TO  ENT-DT4
           MOVE     ACM-SAI  TO  SAI-DT4
           MOVE     ACM-COR  TO  COR-DT4
           COMPUTE  SDA-DT4   =  ACM-SDI  +   ACM-ENT
                              -  ACM-SAI  +   ACM-COR
           COMPUTE  VAR-DT4   =  ACM-SDI  +   ACM-ENT
                              -  ACM-SAI  +   ACM-COR  -  ACM-SDI
           COMPUTE  TOT-GER   =  TOT-GER  +   ACM-SDI  +  ACM-ENT
                              -  ACM-SAI  +   ACM-COR  -  ACM-SDI
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-018  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-004  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-019  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP  WRITE    REG-TMP
           MOVE     TOT-GER  TO  VAL-DT5
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-020  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-005  TO  DET-TMP  WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-021  TO  DET-TMP  WRITE    REG-TMP
           CLOSE    CADSDO       CADTMP   DISPLAY  PAG-T01
           MOVE     PAG-CB1  TO  PAG-IMP
           CALL    "DYIMPR"      USING    PRM-IMP
           DELETE   FILE         CADTMP   GOBACK.
