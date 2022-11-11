       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYRLMC.
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

           SELECT      CADCTR        ASSIGN  TO  DISK
                       FILE          STATUS  IS  FST-CTR.

           SELECT      CADCXA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CXA
                       FILE          STATUS  IS  FST-CXA.

           SELECT      CADECR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ECR
                       FILE          STATUS  IS  FST-ECR.

           SELECT      CADLCP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCP
                       FILE          STATUS  IS  FST-LCP.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS
                       FILE          STATUS  IS  FST-PRS.

           SELECT      CADTNQ        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TNQ
                       FILE          STATUS  IS  FST-TNQ.

           SELECT      CADTMP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMP
                       FILE          STATUS  IS  FST-TMP.

       DATA            DIVISION.
       FILE            SECTION.

       COPY  CADCMB.CPY.
       COPY  CADCTR.CPY.
       COPY  CADCXA.CPY.
       COPY  CADECR.CPY.
       COPY  CADLCP.CPY.
       COPY  CADPRS.CPY.
       COPY  CADTNQ.CPY.
       COPY  CADTMP.CPY.

       WORKING-STORAGE SECTION.

       01  AUXLIARES.
           03  FST-CMB          PIC  X(02).
           03  FST-CTR          PIC  X(02).
           03  FST-CXA          PIC  X(02).
           03  FST-ECR          PIC  X(02).
           03  FST-LCP          PIC  X(02).
           03  FST-PRS          PIC  X(02).
           03  FST-TNQ          PIC  X(02).
           03  FST-TMP          PIC  X(02).

           03  IND1             PIC  9(02).
           03  ACM-PAG          PIC  9(03).
           03  ACM-SDI          PIC  9(06)V9.
           03  ACM-SDF          PIC  9(06)V9.
           03  ACM-CMP          PIC S9(06)V9.
           03  ACM-VDA          PIC  9(06)V9.
           03  ACM-VDD          PIC  9(07)V99.
           03  ACM-VDM          PIC  9(08)V99.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  QTD-AUX          PIC  ZZ.ZZ9,9.
           03  TNQ-AUX.
               05  FILLER       PIC  X(06)  VALUE  "TANQ.".
               05  NUM-AUX      PIC  9(02).

           03  TAB-TNQ          OCCURS  10.
               05  TNQ-TAB      PIC  X(02).
               05  DES-TAB      PIC  X(08).
               05  SDI-TAB      PIC  X(08).
               05  SDF-TAB      PIC  X(08).

       01  CAB-001.
           03  FILLER           PIC  X(75)  VALUE  "-DYNAMIC-
      -        "LIVRO DE MOVIMENTACAO DOS COMBUSTIVEIS - LMC".
           03  CLI-C01          PIC  X(49).
           03  FILLER           PIC  X(10)  VALUE  "PAGINA .:".
           03  PAG-C01          PIC  9(03).

       01  CAB-002.
           03  FILLER           PIC  X(69)  VALUE  "ÚÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".

       01  CAB-003.
           03  FILLER           PIC  X(19)  VALUE  "³ 1.0 ³  PRODUTO :".
           03  CMB-C03          PIC  X(91).
           03  FILLER           PIC  X(16)  VALUE  "³ 2.0 ³  DATA :".
           03  DIA-C03          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-C03          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-C03          PIC  9(02).
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  CAB-004.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-005.
           03  FILLER           PIC X(136)  VALUE  "³ 3.0 ³  ESTOQUE DE
      -        "ABERTURA  ( MEDICAO FISICA NO INICIO DO DIA )".
           03  FILLER           PIC  X(01)  VALUE  "³".

       01  CAB-006.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÁÄÄÄÄÂÄÄÄÄÄÄÄÄ
      -        "ÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂ
      -        "ÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-007.
           03  FILLER           PIC  X(02)  VALUE  "³".
           03  T01-C07          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T02-C07          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T03-C07          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T04-C07          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T05-C07          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T06-C07          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T07-C07          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T08-C07          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T09-C07          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T10-C07          PIC  X(08).
           03  FILLER           PIC  X(28)  VALUE
               " ³ 3.1 ³  ESTOQUE INICIAL  ³".

       01  CAB-008.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ
      -        "ÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅ
      -        "ÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-009.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÂÄÄÄÄÁÄÄÄÄÄÄÄÄ
      -        "ÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÁÄÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁ
      -        "ÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-010.
           03  FILLER           PIC  X(69)  VALUE  "³ 4.0 ³  VOLUME RECE
      -        "BIDO NO DIA  ( EM LITROS )         ³ 4.1 ³  TANQU".
           03  FILLER           PIC  X(68)  VALUE  "ES DE DESCARGA
      -        "                     ³ 4.2 ³  VOLUME RECEBIDO  ³".

       01  CAB-011.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÁÄÄÄÄÄÄÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-012.
           03  FILLER           PIC  X(69)  VALUE  "³  NOTA FISCAL NUMER
      -        "O ..:             DE               ³      -    - ".
           03  FILLER           PIC  X(68)  VALUE  "   -    -    -    -
      -        "   -    -    -       ³                    0,0  ³".

       01  CAB-013.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÄÄÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-014.
           03  FILLER           PIC  X(55)  VALUE  "³".
           03  FILLER           PIC  X(69)  VALUE  "³ 4.3 ³  VOLUME TOTA
      -        "L RECEBIDO                         ³".
           03  TOT-C14          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  CAB-015.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-016.
           03  FILLER           PIC  X(55)  VALUE  "³ 5.0 ³  VOLUME VEND
      -        "IDO NO DIA  ( EM LITROS )".
           03  FILLER           PIC  X(69)  VALUE  "³ 4.4 ³  VOLUME DISP
      -        "ONIVEL  ( 3.1  +  4.3 )            ³".
           03  TOT-C16          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  CAB-017.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÂÄ
      -        "ÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÅÄÄÄÄÄÂÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂ
      -        "ÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-018.
           03  FILLER           PIC  X(69)  VALUE  "³ 5.1 ³  TANQUES  ³
      -        "5.2 ³  BICOS  ³ 5.3 ³  FECHAMENTO ( + )  ³ 5.4 ³ ".
           03  FILLER           PIC  X(68)  VALUE  "  ABERTURA ( - )   ³
      -        " 5.5 ³ AFERICOES (-) ³ 5.6 ³ VENDAS / BICO (=) ³".

       01  CAB-019.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÅÄ
      -        "ÄÄÄÄÁÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÁÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅ
      -        "ÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-020.
           03  FILLER           PIC  X(69)  VALUE  "³                 ³
      -        "              ³                          ³       ".
           03  FILLER           PIC  X(68)  VALUE  "                   ³
      -        "                     ³                         ³".

       01  CAB-021.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÁÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÂÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-022.
           03  FILLER           PIC  X(69)  VALUE  "³ 10.0 ³  VALOR DAS
      -        "VENDAS  ( EM R$ )                        ³ 5.7 ³ ".
           03  FILLER           PIC  X(55)  VALUE
               " VENDAS NO DIA  ( EM LITROS )            ³".
           03  TOT-C22          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  CAB-023.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-024.
           03  FILLER           PIC  X(47)  VALUE
               "³ 10.1 ³  DIA  ( 5.7  X  PRE€O )  ³".
           03  VAL-C24          PIC  Z.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(65)  VALUE
               "  ³ 6.0 ³  ESTOQUE ESCRITURAL  ( 4.4  -  5.7 )     ³".
           03  QTD-C24          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  CAB-025.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-026.
           03  FILLER           PIC  X(46)  VALUE
               "³ 10.2 ³  VALOR ACUMULADO NO MES  ³".
           03  VAL-C26          PIC  ZZ.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(65)  VALUE
               "  ³ 7.0 ³  VOLUME DE FECHAMENTO  ( 9.1 )           ³".
           03  QTD-C26          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  CAB-027.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-028.
           03  FILLER           PIC  X(69)  VALUE  "³ 11.0 ³  DESTINADO
      -        "A FISCALIZACAO                           ³ 8.0 ³ ".
           03  FILLER           PIC  X(55)  VALUE  " PERDAS OU SOBRAS  (
      -        " 7  -  6 )           ³".
           03  QTD-C28          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  CAB-029.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÁÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-030.
           03  FILLER           PIC  X(61)  VALUE  "³ A.N.P.".
           03  FILLER           PIC  X(75)  VALUE  "³ OBSERVACOES".
           03  FILLER           PIC  X(01)  VALUE  "³".

       01  CAB-031.
           03  FILLER           PIC  X(61)  VALUE  "³".
           03  FILLER           PIC  X(75)  VALUE  "³".
           03  FILLER           PIC  X(01)  VALUE  "³".

       01  CAB-032.
           03  FILLER           PIC  X(01)  VALUE  "Ã".
           03  FILLER           PIC  X(60)  VALUE  ALL  "Ä".
           03  FILLER           PIC  X(75)  VALUE  "´".
           03  FILLER           PIC  X(01)  VALUE  "³".

       01  CAB-033.
           03  FILLER           PIC  X(61)  VALUE
               "³ OUTROS ORGAOS FISCAIS".
           03  FILLER           PIC  X(75)  VALUE  "³".
           03  FILLER           PIC  X(01)  VALUE  "³".

       01  CAB-034.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-035.
           03  FILLER           PIC X(136)  VALUE  "³ 9.0 ³  ESTOQUE DE
      -        "FECHAMENTO  ( MEDICAO FISICA NO FIM DO DIA )".
           03  FILLER           PIC  X(01)  VALUE  "³".

       01  CAB-036.
           03  FILLER           PIC  X(02)  VALUE  "³".
           03  T01-C36          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T02-C36          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T03-C36          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T04-C36          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T05-C36          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T06-C36          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T07-C36          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T08-C36          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T09-C36          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T10-C36          PIC  X(08).
           03  FILLER           PIC  X(28)  VALUE
               " ³ 9.1 ³   ESTOQUE FINAL   ³".

       01  CAB-037.
           03  FILLER           PIC  X(69)  VALUE  "ÃÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄ
      -        "ÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄ".
           03  FILLER           PIC  X(68)  VALUE  "ÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁ
      -        "ÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".

       01  CAB-038.
           03  FILLER           PIC  X(69)  VALUE  "³             ( * )
      -        " ATENCAO !!!  SE O RESULTADO FOR NEGATIVO, PODE E".
           03  FILLER           PIC  X(68)  VALUE  "STAR HAVENDO VAZAMEN
      -        "TO DE PRODUTO PARA O MEIO AMBIENTE             ³".

       01  CAB-039.
           03  FILLER           PIC  X(01)  VALUE  "À".
           03  FILLER           PIC X(135)  VALUE  ALL  "Ä".
           03  FILLER           PIC  X(01)  VALUE  "Ù".

       01  CAB-040              PIC X(137)  VALUE  ALL  "- ".

       01  DET-001.
           03  FILLER           PIC  X(02)  VALUE  "³".
           03  T01-D01          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T02-D01          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T03-D01          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T04-D01          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T05-D01          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T06-D01          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T07-D01          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T08-D01          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T09-D01          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T10-D01          PIC  X(08).
           03  FILLER           PIC  X(16)  VALUE  " ³".
           03  TOT-D01          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  DET-002.
           03  FILLER           PIC  X(28)  VALUE
               "³  NOTA FISCAL NUMERO ..:".
           03  NTF-D02          PIC  999.999.
           03  FILLER           PIC  X(08)  VALUE  "   DE".
           03  DIA-D02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-D02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-D02          PIC  9(02).
           03  FILLER           PIC  X(08)  VALUE  "    ³".
           03  T01-D02          PIC  X(02).
           03  FILLER           PIC  X(03)  VALUE  " -".
           03  T02-D02          PIC  X(02).
           03  FILLER           PIC  X(03)  VALUE  " -".
           03  T03-D02          PIC  X(02).
           03  FILLER           PIC  X(03)  VALUE  " -".
           03  T04-D02          PIC  X(02).
           03  FILLER           PIC  X(03)  VALUE  " -".
           03  T05-D02          PIC  X(02).
           03  FILLER           PIC  X(03)  VALUE  " -".
           03  T06-D02          PIC  X(02).
           03  FILLER           PIC  X(03)  VALUE  " -".
           03  T07-D02          PIC  X(02).
           03  FILLER           PIC  X(03)  VALUE  " -".
           03  T08-D02          PIC  X(02).
           03  FILLER           PIC  X(03)  VALUE  " -".
           03  T09-D02          PIC  X(02).
           03  FILLER           PIC  X(03)  VALUE  " -".
           03  T10-D02          PIC  X(02).
           03  FILLER           PIC  X(19)  VALUE  "    ³".
           03  QTD-D02          PIC  ---.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  DET-003.
           03  FILLER           PIC  X(05)  VALUE  "³".
           03  T01-D03          PIC  9(02).
           03  FILLER           PIC  X(05)  VALUE  "  /".
           03  T02-D03          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(12)  VALUE  "    ³".
           03  BMB-D03          PIC  9(02).
           03  FILLER           PIC  X(16)  VALUE  "      ³".
           03  FDL-D03          PIC  999999,9.
           03  FILLER           PIC  X(19)  VALUE  "         ³".
           03  IDL-D03          PIC  999999,9.
           03  FILLER           PIC  X(18)  VALUE  "         ³".
           03  AFR-D03          PIC  Z.ZZ9.
           03  FILLER           PIC  X(24)  VALUE  "        ³".
           03  VDA-D03          PIC  ZZ.ZZ9,9.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

       01  DET-004.
           03  FILLER           PIC  X(02)  VALUE  "³".
           03  T01-D04          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T02-D04          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T03-D04          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T04-D04          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T05-D04          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T06-D04          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T07-D04          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T08-D04          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T09-D04          PIC  X(08).
           03  FILLER           PIC  X(03)  VALUE  " ³".
           03  T10-D04          PIC  X(08).
           03  FILLER           PIC  X(16)  VALUE  " ³".
           03  TOT-D04          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(03)  VALUE  "  ³".

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
           03  LIN-IMP          PIC  9(02)  VALUE  17.
           03  CL1-IMP          PIC  9(02)  VALUE  48.
           03  CL2-IMP          PIC  9(02)  VALUE  47.

       SCREEN          SECTION.

       01  TELA-01.
           03  BCK-CLR 01 FOR-CLR 07.
               05  LINE 15 COLUMN 26 PIC X(30) FROM ALL "ß".
               05  LINE 17 COLUMN 26 PIC X(01) FROM     "Ü".
           03  BCK-CLR 01 FOR-CLR 15.
               05  LINE 16 COLUMN 26 PIC X(30) FROM
                   "   P ginas Montadas Ä        ".
               05  PAG-T01 LINE 16 COLUMN 50 PIC ZZ9 FROM ACM-PAG.
           03  BCK-CLR 00 FOR-CLR 01.
               05  LINE 16 COLUMN 56 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 56 PIC X(01) FROM     " ".
               05  LINE 17 COLUMN 27 PIC X(29) FROM ALL "ß".

       PROCEDURE       DIVISION.

       ROT-0000-00.
           OPEN     INPUT        CADCTR   CADCXA
           MOVE     ZEROS    TO  CHV-CXA  ACM-PAG
           PERFORM  ROT-GTLA-00  DISPLAY  TELA-01.

       ROT-PESQ-00.
           START    CADCXA       KEY  >   CHV-CXA
           READ     CADCXA  NEXT
           IF  FLG-CXA   =    1  GO       ROT-PESQ-00.
           MOVE     DTA-CXA  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-C03  DIA-CAB
           MOVE     MES-AUX  TO  MES-C03  MES-CAB
           MOVE     ANO-AUX  TO  ANO-C03  ANO-CAB
           READ     CADCTR       PERFORM  ROT-NOME-00
           ADD      1        TO  LMC-CTR
           MOVE     LMC-CTR  TO  PAG-C01
           CLOSE    CADCTR       CADCXA
           ACCEPT   ARQ-IMP       FROM    TIME
           OPEN     OUTPUT       CADTMP
                    INPUT        CADCMB   CADECR
                    CADLCP       CADPRS   CADTNQ
           MOVE     ZEROS    TO  CHV-CMB  CHV-TMP.

       ROT-IMPR-00.
           START    CADCMB       KEY  >   CHV-CMB
                    INVALID      KEY  GO  ROT-IMPR-65.
           READ     CADCMB  NEXT DISPLAY  PAG-T01
           ADD      1        TO  ACM-PAG  CHV-TMP
           MOVE     1        TO  LIN-TMP
           MOVE     ACM-PAG  TO  PAG-TMP
           MOVE     DES-CMB  TO  CMB-C03
           MOVE     CAB-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-002  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-003  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-004  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-005  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-006  TO  DET-TMP
           WRITE    REG-TMP      MOVE  0  TO  IND1.

       ROT-IMPR-05.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  TNQ-TAB  (IND1)
                                 DES-TAB  (IND1)
                                 SDI-TAB  (IND1)
                                 SDF-TAB  (IND1)
           IF  IND1    <     10  GO       ROT-IMPR-05.
           MOVE     ZEROS    TO  CHV-TNQ  IND1
                                 ACM-SDI  ACM-SDF
                                 ACM-CMP  ACM-VDA.
       ROT-IMPR-10.
           START    CADTNQ       KEY  >   CHV-TNQ
                    INVALID      KEY  GO  ROT-IMPR-15.
           READ     CADTNQ  NEXT
           IF  CMB-TNQ  NOT   =  CHV-CMB
                                 GO       ROT-IMPR-10.
           ADD      1        TO  IND1
           ADD      SDI-TNQ  TO  ACM-SDI
           ADD      SDF-TNQ  TO  ACM-SDF
           MOVE     CHV-TNQ  TO  NUM-AUX
                                 TNQ-TAB (IND1)
           MOVE     TNQ-AUX  TO  DES-TAB (IND1)
           MOVE     SDI-TNQ  TO  QTD-AUX
           MOVE     QTD-AUX  TO  SDI-TAB (IND1)
           MOVE     SDF-TNQ  TO  QTD-AUX
           MOVE     QTD-AUX  TO  SDF-TAB (IND1)
           GO       ROT-IMPR-10.

       ROT-IMPR-15.
           MOVE     SDI-TAB (01) TO  T01-D01
           MOVE     SDI-TAB (02) TO  T02-D01
           MOVE     SDI-TAB (03) TO  T03-D01
           MOVE     SDI-TAB (04) TO  T04-D01
           MOVE     SDI-TAB (05) TO  T05-D01
           MOVE     SDI-TAB (06) TO  T06-D01
           MOVE     SDI-TAB (07) TO  T07-D01
           MOVE     SDI-TAB (08) TO  T08-D01
           MOVE     SDI-TAB (09) TO  T09-D01
           MOVE     SDI-TAB (10) TO  T10-D01
           MOVE     TNQ-TAB (01) TO  T01-D02
           MOVE     TNQ-TAB (02) TO  T02-D02
           MOVE     TNQ-TAB (03) TO  T03-D02
           MOVE     TNQ-TAB (04) TO  T04-D02
           MOVE     TNQ-TAB (05) TO  T05-D02
           MOVE     TNQ-TAB (06) TO  T06-D02
           MOVE     TNQ-TAB (07) TO  T07-D02
           MOVE     TNQ-TAB (08) TO  T08-D02
           MOVE     TNQ-TAB (09) TO  T09-D02
           MOVE     TNQ-TAB (10) TO  T10-D02
           MOVE     SDF-TAB (01) TO  T01-D04
           MOVE     SDF-TAB (02) TO  T02-D04
           MOVE     SDF-TAB (03) TO  T03-D04
           MOVE     SDF-TAB (04) TO  T04-D04
           MOVE     SDF-TAB (05) TO  T05-D04
           MOVE     SDF-TAB (06) TO  T06-D04
           MOVE     SDF-TAB (07) TO  T07-D04
           MOVE     SDF-TAB (08) TO  T08-D04
           MOVE     SDF-TAB (09) TO  T09-D04
           MOVE     SDF-TAB (10) TO  T10-D04
           MOVE     DES-TAB (01) TO  T01-C07  T01-C36
           MOVE     DES-TAB (02) TO  T02-C07  T02-C36
           MOVE     DES-TAB (03) TO  T03-C07  T03-C36
           MOVE     DES-TAB (04) TO  T04-C07  T04-C36
           MOVE     DES-TAB (05) TO  T05-C07  T05-C36
           MOVE     DES-TAB (06) TO  T06-C07  T06-C36
           MOVE     DES-TAB (07) TO  T07-C07  T07-C36
           MOVE     DES-TAB (08) TO  T08-C07  T08-C36
           MOVE     DES-TAB (09) TO  T09-C07  T09-C36
           MOVE     DES-TAB (10) TO  T10-C07  T10-C36
           MOVE     ACM-SDI  TO  TOT-D01
           MOVE     ACM-SDF  TO  TOT-D04
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-007  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-008  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-001  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-009  TO  DET-TMP
           WRITE    REG-TMP.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-010  TO  DET-TMP
           WRITE    REG-TMP.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-011  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     SPACES   TO  CHV-LCP
           MOVE     1        TO  PRX-LCP
           MOVE     CHV-CMB  TO  SUF-LCP
           MOVE     DTA-AUX  TO  DTA-LCP.

       ROT-IMPR-20.
           START    CADLCP       KEY  >   CHV-LCP
                    INVALID      KEY  GO  ROT-IMPR-25.
           READ     CADLCP  NEXT
           IF  SUF-LCP  NOT   =  CHV-CMB  OR  PRX-LCP
               NOT  =    1   OR  DTA-LCP  >   DTA-AUX
                                 GO       ROT-IMPR-25.
           MOVE     NTF-LCP  TO  NTF-D02
           MOVE     DIA-AUX  TO  DIA-D02
           MOVE     MES-AUX  TO  MES-D02
           MOVE     ANO-AUX  TO  ANO-D02
           MOVE     QTD-LCP  TO  QTD-D02
           ADD      QTD-LCP  TO  ACM-CMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-002  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-20.

       ROT-IMPR-25.
           IF  ACM-CMP   >    0  GO       ROT-IMPR-30.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-012  TO  DET-TMP
           WRITE    REG-TMP.

       ROT-IMPR-30.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-013  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     ACM-CMP  TO  TOT-C14
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-014  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-015  TO  DET-TMP
           WRITE    REG-TMP
           COMPUTE  TOT-C16   =  ACM-SDI  +   ACM-CMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-016  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-017  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-018  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-019  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     ZEROS    TO  CHV-ECR  IND1.

       ROT-IMPR-35.
           START    CADECR       KEY  >   CHV-ECR
                    INVALID      KEY  GO  ROT-IMPR-40.
           READ     CADECR  NEXT
           IF  CMB-ECR  NOT   =  CHV-CMB
                                 GO       ROT-IMPR-35.
           ADD      1        TO  IND1
           MOVE     TQ1-ECR  TO  T01-D03
           MOVE     TQ2-ECR  TO  T02-D03
           MOVE     CHV-ECR  TO  BMB-D03
           MOVE     FCL-ECR  TO  FDL-D03
           MOVE     IDL-ECR  TO  IDL-D03
           MOVE     AFD-ECR  TO  AFR-D03
           MOVE     VDD-ECR  TO  VDA-D03
           ADD      VDD-ECR  TO  ACM-VDA
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-003  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-35.

       ROT-IMPR-40.
           IF  IND1      >   07  GO       ROT-IMPR-45.
           ADD      1        TO  IND1
                                 CHV-TMP  LIN-TMP
           MOVE     CAB-020  TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-35.

       ROT-IMPR-45.
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-021  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     ACM-VDA  TO  TOT-C22
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-022  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-023  TO  DET-TMP
           WRITE    REG-TMP
           MOVE     ZEROS    TO  CHV-PRS
                                 ACM-VDD  ACM-VDM.
       ROT-IMPR-50.
           ADD      1        TO  CXA-PRS
           MOVE     1        TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS.

       ROT-IMPR-55.
           START    CADPRS       KEY  >   CHV-PRS
                    INVALID      KEY  GO  ROT-IMPR-60.
           READ     CADPRS  NEXT
           IF  ATV-PRS  NOT   =  1    OR  PRX-PRS
               >    CHV-CMB      GO       ROT-IMPR-50.
           IF  PRX-PRS  NOT   =  CHV-CMB
                                 GO       ROT-IMPR-55.
           ADD      VDD-PRS  TO  ACM-VDD
           ADD      VDM-PRS  TO  ACM-VDM
           GO       ROT-IMPR-50.

       ROT-IMPR-60.
           MOVE     ACM-VDD  TO  VAL-C24
           COMPUTE  QTD-C24   =  ACM-SDI
                              +  ACM-CMP  -   ACM-VDA
           MOVE     ACM-VDM  TO  VAL-C26
           MOVE     ACM-SDF  TO  QTD-C26
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-024  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-025  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-026  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-027  TO  DET-TMP
           WRITE    REG-TMP
           COMPUTE  QTD-C28   =  ACM-SDF  +   ACM-VDA
                              -  ACM-CMP  -   ACM-SDI
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-028  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-029  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-030  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-031  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-031  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-031  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-032  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-033  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-031  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-031  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-031  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-034  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-035  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-006  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-036  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-008  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     DET-004  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-037  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-038  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP  LIN-TMP
           MOVE     CAB-039  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     91       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     92       TO  LIN-TMP
           MOVE     CAB-040  TO  DET-TMP
           WRITE    REG-TMP
           ADD      1        TO  CHV-TMP
           MOVE     93       TO  LIN-TMP
           MOVE     SPACES   TO  DET-TMP
           WRITE    REG-TMP      GO       ROT-IMPR-00.

       ROT-IMPR-65.
           MOVE     ACM-PAG  TO  PAG-IMP
           DISPLAY  PAG-T01      CLOSE    CADTMP
           CALL    "DYIMPR"      USING    PRM-IMP
           PERFORM  ROT-RTLA-00  GO       ROT-EXIT-00.

       ROT-GTLA-00.
           MOVE   "G"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

       ROT-RTLA-00.
           MOVE   "R"        TO    FLG-SCR
           CALL   "DYTELA"  USING  PRM-SCR.

       ROT-NOME-00.
           CALL   "DYNOME"  USING  PRM-NOM
                                   PRM-CAB
           MOVE    CLI-NOM   TO    CLI-C01.

       ROT-EXIT-00.
           CLOSE   CADCMB  CADECR
                   CADLCP  CADPRS  CADTNQ
           DELETE   FILE   CADTMP  GOBACK.
