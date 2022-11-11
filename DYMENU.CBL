       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYMENU.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT  IS  COMMA.

       DATA            DIVISION.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  MENU-AX          PIC  9(02).
           03  TELA-AX          PIC  9(02).
           03  LINE-AX          PIC  9(02).
           03  SETA-AX          PIC  X(22).
           03  NOPC-AX          PIC  9(01)  VALUE  1.

       01  TB-SETA.
           03  M-0000  PIC  X(09)  VALUE  "052206064".
           03  M-1000  PIC  X(09)  VALUE  "052207131".
           03  M-1100  PIC  X(09)  VALUE  "052208205".
           03  M-1120  PIC  X(09)  VALUE  "022009276".
           03  M-1150  PIC  X(09)  VALUE  "062009276".
           03  M-1200  PIC  X(09)  VALUE  "052208205".
           03  M-1220  PIC  X(09)  VALUE  "022109276".
           03  M-1250  PIC  X(09)  VALUE  "052109276".
           03  M-1300  PIC  X(09)  VALUE  "052208205".
           03  M-1350  PIC  X(09)  VALUE  "062009276".
           03  M-1400  PIC  X(09)  VALUE  "052208205".
           03  M-1450  PIC  X(09)  VALUE  "022009276".
           03  M-1500  PIC  X(09)  VALUE  "052208205".
           03  M-1520  PIC  X(09)  VALUE  "062209276".
           03  M-1530  PIC  X(09)  VALUE  "022009276".
           03  M-1550  PIC  X(09)  VALUE  "062109276".
           03  M-3000  PIC  X(09)  VALUE  "022207131".
           03  M-3200  PIC  X(09)  VALUE  "042208205".
           03  M-4000  PIC  X(09)  VALUE  "052207131".
           03  M-5000  PIC  X(09)  VALUE  "021907131".
       01  RD-SETA     REDEFINES   TB-SETA.
           03  RG-SETA          OCCURS 20.
               05  VRT-STA      PIC  9(02).
               05  HRZ-STA      PIC  9(02).
               05  LIN-STA      PIC  9(02).
               05  COL-STA      PIC  9(02).
               05  COR-STA      PIC  9(01).

       COPY  TBMENU.CPY.

       01  PRM-OPR              PIC  X(10)  VALUE  SPACES.

       01  PRM-HLP.
           03  PRG-HLP          PIC  X(04)  VALUE  "MENU".
           03  BCK-HLP          PIC  9(01)  VALUE  3.
           03  FOR-HLP          PIC  9(01)  VALUE  6.
           03  LET-HLP          PIC  9(01)  VALUE  7.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02)  VALUE  1.
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

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

       01  PRM-SCR              OCCURS  04.
           03  FLG-SCR          PIC  X(01).
           03  P01-SCR          PIC  X(01).
           03  P02-SCR          PIC S9(04).
           03  POS-SCR.
               05  LIN-SCR      PIC  9(02)  COMP-X.
               05  COL-SCR      PIC  9(02)  COMP-X.
           03  P03-SCR          PIC X(2000).
           03  P04-SCR          PIC X(2000).
           03  P05-SCR          PIC  9(04)  COMP-X.

       SCREEN          SECTION.

       01  TELA-01 BLANK SCREEN.

       01  TELA-02.
           03  BCK-CLR 07 FOR-CLR 04
               LINE 01 COLUMN 01 VALUE " -DYNAMIC- ".
           03  BCK-CLR 04.
               05  LINE 01 COLUMN 12 PIC X(69)   FROM SPACES.
               05  LINE 01 COLUMN 23 PIC X(40)   FROM CLI-NOM.
               05  LINE 01 COLUMN 73 PIC 9999/99 FROM COD-NOM.
           03  LINE 03 COLUMN 01 PIC X(80) FROM " SISTEMA AUTO-POSTO  -
      -        " Ver.: 8.40"  BCK-CLR 02 FOR-CLR 00.
           03  LINE 25 COLUMN 01 VALUE " F1 - Ajuda "     BCK-CLR 01.
           03  LINE 25 COLUMN 69 VALUE " ESC - Sair "     BCK-CLR 01.
           03  LINE 25 COLUMN 13 PIC X(56) FROM SPACES    BCK-CLR 03.
           03  LINE 04 COLUMN 01 PIC X(1600) FROM ALL "Û" FOR-CLR 08.

       01  TL-0000 BCK-CLR 04 FOR-CLR 07.
           03 LINE 05 COLUMN 03 VALUE " ÉÍÍÍÍÍÍ          ÍÍÍÍÍÍ» ".
           03 LINE 06 COLUMN 03 VALUE " º  Administra‡Æo Global  º ".
           03 LINE 07 COLUMN 03 VALUE " º  Lan‡amentos de Pista  º ".
           03 LINE 08 COLUMN 03 VALUE " º  Mala-Direta Clientes  º ".
           03 LINE 09 COLUMN 03 VALUE " º  Manuten‡Æo de Senhas  º ".
           03 LINE 10 COLUMN 03 VALUE " º  Back-up de Seguran‡a  º ".
           03 LINE 11 COLUMN 03 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 05 COLUMN 13 VALUE "Abertura" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 06 COLUMN 31 VALUE "  ".
              05 LINE 07 COLUMN 31 VALUE "  ".
              05 LINE 08 COLUMN 31 VALUE "  ".
              05 LINE 09 COLUMN 31 VALUE "  ".
              05 LINE 10 COLUMN 31 VALUE "  ".
              05 LINE 11 COLUMN 31 VALUE "  ".
              05 LINE 12 COLUMN 05 PIC X(28) FROM SPACES.

       01  TL-1000 BCK-CLR 01 FOR-CLR 07.
           03 LINE 06 COLUMN 10 VALUE " ÉÍÍÍ               ÍÍÍÍ» ".
           03 LINE 07 COLUMN 10 VALUE " º  Controle de Estoques  º ".
           03 LINE 08 COLUMN 10 VALUE " º  Controle de Receitas  º ".
           03 LINE 09 COLUMN 10 VALUE " º  Controle de Despesas  º ".
           03 LINE 10 COLUMN 10 VALUE " º  Controle C. Banc ria  º ".
           03 LINE 11 COLUMN 10 VALUE " º  Controle de Clientes  º ".
           03 LINE 12 COLUMN 10 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 06 COLUMN 17 VALUE "Administra‡Æo" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 07 COLUMN 38 VALUE "  ".
              05 LINE 08 COLUMN 38 VALUE "  ".
              05 LINE 09 COLUMN 38 VALUE "  ".
              05 LINE 10 COLUMN 38 VALUE "  ".
              05 LINE 11 COLUMN 38 VALUE "  ".
              05 LINE 12 COLUMN 38 VALUE "  ".
              05 LINE 13 COLUMN 12 PIC X(28) FROM SPACES.

       01  TL-1100 BCK-CLR 05 FOR-CLR 07.
           03 LINE 07 COLUMN 17 VALUE " ÉÍÍÍÍÍÍ         ÍÍÍÍÍÍÍ» ".
           03 LINE 08 COLUMN 17 VALUE " º  Cadastro de Produtos  º ".
           03 LINE 09 COLUMN 17 VALUE " º  Movimentos e Acertos  º ".
           03 LINE 10 COLUMN 17 VALUE " º  Controle dos Tanques  º ".
           03 LINE 11 COLUMN 17 VALUE " º  Posi‡Æo do Movimento  º ".
           03 LINE 12 COLUMN 17 VALUE " º  EmissÆo de Relat¢rio  º ".
           03 LINE 13 COLUMN 17 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 07 COLUMN 27 VALUE "Estoque" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 08 COLUMN 45 VALUE "  ".
              05 LINE 09 COLUMN 45 VALUE "  ".
              05 LINE 10 COLUMN 45 VALUE "  ".
              05 LINE 11 COLUMN 45 VALUE "  ".
              05 LINE 12 COLUMN 45 VALUE "  ".
              05 LINE 13 COLUMN 45 VALUE "  ".
              05 LINE 14 COLUMN 19 PIC X(28) FROM SPACES.

       01  TL-1120 BCK-CLR 06 FOR-CLR 07.
           03 LINE 08 COLUMN 24 VALUE " ÉÍÍÍÍ            ÍÍÍÍ» ".
           03 LINE 09 COLUMN 24 VALUE " º  Compra de Produtos  º ".
           03 LINE 10 COLUMN 24 VALUE " º  Reajuste de Pre‡os  º ".
           03 LINE 11 COLUMN 24 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 08 COLUMN 32 VALUE "Movimentos" HIGHLIGHT.
           03 BCK-CLR 0 FOR-CLR 8.
              05 LINE 09 COLUMN 50 VALUE "  ".
              05 LINE 10 COLUMN 50 VALUE "  ".
              05 LINE 11 COLUMN 50 VALUE "  ".
              05 LINE 12 COLUMN 26 PIC X(26) FROM "Æo de Relat¢rio  º".

       01  TL-1150 BCK-CLR 06 FOR-CLR 07.
           03 LINE 08 COLUMN 24 VALUE " ÉÍÍÍÍ            ÍÍÍÍ» ".
           03 LINE 09 COLUMN 24 VALUE " º  Pre‡os de Produtos  º ".
           03 LINE 10 COLUMN 24 VALUE " º  Produtos … Comprar  º ".
           03 LINE 11 COLUMN 24 VALUE " º  Vendas de Produtos  º ".
           03 LINE 12 COLUMN 24 VALUE " º  Compras no Per¡odo  º ".
           03 LINE 13 COLUMN 24 VALUE " º  Corre‡Æo de Saldos  º ".
           03 LINE 14 COLUMN 24 VALUE " º  Relat¢rio Completo  º ".
           03 LINE 15 COLUMN 24 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 08 COLUMN 32 VALUE "Relat¢rios" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 09 COLUMN 50 VALUE "  ".
              05 LINE 10 COLUMN 50 VALUE "  ".
              05 LINE 11 COLUMN 50 VALUE "  ".
              05 LINE 12 COLUMN 50 VALUE "  ".
              05 LINE 13 COLUMN 50 VALUE "  ".
              05 LINE 14 COLUMN 50 VALUE "  ".
              05 LINE 15 COLUMN 50 VALUE "  ".
              05 LINE 16 COLUMN 26 PIC X(26) FROM SPACES.

       01  TL-1200 BCK-CLR 05 FOR-CLR 07.
           03 LINE 07 COLUMN 17 VALUE " ÉÍÍÍÍÍÍ          ÍÍÍÍÍÍ» ".
           03 LINE 08 COLUMN 17 VALUE " º  Movimento dos Caixas  º ".
           03 LINE 09 COLUMN 17 VALUE " º  Pre‡os e Encerrantes  º ".
           03 LINE 10 COLUMN 17 VALUE " º  Zeramento do Per¡odo  º ".
           03 LINE 11 COLUMN 17 VALUE " º  Posi‡Æo do Movimento  º ".
           03 LINE 12 COLUMN 17 VALUE " º  EmissÆo de Relat¢rio  º ".
           03 LINE 13 COLUMN 17 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 07 COLUMN 27 VALUE "Receitas" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 08 COLUMN 45 VALUE "  ".
              05 LINE 09 COLUMN 45 VALUE "  ".
              05 LINE 10 COLUMN 45 VALUE "  ".
              05 LINE 11 COLUMN 45 VALUE "  ".
              05 LINE 12 COLUMN 45 VALUE "  ".
              05 LINE 13 COLUMN 45 VALUE "  ".
              05 LINE 14 COLUMN 19 PIC X(28) FROM SPACES.

       01  TL-1220 BCK-CLR 06 FOR-CLR 07.
           03 LINE 08 COLUMN 24 VALUE " ÉÍÍÍÍÍ           ÍÍÍÍÍ» ".
           03 LINE 09 COLUMN 24 VALUE " º  Corre‡Æo dos Pre‡os  º ".
           03 LINE 10 COLUMN 24 VALUE " º  Troca de Encerrante  º ".
           03 LINE 11 COLUMN 24 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 08 COLUMN 33 VALUE "Corre‡äes" HIGHLIGHT.
           03 BCK-CLR 0 FOR-CLR 8.
              05 LINE 09 COLUMN 51 VALUE "  ".
              05 LINE 10 COLUMN 51 VALUE "  ".
              05 LINE 11 COLUMN 51 VALUE "  ".
              05 LINE 12 COLUMN 26 PIC X(27) FROM "Æo de Relat¢rio  º".

       01  TL-1250 BCK-CLR 06 FOR-CLR 07.
           03 LINE 08 COLUMN 24 VALUE " ÉÍÍÍÍ            ÍÍÍÍÍ» ".
           03 LINE 09 COLUMN 24 VALUE " º  Resumo do Movimento  º ".
           03 LINE 10 COLUMN 24 VALUE " º  Relat¢rio Anal¡tico  º ".
           03 LINE 11 COLUMN 24 VALUE " º  Di rio Vendas / LMC  º ".
           03 LINE 12 COLUMN 24 VALUE " º  PrevisÆo Movimentos  º ".
           03 LINE 13 COLUMN 24 VALUE " º  An lise Operacional  º ".
           03 LINE 14 COLUMN 24 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 08 COLUMN 32 VALUE "Relat¢rios" HIGHLIGHT.
           03 BCK-CLR 0 FOR-CLR 8.
              05 LINE 09 COLUMN 51 VALUE "  ".
              05 LINE 10 COLUMN 51 VALUE "  ".
              05 LINE 11 COLUMN 51 VALUE "  ".
              05 LINE 12 COLUMN 51 VALUE "  ".
              05 LINE 13 COLUMN 51 VALUE "  ".
              05 LINE 14 COLUMN 51 VALUE "  ".
              05 LINE 15 COLUMN 51 VALUE "  ".
              05 LINE 15 COLUMN 26 PIC X(27) FROM SPACES.

       01  TL-1300 BCK-CLR 05 FOR-CLR 07.
           03 LINE 07 COLUMN 17 VALUE " ÉÍÍÍÍÍÍ          ÍÍÍÍÍÍ» ".
           03 LINE 08 COLUMN 17 VALUE " º  Cadastro de Despesas  º ".
           03 LINE 09 COLUMN 17 VALUE " º  Lan‡a Contas … Pagar  º ".
           03 LINE 10 COLUMN 17 VALUE " º  Corre‡Æo Lan‡amentos  º ".
           03 LINE 11 COLUMN 17 VALUE " º  Posi‡Æo do Movimento  º ".
           03 LINE 12 COLUMN 17 VALUE " º  EmissÆo de Relat¢rio  º ".
           03 LINE 13 COLUMN 17 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 07 COLUMN 27 VALUE "Despesas" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 08 COLUMN 45 VALUE "  ".
              05 LINE 09 COLUMN 45 VALUE "  ".
              05 LINE 10 COLUMN 45 VALUE "  ".
              05 LINE 11 COLUMN 45 VALUE "  ".
              05 LINE 12 COLUMN 45 VALUE "  ".
              05 LINE 13 COLUMN 45 VALUE "  ".
              05 LINE 14 COLUMN 19 PIC X(28) FROM SPACES.

       01  TL-1350 BCK-CLR 06 FOR-CLR 07.
           03 LINE 08 COLUMN 24 VALUE " ÉÍÍÍÍ            ÍÍÍÍ» ".
           03 LINE 09 COLUMN 24 VALUE " º  Rela‡Æo de C¢digos  º ".
           03 LINE 10 COLUMN 24 VALUE " º  Extrato de C¢digos  º ".
           03 LINE 11 COLUMN 24 VALUE " º  Movimentos Per¡odo  º ".
           03 LINE 12 COLUMN 24 VALUE " º  Pagamentos Per¡odo  º ".
           03 LINE 13 COLUMN 24 VALUE " º  Movimentos … Pagar  º ".
           03 LINE 14 COLUMN 24 VALUE " º  Acumulados Per¡odo  º ".
           03 LINE 15 COLUMN 24 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 08 COLUMN 32 VALUE "Relat¢rios" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 09 COLUMN 50 VALUE "  ".
              05 LINE 10 COLUMN 50 VALUE "  ".
              05 LINE 11 COLUMN 50 VALUE "  ".
              05 LINE 12 COLUMN 50 VALUE "  ".
              05 LINE 13 COLUMN 50 VALUE "  ".
              05 LINE 14 COLUMN 50 VALUE "  ".
              05 LINE 15 COLUMN 50 VALUE "  ".
              05 LINE 16 COLUMN 26 PIC X(26) FROM SPACES.

       01  TL-1400 BCK-CLR 05 FOR-CLR 07.
           03 LINE 07 COLUMN 17 VALUE " ÉÍÍÍÍÍÍÍ        ÍÍÍÍÍÍÍ» ".
           03 LINE 08 COLUMN 17 VALUE " º  Cadastro C. Corrente  º ".
           03 LINE 09 COLUMN 17 VALUE " º  Lan‡amento Movimento  º ".
           03 LINE 10 COLUMN 17 VALUE " º  Corre‡Æo Lan‡amentos  º ".
           03 LINE 11 COLUMN 17 VALUE " º  Posi‡Æo do Movimento  º ".
           03 LINE 12 COLUMN 17 VALUE " º  EmissÆo de Relat¢rio  º ".
           03 LINE 13 COLUMN 17 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 07 COLUMN 28 VALUE "Bancos" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 08 COLUMN 45 VALUE "  ".
              05 LINE 09 COLUMN 45 VALUE "  ".
              05 LINE 10 COLUMN 45 VALUE "  ".
              05 LINE 11 COLUMN 45 VALUE "  ".
              05 LINE 12 COLUMN 45 VALUE "  ".
              05 LINE 13 COLUMN 45 VALUE "  ".
              05 LINE 14 COLUMN 19 PIC X(28) FROM SPACES.

       01  TL-1450 BCK-CLR 06 FOR-CLR 07.
           03 LINE 08 COLUMN 24 VALUE " ÉÍÍÍÍ            ÍÍÍÍ» ".
           03 LINE 09 COLUMN 24 VALUE " º  Extrato do Per¡odo  º ".
           03 LINE 10 COLUMN 24 VALUE " º  Cheques Devolvidos  º ".
           03 LINE 11 COLUMN 24 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 08 COLUMN 32 VALUE "Relat¢rios" HIGHLIGHT.
           03 BCK-CLR 0 FOR-CLR 8.
              05 LINE 09 COLUMN 50 VALUE "  ".
              05 LINE 10 COLUMN 50 VALUE "  ".
              05 LINE 11 COLUMN 50 VALUE "  ".
              05 LINE 12 COLUMN 26 PIC X(26) FROM "Æo de Relat¢rio  º".

       01  TL-1500 BCK-CLR 05 FOR-CLR 07.
           03 LINE 07 COLUMN 17 VALUE " ÉÍÍÍÍÍÍ          ÍÍÍÍÍÍ» ".
           03 LINE 08 COLUMN 17 VALUE " º  Cadastro de Empresas  º ".
           03 LINE 09 COLUMN 17 VALUE " º  Movimentos e Acertos  º ".
           03 LINE 10 COLUMN 17 VALUE " º  Faturamento Empresas  º ".
           03 LINE 11 COLUMN 17 VALUE " º  Posi‡Æo do Movimento  º ".
           03 LINE 12 COLUMN 17 VALUE " º  EmissÆo de Relat¢rio  º ".
           03 LINE 13 COLUMN 17 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 07 COLUMN 27 VALUE "Clientes" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 08 COLUMN 45 VALUE "  ".
              05 LINE 09 COLUMN 45 VALUE "  ".
              05 LINE 10 COLUMN 45 VALUE "  ".
              05 LINE 11 COLUMN 45 VALUE "  ".
              05 LINE 12 COLUMN 45 VALUE "  ".
              05 LINE 13 COLUMN 45 VALUE "  ".
              05 LINE 14 COLUMN 19 PIC X(28) FROM SPACES.

       01  TL-1520 BCK-CLR 06 FOR-CLR 07.
           03 LINE 08 COLUMN 24 VALUE " ÉÍÍÍÍÍ            ÍÍÍÍÍ» ".
           03 LINE 09 COLUMN 24 VALUE " º  Requisi‡äes Empresas  º ".
           03 LINE 10 COLUMN 24 VALUE " º  Cheques  Pr‚-Datados  º ".
           03 LINE 11 COLUMN 24 VALUE " º  America Express Card  º ".
           03 LINE 12 COLUMN 24 VALUE " º  Mastercard e Diner's  º ".
           03 LINE 13 COLUMN 24 VALUE " º  CartÆo Visa e Outros  º ".
           03 LINE 14 COLUMN 24 VALUE " º  Vales Frete Diversos  º ".
           03 LINE 15 COLUMN 24 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 08 COLUMN 33 VALUE "Movimentos" HIGHLIGHT.
           03 BCK-CLR 0 FOR-CLR 8.
              05 LINE 09 COLUMN 52 VALUE "  ".
              05 LINE 10 COLUMN 52 VALUE "  ".
              05 LINE 11 COLUMN 52 VALUE "  ".
              05 LINE 12 COLUMN 52 VALUE "  ".
              05 LINE 13 COLUMN 52 VALUE "  ".
              05 LINE 14 COLUMN 52 VALUE "  ".
              05 LINE 15 COLUMN 52 VALUE "  ".
              05 LINE 16 COLUMN 26 PIC X(28) FROM SPACES.

       01  TL-1530 BCK-CLR 06 FOR-CLR 07.
           03 LINE 08 COLUMN 24 VALUE " ÉÍÍÍ             ÍÍÍÍ» ".
           03 LINE 09 COLUMN 24 VALUE " º  Fechamento Empresa  º ".
           03 LINE 10 COLUMN 24 VALUE " º  Fechamento Per¡odo  º ".
           03 LINE 11 COLUMN 24 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 08 COLUMN 31 VALUE "Faturamento" HIGHLIGHT.
           03 BCK-CLR 0 FOR-CLR 8.
              05 LINE 09 COLUMN 50 VALUE "  ".
              05 LINE 10 COLUMN 50 VALUE "  ".
              05 LINE 11 COLUMN 50 VALUE "  ".
              05 LINE 12 COLUMN 26 PIC X(26) FROM "Æo de Relat¢rio  º".

       01  TL-1550 BCK-CLR 06 FOR-CLR 07.
           03 LINE 08 COLUMN 24 VALUE " ÉÍÍÍÍ            ÍÍÍÍÍ» ".
           03 LINE 09 COLUMN 24 VALUE " º  Rela‡Æo de Empresas  º ".
           03 LINE 10 COLUMN 24 VALUE " º  Faturas em Cobran‡a  º ".
           03 LINE 11 COLUMN 24 VALUE " º  Rela‡Æo de Cr‚ditos  º ".
           03 LINE 12 COLUMN 24 VALUE " º  Recibos p/ Empresas  º ".
           03 LINE 13 COLUMN 24 VALUE " º  Boletos de Cobran‡a  º ".
           03 LINE 14 COLUMN 24 VALUE " º  Etiq. para Empresas  º ".
           03 LINE 15 COLUMN 24 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 08 COLUMN 32 VALUE "Relat¢rios" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 09 COLUMN 51 VALUE "  ".
              05 LINE 10 COLUMN 51 VALUE "  ".
              05 LINE 11 COLUMN 51 VALUE "  ".
              05 LINE 12 COLUMN 51 VALUE "  ".
              05 LINE 13 COLUMN 51 VALUE "  ".
              05 LINE 14 COLUMN 51 VALUE "  ".
              05 LINE 15 COLUMN 51 VALUE "  ".
              05 LINE 16 COLUMN 26 PIC X(27) FROM SPACES.

       01  TL-3000 BCK-CLR 01 FOR-CLR 07.
           03 LINE 06 COLUMN 10 VALUE " ÉÍÍÍÍ             ÍÍÍÍÍ» ".
           03 LINE 07 COLUMN 10 VALUE " º  Cadastro de Clientes  º ".
           03 LINE 08 COLUMN 10 VALUE " º  EmissÆo de Relat¢rio  º ".
           03 LINE 09 COLUMN 10 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 06 COLUMN 18 VALUE "Mala-Direta" HIGHLIGHT.
           03 BCK-CLR 0 FOR-CLR 8.
              05 LINE 07 COLUMN 38 VALUE "  ".
              05 LINE 08 COLUMN 38 VALUE "  ".
              05 LINE 09 COLUMN 38 VALUE "  ".
              05 LINE 10 COLUMN 12 PIC X(28) FROM "up de Seguran‡a  º".

       01  TL-3200 BCK-CLR 05 FOR-CLR 07.
           03 LINE 07 COLUMN 17 VALUE " ÉÍÍÍÍÍ            ÍÍÍÍÍ» ".
           03 LINE 08 COLUMN 17 VALUE " º  Rela‡Æo dos Clientes  º ".
           03 LINE 09 COLUMN 17 VALUE " º  Etiq. Endere‡o Posto  º ".
           03 LINE 10 COLUMN 17 VALUE " º  Etiq. p/ Mala-Direta  º ".
           03 LINE 11 COLUMN 17 VALUE " º  Etiq. Aniversariante  º ".
           03 LINE 12 COLUMN 17 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 07 COLUMN 26 VALUE "Relat¢rios" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 08 COLUMN 45 VALUE "  ".
              05 LINE 09 COLUMN 45 VALUE "  ".
              05 LINE 10 COLUMN 45 VALUE "  ".
              05 LINE 11 COLUMN 45 VALUE "  ".
              05 LINE 12 COLUMN 45 VALUE "  ".
              05 LINE 13 COLUMN 19 PIC X(28) FROM SPACES.

       01  TL-4000 BCK-CLR 01 FOR-CLR 07.
           03 LINE 06 COLUMN 10 VALUE " ÉÍÍÍÍÍÍÍ        ÍÍÍÍÍÍÍ» ".
           03 LINE 07 COLUMN 10 VALUE " º  Altera‡Æo das Senhas  º ".
           03 LINE 08 COLUMN 10 VALUE " º  InclusÆo de Operador  º ".
           03 LINE 09 COLUMN 10 VALUE " º  ExclusÆo de Operador  º ".
           03 LINE 10 COLUMN 10 VALUE " º  Altera‡Æo dos N¡veis  º ".
           03 LINE 11 COLUMN 10 VALUE " º  EmissÆo de Relat¢rio  º ".
           03 LINE 12 COLUMN 10 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 06 COLUMN 21 VALUE "Senhas" HIGHLIGHT.
           03 BCK-CLR 0.
              05 LINE 07 COLUMN 38 VALUE "  ".
              05 LINE 08 COLUMN 38 VALUE "  ".
              05 LINE 09 COLUMN 38 VALUE "  ".
              05 LINE 10 COLUMN 38 VALUE "  ".
              05 LINE 11 COLUMN 38 VALUE "  ".
              05 LINE 12 COLUMN 38 VALUE "  ".
              05 LINE 13 COLUMN 12 PIC X(28) FROM SPACES.

       01  TL-5000 BCK-CLR 01 FOR-CLR 07.
           03 LINE 06 COLUMN 10 VALUE " ÉÍÍÍÍÍ         ÍÍÍÍÍ» ".
           03 LINE 07 COLUMN 10 VALUE " º  Atualizar Back-up  º ".
           03 LINE 08 COLUMN 10 VALUE " º  Restaurar Back-up  º ".
           03 LINE 09 COLUMN 10 VALUE " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ".
           03 LINE 06 COLUMN 19 VALUE "Back-up" HIGHLIGHT.
           03 BCK-CLR 0 FOR-CLR 8.
              05 LINE 07 COLUMN 35 VALUE "  ".
              05 LINE 08 COLUMN 35 VALUE "  ".
              05 LINE 09 COLUMN 35 VALUE "  ".
              05 LINE 10 COLUMN 12 PIC X(25) FROM "up de Seguran‡a  º".

       01  LN-SETA  BCK-CLR  07
                    FOR-CLR  12
                    SIZE     HRZ-STA (MENU-AX)
                    COLUMN   COL-STA (MENU-AX)
                    LINE     LINE-AX PIC X(22) USING SETA-AX.

       01  LN-MENU  FOR-CLR  07
                    BCK-CLR  COR-STA (MENU-AX)
                    SIZE     HRZ-STA (MENU-AX)
                    COLUMN   COL-STA (MENU-AX)
                    LINE     LINE-AX PIC X(22) USING SETA-AX.

       PROCEDURE       DIVISION.

       ROT-TELA-00.
           CALL    "DYNOME"  USING  PRM-NOM  PRM-CAB
           DISPLAY  TELA-01         TELA-02
           CALL    "DYHORA".

       ROT-OPER-00.
           MOVE    "G"        TO    FLG-SCR (1)
           CALL    "DYTELA"  USING  PRM-SCR (1)
           CALL    "DYOPER"  USING  PRM-OPR
           MOVE    "R"        TO    FLG-SCR (1)
           CALL    "DYTELA"  USING  PRM-SCR (1)
           IF  PRM-OPR   =  SPACES  DISPLAY  TELA-01  GOBACK.
           MOVE     1         TO    MENU-AX
           MOVE     0         TO    TELA-AX  DISPLAY  TL-0000.

       ROT-SETA-00.
           COMPUTE  LINE-AX  =  LIN-STA (MENU-AX)  +   NOPC-AX  -  1
           MOVE     DC-MENU    (MENU-AX, NOPC-AX)  TO  SETA-AX
           DISPLAY  LN-SETA.

       ROT-SETA-10.
           CALL    "DYMSGM"       USING    PRM-MSG
           IF  TCL-MSG   =   01   PERFORM  ROT-HELP-00
                                  GO       ROT-SETA-10.
           IF  TCL-MSG   =   05   GO       ROT-SETA-30.
           IF  TCL-MSG   =   06   GO       ROT-SETA-40.
           IF  TCL-MSG   >   00   GO       ROT-SETA-10.
           IF  HEX-MSG   =   27   GO       ROT-SETA-20.
           ADD     1              TO       TELA-AX
           MOVE   "G"             TO       FLG-SCR (TELA-AX)
           CALL   "DYTELA"        USING    PRM-SCR (TELA-AX)
           EVALUATE  MENU-AX
           WHEN  01  EVALUATE NOPC-AX
                     WHEN  1  DISPLAY  TL-1000  MOVE 02  TO  MENU-AX
                     WHEN  2  CALL    "DY2000"  USING    PRM-OPR
                              CANCEL  "DY2000"  GO       ROT-PROG-00
                     WHEN  3  DISPLAY  TL-3000  MOVE 17  TO  MENU-AX
                     WHEN  4  DISPLAY  TL-4000  MOVE 19  TO  MENU-AX
                     WHEN  5  DISPLAY  TL-5000  MOVE 20  TO  MENU-AX
                     END-EVALUATE
           WHEN  02  EVALUATE NOPC-AX
                     WHEN  1  DISPLAY  TL-1100  MOVE 03  TO  MENU-AX
                     WHEN  2  DISPLAY  TL-1200  MOVE 06  TO  MENU-AX
                     WHEN  3  DISPLAY  TL-1300  MOVE 09  TO  MENU-AX
                     WHEN  4  DISPLAY  TL-1400  MOVE 11  TO  MENU-AX
                     WHEN  5  DISPLAY  TL-1500  MOVE 13  TO  MENU-AX
                     END-EVALUATE
           WHEN  03  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1110"  USING    PRM-OPR
                              CANCEL  "DY1110"  GO       ROT-PROG-00
                     WHEN  2  DISPLAY  TL-1120  MOVE 04  TO  MENU-AX
                     WHEN  3  CALL    "DY1130"  USING    PRM-OPR
                              CANCEL  "DY1130"  GO       ROT-PROG-00
                     WHEN  4  CALL    "DY1140"  USING    PRM-OPR
                              CANCEL  "DY1140"  GO       ROT-PROG-00
                     WHEN  5  DISPLAY  TL-1150  MOVE 05  TO  MENU-AX
                     END-EVALUATE
           WHEN  04  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1121"  USING    PRM-OPR
                              CANCEL  "DY1121"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY1122"  USING    PRM-OPR
                              CANCEL  "DY1122"  GO       ROT-PROG-00
                     END-EVALUATE
           WHEN  05  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1151"  USING    PRM-OPR
                              CANCEL  "DY1151"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY1152"  USING    PRM-OPR
                              CANCEL  "DY1152"  GO       ROT-PROG-00
                     WHEN  3  CALL    "DY1153"  USING    PRM-OPR
                              CANCEL  "DY1153"  GO       ROT-PROG-00
                     WHEN  4  CALL    "DY1154"  USING    PRM-OPR
                              CANCEL  "DY1154"  GO       ROT-PROG-00
                     WHEN  5  CALL    "DY1155"  USING    PRM-OPR
                              CANCEL  "DY1155"  GO       ROT-PROG-00
                     WHEN  6  CALL    "DY1156"  USING    PRM-OPR
                              CANCEL  "DY1156"  GO       ROT-PROG-00
                     END-EVALUATE
           WHEN  06  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1210"  USING    PRM-OPR
                              CANCEL  "DY1210"  GO       ROT-PROG-00
                     WHEN  2  DISPLAY  TL-1220  MOVE 07  TO  MENU-AX
                     WHEN  3  CALL    "DY1230"  USING    PRM-OPR
                              CANCEL  "DY1230"  GO       ROT-PROG-00
                     WHEN  4  CALL    "DY1240"  USING    PRM-OPR
                              CANCEL  "DY1240"  GO       ROT-PROG-00
                     WHEN  5  DISPLAY  TL-1250  MOVE 08  TO  MENU-AX
                     END-EVALUATE
           WHEN  07  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1221"  USING    PRM-OPR
                              CANCEL  "DY1221"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY1222"  USING    PRM-OPR
                              CANCEL  "DY1222"  GO       ROT-PROG-00
                     END-EVALUATE
           WHEN  08  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1251"  USING    PRM-OPR
                              CANCEL  "DY1251"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY1252"  USING    PRM-OPR
                              CANCEL  "DY1252"  GO       ROT-PROG-00
                     WHEN  3  CALL    "DY1253"  USING    PRM-OPR
                              CANCEL  "DY1253"  GO       ROT-PROG-00
                     WHEN  4  CALL    "DY1254"  USING    PRM-OPR
                              CANCEL  "DY1254"  GO       ROT-PROG-00
                     WHEN  5  CALL    "DY1255"  USING    PRM-OPR
                              CANCEL  "DY1255"  GO       ROT-PROG-00
                     END-EVALUATE
           WHEN  09  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1310"  USING    PRM-OPR
                              CANCEL  "DY1310"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY1320"  USING    PRM-OPR
                              CANCEL  "DY1320"  GO       ROT-PROG-00
                     WHEN  3  CALL    "DY1330"  USING    PRM-OPR
                              CANCEL  "DY1330"  GO       ROT-PROG-00
                     WHEN  4  CALL    "DY1340"  USING    PRM-OPR
                              CANCEL  "DY1340"  GO       ROT-PROG-00
                     WHEN  5  DISPLAY  TL-1350  MOVE 10  TO  MENU-AX
                     END-EVALUATE
           WHEN  10  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1351"  USING    PRM-OPR
                              CANCEL  "DY1351"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY1352"  USING    PRM-OPR
                              CANCEL  "DY1352"  GO       ROT-PROG-00
                     WHEN  3  CALL    "DY1353"  USING    PRM-OPR
                              CANCEL  "DY1353"  GO       ROT-PROG-00
                     WHEN  4  CALL    "DY1354"  USING    PRM-OPR
                              CANCEL  "DY1354"  GO       ROT-PROG-00
                     WHEN  5  CALL    "DY1355"  USING    PRM-OPR
                              CANCEL  "DY1355"  GO       ROT-PROG-00
                     WHEN  6  CALL    "DY1356"  USING    PRM-OPR
                              CANCEL  "DY1356"  GO       ROT-PROG-00
                     END-EVALUATE
           WHEN  11  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1410"  USING    PRM-OPR
                              CANCEL  "DY1410"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY1420"  USING    PRM-OPR
                              CANCEL  "DY1420"  GO       ROT-PROG-00
                     WHEN  3  CALL    "DY1430"  USING    PRM-OPR
                              CANCEL  "DY1430"  GO       ROT-PROG-00
                     WHEN  4  CALL    "DY1440"  USING    PRM-OPR
                              CANCEL  "DY1440"  GO       ROT-PROG-00
                     WHEN  5  DISPLAY  TL-1450  MOVE 12  TO  MENU-AX
                     END-EVALUATE
           WHEN  12  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1451"  USING    PRM-OPR
                              CANCEL  "DY1451"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY1452"  USING    PRM-OPR
                              CANCEL  "DY1452"  GO       ROT-PROG-00
                     END-EVALUATE
           WHEN  13  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1510"  USING    PRM-OPR
                              CANCEL  "DY1510"           GO  ROT-PROG-00
                     WHEN  2  DISPLAY  TL-1520  MOVE 14  TO  MENU-AX
                     WHEN  3  DISPLAY  TL-1530  MOVE 15  TO  MENU-AX
                     WHEN  4  CALL    "DY1540"  USING    PRM-OPR
                              CANCEL  "DY1540"           GO  ROT-PROG-00
                     WHEN  5  DISPLAY  TL-1550  MOVE 16  TO  MENU-AX
                     END-EVALUATE
           WHEN  14  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1521"  USING    PRM-OPR
                              CANCEL  "DY1521"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY1522"  USING    PRM-OPR
                              CANCEL  "DY1522"  GO       ROT-PROG-00
                     WHEN  3  CALL    "DY1523"  USING    PRM-OPR
                              CANCEL  "DY1523"  GO       ROT-PROG-00
                     WHEN  4  CALL    "DY1524"  USING    PRM-OPR
                              CANCEL  "DY1524"  GO       ROT-PROG-00
                     WHEN  5  CALL    "DY1525"  USING    PRM-OPR
                              CANCEL  "DY1525"  GO       ROT-PROG-00
                     WHEN  6  CALL    "DY1526"  USING    PRM-OPR
                              CANCEL  "DY1526"  GO       ROT-PROG-00
                     END-EVALUATE
           WHEN  15  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1531"  USING    PRM-OPR
                              CANCEL  "DY1531"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY1532"  USING    PRM-OPR
                              CANCEL  "DY1532"  GO       ROT-PROG-00
                     END-EVALUATE
           WHEN  16  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY1551"  USING    PRM-OPR
                              CANCEL  "DY1551"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY1552"  USING    PRM-OPR
                              CANCEL  "DY1552"  GO       ROT-PROG-00
                     WHEN  3  CALL    "DY1553"  USING    PRM-OPR
                              CANCEL  "DY1553"  GO       ROT-PROG-00
                     WHEN  4  CALL    "DY1554"  USING    PRM-OPR
                              CANCEL  "DY1554"  GO       ROT-PROG-00
                     WHEN  5  CALL    "DY1555"  USING    PRM-OPR
                              CANCEL  "DY1555"  GO       ROT-PROG-00
                     WHEN  6  CALL    "DY1556"  USING    PRM-OPR
                              CANCEL  "DY1556"  GO       ROT-PROG-00
                     END-EVALUATE
           WHEN  17  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY3100"  USING    PRM-OPR
                              CANCEL  "DY3100"  GO       ROT-PROG-00
                     WHEN  2  DISPLAY  TL-3200  MOVE 18  TO  MENU-AX
                     END-EVALUATE
           WHEN  18  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY3210"  USING    PRM-OPR
                              CANCEL  "DY3210"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY3220"  USING    PRM-OPR
                              CANCEL  "DY3220"  GO       ROT-PROG-00
                     WHEN  3  CALL    "DY3230"  USING    PRM-OPR
                              CANCEL  "DY3230"  GO       ROT-PROG-00
                     WHEN  4  CALL    "DY3240"  USING    PRM-OPR
                              CANCEL  "DY3240"  GO       ROT-PROG-00
                     END-EVALUATE
           WHEN  19  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY4100"  USING    PRM-OPR
                              CANCEL  "DY4100"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY4200"  USING    PRM-OPR
                              CANCEL  "DY4200"  GO       ROT-PROG-00
                     WHEN  3  CALL    "DY4300"  USING    PRM-OPR
                              CANCEL  "DY4300"  GO       ROT-PROG-00
                     WHEN  4  CALL    "DY4400"  USING    PRM-OPR
                              CANCEL  "DY4400"  GO       ROT-PROG-00
                     WHEN  5  CALL    "DY4500"  USING    PRM-OPR
                              CANCEL  "DY4500"  GO       ROT-PROG-00
                     END-EVALUATE
           WHEN  20  EVALUATE NOPC-AX
                     WHEN  1  CALL    "DY5100"  USING    PRM-OPR
                              CANCEL  "DY5100"  GO       ROT-PROG-00
                     WHEN  2  CALL    "DY5200"  USING    PRM-OPR
                              CANCEL  "DY5200"  GO       ROT-PROG-00
                     END-EVALUATE
           END-EVALUATE  MOVE  1  TO  NOPC-AX   GO       ROT-SETA-00.

       ROT-SETA-20.
           EVALUATE      MENU-AX
               WHEN  01  DISPLAY    TELA-01  GOBACK
               WHEN  02  MOVE 01 TO MENU-AX  MOVE 1 TO NOPC-AX
               WHEN  03  MOVE 02 TO MENU-AX  MOVE 1 TO NOPC-AX
               WHEN  04  MOVE 03 TO MENU-AX  MOVE 2 TO NOPC-AX
               WHEN  05  MOVE 03 TO MENU-AX  MOVE 5 TO NOPC-AX
               WHEN  06  MOVE 02 TO MENU-AX  MOVE 2 TO NOPC-AX
               WHEN  07  MOVE 06 TO MENU-AX  MOVE 2 TO NOPC-AX
               WHEN  08  MOVE 06 TO MENU-AX  MOVE 5 TO NOPC-AX
               WHEN  09  MOVE 02 TO MENU-AX  MOVE 3 TO NOPC-AX
               WHEN  10  MOVE 09 TO MENU-AX  MOVE 5 TO NOPC-AX
               WHEN  11  MOVE 02 TO MENU-AX  MOVE 4 TO NOPC-AX
               WHEN  12  MOVE 11 TO MENU-AX  MOVE 5 TO NOPC-AX
               WHEN  13  MOVE 02 TO MENU-AX  MOVE 5 TO NOPC-AX
               WHEN  14  MOVE 13 TO MENU-AX  MOVE 2 TO NOPC-AX
               WHEN  15  MOVE 13 TO MENU-AX  MOVE 3 TO NOPC-AX
               WHEN  16  MOVE 13 TO MENU-AX  MOVE 5 TO NOPC-AX
               WHEN  17  MOVE 01 TO MENU-AX  MOVE 3 TO NOPC-AX
               WHEN  18  MOVE 17 TO MENU-AX  MOVE 2 TO NOPC-AX
               WHEN  19  MOVE 01 TO MENU-AX  MOVE 4 TO NOPC-AX
               WHEN  20  MOVE 01 TO MENU-AX  MOVE 5 TO NOPC-AX
           END-EVALUATE
           MOVE  "R"         TO     FLG-SCR  (TELA-AX)
           CALL  "DYTELA"   USING   PRM-SCR  (TELA-AX)
           SUBTRACT    1    FROM    TELA-AX   GO   ROT-SETA-00.

       ROT-SETA-30.
           DISPLAY  LN-MENU
           IF  NOPC-AX    =   1  MOVE  VRT-STA (MENU-AX)  TO  NOPC-AX
           ELSE     SUBTRACT  1  FROM  NOPC-AX.      GO   ROT-SETA-00.

       ROT-SETA-40.
           DISPLAY  LN-MENU
           IF  NOPC-AX  <  VRT-STA (MENU-AX)  ADD  1  TO  NOPC-AX
           ELSE   MOVE  1       TO  NOPC-AX.  GO      ROT-SETA-00.

       ROT-HELP-00.
           CALL    "DYHELP"  USING  PRM-HLP.

       ROT-PROG-00.
           MOVE    "R"        TO    FLG-SCR (TELA-AX)
           CALL    "DYTELA"  USING  PRM-SCR (TELA-AX)
           SUBTRACT      1   FROM   TELA-AX  GO   ROT-SETA-10.
