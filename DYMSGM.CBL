       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYMSGM.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT  IS  COMMA.

       DATA            DIVISION.
       FILE            SECTION.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND-LET          PIC  9(02).
           03  LET-AUX          PIC  X(01).
           03  MSG-AUX          PIC  X(85).
           03  QTD-AUX          PIC  9(03).

           03  HRA-001.
               05  FILLER       PIC  X(06).
               05  QTD-001      PIC  9(02).

           03  HRA-002.
               05  FILLER       PIC  X(06).
               05  QTD-002      PIC  9(02).

       01  TB-MSGM.
           03  M-01  PIC  X(85)  VALUE  "
      -        "     Use [] ou [] p/ Selecionar a Op‡Æo Desejada ...".
           03  M-02  PIC  X(85)  VALUE  "                              U
      -        "se [] ou [] p/ Selecionar, ou [ENTER] p/ Alterar ...".
           03  M-03  PIC  X(85)  VALUE  "                             Us
      -        "e [] ou [] p/ Selecionar, ou [ENTER] p/ Estornar ...".
           03  M-04  PIC  X(85)  VALUE  "                    Use [] ou
      -        "[] p/ Selecionar, [DEL] Excluir, ou [INS] Incluir ...".
           03  M-05  PIC  X(85)  VALUE  "   Use [] ou [] p/ Selecionar
      -        ", [ENTER] Alterar, [DEL] Excluir, ou [INS] Incluir ...".
           03  M-06  PIC  X(85)  VALUE  "                     Use [] ou
      -        " [] p/ Selecionar, [ENTER] p/ Marcar ou Desmarcar ...".
           03  M-07  PIC  X(85)  VALUE  "                 Use [] Pr¢xim
      -        "o, [] Anterior, [ENTER] Alterar, ou [DEL] Excluir ...".
           03  M-08  PIC  X(85)  VALUE  "                           Use
      -        "[] Pr¢ximo, [] Anterior, ou [ENTER] p/ Confirmar ...".
           03  M-09  PIC  X(85)  VALUE  "
      -        "    Use as Teclas de Dire‡Æo p/ Ajustar a Exibi‡Æo ...".
           03  M-10  PIC  X(85)  VALUE  "
      -        "            Tecle [ENTER] p/ Concluir o Lan‡amento ...".
           03  M-11  PIC  X(85)  VALUE  "                    Ap¢s Conect
      -        "ar o PenDrive, Tecle [ENTER] p/ Iniciar o Processo ...".
           03  M-12  PIC  X(85)  VALUE  "
      -        "           CàDIGO Jµ CADASTRADO POR OUTRO OPERADOR !!!".
           03  M-13  PIC  X(85)  VALUE  "
      -        "                 REGISTRO COM MOVIMENTO NO PERÖODO !!!".
           03  M-14  PIC  X(85)  VALUE  "
      -        "       REGISTRO NÇO ENCONTRADO, VERIFIQUE O CàDIGO !!!".
           03  M-15  PIC  X(85)  VALUE  "
      -        "         DATA INVµLIDA, DIGITE NO FORMATO DD/MM/AA !!!".
           03  M-16  PIC  X(85)  VALUE  "
      -        "            DATA INVµLIDA, DIGITE NO FORMATO DD/MM !!!".
           03  M-17  PIC  X(85)  VALUE  "
      -        "                     DATA DO NASCIMENTO INCOERENTE !!!".
           03  M-18  PIC  X(85)  VALUE  "
      -        "         DATA INCOERENTE COM O PERÖODO EM EXECU€ÇO !!!".
           03  M-19  PIC  X(85)  VALUE  "
      -        "              O PERÖODO SOLICITADO ESTµ INCOERENTE !!!".
           03  M-20  PIC  X(85)  VALUE  "
      -        "                REGISTRO BLOQUEADO TEMPORARIAMENTE !!!".
           03  M-21  PIC  X(85)  VALUE  "
      -        "         CàDIGO NÇO ENCONTRADO, PESQUISE PELO NOME !!!".
           03  M-22  PIC  X(85)  VALUE  "
      -        "           FIM DA PESQUISA, PALAVRA NÇO ENCONTRADA !!!".
           03  M-23  PIC  X(85)  VALUE  "
      -        "    NOME NÇO ENCONTRADO, VERIFIQUE SE ESTµ CORRETO !!!".
           03  M-24  PIC  X(85)  VALUE  "
      -        "             NOME Jµ UTILIZADO, ESCOLHA OUTRO NOME !!!".
           03  M-25  PIC  X(85)  VALUE  "
      -        "      SENHA DE ACESSO NÇO CONFERE, TENTE NOVAMENTE !!!".
           03  M-26  PIC  X(85)  VALUE  "
      -        "    CONFIRMA€ÇO DE SENHA INVµLIDA, TENTE NOVAMENTE !!!".
           03  M-27  PIC  X(85)  VALUE  "
      -        "          SUA SENHA NÇO PERMITE ACESSAR ESTA OP€ÇO !!!".
           03  M-28  PIC  X(85)  VALUE  "
      -        "                NÇO FOI ENCONTRADO NENHUM REGISTRO !!!".
           03  M-29  PIC  X(85)  VALUE  "
      -        "           NÇO FOI ENCONTRADO MOVIMENTO NESTA DATA !!!".
           03  M-30  PIC  X(85)  VALUE  "
      -        "          NÇO EXISTE MOVIMENTO NO CAIXA SOLICITADO !!!".
           03  M-31  PIC  X(85)  VALUE  "
      -        "           NÇO FOI ENCONTRADO MOVIMENTO NO PERÖODO !!!".
           03  M-32  PIC  X(85)  VALUE  "
      -        "     NÇO EXISTEM REGISTROS NO INTERVALO SOLICITADO !!!".
           03  M-33  PIC  X(85)  VALUE  "
      -        "       A IMPRESSORA ESTµ DESLIGADA OU DESCONECTADA !!!".
           03  M-34  PIC  X(85)  VALUE  "
      -        "    CAIXAS Jµ CONCLUÖDOS, EMITA O DIµRIO DE VENDAS !!!".
           03  M-35  PIC  X(85)  VALUE  "
      -        "            CAIXA SENDO LAN€ADO POR OUTRO OPERADOR !!!".
           03  M-36  PIC  X(85)  VALUE  "
      -        "           REGISTRO Jµ EXCLUÖDO POR OUTRO OPERADOR !!!".
           03  M-37  PIC  X(85)  VALUE  "
      -        "   ESTE MOVIMENTO Jµ FOI LAN€ADO, CONFIRA OS DADOS !!!".
           03  M-38  PIC  X(85)  VALUE  "
      -        "    NÇO EXISTE NENHUM MOVIMENTO DE CAIXA CONCLUÖDO !!!".
           03  M-39  PIC  X(85)  VALUE  "
      -        "         NÇO FOI CONCLUÖDO NENHUM MOVIMENTO DIµRIO !!!".
           03  M-40  PIC  X(85)  VALUE  "
      -        "                NéMERO INVµLIDO P/ FOLHA DO L.M.C. !!!".
           03  M-41  PIC  X(85)  VALUE  "
      -        "            A SEQUÒNCIA SOLICITADA ESTµ INCOERENTE !!!".
           03  M-42  PIC  X(85)  VALUE  "
      -        "        CàDIGO INVµLIDO P/ INCLUSÇO DE COMBUSTÖVEL !!!".
           03  M-43  PIC  X(85)  VALUE  "
      -        "           LAN€AMENTO ESTORNADO POR OUTRO OPERADOR !!!".
           03  M-44  PIC  X(85)  VALUE  "
      -        "          VENDA INCOERENTE, CONFIRA OS ENCERRANTES !!!".
           03  M-45  PIC  X(85)  VALUE  "
      -        "              DESCULPE, OP€ÇO AINDA NÇO DISPONÖVEL !!!".
           03  M-46  PIC  X(85)  VALUE  "
      -        "         PRE€O DE VENDA MENOR QUE O PRE€O DE CUSTO !!!".
           03  M-47  PIC  X(85)  VALUE  "
      -        "             FALTA CONCLUIR O éLTIMO CAIXA LAN€ADO !!!".
           03  M-48  PIC  X(85)  VALUE  "
      -        "     VALOR LAN€ADO INCOERENTE COM O VALOR PENDENTE !!!".
           03  M-49  PIC  X(85)  VALUE  "
      -        "        NÇO EXISTE LAN€AMENTOS PENDENTES · RECEBER !!!".
           03  M-50  PIC  X(85)  VALUE  "
      -        "                                                      ".
       01  RD-MSGM              REDEFINES   TB-MSGM.
           03  MSGM-TB                      OCCURS  50.
               05  LET-TAB      PIC  X(01)  OCCURS  85.

       01  PRM-KBD              PIC  X(01)  COMP-X.

       01  PRM-FNC              PIC  9(02)  COMP-X  VALUE  26.

       01  PRM-TCL.
           05  FILLER           PIC  X(01).
           05  TCL-TCL          PIC  9(02)  COMP-X.
           05  HEX-TCL          PIC  9(02)  COMP-X.

       LINKAGE         SECTION.

       01  PRM-MSG.
           03  IND-MSG          PIC  9(02).
           03  TCL-MSG          PIC  9(02).
           03  HEX-MSG          PIC  9(02).

       SCREEN          SECTION.

       01  LPA-TLA  LINE 25 COLUMN 14 PIC X(54) FROM SPACES
                    BCK-CLR  03.
       01  MSGM-01  LINE 25 COLUMN 14 PIC X(54) USING MSGM-TB (IND-MSG)
                    BCK-CLR  03  FOR-CLR 15.

       PROCEDURE       DIVISION  USING  PRM-MSG.

       ROT-0000-00.
           IF  IND-MSG   >   11  CALL  X"E5".

       ROT-MSGM-00.
           MOVE     1        TO  IND-LET
           CALL    "DYHORA"      DISPLAY         MSGM-01
           MOVE     MSGM-TB     (IND-MSG)    TO  MSG-AUX
           MOVE     LET-TAB     (IND-MSG, 1) TO  LET-AUX
           ACCEPT   HRA-001 FROM TIME.


       ROT-MSGM-10.
           ACCEPT   HRA-002 FROM TIME
           MOVE     QTD-002  TO  QTD-AUX
           IF  QTD-AUX  <        QTD-001
               ADD      100  TO  QTD-AUX.
           CALL    "CBL_GET_KBD_STATUS" USING  PRM-KBD
           IF  PRM-KBD   >   00  GO        ROT-MSGM-20.
           IF  QTD-AUX   <      (QTD-001 + 12)
                                 GO        ROT-MSGM-10.
           ADD      1        TO  IND-LET
           MOVE     LET-TAB     (IND-MSG, IND-LET)  TO
                    LET-TAB     (IND-MSG, IND-LET - 1)
           IF  IND-LET   <   85  GO        ROT-MSGM-10.
           MOVE     LET-AUX  TO  LET-TAB (IND-MSG, IND-LET)
           GO       ROT-MSGM-00.

       ROT-MSGM-20.
           CALL     X"AF"  USING PRM-FNC  PRM-TCL
           MOVE     TCL-TCL  TO  TCL-MSG
           MOVE     HEX-TCL  TO  HEX-MSG
           MOVE     MSG-AUX  TO  MSGM-TB (IND-MSG).
           DISPLAY  LPA-TLA      GOBACK.
