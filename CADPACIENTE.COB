       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP004.
      *AUTHOR. LUCAS ALVES VIEIRA.
      ********************************************************
      * CADASTRO DE PACIENTES   *
      ********************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT CADPACI ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CPF
                      FILE STATUS  IS ST-ERRO
                      ALTERNATE RECORD KEY IS NOMEPACI 
                                    WITH DUPLICATES.
                      

            SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS  ENDERECO
                                    WITH DUPLICATES.

            SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION  IS INDEXED
                    ACCESS MODE   IS DYNAMIC
                    RECORD KEY    IS CODIGO
                    FILE STATUS   IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                    WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPACI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPACI.DAT". 
       01 REGCADPACI.
          03 CPF            PIC 9(04).
          03 NOMEPACI       PIC X(30).
          03 EMAIL          PIC X(30).
          03 SEXO           PIC X(01).
          03 GENERO         PIC X(01).
          03 DATANASCT.
                   05 DIA          PIC 9(02).
                   05 MES          PIC 9(02).
                   05 ANO.
                      07 SEC       PIC 9(02).
                      07 ANOD      PIC 9(02).
          03 CONVPACI       PIC 9(04).
          03 CEPPACI        PIC 9(08).  
          03 NUMENDERECO    PIC 9(04).
          03 COMPENDERECO   PIC X(12).
          03 TELEFONE       PIC 9(11).
      *
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCADCEP.
                03 CEP               PIC 9(08).
                03 ENDERECO          PIC X(30).
                03 BAIRRO            PIC X(20).
                03 CIDADE            PIC X(20).
                03 ESTADO            PIC X(02).
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".
       01 REGCADCONV.
                03 CODIGO            PIC 9(04).
                03 NOME              PIC X(30).
                03 TPPLANO           PIC 9(02).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       01 MENS.
          02 MENS1      PIC X(48) VALUE SPACES.
          02 MENS2      PIC ZZZ.
       77 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 IND           PIC 9(02) VALUE ZEROS.
       01 TXTSEXO       PIC X(12) VALUE SPACES.
       01 MASC1         PIC 99.99.9999.

       01 TABGENEROX.
          03 FILLER     PIC X(15) VALUE "HHETEROSEXUAL".
          03 FILLER     PIC X(15) VALUE "TTRANSEXUAL".
          03 FILLER     PIC X(15) VALUE "BBISSESUXUAL".
          03 FILLER     PIC X(15) VALUE "PPANSEXUAL".
          03 FILLER     PIC X(15) VALUE "NNAO DECLARADO".
          03 FILLER     PIC X(15) VALUE "OOUTROS".
       01 TABGENERO REDEFINES TABGENEROX.
          03 TBGENERO   PIC X(15) OCCURS 6 TIMES.
       01 TXTGENERO.
          03 TXTGENERO1 PIC X(01) VALUE SPACES.
          03 TXTGENERO2 PIC X(14) VALUE SPACES. 
          

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       
       INC-OP0.
           OPEN I-O CADPACI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPACI
                 CLOSE CADPACI
                 MOVE "*** ARQUIVO CADPACI FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-OP0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADPACI =>" TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *
       INC-OP1.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "* ARQUIVO CADCEP NAO ENCONTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CEP " TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       INC-OP2.
           OPEN INPUT CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "* ARQUIVO CADCONV NAO ENCONTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CONV " TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       INC-001.
           MOVE ZEROS TO CPF W-SEL CEPPACI NUMENDERECO DATANASCT
           MOVE SPACES TO NOME EMAIL SEXO GENERO NOMEPACI
           MOVE SPACES TO TXTGENERO TXTSEXO 
           MOVE SPACES TO COMPENDERECO ESTADO BAIRRO CIDADE ESTADO.
                    DISPLAY (01, 01) ERASE.
                    DISPLAY (01, 20) "BEM-VINDO AO CADASTRO DE PACIENTE"
                    DISPLAY (03, 01) "CPF"
                    DISPLAY (03, 42) "NOME"
                    DISPLAY (05, 01) "EMAIL"
                    DISPLAY (05, 42) "TELEFONE"
                    DISPLAY (07, 01) "SEXO "                  
                    DISPLAY (07, 42) "GENERO"
                    DISPLAY (09, 01) "DATA DE NASC."
                    DISPLAY (10, 01) "    --------------------[ E N D E".
                    DISPLAY (10, 35) "R E C O ]-------------------------------".
                    DISPLAY (11, 01) "CEP :"
                    DISPLAY (11, 17) "ENDERECO :"
                    DISPLAY (11, 60) "NUM :".
                    DISPLAY (13, 01) "COMPLEM.:"
                    DISPLAY (13, 25) "BAIRRO :".
                    DISPLAY (15, 01) "CIDADE :". 
                    DISPLAY (15, 43) "UF"
                    DISPLAY (15, 46) ":".
                    DISPLAY (16, 01) "    ----------------------[ P L A".
                    DISPLAY (16, 35) "N O ]-----------------------------------".
                    DISPLAY (17, 01) "CODIGO".
                    DISPLAY (19, 01) "TIPO DO PLANO".
                    DISPLAY (23, 01) " MENSAGEM :".

       INC-002.
                ACCEPT  (03, 11) CPF
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADPACI
                   GO TO ROT-FIM.
                IF CPF  = ZEROS
                   MOVE "*** CPF INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-CADPACI01.
                MOVE 0 TO W-SEL
                READ CADPACI
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM INC-008A
                      PERFORM INC-012A
                      DISPLAY (03, 11) CPF
                      DISPLAY (03, 47) NOMEPACI
                      DISPLAY (05, 11) EMAIL                   
                      DISPLAY (05, 51) TELEFONE
                      DISPLAY (07, 11) SEXO        
                      DISPLAY (07, 49) GENERO
                      MOVE DATANASCT TO MASC1
                      DISPLAY (09, 16) MASC1
                      DISPLAY (11, 07) CEPPACI
                      DISPLAY (11, 66) NUMENDERECO
                      DISPLAY (13, 11) COMPENDERECO
                      DISPLAY (11, 28) ENDERECO
                      DISPLAY (13, 34) BAIRRO
                      DISPLAY (15, 10) CIDADE
                      DISPLAY (15, 48) ESTADO
                      DISPLAY (17, 08) CODIGO
                      DISPLAY (19, 15) TPPLANO

                      MOVE "*** CPF JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CADPACI"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT (03, 47) NOMEPACI WITH UPDATE
                IF NOMEPACI  = SPACES
                   MOVE "*** NOME INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
 
       INC-004.
                ACCEPT (05, 11) EMAIL
                IF EMAIL  = SPACES
                   MOVE "*** EMAIL INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-011.
                ACCEPT (05, 51) TELEFONE
                IF TELEFONE  = ZEROS
                   MOVE "*** TELEFONE INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-011.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-005.
                ACCEPT (07, 11) SEXO.
       INC-005A.
                IF SEXO = "M" OR SEXO = "m"
                    MOVE "MASCULINO" TO TXTSEXO
                ELSE
                    IF SEXO = "F" OR SEXO = "f"
                    MOVE "FEMININO" TO TXTSEXO
                ELSE
                    MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-005.
                    DISPLAY(07, 13) TXTSEXO.
       INC-006.
           MOVE 1 TO IND
           ACCEPT (07, 49) GENERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO INC-005.
       INC-006A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO IND
              IF IND < 7
                 GO TO INC-006A
              ELSE
                 MOVE "*** TIPO GENERO INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-006
           ELSE
               DISPLAY (07, 49) TXTGENERO.
       INC-007.
           ACCEPT (09, 16) DATANASCT
                MOVE DATANASCT TO MASC1
                DISPLAY (09, 16) MASC1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
      *
       INC-008.
           ACCEPT (11, 07) CEPPACI.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 GO TO INC-007.
           IF CEPPACI = 0 
                 MOVE "*** CEP NAO INFORMADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-008.
      *
       INC-008A.
           MOVE CEPPACI TO CEP
           READ CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "* CEP NAO CADASTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-008
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CEP " TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.

       INC-009.
           ACCEPT (11, 66) NUMENDERECO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 GO TO INC-008.
           IF NUMENDERECO = SPACES 
                 MOVE "* ENDERECO NAO INFORMADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-009.
       INC-010.
           ACCEPT (13, 10) COMPENDERECO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 GO TO INC-009.
           IF COMPENDERECO = SPACES 
                 MOVE "* NUMERO RESIDENCIA NAO INFORMADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-010.
           IF W-ACT = 01 GO TO  INC-010.

       INC-012.
           ACCEPT (17, 08) CONVPACI.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 GO TO INC-008.
           IF CONVPACI = 0 
                 MOVE "*** CODIGO NAO INFORMADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-011.
      *
       INC-012A.
           MOVE CONVPACI TO CODIGO
           READ CADCONV
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "* CODIGO NAO CADASTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-012
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CEP " TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.    
           

      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO INC-010.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCADPACI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                  MOVE "* AMIGO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO INC-001  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADPACI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO AMIGO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO INC-010.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCADPACI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADPACI CADCEP CADCONV.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      

