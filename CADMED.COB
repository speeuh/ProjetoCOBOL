       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADMED.
       AUTHOR. LUCAS ALVES VIEIRA.
      **************************************
      * CADASTRO DE MEDICOS *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = NOME
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT".
       01 REGCADMED.
                03 CRM               PIC 9(06).
                03 NOME              PIC X(30).
                03 EMAIL             PIC X(30).
                03 SEXO              PIC X(01).
                03 TPESPECIALIDADE   PIC 9(02).
                03 DATANASCT.
                   05 DIA            PIC 9(02).
                   05 MES            PIC 9(02).
                   05 ANO.
                      07 SEC       PIC 9(02).
                      07 ANOD      PIC 9(02).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 MASC1        PIC 99.99.9999.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       01 TEXSEXO      PIC X(12) VALUE SPACES.
       01 TXTTPMEDICO   PIC X(18) VALUE SPACES.
       01 TABMEDICO.
          03 TBESPECIALIDADE    PIC X(18) OCCURS 7 TIMES.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "CLINICA MEDICA"   TO TBESPECIALIDADE(01)
           MOVE "UROLOGIA   "      TO TBESPECIALIDADE(02)
           MOVE "GINECOLOGISTA"    TO TBESPECIALIDADE(03)
           MOVE "PEDIATRA"         TO TBESPECIALIDADE(04)
           MOVE "CARDIOLOGISTA"    TO TBESPECIALIDADE(05)
           MOVE "NEUROCIRURGIAO"   TO TBESPECIALIDADE(06)
           MOVE "OBSTETRA"         TO TBESPECIALIDADE(07).
           
      *
       INC-OP0.
           OPEN I-O CADMED
           IF ST-ERRO NOT = "00"
                IF ST-ERRO = "30"
                      OPEN OUTPUT CADMED
                      CLOSE CADMED
                      MOVE "*** ARQUIVO CADMED SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO CADMED " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO DATANASCT CRM
                MOVE SPACES TO NOME EMAIL.
                DISPLAY (01, 01) ERASE.
                DISPLAY (01, 20) "BEM-VINDO AO CADASTRO DE MEDICOS"
                DISPLAY (04, 01) "CRM:             "
                DISPLAY (05, 01) "NOME:                "
                DISPLAY (06, 01) "EMAIL:               "
                DISPLAY (07, 01) "SEXO:          ".
                DISPLAY (08, 01) "ESPECIALIDADE:          ".
                DISPLAY (09, 01) "NASCIMENTO:          ".
             
       INC-002.
                ACCEPT  (04, 21) CRM
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADMED
                   GO TO ROT-FIM.
                IF CRM  = ZEROS
                   MOVE "*** CRM INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-CADMED01.
                MOVE 0 TO W-SEL
                READ CADMED
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY (04, 21) CRM
                      DISPLAY (05, 21) NOME
                      DISPLAY (06, 21) EMAIL
                      DISPLAY (07, 21) SEXO
                      DISPLAY (08, 21) TXTTPMEDICO
                      MOVE DATANASCT TO MASC1
                      DISPLAY (09, 21) MASC1

                      MOVE "*** MEDICO JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CADMED"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT (05, 21) NOME WITH UPDATE
                IF NOME  = SPACES
                   MOVE "*** NOME INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT (06, 21) EMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT (07, 21) SEXO.
       INC-005A.
                IF SEXO = "M" OR SEXO = "m"
                    MOVE "MASCULINO" TO TEXSEXO
                ELSE
                    IF SEXO = "F" OR SEXO = "f"
                    MOVE "FEMININO" TO TEXSEXO
                ELSE
                    MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-005.
                    DISPLAY(07, 21) TEXSEXO.        
       INC-006.
                ACCEPT (08, 21) TPESPECIALIDADE.
                IF TPESPECIALIDADE = 00 
                    MOVE "*** DIGITE APENAS DE 01 ATE 07 ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-006.            
       INC-006A.
                MOVE TBESPECIALIDADE(TPESPECIALIDADE) TO TXTTPMEDICO
                DISPLAY (08, 21) TXTTPMEDICO. 
       INC-007A.
                ACCEPT (09, 21) DATANASCT
                MOVE DATANASCT TO MASC1
                DISPLAY (09, 21) MASC1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.

      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCADMED
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** MEDICO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADMED"
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
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
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
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADMED RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO MENS
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
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCADMED
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 1500
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
