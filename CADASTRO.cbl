      ******************************************************************
      * Author:GABRIEL CERQUEIRA
      * Date:
      * Purpose:CADASTRAR USUARIOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADASTRO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
                SELECT USUARIOS ASSIGN TO
                "C:\Users\gasilva\PROJETO001\USUARIOS.DAT"
                ORGANISATION IS INDEXED
                ACCESS  MODE IS RANDOM
                RECORD  KEY  IS ID-USUARIO
                FILE STATUS  IS WS-FS.


       DATA DIVISION.
       FILE SECTION.
       FD USUARIOS.
           COPY LAYOUT.
       WORKING-STORAGE SECTION.
       01 WS-DADOS                PIC X(220) VALUE SPACES.
       01 FILLER REDEFINES WS-DADOS.
          03 WS-NOME                  PIC X(100).
          03 WS-PASSWORD              PIC X(8).
          03 WS-EMAIL                 PIC X(100).
          03 WS-PHONE                 PIC 9(12).
          03 WS-ID-USUARIO            PIC 99.
       77 WS-FS                   PIC 99.
          88 FS-OK                VALUE 0.
       77 WS-EXIT                 PIC X.
          88 EXIT-OK              VALUE "F" FALSE "N".
       77 WS-COUNT                PIC 9.
       77 WS-EOF                  PIC X.
          88 EOF-OK               VALUE "S" FALSE "N".
       01 CHARS.
           03 WS-NAME             PIC X(20).
           03 WS-LAST-NAME        PIC X(20).
           03 WS-USER             PIC X(25).
           03 WS-DOMAIN           PIC X(25).
       01 AUXILIARES.
           05 WS-COUNT-DOM        PIC X.
           05 WS-BRA              PIC X(20) VALUE "bradesco.com".
           05 WS-CAP              PIC X(20) VALUE "capgemini.com".
       01 SENHA.
           05 WS-UPPER-CASE          PIC 9.
           05 WS-LOWER-CASE          PIC 9.
           05 WS-SPECIAL-CHAR        PIC 9.
           05 WS-NUMBER-ONLY         PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            SET EXIT-OK           TO FALSE
            PERFORM P100-DADOS THRU P100-FIM UNTIL EXIT-OK.
            PERFORM P999-FIM.

            P000-ERRO.
               DISPLAY "**********************************************"
               DISPLAY "*              DADO INCORRENTO                *"
               DISPLAY "*                                             *"
               DISPLAY "* Verfique se:                                *"
               DISPLAY "* - Seu nome tem pelo menos 2 palavras        *"
               DISPLAY "* - Sua senha tem: 8 caracteres, 1 numero,    *"
               DISPLAY "*  1 letra maiuscula, 1 letra minúscula       *"
               DISPLAY "*  e um caracter especial                     *"
               DISPLAY "* - Seu E-Mail tem: 10 caracteres, um '@'     *"
               DISPLAY "*  um caractere antes do '@' e pertence       *"
               DISPLAY "*  ao dominio 'capgemini.com' ou 'badesco.com *"
               DISPLAY "* - Seu telefone tem no minimo 11 caracteres  *"
               DISPLAY "*  e no maximo 12 caracteres. Ex: 999999999999*"
               DISPLAY "*                                             *"
               DISPLAY "*             TENTE NOVAMENTE                 *"
               DISPLAY "*                                             *"
               DISPLAY "**********************************************"
               PERFORM P100-DADOS
            .
            P100-DADOS.
               SET FS-OK               TO TRUE
               SET EOF-OK              TO FALSE

               DISPLAY "Informe seu ID: "
               ACCEPT WS-ID-USUARIO

               DISPLAY "Informe seu nome e sobrenome: "
               ACCEPT WS-NOME.

               DISPLAY "Informe sua senha: "
               ACCEPT WS-PASSWORD.

               DISPLAY "Informe seu e-mail: "
               ACCEPT WS-EMAIL.

               DISPLAY "Digite seu telefone: "
               ACCEPT WS-PHONE


      ********************* VALIDANDO NOME *****************************
               UNSTRING WS-NOME DELIMITED BY " " INTO
                   WS-NAME
                   WS-LAST-NAME
               END-UNSTRING
               IF WS-LAST-NAME IS EQUAL " " THEN
                   PERFORM P000-ERRO
               END-IF

      *********************** VALIDANDO E-MAIL ************************
               UNSTRING WS-EMAIL DELIMITED BY "@" INTO
                   WS-USER
                   WS-DOMAIN
               END-UNSTRING

               IF WS-EMAIL IS LESS THAN 10 THEN
                   PERFORM P000-ERRO
               END-IF

               IF WS-USER IS LESS THAN 1 THEN
                   PERFORM P000-ERRO
               END-IF

               IF WS-DOMAIN IS NOT EQUAL TO WS-BRA AND WS-CAP THEN
                   PERFORM P000-ERRO
               END-IF

      *********************** VALIDANDO SENHA **************************

               IF WS-PASSWORD IS LESS THAN 8 THEN
                   PERFORM P000-ERRO
               END-IF

               INSPECT WS-PASSWORD TALLYING WS-UPPER-CASE
               FOR ALL "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "L" "M"
               "N" "O" "P" "Q" "R" "S" "T" "U" "V" "X" "Z" "Y"
               IF WS-UPPER-CASE IS LESS THAN 1 THEN
                   PERFORM P000-ERRO
               END-IF

               INSPECT WS-PASSWORD TALLYING WS-LOWER-CASE
               FOR ALL "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "l" "m"
               "n" "o" "p" "q" "r" "s" "t" "u" "v" "x" "z" "y"
               IF WS-LOWER-CASE IS LESS THAN 1 THEN
                   PERFORM P000-ERRO
               END-IF

               INSPECT WS-PASSWORD TALLYING WS-NUMBER-ONLY
               FOR ALL "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
               IF WS-NUMBER-ONLY IS LESS THAN 1 THEN
                   PERFORM P000-ERRO
               END-IF

               INSPECT WS-PASSWORD TALLYING WS-SPECIAL-CHAR
               FOR ALL "!" "@" "#" "$" "%" "¨" "&" "*" "(" ")" "-" "+"
               "_" "=" "." "?" "{" "}" "´" "`" "|" "/" "\" "]" "["
               IF WS-SPECIAL-CHAR IS LESS THAN 1 THEN
                   PERFORM P000-ERRO
               END-IF

      *********************** VALIDANDO TELEFONE ***********************

               IF (WS-PHONE IS LESS THAN 11 AND GREATER THAN 12) THEN
                   PERFORM P000-ERRO
               END-IF

      ********************** INSERINDO OS DADOS ************************

               OPEN I-O USUARIOS

               IF WS-FS EQUAL 35 THEN
                   OPEN OUTPUT USUARIOS
               END-IF


               IF FS-OK THEN
                   MOVE WS-NOME            TO NOME
                   MOVE WS-PASSWORD        TO PASSWORD
                   MOVE WS-EMAIL           TO EMAIL
                   MOVE WS-PHONE           TO PHONE
                   MOVE WS-ID-USUARIO      TO ID-USUARIO

                   WRITE DADOS
                       INVALID KEY
                           DISPLAY "Contato já cadastrado."
                       NOT INVALID KEY
                           DISPLAY "Contado gravado com sucesso!"
              ELSE
                       DISPLAY "Erro ao abrir o arquivo de usuarios."
                       DISPLAY "FILE STATUS: " WS-FS
              END-IF

              CLOSE USUARIOS



               DISPLAY WS-COUNT
               DISPLAY "Deseja continuar? <digite qualquer tecla> "
               DISPLAY "Digite <F> para sair."
               ACCEPT WS-EXIT

            .
            P100-FIM.
            P999-FIM.
            STOP RUN.
       END PROGRAM CADASTRO.
