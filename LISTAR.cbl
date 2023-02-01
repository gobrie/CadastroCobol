      ******************************************************************
      * Author:GABRIEL CERQUEIRA
      * Date: 26/01/2023
      * Purpose:CADASTRAR USUARIOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISTAR.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
                SELECT USUARIOS ASSIGN TO
                "C:\Users\gasilva\PROJETO001\USUARIOS.DAT"
                ORGANISATION IS INDEXED
                ACCESS  MODE IS SEQUENTIAL
                RECORD  KEY  IS ID-USUARIO
                FILE STATUS  IS WS-FS.


       DATA DIVISION.
       FILE SECTION.
       FD USUARIOS.
           COPY LAYOUT.
       WORKING-STORAGE SECTION.
       01 WS-DADOS                PIC X(220) VALUE SPACES.
       01 FILLER REDEFINES WS-DADOS.
          03 WS-NOME              PIC X(100).
          03 WS-PASSWORD          PIC X(8).
          03 WS-EMAIL             PIC X(100).
          03 WS-PHONE             PIC 9(12).
          03 WS-ID-USUARIO        PIC 99.
       77 WS-FS                   PIC 99.
          88 FS-OK                VALUE 0.
       77 WS-OPCAO                PIC X.
       77 WS-COUNT                PIC 9(003) VALUE ZEROS.
       77 WS-EOF                  PIC X.
          88 EOF-OK               VALUE "S" FALSE "N".
       77 WS-EXIT                 PIC X.
          88 EXIT-OK              VALUE "F" FALSE "N".
       01 CHARS.
           03 WS-NAME             PIC X(20).
           03 WS-LAST-NAME        PIC X(20).
           03 WS-USER             PIC X(25).
           03 WS-DOMAIN           PIC X(25).
       01 AUXILIARES.
           05 WS-COUNT-DOM        PIC X.
           05 WS-BRA              PIC X(20) VALUE "bradesco.com".
           05 WS-CAP              PIC X(20) VALUE "capgemini.com".
       88 WS-LOWER-CASE           VALUE "a" THRU "i",
                                        "j" THRU "r",
                                        "s" THRU "z".
       88 WS-UPPER-CASE           VALUE "A" THRU "I",
                                        "J" THRU "R",
                                        "S" THRU "Z".
       88 WS-NUMBERS-ONLY         VALUE "0" THRU "9".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "***   LISTA DE CONTATOS     ***"
            SET EXIT-OK                TO FALSE
            PERFORM P300-CADASTRA      THRU P300-FIM
            PERFORM P900-FIM
            .

       P300-CADASTRA.
            SET EOF-OK             TO FALSE
            SET FS-OK              TO TRUE
            SET WS-COUNT           TO 0.

            OPEN INPUT USUARIOS


      *LOGICA DE LEITURA
            IF FS-OK THEN
               PERFORM UNTIL EOF-OK
                  READ USUARIOS INTO WS-DADOS
                       AT END
                          SET EOF-OK TO TRUE
                       NOT AT END
                           ADD 1 TO  WS-COUNT
                       DISPLAY "ID: "       WS-ID-USUARIO
                       DISPLAY "Nome: "     WS-NOME
                       DISPLAY "Telefone: " WS-PHONE
                       DISPLAY "E-mail: "   WS-EMAIL
                       DISPLAY "Senha: "    WS-PASSWORD
                       DISPLAY "----------*------------"
                  END-READ
                  END-PERFORM
            ELSE
                 DISPLAY "ERRO AO ABRIR O ARQUIVO DE CONTATOS."
                 DISPLAY "FILE STATUS: " WS-FS

            END-IF

            CLOSE USUARIOS


            .
            P300-FIM.
            P900-FIM.
            STOP RUN.
       END PROGRAM LISTAR.
