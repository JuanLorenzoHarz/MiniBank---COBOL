       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINIBANK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-SALDO
               ASSIGN TO "saldo.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SALDO.
           SELECT ARQ-HISTORICO
               ASSIGN TO "historico.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-HISTORICO.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-SALDO.
       01 REG-SALDO             PIC 9(11).

       FD ARQ-HISTORICO.
       01 REG-HISTORICO         PIC X(100).

       WORKING-STORAGE SECTION.
       01 FS-SALDO             PIC XX.
       01 FS-HISTORICO         PIC XX.
       01 WS-SALDO             PIC 9(9)V99 VALUE ZERO.
       01 WS-NOVO-SALDO        PIC 9(9)V99 VALUE ZERO.
       01 WS-CANDIDATO         PIC 9(10)V99 VALUE ZERO.
       01 WS-VALOR             PIC 9(7)V99 VALUE ZERO.
       01 WS-ENTRADA           PIC X(20).
       01 WS-OPCAO             PIC X VALUE SPACE.
       01 WS-VALOR-VALIDO      PIC 9 VALUE ZERO.
       01 WS-CONTA-DESTINO     PIC X(20) VALUE SPACES.
       01 WS-CONTA-VALIDA      PIC 9 VALUE ZERO.
       01 WS-TIPO-OPERACAO     PIC X(13).
       01 WS-SALDO-EDIT        PIC Z(8)9.99.
       01 WS-VALOR-EDIT        PIC Z(6)9.99.
       01 WS-TAMANHO           PIC 99 VALUE ZERO.
       01 WS-INTEIROS          PIC 99 VALUE ZERO.
       01 WS-DECIMAIS          PIC 99 VALUE ZERO.
       01 WS-PONTO             PIC 9 VALUE ZERO.
       01 WS-I                 PIC 99 VALUE ZERO.
       01 WS-CARACTERE         PIC X.
       01 WS-FIM               PIC 9 VALUE ZERO.
       01 WS-SALVO             PIC 9 VALUE ZERO.

       PROCEDURE DIVISION.
       PRINCIPAL.
           PERFORM CARREGAR-SALDO
           PERFORM UNTIL WS-OPCAO = "0"
               DISPLAY SPACE
               DISPLAY "===== MINIBANK COBOL ====="
               DISPLAY "1 - Consultar saldo"
               DISPLAY "2 - Depositar"
               DISPLAY "3 - Sacar"
               DISPLAY "4 - Consultar extrato"
               DISPLAY "5 - Transferir"
               DISPLAY "0 - Sair"
               DISPLAY "Escolha uma opcao: "
               ACCEPT WS-OPCAO
               EVALUATE WS-OPCAO
                   WHEN "1" PERFORM CONSULTAR-SALDO
                   WHEN "2" PERFORM DEPOSITAR
                   WHEN "3" PERFORM SACAR
                   WHEN "4" PERFORM EXTRATO
                   WHEN "5" PERFORM TRANSFERIR
                   WHEN "0" DISPLAY "Encerrando programa..."
                   WHEN OTHER DISPLAY "Opcao invalida!"
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       CARREGAR-SALDO.
           OPEN INPUT ARQ-SALDO
           EVALUATE FS-SALDO
               WHEN "35"
                   MOVE ZERO TO WS-SALDO
               WHEN "00"
                   READ ARQ-SALDO
                   IF FS-SALDO = "00" AND REG-SALDO IS NUMERIC
                       COMPUTE WS-SALDO = REG-SALDO / 100
                   ELSE
                       DISPLAY "ERRO: saldo.txt vazio ou invalido."
                       CLOSE ARQ-SALDO
                       STOP RUN
                   END-IF
                   CLOSE ARQ-SALDO
                   IF FS-SALDO NOT = "00"
                       DISPLAY "ERRO ao fechar saldo.txt: "
                           FS-SALDO
                       STOP RUN
                   END-IF
               WHEN OTHER
                   DISPLAY "ERRO ao abrir saldo.txt: " FS-SALDO
                   STOP RUN
           END-EVALUATE.

       CONSULTAR-SALDO.
           MOVE WS-SALDO TO WS-SALDO-EDIT
           DISPLAY "Saldo atual: R$ "
               FUNCTION TRIM(WS-SALDO-EDIT).

       LER-VALOR.
           MOVE ZERO TO WS-VALOR WS-VALOR-VALIDO
           MOVE ZERO TO WS-INTEIROS WS-DECIMAIS WS-PONTO
           DISPLAY "Informe o valor (ex: 50.00): "
           ACCEPT WS-ENTRADA
           MOVE FUNCTION TRIM(WS-ENTRADA) TO WS-ENTRADA
           COMPUTE WS-TAMANHO =
               FUNCTION LENGTH(FUNCTION TRIM(WS-ENTRADA))
           IF WS-TAMANHO = ZERO
               DISPLAY "Valor invalido!"
               EXIT PARAGRAPH
           END-IF
           MOVE 1 TO WS-VALOR-VALIDO
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TAMANHO
               MOVE WS-ENTRADA(WS-I:1) TO WS-CARACTERE
               EVALUATE TRUE
                   WHEN WS-CARACTERE >= "0"
                        AND WS-CARACTERE <= "9"
                       IF WS-PONTO = 0
                           ADD 1 TO WS-INTEIROS
                       ELSE
                           ADD 1 TO WS-DECIMAIS
                       END-IF
                   WHEN WS-CARACTERE = "." AND WS-PONTO = 0
                       MOVE 1 TO WS-PONTO
                   WHEN OTHER
                       MOVE ZERO TO WS-VALOR-VALIDO
               END-EVALUATE
           END-PERFORM
           IF WS-VALOR-VALIDO = 0
               OR WS-INTEIROS = 0
               OR WS-INTEIROS > 7
               OR WS-DECIMAIS > 2
               OR (WS-PONTO = 1 AND WS-DECIMAIS = 0)
               DISPLAY "Valor invalido: use ate 7 digitos"
                   " inteiros e 2 casas decimais."
               MOVE ZERO TO WS-VALOR-VALIDO
               EXIT PARAGRAPH
           END-IF
           IF FUNCTION NUMVAL(WS-ENTRADA) <= ZERO
               DISPLAY "O valor deve ser pelo menos R$ 0.01!"
               MOVE ZERO TO WS-VALOR-VALIDO
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-VALOR = FUNCTION NUMVAL(WS-ENTRADA).

       VALIDAR-CONTA.
           MOVE ZERO TO WS-CONTA-VALIDA
           MOVE FUNCTION TRIM(WS-CONTA-DESTINO)
               TO WS-CONTA-DESTINO
           COMPUTE WS-TAMANHO =
               FUNCTION LENGTH(FUNCTION TRIM(WS-CONTA-DESTINO))
           IF WS-TAMANHO = ZERO
               DISPLAY "Numero de conta invalido!"
               EXIT PARAGRAPH
           END-IF
           MOVE 1 TO WS-CONTA-VALIDA
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TAMANHO
               MOVE WS-CONTA-DESTINO(WS-I:1) TO WS-CARACTERE
               IF WS-CARACTERE < "0" OR WS-CARACTERE > "9"
                   MOVE ZERO TO WS-CONTA-VALIDA
               END-IF
           END-PERFORM
           IF WS-CONTA-VALIDA = 0
               DISPLAY "Conta destino deve conter so digitos."
           END-IF.

       DEPOSITAR.
           PERFORM LER-VALOR
           IF WS-VALOR-VALIDO = 1
               COMPUTE WS-CANDIDATO = WS-SALDO + WS-VALOR
               IF WS-CANDIDATO > 999999999.99
                   DISPLAY "Limite de saldo excedido!"
               ELSE
                   MOVE WS-CANDIDATO TO WS-NOVO-SALDO
                   MOVE "DEPOSITO" TO WS-TIPO-OPERACAO
                   PERFORM CONFIRMAR-OPERACAO
               END-IF
           END-IF.

       SACAR.
           PERFORM LER-VALOR
           IF WS-VALOR-VALIDO = 1
               IF WS-VALOR > WS-SALDO
                   DISPLAY "Saldo insuficiente!"
               ELSE
                   COMPUTE WS-NOVO-SALDO = WS-SALDO - WS-VALOR
                   MOVE "SAQUE" TO WS-TIPO-OPERACAO
                   PERFORM CONFIRMAR-OPERACAO
               END-IF
           END-IF.

       TRANSFERIR.
           DISPLAY "Informe o numero da conta destino: "
           ACCEPT WS-CONTA-DESTINO
           PERFORM VALIDAR-CONTA
           IF WS-CONTA-VALIDA = 1
               PERFORM LER-VALOR
               IF WS-VALOR-VALIDO = 1
                   IF WS-VALOR > WS-SALDO
                       DISPLAY "Saldo insuficiente!"
                   ELSE
                       COMPUTE WS-NOVO-SALDO = WS-SALDO - WS-VALOR
                       MOVE "TRANSFERENCIA" TO WS-TIPO-OPERACAO
                       PERFORM CONFIRMAR-OPERACAO
                   END-IF
               END-IF
           END-IF.

       CONFIRMAR-OPERACAO.
           PERFORM SALVAR-SALDO
           IF WS-SALVO = 1
               MOVE WS-NOVO-SALDO TO WS-SALDO
               PERFORM REGISTRAR-OPERACAO
               EVALUATE WS-TIPO-OPERACAO
                   WHEN "DEPOSITO" DISPLAY "Deposito realizado!"
                   WHEN "SAQUE" DISPLAY "Saque realizado!"
                   WHEN "TRANSFERENCIA"
                       DISPLAY "Transferencia para a conta "
                           FUNCTION TRIM(WS-CONTA-DESTINO)
                           " realizada!"
               END-EVALUATE
               PERFORM CONSULTAR-SALDO
           ELSE
               DISPLAY "ERRO: operacao nao confirmada."
               DISPLAY "Confira saldo.txt antes de tentar de novo."
               STOP RUN
           END-IF.

       SALVAR-SALDO.
           MOVE ZERO TO WS-SALVO
           OPEN OUTPUT ARQ-SALDO
           IF FS-SALDO NOT = "00"
               DISPLAY "ERRO ao abrir saldo.txt: " FS-SALDO
               EXIT PARAGRAPH
           END-IF
           COMPUTE REG-SALDO = WS-NOVO-SALDO * 100
           WRITE REG-SALDO
           IF FS-SALDO NOT = "00"
               DISPLAY "ERRO ao gravar saldo.txt: " FS-SALDO
               CLOSE ARQ-SALDO
               EXIT PARAGRAPH
           END-IF
           CLOSE ARQ-SALDO
           IF FS-SALDO = "00"
               MOVE 1 TO WS-SALVO
           ELSE
               DISPLAY "ERRO ao fechar saldo.txt: " FS-SALDO
           END-IF.

       REGISTRAR-OPERACAO.
           OPEN EXTEND ARQ-HISTORICO
           IF FS-HISTORICO = "35"
               OPEN OUTPUT ARQ-HISTORICO
           END-IF
           IF FS-HISTORICO NOT = "00"
               DISPLAY "AVISO: extrato indisponivel: "
                   FS-HISTORICO
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO REG-HISTORICO
           MOVE WS-VALOR TO WS-VALOR-EDIT
           EVALUATE WS-TIPO-OPERACAO
               WHEN "TRANSFERENCIA"
                   STRING
                       FUNCTION TRIM(WS-TIPO-OPERACAO)
                           DELIMITED BY SIZE
                       " | R$ " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-VALOR-EDIT)
                           DELIMITED BY SIZE
                       " | CONTA: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-CONTA-DESTINO)
                           DELIMITED BY SIZE
                       INTO REG-HISTORICO
                   END-STRING
               WHEN OTHER
                   STRING
                       FUNCTION TRIM(WS-TIPO-OPERACAO)
                           DELIMITED BY SIZE
                       " | R$ " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-VALOR-EDIT)
                           DELIMITED BY SIZE
                       INTO REG-HISTORICO
                   END-STRING
           END-EVALUATE
           WRITE REG-HISTORICO
           IF FS-HISTORICO NOT = "00"
               DISPLAY "AVISO: nao foi possivel registrar extrato: "
                   FS-HISTORICO
           END-IF
           CLOSE ARQ-HISTORICO
           IF FS-HISTORICO NOT = "00"
               DISPLAY "AVISO: erro ao fechar historico.txt: "
                   FS-HISTORICO
           END-IF.

       EXTRATO.
           DISPLAY "===== EXTRATO ====="
           OPEN INPUT ARQ-HISTORICO
           EVALUATE FS-HISTORICO
               WHEN "35"
                   DISPLAY "Nenhuma movimentacao registrada."
               WHEN "00"
                   MOVE ZERO TO WS-FIM
                   PERFORM UNTIL WS-FIM = 1
                       READ ARQ-HISTORICO
                       EVALUATE FS-HISTORICO
                           WHEN "00"
                               DISPLAY FUNCTION TRIM(
                                   REG-HISTORICO TRAILING)
                           WHEN "10" MOVE 1 TO WS-FIM
                           WHEN OTHER
                               DISPLAY "ERRO lendo extrato: "
                                   FS-HISTORICO
                               MOVE 1 TO WS-FIM
                       END-EVALUATE
                   END-PERFORM
                   CLOSE ARQ-HISTORICO
                   IF FS-HISTORICO NOT = "00"
                       DISPLAY "ERRO ao fechar extrato: "
                           FS-HISTORICO
                   END-IF
               WHEN OTHER
                   DISPLAY "ERRO ao abrir extrato: " FS-HISTORICO
           END-EVALUATE
           PERFORM CONSULTAR-SALDO.