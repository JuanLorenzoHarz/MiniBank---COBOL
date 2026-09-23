# MiniBank COBOL

Simulador didatico de uma conta bancaria em GnuCOBOL (formato livre).

## Compilacao (MSYS2 UCRT64 ou outro ambiente com GnuCOBOL)

```bash
cobc -x -free -o banco.exe banco.cob
./banco.exe
```

No Linux, voce pode gerar `banco` em vez de `banco.exe`.

Execute o programa na pasta do projeto: os arquivos sao relativos ao diretorio atual.

## Arquivos gerados automaticamente

- `saldo.txt`: saldo em centavos, como numero de 11 digitos. Exemplo: R$ 150,25 vira `00000015025`.
- `historico.txt`: uma linha de texto por operacao confirmada; a conta destino e registrada nas transferencias.

Se `saldo.txt` nao existir na primeira execucao, o saldo inicial sera R$ 0,00. Os arquivos sao criados na primeira operacao aprovada. Nao edite o arquivo de saldo manualmente. Nao execute duas instancias do MiniBank ao mesmo tempo.

## Testes manuais

1. Na primeira execucao, consultar saldo: R$ 0,00.
2. Depositar `500.00`, fechar (opcao `0`), reabrir e consultar saldo: R$ 500,00.
3. Transferir `150.00` para `123456`, fechar, reabrir: saldo de R$ 350,00 e duas linhas no extrato.
4. Tentar depositar `10.999`, `0.001`, `-50`, `abc` e `0`: todos devem ser recusados sem alterar saldo/historico.
5. Tentar transferir para `ABC123` ou conta vazia: recusar sem alterar saldo.
6. Tentar sacar acima do saldo: recusar sem alterar saldo.
7. Fazer mais de 100 movimentacoes e confirmar que o arquivo de historico continua recebendo registros.

## Limitacoes

Projeto didatico: nao verifica existencia real da conta destino nem implementa transacoes atomicas ou recuperacao de falhas de disco. O arquivo de saldo e sobrescrito a cada operacao confirmada. Em caso de falha de energia/gravacao, pode ficar inconsistente; para uso real seriam necessarios mecanismos mais robustos. Para manter saldo e extrato consistentes, evite editar os arquivos diretamente.