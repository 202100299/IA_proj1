# Manual do Utilizador - Jogo de Tabuleiro com Cavalos

## Objetivos do Programa

O programa tem como objetivo resolver problemas específicos de um jogo de tabuleiro com cavalos em um tabuleiro 10x10. As funcionalidades principais incluem a leitura de tabuleiros a partir de um arquivo, a escolha de tabuleiros, a aplicação de algoritmos de busca e a análise de estatísticas de desempenho.

## Funcionamento Geral

O programa segue uma estrutura de menus, onde o utilizador pode escolher diferentes opções. As principais ações incluem a seleção de tabuleiros específicos, a recarga de tabuleiros, a aplicação de algoritmos de busca e a visualização de estatísticas.

## Utilização do Programa

### Menu Principal:

1. Computador contra o Computador:

    - Selecione esta opção para escolher Computador contra o Computador.

2. Jogador contra o Computador:

    - Selecione esta opção para escolher Jogador contra o Computador.

3. Sair:

    - Escolha esta opção para encerrar o programa.

## Computador contra o Computador:

- Se optar por escolher Computador contra o Computador, siga estas instruções:

    3.1. Insira o tempo máximo para cada jogada:

        - Insira o valor.

    3.2. Insira o caminho do ficheiro log.dat:

        - Insira o caminho.

## Estado Inicial:

1. A     B     C     D     E     F     G     H     I     J
  0   |  -1 |  74 |  50 |  90 |  56 |  42 |  58 |  69 |  20 |  25 |
  1   |  22 |  10 |  55 |  12 |  51 |  35 |   2 |  87 |   1 |  91 |
  2   |  24 |  80 |  47 |  71 |  86 |  27 |  57 |  36 |  54 |  19 |
  3   |  59 |  66 |  46 |  64 |  73 |  49 |  68 |  28 |  72 |  83 |
  4   |  92 |   8 |  67 |  85 |  62 |  31 |  39 |  38 |  29 |  94 |
  5   |  78 |  44 |  98 |  70 |  21 |  99 |   7 |  41 |   3 |  14 |
  6   |   5 |  37 |  76 |  89 |  18 |  53 |  32 |  34 |   0 |  84 |
  7   |  65 |  48 |  13 |  96 |  40 |  33 |  16 |   9 |  30 |  26 |
  8   |  63 |  82 | NIL |  75 |  77 |  95 |  15 |  60 |  17 |  43 |
  9   |  81 |  45 |  11 |  93 |  88 |  23 |  52 |   6 |  61 |   4 |
  

2. Player 1 joga - Cavalo Preto a jogar

Player- 1 
  Caminho - Inicio -> A0
  Pontos  - 97 pts

Player- 2
  Caminho - Inicio 
  Pontos  - 0 pts

3. Estatisticas da primeira jogada

Tempo de Execucao: 469 milisegundos
Nos Analisados: 542
Cortes Alfa: 60
Cortes Beta: 40


## Estado Final:

A     B     C     D     E     F     G     H     I     J
  0   |  22 | NIL | NIL |  56 | NIL | NIL | NIL | NIL | NIL | NIL |
  1   | NIL |  65 | NIL | NIL |  12 |   0 | NIL | NIL |   3 |  31 |
  2   | NIL | NIL | NIL | NIL | NIL | NIL | NIL | NIL | NIL | NIL |
  3   | NIL | NIL | NIL | NIL | NIL | NIL | NIL | NIL |  20 | NIL |
  4   | NIL | NIL |  32 | NIL |  23 | NIL | NIL | NIL | NIL | NIL |
  5   | NIL |  -2 | NIL | NIL | NIL | NIL | NIL | NIL | NIL |   2 |
  6   | NIL | NIL | NIL | NIL | NIL | NIL | NIL | NIL |  42 | NIL |
  7   | NIL | NIL | NIL |  -1 | NIL | NIL | NIL | NIL |   8 |  80 |
  8   | NIL | NIL | NIL | NIL | NIL | NIL | NIL |  24 |  21 | NIL |
  9   | NIL | NIL | NIL | NIL | NIL | NIL | NIL |  13 | NIL |  30 |


1. Player 1 joga - Cavalo Preto a jogar


Player- 1 
  Caminho - Inicio -> E0 -> D2 -> B3 -> C5 -> B7 -> A9 -> C8 -> E7 -> F9 -> D8 -> E6 -> G7 -> F5 -> E3 -> G4 -> E5 -> F7 -> G9 -> E8 -> C9 -> D7
  Pontos  - 1404 pts

Player- 2
  Caminho - Inicio -> I9 -> G8 -> F6 -> D5 -> F4 -> G2 -> H0 -> I2 -> G1 -> E2 -> F0 -> D1 -> B0 -> A2 -> B4 -> A6 -> B8 -> C6 -> A7 -> B5
  Pontos  - 1166 pts

2. Estatisticas do lance final

Tempo de Execucao: 0 milisegundos
Nos Analisados: 1
Cortes Alfa: 0
Cortes Beta: 0

O jogo terminou!
NIL


## Jogador contra o Computador:

- Se optar por escolher Jogador contra o Computador, siga estas instruções:

    3.1. Insira o tempo máximo para cada jogada:

        - Insira o valor.

    3.2. Insira o caminho do ficheiro log.dat:

        - Insira o caminho.

    3.3. Insira o Cavalo com que deseja jogar:

        - Insira o (P) ou (B).


## Estado Inicial:

A     B     C     D     E     F     G     H     I     J
  0   |  69 |  73 |  67 |  15 |  54 |  32 |  23 |  26 |  88 |  44 |
  1   |  89 |  24 |  22 |  12 |  96 |  13 |  40 |  86 |  27 |  77 |
  2   |  16 |  10 |  48 |  81 |  79 |  92 |  64 |  38 |  46 |   1 |
  3   |  90 |  50 |  80 |   8 |  20 |  45 |  97 |   6 |   9 |  87 |
  4   |  58 |  93 |  39 |  14 |   2 |  68 |  35 |  82 |  57 |  41 |
  5   |  43 |  53 |   0 |   4 |  91 |   7 |  85 |  19 |  29 |  61 |
  6   |  42 |  75 |  56 |  62 |  72 |  74 |  33 |  59 |  36 |  66 |
  7   |  37 |  28 |  70 |  51 |  78 |  25 |  47 |  21 |  98 |  30 |
  8   |  55 |  95 |  31 |  18 |  52 |  83 |  84 |  60 |  99 |  34 |
  9   |  71 |  63 |  17 |  94 |   3 |  11 |  49 |  65 |   5 |  76 |

1. Player 1 joga - Cavalo Branco a jogar


Player- 1 
  Caminho - Inicio 
  Pontos  - 0 pts

Player- 2
  Caminho - Inicio 
  Pontos  - 0 pts

- Qual e a sua opcao
-- i

## Estado de jogo numa rodada

 A     B     C     D     E     F     G     H     I     J
  0   |  69 |  73 |  67 |  15 |  54 |  32 |  23 |  26 | NIL |  44 |
  1   |  89 |  24 |  22 |  12 |  96 |  13 |  -1 |  86 |  27 |  77 |
  2   |  16 |  10 |  48 |  81 |  79 |  92 |  64 |  38 |  46 |   1 |
  3   |  90 |  50 |  80 |   8 |  20 |  45 |  97 |   6 |   9 | NIL |
  4   |  58 |  93 |  39 |  14 |   2 |  68 |  35 |  82 |  57 |  41 |
  5   |  43 |  53 |   0 | NIL |  91 |   7 |  85 |  19 |  29 |  61 |
  6   |  42 |  75 |  56 |  62 |  72 |  74 |  33 |  59 |  36 |  66 |
  7   |  37 |  28 |  70 |  51 |  -2 |  25 |  47 |  21 |  98 |  30 |
  8   |  55 |  95 |  31 |  18 |  52 |  83 |  84 |  60 | NIL |  34 |
  9   |  71 |  63 |  17 | NIL |   3 |  11 | NIL |  65 |   5 |  76 |
  

1. Player 1 a jogar - Cavalo Branco a jogar

Player- 1 
  Caminho - Inicio -> I0 -> G1
  Pontos  - 128 pts

Player- 2
  Caminho - Inicio -> D9 -> E7
  Pontos  - 172 pts

2. Estatisticas da jogada

Tempo de Execucao: 675 milisegundos
Nos Analisados: 956
Cortes Alfa: 109
Cortes Beta: 38