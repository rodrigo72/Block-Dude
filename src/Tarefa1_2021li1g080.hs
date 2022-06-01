module Tarefa1_2021li1g080 where

import LI12122

{- | A função validaPotencialMapa valida se um mapa é valido, verificando 5 condições:

1. Não haver mais do que uma declaração de peça para a mesma posição;
2. Declarar exactamente uma porta;
3. Todas as caixas devem estar posicionadas em cima de outra caixa ou
bloco, i.e. não podem haver caixas a “flutuar”;
4. Devem existir espaços vazios (no mínimo um), i.e. o mapa não pode
estar totalmente preenchido por caixas, blocos e porta.
5. A base do mapa deve ser composta por blocos, i.e. deve existir um
chão ao longo do mapa.

Para isso utiliza 5 funções, e todas têm de retornar Verdadeira para o mapa ser válido.
-}

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False 
validaPotencialMapa l = verificaPorta l == 1 && verificaPosicoes l && verificaCaixas l && verificaVazios l && verificaChao l 

{- | Verifica se existe exatamente uma porta, caso não exista retorna falso. -}

verificaPorta :: [(Peca, Coordenadas)] -> Int
verificaPorta [] = 0  
verificaPorta ((a,_):t) 
    | a == Porta = 1 + verificaPorta t 
    | otherwise = verificaPorta t 

{- | Conta as caixas que existem numa lista de pares de peças e coordenadas. -}

contaCaixas :: [(Peca, Coordenadas)] -> Int
contaCaixas [] = 0
contaCaixas ((a,_):t)
    | a == Caixa = 1 + contaCaixas t 
    | otherwise = contaCaixas t

{- | Recebe uma lista de peças e coordenadas e devolve apenas as coordenadas. -}

apenasPosicoes :: [(Peca, Coordenadas)] -> [Coordenadas]
apenasPosicoes [] = [] 
apenasPosicoes ((_, (x,y)):t) = (x,y): apenasPosicoes t 

{- | A função verificaPosicoes retorna True, caso não haja mais do que uma declaração de peça para a mesma posição, e False caso contrário.

São utilizadas duas funções auxiliares chamadas verificaPosicoes2 (nome muito original), e apenasPosicoes:
A funcao auxiliar verificaPosicoes retorna True, caso não haja mais do que uma declaração de peça para a mesma posição, e False caso contrário:

@
apenasPosicoes :: [(Peca, Coordenadas)] -> [Coordenadas]
apenasPosicoes [] = [] 
apenasPosicoes ((_, (x,y)):t) = (x,y): apenasPosicoes t 
@

A função auxilar verificaPosicoes2 recebe uma lista de posições e, caso as primeiras coordenadas estejam presentes no resto da lista, 
então retorna falso, caso contrário continua a verificar o resto da lista até a lista ficar vazia e retornar True.
-}

verificaPosicoes :: [(Peca, Coordenadas)] -> Bool 
verificaPosicoes l = verificaPosicoes2 (apenasPosicoes l)
    where verificaPosicoes2 :: [Coordenadas] -> Bool 
          verificaPosicoes2 [] = True
          verificaPosicoes2 ((x,y):t)
            | (x,y) `elem` t = False 
            | otherwise = verificaPosicoes2 t

{- | Recebe uma lista de peças e as respetivas coordenadas, e devolve apenas as caixas e as suas respetivas coordenadas. -}

apenasCaixas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
apenasCaixas [] = []
apenasCaixas ((h, (x,y)):t) 
    | h == Caixa = (h, (x,y)) : apenasCaixas t 
    | otherwise = apenasCaixas t 

{- | Remove da lista as peças que sejam Portas ou Caixas e juntamente com as suas coordenadas-}

removePortaVazio :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
removePortaVazio [] = []
removePortaVazio ((h,(x,y)):t) 
    | (h == Porta || h == Vazio) = t 
    | otherwise = (h,(x,y)) : removePortaVazio t 

{- | A função verificaCaixas verifica se todas as caixas estão posicionadas em cima de outra caixa ou bloco.
Para isso é usada uma função auxiliar que revebe uma lista apenas das caixas e outra lista sem a Porta.
Caso a primeira caixa da primeira lista esteja posicionada em cima de um bloco ou caixa da segunda lista, então a função retira essa caixa
da primeira lista e verifica a segunda caixa até à lista ficar vazio ou caso uma das caixas não corresponda à condição.
-}

verificaCaixas :: [(Peca, Coordenadas)] -> Bool 
verificaCaixas l = verificaCaixas2 (apenasCaixas l) (removePortaVazio l) || contaCaixas l == 0 
    where verificaCaixas2 :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Bool 
          verificaCaixas2 [] _ = True
          verificaCaixas2 ((h,(x,y)):t) l1
                | (x, y+1) `elem` (apenasPosicoes l1) = verificaCaixas2 t l1
                | otherwise = False 

{- | A função maiorX encontra o maior valor de x de uma lista de Peças e Coordenadas (adicionando 1 ao resultado final*). 
* de modo a que seja possível determinar a área na função verificaVazios.
-}

maiorX :: [(Peca, Coordenadas)] -> Int
maiorX [] = (-1)
maiorX l = maiorXApenasCoordenadas (apenasPosicoes l) 
    where maiorXApenasCoordenadas :: [Coordenadas] -> Int 
          maiorXApenasCoordenadas [(x,_)] = (x+1) 
          maiorXApenasCoordenadas ((x,y):(x1,y1):t) 
            | x >= x1 = maiorXApenasCoordenadas ((x,y):t)
            | otherwise = maiorXApenasCoordenadas ((x1,y1):t)

{- | A função maiorY encontra o maior valor de y de uma lista de Peças e Coordenadas (adicionando 1 ao resultado final*). 
* de modo a que seja possível determinar a área na função verificaVazios.
-}

maiorY :: [(Peca, Coordenadas)] -> Int
maiorY [] = (-1)
maiorY l = maiorYApenasCoordenadas (apenasPosicoes l) 
    where maiorYApenasCoordenadas :: [Coordenadas] -> Int 
          maiorYApenasCoordenadas [(_,y)] = (y+1) 
          maiorYApenasCoordenadas ((x,y):(x1,y1):t) 
            | y >= y1 = maiorYApenasCoordenadas ((x,y):t)
            | otherwise = maiorYApenasCoordenadas ((x1,y1):t)

{- | Retorna a lista sem vazios. -}

removeVazio :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
removeVazio [] = []
removeVazio ((h,(x,y)):t) 
    | h == Vazio = t 
    | otherwise = (h,(x,y)) : removePortaVazio t 

{- | A função verificaVazios testa se existe um ou mais espaços vazios no mapa. 

Compara o tamanho da lista com a área da grelha resultante da lista sem espaços vazios, mas com possíveis omissões.
Caso seja menor, então existem espaços vazios omitidos. Caso seja igual, então não existem espaços omitidos no mapa.
Para além disso, verifica-se também se existe a componente "Vazio" na lista, caso exista a função retorna True.
-}

verificaVazios :: [(Peca, Coordenadas)] -> Bool 
verificaVazios l = (length l) < (maiorX (removeVazio l)) * (maiorY (removeVazio l)) || verificaVazios2 l 
    where verificaVazios2 :: [(Peca, Coordenadas)] -> Bool
          verificaVazios2 [] = False 
          verificaVazios2 ((h, (x,y)):t)
            | h == Vazio = True 
            | otherwise = verificaVazios2 t 

{- | Retorna apenas os elementos com um certo valor de x de uma lista.-}

retiraColuna :: [(Peca, Coordenadas)] -> Int -> [(Peca, Coordenadas)]
retiraColuna [] n = []
retiraColuna ((h, (x,y)):t) n 
    | x == n = retiraColuna t n
    | otherwise = (h, (x,y)) : retiraColuna t n 

{- | Recebe uma lista de peças e as respetivas coordenadas, e devolve apenas os blocos e as suas respetivas coordenadas. -}

apenasBlocos :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
apenasBlocos [] = []
apenasBlocos ((h, (x,y)):t) 
    | h == Bloco = (h, (x,y)) : apenasBlocos t 
    | otherwise = apenasBlocos t 

{- | A função maiorY encontra o maior valor de y de uma lista de Peças e Coordenadas -}

maiorY2 :: [(Peca, Coordenadas)] -> Int
maiorY2 [] = (-1)
maiorY2 l = maiorYApenasCoordenadas (apenasPosicoes l) 
    where maiorYApenasCoordenadas :: [Coordenadas] -> Int 
          maiorYApenasCoordenadas [(_,y)] = (y) 
          maiorYApenasCoordenadas ((x,y):(x1,y1):t) 
            | y >= y1 = maiorYApenasCoordenadas ((x,y):t)
            | otherwise = maiorYApenasCoordenadas ((x1,y1):t)

{- | A função menorY encontra o menor valor de y de uma lista de Peças e Coordenadas -}

menorY :: [(Peca, Coordenadas)] -> Int
menorY [] = (-1)
menorY l = menorYApenasCoordenadas (apenasPosicoes l) 
    where menorYApenasCoordenadas :: [Coordenadas] -> Int 
          menorYApenasCoordenadas [(_,y)] = (y) 
          menorYApenasCoordenadas ((x,y):(x1,y1):t) 
            | y <= y1 = menorYApenasCoordenadas ((x,y):t)
            | otherwise = menorYApenasCoordenadas ((x1,y1):t)

{- | Recebe uma lista de peças e as respetivas coordenadas, e devolve apenas as peças pertencentes a uma determinada coluna. -}

apenasColuna :: [(Peca, Coordenadas)] -> Int -> [(Peca, Coordenadas)]
apenasColuna [] n = []
apenasColuna ((h, (x,y)):t) n 
    | x == n = (h, (x,y)) : apenasColuna t n
    | otherwise = apenasColuna t n 

{- | Recebe uma lista e retira o elemento cujas coordenadas são iguais às fornecidas. -}

eliminaElemento :: [(Peca, Coordenadas)] -> Coordenadas -> [(Peca, Coordenadas)]
eliminaElemento [] _ = []
eliminaElemento ((h, (x,y)):t) (x1,y1)
    | x == x1 && y == y1 = t 
    | otherwise = (h, (x,y)) : eliminaElemento t (x1,y1)

{- | A função verificaChao verifica se existe um chão continuo ao longo do mapa.

Para isso foi usado um sistema de prioridades: primeiro verifica se existem blocos na coluna seguinte na mesma linha, uma linha acima ou
uma linha abaixo, e, caso não existam, verifica se existe um bloco acima. 
Se não existir, então retorna falso. 
Se existir um bloco acima, verifica novamente os blocos da coluna seguinte próximos a esse bloco. 
Se existir um proximo, utiliza-se novamente a função nesse bloco, sendo a primeira coluna eliminada.
 -} 

verificaChao :: [(Peca, Coordenadas)] -> Bool 
verificaChao l = verificaChao2 (apenasBlocos l) 0 [] 0 && apenasColuna l 0 /= []
    where verificaChao2 :: [(Peca, Coordenadas)] -> Int -> [(Peca, Coordenadas)] -> Int -> Bool
          verificaChao2 l ac ac2 ac3
              | (ac+1, (maiorY2 (apenasColuna l ac)))     `elem` (apenasPosicoes l) = verificaChao2 (retiraColuna l ac) (ac+1) (apenasColuna l ac) 0 
              | (ac+1, (maiorY2 (apenasColuna l ac)) + 1) `elem` (apenasPosicoes l) = verificaChao2 (retiraColuna l ac) (ac+1) (apenasColuna l ac) 1
              | (ac+1, (maiorY2 (apenasColuna l ac)) - 1) `elem` (apenasPosicoes l) = verificaChao2 (retiraColuna l ac) (ac+1) (apenasColuna l ac) 0 
              | maiorY2 (apenasColuna l ac) > maiorY2 (apenasColuna l (ac+1)) && (ac, (maiorY2 l) - 1)     `elem` (apenasPosicoes l) = 
                verificaChao2 (eliminaElemento l (ac, (maiorY2 (apenasColuna l ac)))) ac ac2 ac3
              | maiorY2 (apenasColuna l (ac+1)) > ((maiorY2 ac2) + ac3)       && (ac, (maiorY2 ac2)+ac3+1) `elem` (apenasPosicoes l) = 
                verificaChao2 (eliminaElemento l (ac, (maiorY2 ac2)+ac3)) ac ac2 (ac3+1)
              | ac == ((maiorX l) - 1) = True
              | otherwise = False  
