module Tarefa2_2021li1g080 where

import LI12122
import Tarefa1_2021li1g080

{- | A função retiraLinha retira uma linha específica de uma lista. -}

retiraLinha :: [(Peca, Coordenadas)] -> Int -> [(Peca, Coordenadas)]
retiraLinha [] n = []
retiraLinha ((h, (x,y)):t) n 
    | y /= n = retiraLinha t n 
    | otherwise = (h, (x,y)) : retiraLinha t n 

{- | A função maiorX2 encontra o maior valor de x de uma lista de Peças e Coordenadas. -}

maiorX2 :: [(Peca, Coordenadas)] -> Int
maiorX2 [] = (-1)
maiorX2 l = maiorXApenasCoordenadas (apenasPosicoes l) 
    where maiorXApenasCoordenadas :: [Coordenadas] -> Int 
          maiorXApenasCoordenadas [(x,_)] = x 
          maiorXApenasCoordenadas ((x,y):(x1,y1):t) 
            | x >= x1 = maiorXApenasCoordenadas ((x,y):t)
            | otherwise = maiorXApenasCoordenadas ((x1,y1):t)

{- | A função ordenaLinha ordena elementos da mesma linha, utilizando o algoritmo Insertion Sort, ou seja, a função insere apresentada abaixo. -}

ordenaLinha :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaLinha [] = []
ordenaLinha ((h, (x,y)):t) = insere (h, (x,y)) (ordenaLinha t)

{- | A função insere insere uma peça e as respetivas coordenadas ordenadamente numa lista de peças e coordenadas já ordenada.-}

insere :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
insere x [] = [x]
insere (h, (x,y)) ((h1, (x1,y1)):t)
    | x <= x1 = (h, (x,y)):(h1, (x1,y1)):t
    | otherwise = (h1, (x1,y1)) : insere  (h, (x,y)) t 

{- | A função constroiLinha, constroi uma linha do mapa, criando uma lista ordenada de peças e adicionando vazios quando há espaços vazios omitidos na lista recebida. -}

constroiLinha :: [(Peca, Coordenadas)] ->  Int -> Int -> [Peca]
constroiLinha [] ac ac2 
    | ac <= ac2 = Vazio : constroiLinha [] (ac+1) ac2
    | otherwise = []
constroiLinha l@((h, (x,y)):t) ac ac2
    | ac > ac2 = [] 
    | ((ac,y) `elem` (apenasPosicoes l)) = h : constroiLinha t (ac+1) ac2
    | otherwise = Vazio : constroiLinha l (ac+1) ac2

{- | A função constroiMapa utiliza a funçao constroiLinha para construir todas as linhas do mapa, e junta-as numa lista de modo a formar uma lista de listas de
peças, ou seja, um mapa. -}

constroiMapa :: [(Peca, Coordenadas)] -> Mapa 
constroiMapa l = constroiMapa2 l 0 
    where constroiMapa2 :: [(Peca, Coordenadas)] -> Int -> Mapa 
          constroiMapa2 l ac 
            | ac <= (maiorY2 l) = [constroiLinha (ordenaLinha (retiraLinha l ac)) 0 (maiorX2 l)] ++ (constroiMapa2 l (ac+1))
            | otherwise = []

{- | A função descontroiMapa utiliza a função desconstroiMapa2 dando o valor 0 a uma das variáveis. -}

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa l = desconstroiMapa2 l 0  

{- | A função desconstroiMapa2 utiliza a função desconstroiLinha para desconstruir todas as linhas do mapa, e junta os resultados numa lista. -}

desconstroiMapa2 :: Mapa -> Int -> [(Peca, Coordenadas)]
desconstroiMapa2 [] _ = []
desconstroiMapa2 (x:xs) ac = (desconstroiLinha x 0 ac) ++ (desconstroiMapa2 xs (ac+1))

{- | A função desconstroiLinha adiciona coordenadas às peças que não sejas vazios e retira os vazios da lista.
Onde ac equivale a x, e por isso varia na recursividade, e ac2 equivale a y, que varia apenas na função anterior quando desconstroi uma nova linha.
 -}

desconstroiLinha :: [Peca] -> Int -> Int -> [(Peca, Coordenadas)]
desconstroiLinha [] _ _ = [] 
desconstroiLinha (h:t) ac ac2
    | h /= Vazio = (h, (ac, ac2)) : (desconstroiLinha t (ac+1) ac2)
    | otherwise = desconstroiLinha t (ac+1) ac2 