module Tarefa4_2021li1g080 where

import LI12122
import Tarefa1_2021li1g080
import Tarefa2_2021li1g080
import Tarefa3_2021li1g080

{- | A função moveJogador move o jogador, sendo que o pode fazer de 5 maneiras:
Andar para a esquerda, andar para a direita, subir ou interagir com uma caixa, isto é, pegar numa caixa ou então largar uma caixa.
-}

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador j m = 
    case m of 
        AndarEsquerda -> inputEsquerda j 
        AndarDireita  -> inputDireita  j
        Trepar        -> inputSubir    j
        InterageCaixa -> inputCaixa    j

{- | A função correMovimentos aplica uma série de movimentos utilizando a função moveJogador. -}

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j [] = j 
correrMovimentos j (h:t) = correrMovimentos (moveJogador j h) t 

{- | A função inputEsquerda move o jogador para a esquerda.
Caso o jogador se encontre no fim do mapa, não se poderá mover para a esquerda.
Caso exista uma peça à sua frente, não se poderá mover para a esquerda.
Caso exista uma peça a cima e à sua frente, apenas se poderá mover para a esquerda caso não esteja a levar um caixa.
-}

inputEsquerda :: Jogo -> Jogo 
inputEsquerda j1@(Jogo m@(h:t) j@(Jogador (x,y) dir bool)) = if x == 0 
                                                             then Jogo m (Jogador (x,y) Oeste bool)
                                                             else if (verificaPecaFrente (x-1,y) m) && ((mostraPeca (x-1,y) m) /= Porta) 
                                                             then Jogo m (Jogador (x,y) Oeste bool)            
                                                             else if (verificaPecaFrente (x-1,y-1) m) && bool == True              
                                                                  then Jogo m (Jogador (x,y) Oeste bool)    
                                                                  else if (verificaPecaFrente (x-1,y+1) (retiraPorta m))
                                                                       then Jogo m (Jogador (x-1,y) Oeste bool)
                                                                       else Jogo m (Jogador (x-1, (menorY (retiraElementosMenoresQue y (apenasColuna (desconstroiMapa (retiraPorta m)) (x-1)))) - 1) Oeste bool)

{- | A função inputDireita é muito semelhante à função inputEsquerda. O raciocínio/ método utilizado é o mesmo, apenas muda a direção. -}

inputDireita :: Jogo -> Jogo 
inputDireita j1@(Jogo m@(h:t) j@(Jogador (x,y) dir bool)) = if x == maiorX2 (desconstroiMapa m)
                                                            then Jogo m (Jogador (x,y) Este bool)
                                                            else if (verificaPecaFrente (x+1,y) m) && ((mostraPeca (x+1,y) m) /= Porta)
                                                             then Jogo m (Jogador (x,y) Este bool)            
                                                             else if (verificaPecaFrente (x+1,y-1) m) && bool == True              
                                                                  then Jogo m (Jogador (x,y) Este bool)    
                                                                  else if (verificaPecaFrente (x+1,y+1) (retiraPorta m))
                                                                       then Jogo m (Jogador (x+1,y) Este bool)
                                                                       else Jogo m (Jogador (x+1, (menorY (retiraElementosMenoresQue y (apenasColuna (desconstroiMapa (retiraPorta m)) (x+1)))) - 1) Este bool)

{- | A função retiraElementosMenoresQue retira elementos que são menores que um certo valor de y de uma lista de peças e as respetivas coordenadas. -}

retiraElementosMenoresQue :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] 
retiraElementosMenoresQue _ [] = []
retiraElementosMenoresQue y ((h, (x1,y1)):t)
    | y1 < y = retiraElementosMenoresQue y t 
    | otherwise = (h, (x1,y1)) : retiraElementosMenoresQue y t 

{- | A função verificaPecaFrente, verifica se existe uma peça numa determinada posição no mapa e retorna um bool. -}

verificaPecaFrente :: Coordenadas -> Mapa -> Bool
verificaPecaFrente (x,y) m = verificaPecaFrente2 (x,y) (desconstroiMapa m)
    where verificaPecaFrente2 :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
          verificaPecaFrente2 _ [] = False 
          verificaPecaFrente2 (x,y) ((h, (x1,y1)):t)
            | x == x1 && y == y1 = True
            | otherwise = verificaPecaFrente2 (x,y) t 

{- | A função mostraPeca, devolve qual é a peça numa determinada posição. -}

mostraPeca :: Coordenadas -> Mapa -> Peca
mostraPeca (x,y) m = mostraPeca2 (x,y) (desconstroiMapa m)
    where mostraPeca2 :: Coordenadas -> [(Peca, Coordenadas)] -> Peca
          mostraPeca2 _ [] = Vazio 
          mostraPeca2 (x,y) ((h, (x1,y1)):t)
            | x == x1 && y == y1 = h 
            | otherwise = mostraPeca2 (x,y) t 

retiraPorta :: Mapa -> Mapa 
retiraPorta [] = []
retiraPorta (h:t) = [retiraPortaLinha h] ++ (retiraPorta t)
    where retiraPortaLinha :: [Peca] -> [Peca] 
          retiraPortaLinha [] = []
          retiraPortaLinha (h:t) 
            | h == Porta = Vazio : (retiraPortaLinha t) 
            | otherwise  = h     : (retiraPortaLinha t) 

{- | A função inputSubir faz com que o jogador suba, caso seja possível, apenas um bloco de altura. -}

inputSubir :: Jogo -> Jogo 
inputSubir j1@(Jogo m@(h:t) j@(Jogador (x,y) dir bool))
    | dir == Este  && bool == True && y == 1  = Jogo m (Jogador (x,y) Este bool)
    | dir == Oeste && bool == True && y == 1  = Jogo m (Jogador (x,y) Oeste bool) 
    | dir == Este  && (mostraPeca (x+1,y) m) == Porta = Jogo m (Jogador (x,y) Este bool)
    | dir == Oeste && (mostraPeca (x-1,y) m) == Porta = Jogo m (Jogador (x,y) Oeste bool)
    | dir == Este = if bool == True 
                    then if (verificaPecaFrente (x+1,y) m) && not (verificaPecaFrente (x+1,y-1) (retiraPorta m)) && not (verificaPecaFrente (x+1,y-2) m) && not (verificaPecaFrente (x,y-2) m)
                         then Jogo m (Jogador (x+1,y-1) Este bool)
                         else Jogo m (Jogador (x,y) Este bool)
                    else if (verificaPecaFrente (x+1,y) m) && not (verificaPecaFrente (x+1,y-1) (retiraPorta m)) && not (verificaPecaFrente (x,y-1) m)
                         then Jogo m (Jogador (x+1,y-1) Este bool)
                         else Jogo m (Jogador (x,y) Este bool) 
    | otherwise   = if bool == True 
                    then if (verificaPecaFrente (x-1,y) m) && not (verificaPecaFrente (x-1,y-1) (retiraPorta m)) && not (verificaPecaFrente (x-1,y-2) m) && not (verificaPecaFrente (x,y-2) m)
                         then Jogo m (Jogador (x-1,y-1) Oeste bool)
                         else Jogo m (Jogador (x,y) Oeste bool)
                    else if (verificaPecaFrente (x-1,y) m) && not (verificaPecaFrente (x-1,y-1) (retiraPorta m)) && not (verificaPecaFrente (x,y-1) m)
                         then Jogo m (Jogador (x-1,y-1) Oeste bool)
                         else Jogo m (Jogador (x,y) Oeste bool)

{- | A função adicionaCaixa adiciona uma caixa ao mapa. -}

adicionaCaixa:: Coordenadas -> Mapa -> Mapa 
adicionaCaixa (x,y) m = adicionaCaixa2 (x,y) (desconstroiMapa m)
    where adicionaCaixa2 :: Coordenadas -> [(Peca, Coordenadas)] -> Mapa
          adicionaCaixa2 (x,y) m = constroiMapa ((Caixa, (x,y)) : m) 

{- | A função inputCaixa, define a interação do jogador com uma caixa, os casos em que pega ou larga uma caixa. -}

inputCaixa :: Jogo -> Jogo 
inputCaixa j1@(Jogo m@(h:t) j@(Jogador (x,y) dir bool))
    | bool == True  && dir == Oeste && (mostraPeca (x-1,y) m) == Porta = j1 
    | bool == True  && dir == Este  && (mostraPeca (x+1,y) m) == Porta = j1
    | bool == False && dir == Oeste = if (mostraPeca (x-1,y) m) == Caixa && not (verificaPecaFrente (x-1,y-1) m) && not (verificaPecaFrente (x,y-1) m)
                                      then Jogo (constroiMapa((eliminaElemento (desconstroiMapa m) (x-1,y)))) (Jogador (x,y) Oeste True)
                                      else Jogo m (Jogador (x,y) Oeste False)
    | bool == False && dir == Este  = if (mostraPeca (x+1,y) m) == Caixa && not (verificaPecaFrente (x+1,y-1) m) && not (verificaPecaFrente (x,y-1) m)
                                      then Jogo (constroiMapa((eliminaElemento (desconstroiMapa m) (x+1,y)))) (Jogador (x,y) Este True)
                                      else Jogo m (Jogador (x,y) Este False)
    | bool == True  && dir == Oeste = if (verificaPecaFrente (x-1,y-1) m)
                                      then j1 
                                      else if not (verificaPecaFrente (x-1,y+1) m) && not (verificaPecaFrente (x-1,y) m)
                                           then Jogo (adicionaCaixa (x-1, (menorY (retiraElementosMenoresQue y (apenasColuna (desconstroiMapa m) (x-1)))) - 1) m) (Jogador (x,y) Oeste False)
                                           else if (verificaPecaFrente (x-1, y) m) 
                                                then Jogo (adicionaCaixa (x-1,y-1) m) (Jogador (x,y) Oeste False)
                                                else if not (verificaPecaFrente (x-1,y) m) 
                                                     then Jogo (adicionaCaixa (x-1, y) m) (Jogador (x,y) Oeste False)
                                                     else Jogo m (Jogador (x,y) Oeste True)
    | bool == True  && dir == Este  = if (verificaPecaFrente (x+1,y-1) m)
                                      then j1 
                                      else if not (verificaPecaFrente (x+1,y+1) m) && not (verificaPecaFrente (x+1,y) m)
                                           then Jogo (adicionaCaixa (x+1, (menorY (retiraElementosMenoresQue y (apenasColuna (desconstroiMapa m) (x+1)))) - 1) m) (Jogador (x,y) Este False)
                                           else if (verificaPecaFrente (x+1, y) m) 
                                                then Jogo (adicionaCaixa (x+1,y-1) m) (Jogador (x,y) Este False)
                                                else if not (verificaPecaFrente (x+1,y) m) 
                                                     then Jogo (adicionaCaixa (x+1, y) m) (Jogador (x,y) Este False)
                                                     else Jogo m (Jogador (x,y) Este True)