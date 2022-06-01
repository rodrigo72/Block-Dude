module Tarefa6_2021li1g080 where 

import LI12122
import Tarefa4_2021li1g080

{- | O tipo de dados utilizado foi uma Rose Tree, uma árvore irregular. -}

data RTree a = R a [RTree a]
  deriving (Show)

{- Devolve True caso seja um Just, ou False caso seja um Nothing. -}
checkMaybe :: Maybe a -> Bool
checkMaybe x = case x of Nothing -> False
                         Just _  -> True 

{- Devolve o que está dentro do Just. -}
devolveJust :: Maybe a -> a
devolveJust (Just n) = n 

{- Resolve um jogo recebendo um limite de movimentos, devolvendo uma lista de movimentos ou então Nothing, caso o limite seja atingido. -}
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo x j = resolveJogo2 x (R j [])

{- Resolve o jogo utilizando uma Rose Tree de Jogos. -}
resolveJogo2 :: Int -> RTree Jogo -> Maybe [Movimento]
resolveJogo2 n rtree@(R j l)
    | n < 0 = Nothing 
    | checkMaybe path = Just (converteJogos (devolveJust path))
    | otherwise = resolveJogo2 (n-1) (constroiTree rtree)
    where path = findPath rtree

{- Constroi uma camada da árvore. -}
constroiTree :: RTree Jogo -> RTree Jogo
constroiTree (R j []) 
    | j == j1 && j == j2 && j == j3 && j == j4 = R j []
    | j /= j1 && j == j2 && j == j3 && j == j4 = R j [
                                                      R j1 []
                                                     ]
    | j == j1 && j /= j2 && j == j3 && j == j4 = R j [
                                                      R j2 []
                                                     ]
    | j == j1 && j == j2 && j /= j3 && j == j4 = R j [
                                                      R j3 []
                                                     ]
    | j == j1 && j == j2 && j == j3 && j /= j4 = R j [
                                                      R j4 []
                                                     ]
    | j /= j1 && j /= j2 && j == j3 && j == j4 = R j [
                                                      R j1 [],
                                                      R j2 []
                                                     ]
    | j /= j1 && j == j2 && j /= j3 && j == j4 = R j [
                                                      R j1 [],
                                                      R j3 []
                                                     ]
    | j /= j1 && j == j2 && j == j3 && j /= j4 = R j [
                                                      R j1 [],
                                                      R j4 []
                                                     ]
    | j == j1 && j /= j2 && j /= j3 && j == j4 = R j [
                                                      R j2 [],
                                                      R j3 []
                                                     ]
    | j == j1 && j /= j2 && j == j3 && j /= j4 = R j [
                                                      R j2 [],
                                                      R j4 []
                                                     ]
    | j == j1 && j == j2 && j /= j3 && j /= j4 = R j [
                                                      R j3 [],
                                                      R j4 []
                                                     ]
    | j == j1 && j /= j2 && j /= j3 && j /= j4 = R j [
                                                      R j2 [],
                                                      R j3 [],
                                                      R j4 []
                                                     ]
    | j /= j1 && j == j2 && j /= j3 && j /= j4 = R j [
                                                      R j1 [],
                                                      R j3 [],
                                                      R j4 []
                                                     ]
    | j /= j1 && j /= j2 && j == j3 && j /= j4 = R j [
                                                      R j1 [],
                                                      R j2 [],
                                                      R j4 []
                                                     ]
    | j /= j1 && j /= j2 && j /= j3 && j == j4 = R j [
                                                      R j1 [],
                                                      R j2 [],
                                                      R j3 []
                                                     ]
    | otherwise                                = R j [
                                                      R j1 [],
                                                      R j2 [],
                                                      R j3 [],
                                                      R j4 []
                                                     ]   
    where j1 = correrMovimentos j [AndarEsquerda] 
          j2 = correrMovimentos j [AndarDireita]
          j3 = correrMovimentos j [InterageCaixa]
          j4 = correrMovimentos j [Trepar]

constroiTree (R j l) = R j (map constroiTree l)

{- | Encontra o Jogo que em que o jogador chega à porta numa Rose Tree de Jogos. -}
findPath :: RTree Jogo -> Maybe [Jogo]
findPath (R j@(Jogo m (Jogador coords dir bool)) [])
    | coords == coordsPorta j = Just [j]
    | otherwise = Nothing
findPath (R j (h:t))
    | checkMaybe (findPath h) = Just (j : (devolveJust (findPath h)))
    | otherwise = findPath (R j t)

{- | Devolve os movimentos que ocorreram numa série de Jogos-}
converteJogos :: [Jogo] -> [Movimento]
converteJogos [] = []
converteJogos [_] = []
converteJogos (x:y:t) = movimentos x y : converteJogos (y:t)

{- | Devolve o movimento que ocorreu entre dois Jogos, verificando as coordenadas do jogador, se mudou de diração e se interagiu com uma caixa. -}
movimentos :: Jogo -> Jogo -> Movimento
movimentos (Jogo m (Jogador (x,y) dir bool)) (Jogo m1 (Jogador (x1,y1) dir1 bool1))
    | y > y1        = Trepar
    | bool /= bool1 = InterageCaixa
    | dir1 == Oeste = AndarEsquerda
    | otherwise     = AndarDireita

{- | Recebendo um jogo, encontra as coordenadas da porta do mapa desse Jogo. -}

coordsPorta :: Jogo -> Coordenadas
coordsPorta (Jogo m j) = coordsPorta' m 0 

-- | Acumulador da função coordsPorta 
coordsPorta' :: Mapa -> Int -> Coordenadas
coordsPorta' (h:t) y = if   (coordsPortaLinha h 0) < 0
                       then  coordsPorta' t (y+1)
                       else (coordsPortaLinha h 0, y) 

-- | Função auxiliar da função coordsPorta'  
coordsPortaLinha :: [Peca] -> Int -> Int 
coordsPortaLinha [] _ = (-1)
coordsPortaLinha (x:xs) ac
    | x /= Porta = coordsPortaLinha xs (ac+1)
    | otherwise  = ac 
