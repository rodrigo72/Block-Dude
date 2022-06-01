module Tarefa3_2021li1g080 where

import LI12122
import Tarefa2_2021li1g080

instance Show Jogo where
  show j = mostraJogo j

mostraJogo :: Jogo -> String 
mostraJogo (Jogo m j) = funcaoFinal m j 

{- | Transforma o jogador no Char '>' caso esteja virado para a direita, ou no Char 
'<' caso esteja virado para a esquerda. -}

jogadorToChar :: Jogador -> Char 
jogadorToChar (Jogador _ direcao _) = case direcao of 
    Este  -> '>'
    Oeste -> '<'

{- | Transforma uma peça no seu respetivo Char. -}

pecaToChar :: Peca -> Char  
pecaToChar p = case p of 
    Vazio -> ' '
    Bloco -> 'X'
    Caixa -> 'C'
    Porta -> 'P'

{- | Transforma o Mapa numa string. -}

mapaToString :: Mapa -> String 
mapaToString [] = ""
mapaToString (h:t)
  | t /= [] = linhaToString h ++ "\n" ++ mapaToString t 
  | otherwise = linhaToString h ++ mapaToString t 
  where linhaToString :: [Peca] -> String 
        linhaToString [] = ""
        linhaToString (h:t) = pecaToChar h : linhaToString t 

{- | Adiciona o jogador ao mapa (utiliza a função anterior: mapaToString). -}

adicionaJogador :: Mapa -> Jogador -> String 
adicionaJogador m j = adicionaJogador2 (0,0) (mapaToString m) j 
  where adicionaJogador2 :: Coordenadas -> String -> Jogador -> String 
        adicionaJogador2 _ [] _ = []
        adicionaJogador2 (x,y) m@(h:t) j@(Jogador (x1,y1) _ _)
            | h == '\n' = h : adicionaJogador2 (0, (y+1)) t j
            | x == x1 && y == y1 = (jogadorToChar j) : t  
            | otherwise = h : adicionaJogador2 ((x+1), y) t j 

{- | Adiciona uma caixa acima do jogador, caso o jogador esteja a segurar numa caixa. -}

temCaixa :: String -> Jogador -> String 
temCaixa m@(h:t) j@(Jogador (x,y) _ b)
  | b == False = m  
  | otherwise  = temCaixa2 (0,0) m j 
  where temCaixa2 :: Coordenadas -> String -> Jogador -> String 
        temCaixa2 (x,y) m@(h:t) j@(Jogador (x1,y1) _ _)
          | y1 == 0 = m 
          | h == '\n' = h : temCaixa2 (0, (y+1)) t j 
          | x == x1 && y == (y1-1) = 'C' : t 
          | otherwise = h : temCaixa2 ((x+1), y) t j 

{- | Utiliza as funções anteriores de modo a mostrar o mapa e o jogador no terminal
em formato String. -}

funcaoFinal :: Mapa -> Jogador -> String 
funcaoFinal m j = temCaixa (adicionaJogador m j) j 