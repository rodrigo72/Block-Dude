{- |
Module      : Tarefa5_2021li1g080
Description : Jogo 

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.

Esta fase do projeto consiste em criar um o jogo semelhante ao jogo Blockdude, com auxílio a funções de tarefas anteriores e 
com a bibliotica Gloss - tornando-o executável, através do ghc.

O caráter visual foi baseado num jogo famoso já existente chamado Undertale. (https://undertale.com/).
Sendo que o logotipo e os menus usam a mesma font que é utilizada em Undertale.
Para além disso, existem 3 mapas, cada um com aspetos visuais diferentes, que representam as três primeiras áreas desse jogo:
Ruins, Snowdin e Waterfall.

Os mapas são todos do mesmo tamanho, apesar de o nível de complexidade dos puzzles variar, visto que existem bordas a limitar. 

O jogo guarda automáticamente ao sair, caso mudanças tenham sido feitas - o estado do Jogo (jogador + mapa) é guardado em ficheiros .txt.
Por fim, uma característica que difere este jogo do jogo original Blockdude é a existência de um timer e de recordes.
Isto é, ao começar, ou ao dar reset a um mapa, o timer/ cronómetro começa no 00 : 00 e para quando o jogador chega ao save point/
porta, caso haja um recorde, o documento .txt que guarda os recordes é atualizado e é mostrado o novo recorde no ecrã.

Discução e conclusão: 

Há muito espaço para criatividade e para diversas implementações de novas mecânicas/ ideias nesta tarefa, e, apesar do tempo ser 
limitado, maior parte dos nossos objetivos foram realizados. Pensamos que o jogo ficou bem visualmente e que as ideias foram
bem implementadas - os diferentes mapas dão variedade ao jogo e o cronómetro incentiva a continuar a jogar mesmo depois de um puzzle
ser resolvido, para obter um melhor resultado/ novo recorde.
Para além do que foi implementado, gostavamos de ter feito um editor de mapas e de ter adicionado animações ao jogo.

Bibliografia:

Contéudo / Fontes de informação / Websites utilizados (data de acesso: 20/12/21 a 08/01/22) :
Youtube, Cesium (Guião Gloss #2 - Desenhar Mapa) - https://youtu.be/t9x59QXlpU4
Material de apoio pedagógico disponibilizado no Blackboad.
Documentação do Gloss - https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html -}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicy) 
import System.Process 
import System.Exit
import Numeric 
import Tarefa4_2021li1g080
import LI12122 

{- | Indica se o estado de um jogo mudou.
Por exemplo, se o jogador se mover (andar para a direita / andar para a esquerda / trepar / interagir com uma caixa) no mapa 1
então, supondo que o estado dos jogos era (0,0,0), então passa a ser (1,0,0).

Este tipo de dados é necessário, pois será utilizado para verificar se houve mudanças no jogo, pois caso não houverem, não vale a 
pena guardar o Jogo no ficheiro - o ficheiro pode permanecer igual.)
-}

type EstadoJogos = (Int, Int, Int) 

{- | Imagens é uma lista de Pictures que são importadas na função main.
Estas imagens são utilizadas para desenhar o jogo, desde os menus até às peças que compõem o mapa.
-}

type Imagens = [Picture]

{- | Recordes é uma lista que guarda os recordes do jogo - o menor tempo no qual o jogador terminou cada um dos mapas. -}

type Recordes = [(Float, Float)]

{- | Timer é um tipo de dados que guarda o valor do timer para cada ModoJogo, para que quando o jogador volte a jogar, o timer 
comece desse valor. -}

type Timer = [(Float,Float)]

{- | É o estado completo do jogo num dado momento. Contém os menus, os jogos, as imagens, os timers, os recordes e o EstadoJogos. -}

type World = (Menu, [Jogo], Imagens, Timer, Recordes, EstadoJogos)

{- | Organiza as diversas "etapas" do jogo - O menu inicial; os mapas; o jogo. -}

data Menu = Controlador Opcoes
          | Controlador2 OpcoesMapa
          | ModoJogo Float 

{- | Mostra as opçoes de Jogar ou Sair. -}

data Opcoes = Jogar 
            | Sair

{- | Mostra os mapas que é possível escolher. -}

data OpcoesMapa = Ruins 
                | Snowdin
                | Waterfall 

{- | Estado inicial do jogo do ModoJogo 1. -}

m1e1 :: Jogo
m1e1 = (Jogo m1r (Jogador (12, 5) Oeste False))

{- | Estado inicial do jogo do ModoJogo 2. -}

m2e2 :: Jogo 
m2e2 = (Jogo m2r (Jogador (7, 8) Este True))

{- | Estado inicial do jogo do ModoJogo 3. -}

m3e3 :: Jogo 
m3e3 = (Jogo m3r (Jogador (6, 6) Este True))

-- | Mapa de m1e1
m1r =
  [ 
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Caixa, Bloco],
    [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Caixa, Vazio, Caixa, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Caixa, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ] 

-- | Mapa de m2e2
m2r =
  [ 
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
    [Bloco, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Caixa, Bloco],
    [Bloco, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Vazio, Bloco, Bloco, Bloco, Caixa, Vazio, Bloco, Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Porta, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ] 

-- | Mapa de m3e3
m3r =
  [ 
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Bloco, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Porta, Vazio, Bloco, Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
    [Bloco, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
    [Bloco, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Vazio, Vazio, Caixa, Caixa, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

{- | Recebendo um mapa, encontra as coordenadas da porta desse mapa. -}

coordsPorta :: Mapa -> Coordenadas
coordsPorta l = coordsPorta' l 0 

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

{- | Recebendo um Jogo, encontra as coordenadas do jogador desse Jogo. -}

coordsJogador :: Jogo -> Coordenadas 
coordsJogador (Jogo m (Jogador (x,y) _ _)) = (x,y)

{- | Indica a altura máxima do Jogo, quando representado visualmente. -}

altura :: Float
altura = 439

{- | Indica o comprimento máximo do Jogo, quando representado visualmente. -}

comprimento :: Float
comprimento = (-599)

{- | Indica a largura/comprimento das peças. -}

l :: Float 
l = 80

{- | Indica que o jogo será reprensetado em FullScreen/ ecrã inteiro. -}

dm :: Display
dm = FullScreen

{- | Indica o framerate/ fps do jogo. -}

fr :: Int 
fr = 25 

{- | Converte o World, em diversos e diferentes casos, para uma Picture. -}

draw :: World -> IO Picture
-- | Mostra uma imagem que representa o segundo menu, em que o mapa Ruins está selecionado
draw (Controlador2 Ruins, jogo, img, timer, r, ej) = return (img !! 9) 
-- | Mostra uma imagem que representa o segundo menu, em que o mapa Snowdin está selecionado
draw (Controlador2 Snowdin, jogo, img, timer, r, ej) = return (img !! 10) 
-- | Mostra uma imagem que representa o segundo menu, em que o mapa Waterfall está selecionado
draw (Controlador2 Waterfall, jogo, img, timer, r, ej) = return (img !! 11) 

-- | Mostra uma imagem que representa o primeiro menu, em que Jogar está selecionado
draw (Controlador Jogar, jogo, img, timer, r, ej) = return (img !! 0) 
-- | Mostra uma imagem que representa o primeiro menu, em que Jogar está selecionado
draw (Controlador Sair, jogo, img, timer, r, ej) = return (img !! 1) 
-- | Mostra uma imagem que representa o primeiro menu, em que Jogar está selecionado

-- | Desenha as bordas, o mapa, o jogador, o timer e o recorde (ordem crescente de sobreposiçao)
draw (ModoJogo 3, (a:b:(Jogo m j):t), img, (x:y:z:zs), (q:w:e:r), ej) = 
    return (Pictures ((img !! 14) : ((drawMapa 3 comprimento altura m img) ++ [drawJogador j img] ++ [drawTimer z] ++ [drawRecorde e] ++ [img !! 16])))

draw (ModoJogo 2, (a:(Jogo m j):t), img, (x:y:ys), (q:w:e), ej) = 
    return (Pictures ((img !! 8) : ((drawMapa 2 comprimento altura m img) ++ [drawJogador j img] ++ [drawTimer y] ++ [drawRecorde w])))

draw (ModoJogo 1, ((Jogo m j):t), img, (x:xs), (y:ys), ej) = 
    return (Pictures ((img !! 7) : ((drawMapa 1 comprimento altura m img) ++ [drawJogador j img] ++ [drawTimer x] ++ [drawRecorde y])))

-- | Recebe um float e retorna um float com um certo numero de decimais
showDecimais floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum "" 

-- | Cor do Timer e dos Recordes
cor :: Color 
cor = (makeColorI 223 223 223 1)

-- | Desenha os Recordes de cada ModoJogo, através da funções do Gloss: Translate, Scale, Color e Text 
drawRecorde :: (Float, Float) -> Picture 
drawRecorde (min,sec) 
  | sec < 10 && min >= 10 = Translate 375 320 $ Scale (0.114) (0.114) $ Color cor $ Text ("Recorde: " ++ (show (round min)) ++ "min 0" ++ (showDecimais sec 2) ++ "s")
  | sec < 10 && min < 10  = Translate 375 320 $ Scale (0.114) (0.114) $ Color cor $ Text ("Recorde: " ++ "0" ++ (show (round min)) ++ "min 0" ++ (showDecimais sec 2) ++ "s")
  | sec >= 10 && min >= 10 = Translate 375 320 $ Scale (0.114) (0.114) $ Color cor $ Text ("Recorde: " ++ (show (round min)) ++ "min " ++ (showDecimais sec 2) ++ "s")
  | sec >= 10 && min < 10  = Translate 375 320 $ Scale (0.114) (0.114) $ Color cor $ Text ("Recorde: " ++ "0" ++ (show (round min)) ++ "min " ++ (showDecimais sec 2) ++ "s") 

-- | Desenha os Timers de cada ModoJogo
drawTimer :: (Float, Float) -> Picture 
drawTimer (min,sec) 
  | sec < 9.5 && min >= 10 = Translate 375 350 $ Scale (0.3) (0.3) $ Color cor $ Text ((show (round min)) ++ " : 0" ++ (show (round sec)))
  | sec < 9.5 && min < 10 = Translate 375 350 $ Scale (0.3) (0.3) $ Color cor $ Text ("0" ++ (show (round min)) ++ " : 0" ++ (show (round sec)))
  | sec > 9.5 && min >= 10 = Translate 375 350 $ Scale (0.3) (0.3) $ Color cor $ Text ((show (round min)) ++ " : " ++ (show (round sec)))
  | sec > 9.5 && min < 10 = Translate 375 350 $ Scale (0.3) (0.3) $ Color cor $ Text ("0" ++ (show (round min)) ++ " : " ++ (show (round sec)))

-- | Desenha o Jogador de cada ModoJogo
drawJogador :: Jogador -> [Picture] -> Picture 
drawJogador (Jogador (x,y) dir bool) img
  | bool == False && dir == Oeste = Translate (comprimento + (i*l)) (altura - (j*l)) e
  | bool == False && dir == Este  = Translate (comprimento + (i*l)) (altura - (j*l)) d 
  | bool == True && dir == Oeste = Pictures [
                                              (Translate (comprimento + (i*l)) (altura - (j*l)) e),
                                              (Translate (comprimento + (i*l)) (altura - ((j-1)*l)) b)
                                            ] 
  | bool == True && dir == Este = Pictures [
                                              (Translate (comprimento + (i*l)) (altura - (j*l)) d),
                                              (Translate (comprimento + (i*l)) (altura - ((j-1)*l)) b)
                                           ] 
  where
    i = fromIntegral x
    j = fromIntegral y
    -- | Imagem do jogador virado para a esquerda / oeste
    e = (img !! 6) 
    -- | Imagem do jogador virado para a direita / este 
    d = (img !! 5) 
    -- | Imagem da caixa
    b = (img !! 3) 

-- | Desenha o mapa 
drawMapa :: Float -> Float -> Float -> Mapa -> [Picture] -> [Picture]
drawMapa lvl x y (h:t) img = (drawLinha lvl x y h img) ++ (drawMapa lvl x (y-l) t img)
drawMapa _ _ _ [] _ = [] 

-- | Função auxuliar da função drawMapa, que desenha uma linha do mapa.
drawLinha :: Float -> Float -> Float -> [Peca] -> [Picture] -> [Picture]
drawLinha lvl x y (h:t) imgs = (drawPeca lvl x y h imgs) : (drawLinha lvl (x+l) y t imgs)
drawLinha _ _ _ [] _ = [] 

-- | Função auxiliar da função drawLinha, que desenha uma peça do mapa; As texturas das peças são diferentes, dependendo do ModoJogo.
drawPeca :: Float -> Float -> Float -> Peca -> [Picture] -> Picture
drawPeca lvl x y peca img 
  | lvl == 1 = case peca of Bloco -> Translate x y (img !! 2)
                            Caixa -> Translate x y (img !! 3)
                            Porta -> Translate x y (img !! 13)
                            _     -> Translate x y (img !! 4)
  | lvl == 2 = case peca of Bloco -> Translate x y (img !! 12)
                            Caixa -> Translate x y (img !! 3)
                            Porta -> Translate x y (img !! 13)
                            _     -> Translate x y (img !! 4)
  | lvl == 3 = case peca of Bloco -> Translate x y (img !! 15)
                            Caixa -> Translate x y (img !! 3)
                            Porta -> Translate x y (img !! 13)
                            _     -> Translate x y (img !! 4)

-- | Verifica se o o tempo que o jogador demorou a chegar à porta é um recorde.
check :: (Float, Float) -> (Float, Float) -> Recordes -> (Float,Float)
check (min,sec) (min2, sec2) r 
  | min < min2 = (min, sec)
  | min > min2 = (min2, sec2)
  | min == min2 && sec < sec2 = (min,sec)
  | min == min2 && sec > sec2 = (min2,sec2)
  | otherwise = (min,sec)

-- | "A function to handle input events." 
event :: Event -> World -> IO World 

-- | Navegação entre os menus 
event (EventKey (SpecialKey KeyEsc) Down _ _) (Controlador2 Ruins, jogo, imgs, timer, r, ej) = return (Controlador Jogar, jogo, imgs, timer, r, ej) 
event (EventKey (SpecialKey KeyEsc) Down _ _) (Controlador2 Snowdin, jogo, imgs, timer, r, ej) = return (Controlador Jogar, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyEsc) Down _ _) (Controlador2 Waterfall, jogo, imgs, timer, r, ej) = return (Controlador Jogar, jogo, imgs, timer, r, ej) 
event (EventKey (SpecialKey KeyEsc) Down _ _) (ModoJogo 1, jogo, imgs, timer, r, ej) = return (Controlador2 Ruins, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyEsc) Down _ _) (ModoJogo 2, jogo, imgs, timer, r, ej) = return (Controlador2 Snowdin, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyEsc) Down _ _) (ModoJogo 3, jogo, imgs, timer, r, ej) = return (Controlador2 Waterfall, jogo, imgs, timer, r, ej)

event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, jogo, imgs, timer, r, ej) =
  return (Controlador2 Ruins, jogo, imgs, timer, r, ej)

event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar, jogo, imgs, timer, r, ej) =
  return (Controlador Sair, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar, jogo, imgs, timer, r, ej) =
  return (Controlador Sair, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair, jogo, imgs, timer, r, ej) =
  return (Controlador Jogar, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair, jogo, imgs, timer, r, ej) =
  return (Controlador Jogar, jogo, imgs, timer, r, ej)

-- | Ao sair do jogo, grava as alterações feitas (Mapas/ Timers)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, (a:b:c:d), imgs, timer, r, (j,k,l)) 
  | j == 0 && k == 0 && l == 0 = exitSuccess
  | j /= 0 && k == 0 && l == 0 = do writeJogo1 a 
                                    writeTimer timer
                                    exitSuccess
  | j == 0 && k /= 0 && l == 0 = do writeJogo2 b 
                                    writeTimer timer
                                    exitSuccess
  | j == 0 && k == 0 && l /= 0 = do writeJogo3 c 
                                    writeTimer timer 
                                    exitSuccess
  | j /= 0 && k /= 0 && l == 0 = do writeJogo1 a  
                                    writeJogo2 b
                                    writeTimer timer
                                    exitSuccess
  | j /= 0 && k == 0 && l /= 0 = do writeJogo1 a  
                                    writeJogo3 c
                                    writeTimer timer
                                    exitSuccess
  | j == 0 && k /= 0 && l /= 0 = do writeJogo2 b  
                                    writeJogo3 c 
                                    writeTimer timer
                                    exitSuccess
  | j /= 0 && k /= 0 && l /= 0 = do writeJogo1 a  
                                    writeJogo2 b
                                    writeJogo3 c
                                    writeTimer timer  
                                    exitSuccess

event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Ruins, jogo, imgs, timer, r, ej) =
  return (Controlador2 Waterfall, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador2 Ruins, jogo, imgs, timer, r, ej) =
  return (Controlador2 Snowdin, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Ruins, jogo, imgs, timer, r, ej) =
  return (ModoJogo 1, jogo, imgs, timer, r, ej)

event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Snowdin, jogo, imgs, timer, r, ej) =
  return (Controlador2 Ruins, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador2 Snowdin, jogo, imgs, timer, r, ej) =
  return (Controlador2 Waterfall, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Snowdin, jogo, imgs, timer, r, ej) =
  return (ModoJogo 2, jogo, imgs, timer, r, ej)

event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Waterfall, jogo, imgs, timer, r, ej) =
  return (Controlador2 Snowdin, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador2 Waterfall, jogo, imgs, timer, r, ej) =
  return (Controlador2 Ruins, jogo, imgs, timer, r, ej)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Waterfall, jogo, imgs, timer, r, ej) =
  return (ModoJogo 3, jogo, imgs, timer, r, ej)

{- | MODO JOGO 1 
Atualiza o ficheiro dos recordes e se o movimento faz com o que o jogador alcance a porta
Verifica se o movimento mudou o Jogo 
Volta para o segundo menu, coloca o timer a 0 e atualiza o EstadoJogos
Permanece no mesmo modo de jogo 
-}

event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo 1, jogo@(a@(Jogo m j):t), imgs, timer@(x:xs), r@(q:w), (i,k,l)) 
  | (coordsJogador andarEsquerda) == (coordsPorta m) = do (writeTxt ((check x q r):w)) 
                                                          return (Controlador2 Ruins, (m1e1:t), imgs, ((0,0):xs), ((check x q r):w), (i+1,k,l)) 
  | otherwise = if a == andarEsquerda 
                then return (ModoJogo 1, (andarEsquerda:t), imgs, timer, r, (i,k,l)) 
                else return (ModoJogo 1, (andarEsquerda:t), imgs, timer, r, (i+1,k,l)) 
  where andarEsquerda = (correrMovimentos a [AndarEsquerda])

event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo 1, jogo@(a@(Jogo m j):t), imgs, timer@(x:xs), r@(q:w), (i,k,l))
  | (coordsJogador andarDireita) == (coordsPorta m) = do (writeTxt ((check x q r):w))
                                                         return (Controlador2 Ruins, (m1e1:t), imgs, ((0,0):xs), ((check x q r):w), (i+1,k,l))
  | otherwise = if a == andarDireita
                then return (ModoJogo 1, (andarDireita:t), imgs, timer, r, (i,k,l)) 
                else return (ModoJogo 1, (andarDireita:t), imgs, timer, r, (i+1,k,l)) 
  where andarDireita = (correrMovimentos a [AndarDireita])

event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo 1, jogo@(a@(Jogo m j):t), imgs, timer@(x:xs), r@(q:w), (i,k,l)) 
  | (coordsJogador trepar) == (coordsPorta m) = do (writeTxt ((check x q r):w))
                                                   return (Controlador2 Ruins, (m1e1:t), imgs, ((0,0):xs), ((check x q r):w), (i+1,k,l))
  | otherwise = if a == trepar 
                then return (ModoJogo 1, (trepar:t), imgs, timer, r, (i,k,l)) 
                else return (ModoJogo 1, (trepar:t), imgs, timer, r, (i+1,k,l)) 
  where trepar = (correrMovimentos a [Trepar])

event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo 1, jogo@(a@(Jogo m j):t), imgs, timer, r, ej@(i,k,l)) 
  | a == interageCaixa = return (ModoJogo 1, (interageCaixa:t), imgs, timer, r, ej) 
  | otherwise = return (ModoJogo 1, (interageCaixa:t), imgs, timer, r, (i+1,k,l)) 
  where interageCaixa = (correrMovimentos a [InterageCaixa])

-- | faz reset para o estado inicial do Jogo
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo 1, (a:t), imgs, (x:xs), r, (i,k,l)) 
  | a == m1e1 = return (ModoJogo 1, (m1e1:t), imgs, ((0,0):xs), r, (i,k,l)) 
  | otherwise = return (ModoJogo 1, (m1e1:t), imgs, ((0,0):xs), r, (i+1,k,l)) 

-- | MODO JOGO 2 

event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo 2, jogo@(a:b@(Jogo m j):t), imgs, timer@(x:y:xs), r@(q:w:e), (i,k,l)) 
  | (coordsJogador andarEsquerda) == (coordsPorta m) = do (writeTxt (q:(check y w r):e))
                                                          return (Controlador2 Snowdin, (a:m2e2:t), imgs, (x:(0,0):xs), (q:(check y w r):e), (i,k+1,l))
  | otherwise = if b == andarEsquerda 
                then return (ModoJogo 2, (a:andarEsquerda:t), imgs, timer, r, (i,k,l)) 
                else return (ModoJogo 2, (a:andarEsquerda:t), imgs, timer, r, (i,k+1,l)) 
  where andarEsquerda = (correrMovimentos b [AndarEsquerda])

event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo 2, jogo@(a:b@(Jogo m j):t), imgs, timer@(x:y:xs), r@(q:w:e), (i,k,l)) 
  | (coordsJogador andarDireita) == (coordsPorta m) = do (writeTxt (q:(check y w r):e))
                                                         return (Controlador2 Snowdin, (a:m2e2:t), imgs, (x:(0,0):xs), (q:(check y w r):e), (i,k+1,l))
  | otherwise = if b == andarDireita 
                then return (ModoJogo 2, (a:andarDireita:t), imgs, timer, r, (i,k,l))
                else return (ModoJogo 2, (a:andarDireita:t), imgs, timer, r, (i,k+1,l))
 where andarDireita = (correrMovimentos b [AndarDireita]) 

event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo 2, jogo@(a:b@(Jogo m j):t), imgs, timer@(x:y:xs), r@(q:w:e), (i,k,l))
  | (coordsJogador trepar) == (coordsPorta m) = do (writeTxt (q:(check y w r):e))
                                                   return (Controlador2 Snowdin, (a:m2e2:t), imgs, (x:(0,0):xs), (q:(check y w r):e), (i,k+1,l))
  | otherwise = if b == trepar 
                then return (ModoJogo 2, (a:trepar:t), imgs, timer, r, (i,k,l)) 
                else return (ModoJogo 2, (a:trepar:t), imgs, timer, r, (i,k+1,l)) 
  where trepar = (correrMovimentos b [Trepar])

event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo 2, (a:b:t), imgs, timer, r, ej@(i,k,l)) 
  | b == interageCaixa = return (ModoJogo 2, (a:interageCaixa:t), imgs, timer, r, ej) 
  | otherwise = return (ModoJogo 2, (a:interageCaixa:t), imgs, timer, r, (i,k+1,l)) 
  where interageCaixa = (correrMovimentos b [InterageCaixa])


event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo 2, (a:b:t), imgs, (x:y:xs), r, (i,k,l))
  | b == m2e2 = return (ModoJogo 2, (a:m2e2:t), imgs, (x:(0,0):xs), r, (i,k,l))
  | otherwise = return (ModoJogo 2, (a:m2e2:t), imgs, (x:(0,0):xs), r, (i,k+1,l))

-- | MODO JOGO 3 

event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo 3, (a:b:c@(Jogo m j):t), imgs, timer@(x:y:z:xs), r@(q:w:e:f), (i,k,l)) 
  | (coordsJogador andarEsquerda) == (coordsPorta m) = do (writeTxt (q:w:(check z e r):f))
                                                          return (Controlador2 Waterfall, (a:b:m3e3:t), imgs, (x:y:(0,0):xs), (q:w:(check z e r):f), (i,k,l+1))
  | otherwise = if c == andarEsquerda 
                then return (ModoJogo 3, (a:b:andarEsquerda:t), imgs, timer, r, (i,k,l)) 
                else return (ModoJogo 3, (a:b:andarEsquerda:t), imgs, timer, r, (i,k,l+1)) 
  where andarEsquerda = (correrMovimentos c [AndarEsquerda])

event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo 3, (a:b:c@(Jogo m j):t), imgs, timer@(x:y:z:xs), r@(q:w:e:f), (i,k,l))  
  | (coordsJogador andarDireita) == (coordsPorta m) = do (writeTxt (q:w:(check z e r):f))
                                                         return (Controlador2 Waterfall, (a:b:m3e3:t), imgs, (x:y:(0,0):xs), (q:w:(check z e r):f), (i,k,l+1))
  | otherwise = if c == andarDireita 
                then return (ModoJogo 3, (a:b:andarDireita:t), imgs, timer, r, (i,k,l))
                else return (ModoJogo 3, (a:b:andarDireita:t), imgs, timer, r, (i,k,l+1))
  where andarDireita = (correrMovimentos c [AndarDireita]) 

event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo 3, (a:b:c@(Jogo m j):t), imgs, timer@(x:y:z:xs), r@(q:w:e:f), (i,k,l)) 
  | (coordsJogador trepar) == (coordsPorta m) = do (writeTxt (q:w:(check z e r):f))
                                                   return (Controlador2 Waterfall, (a:b:m3e3:t), imgs, (x:y:(0,0):xs), (q:w:(check z e r):f), (i,k,l+1))
  | otherwise = if c == trepar 
                then return (ModoJogo 3, (a:b:trepar:t), imgs, timer, r, (i,k,l)) 
                else return (ModoJogo 3, (a:b:trepar:t), imgs, timer, r, (i,k,l+1)) 
  where trepar = (correrMovimentos c [Trepar])

event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo 3, (a:b:c:t), imgs, timer, r, ej@(i,k,l)) 
  | c == interageCaixa = return (ModoJogo 3, (a:b:interageCaixa:t), imgs, timer, r, ej)
  | otherwise = return (ModoJogo 3, (a:b:interageCaixa:t), imgs, timer, r, (i,k,l+1))
  where interageCaixa = (correrMovimentos c [InterageCaixa])
 
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo 3, (a:b:c:t), imgs, (x:y:z:xs), r, (j,k,l))
  | c == m3e3 = return (ModoJogo 3, (a:b:m3e3:t), imgs, (x:y:(0,0):xs), r, (j,k,l))
  | otherwise = return (ModoJogo 3, (a:b:m3e3:t), imgs, (x:y:(0,0):xs), r, (j,k,l+1))

event _ w = return w 

-- | Função utilizada para atualizar o Timer, de modo a que funcione como um cronômetro. 
time :: Float -> World -> IO World 
time _ w@(ModoJogo 1, a,b, ((min,sec):t), r, ej) 
  | sec < 59.5 = return (ModoJogo 1, a, b, ((min,(sec+0.038)):t), r, ej) 
  | sec >= 59.5 = return (ModoJogo 1, a, b, ((min+1,0):t), r, ej)
  | otherwise = return w

time _ w@(ModoJogo 2, a,b, (x:(min,sec):t), r, ej) 
  | sec < 59.5 = return (ModoJogo 2, a, b, (x:(min,(sec+0.038)):t), r, ej) 
  | sec >= 59.5 = return (ModoJogo 2, a, b, (x:(min+1,0):t), r, ej)
  | otherwise = return w

time _ w@(ModoJogo 3, a,b, (x:y:(min,sec):t), r, ej) 
  | sec < 59.5 = return (ModoJogo 3, a, b, (x:y:(min,(sec+0.038)):t), r, ej) 
  | sec >= 59.5 = return (ModoJogo 3, a, b, (x:y:(min+1,0):t), r, ej)
  | otherwise = return w

time n w = return w

-- | Transforma a lista de listas de strings num Jogo; ou seja, traduz e organiza o conteúdo dos ficheiros
organiza :: [[String]] -> Jogo
organiza (h:t) = (Jogo (organiza2 t) (organiza3 h)) 

-- | Função auxiliar da função Organiza, que organiza o mapa 
organiza2 :: [[String]] -> Mapa 
organiza2 [] = [] 
organiza2 (h:t) = organizaLinha h : organiza2 t 
    where organizaLinha :: [String] -> [Peca] 
          organizaLinha [] = [] 
          organizaLinha (h:t) = case h of "Bloco" -> Bloco : organizaLinha t
                                          "Caixa" -> Caixa : organizaLinha t 
                                          "Porta" -> Porta : organizaLinha t 
                                          "Vazio" -> Vazio : organizaLinha t

-- | Função auxiliar da função Organiza, que organiza o Jogador   
organiza3 :: [String] -> Jogador
organiza3 [x,y,d,b] = case d of "Oeste" -> case b of "True"  -> (Jogador ((read x :: Int), (read y :: Int)) Oeste True)
                                                     "False" -> (Jogador ((read x :: Int), (read y :: Int)) Oeste False)
                                "Este"  -> case b of "True"  -> (Jogador ((read x :: Int), (read y :: Int)) Este True)
                                                     "False" -> (Jogador ((read x :: Int), (read y :: Int)) Este False)
                                                     
-- | Transforma a lista de listas de strings no tipo de dados Recordes; ou seja, traduz e organiza o conteúdo dos ficheiros
organize :: [[String]] -> Recordes
organize [[x,x1],[y,y1],[z,z1]] = [((read x), (read x1)), ((read y),(read y1)), ((read z),(read z1))]

-- | Reescreve/ atualiza o ficheiro que guarda o estado dos Timer
writeTimer :: Timer -> IO ()
writeTimer timer = do let file4 = "../ficheirosTexto/timer.txt" 
                      writeFile file4 (recordesToStr timer)

-- | Reescreve/ atualiza o ficheiro que guarda os recordes
writeTxt :: Recordes -> IO ()
writeTxt recordes = do let file = "../ficheirosTexto/recordes.txt" 
                       writeFile file (recordesToStr recordes)

-- | Transforma o tipo de dados Recordes numa string de modo a que seja possível reescrever o ficheiro com a função writeTxt
recordesToStr :: Recordes -> String
recordesToStr [(x,x1), (y,y1), (z,z1)] = (show x) ++ " " ++ (show x1) ++ "\n" ++ (show y) ++ " " ++ (show y1) ++ "\n" ++ (show z) ++ " " ++ (show z1)

-- | Reescreve/ atualiza o ficheiro que guarda o jogo 1 
writeJogo1 :: Jogo -> IO ()
writeJogo1 jogo = do let file1 = "../ficheirosTexto/mapa1.txt"
                     writeFile file1 (jogoToStr jogo)

-- | Reescreve/ atualiza o ficheiro que guarda o jogo 2 
writeJogo2 :: Jogo -> IO ()
writeJogo2 jogo = do let file2 = "../ficheirosTexto/mapa2.txt"
                     writeFile file2 (jogoToStr jogo)

-- | Reescreve/ atualiza o ficheiro que guarda o jogo 3 
writeJogo3 :: Jogo -> IO ()
writeJogo3 jogo = do let file3 = "../ficheirosTexto/mapa3.txt"
                     writeFile file3 (jogoToStr jogo)

-- | Transforma o tipo de dados Jogo numa string de modo a que seja possível reescrever o ficheiro com as funções writeJogo1, writeJogo2 e writeJogo3 
jogoToStr :: Jogo -> String 
jogoToStr (Jogo m j) = (jogadorToStr j) ++ "\n" ++ (mapaToStr m)

-- | Transforma o tipo de dados Jogador numa string
jogadorToStr :: Jogador -> String 
jogadorToStr (Jogador (x,y) d b) = (show x) ++ " " ++ (show y) ++ " " ++ (show d) ++ " " ++ (show b)

-- | Transforma o tipo de dados Mapa numa string 
mapaToStr :: [[Peca]] -> String 
mapaToStr [] = [] 
mapaToStr mapa@(h:t) = if length mapa > 1
                       then (mapaToStrLinha h) ++ "\n" ++ (mapaToStr t)
                       else (mapaToStrLinha h) ++ (mapaToStr t)
    where mapaToStrLinha :: [Peca] -> String
          mapaToStrLinha [] = ""
          mapaToStrLinha [x] = case x of Bloco -> "Bloco" 
                                         Caixa -> "Caixa" 
                                         Porta -> "Porta" 
                                         Vazio -> "Vazio"
          mapaToStrLinha (h:t) = case h of Bloco -> "Bloco" ++ " " ++ (mapaToStrLinha t) 
                                           Caixa -> "Caixa" ++ " " ++ (mapaToStrLinha t) 
                                           Porta -> "Porta" ++ " " ++ (mapaToStrLinha t) 
                                           Vazio -> "Vazio" ++ " " ++ (mapaToStrLinha t)

{- | 

Função principal usada para importar imagens e ler ficheiros e em que se usa a playIO, que utiliza todas as funções anteriores, 
de modo a que seja possível jogar o jogo.
-}

main :: IO ()
main = do

  Just jogadorDireita <- loadJuicy "../imagens/jogadorDireita.png"
  Just jogadorEsquerda <- loadJuicy "../imagens/jogadorEsquerda.png"
  bloco <- loadBMP "../imagens/block.bmp"
  bloco2 <- loadBMP "../imagens/block2.bmp"
  caixa <- loadBMP "../imagens/caixa.bmp"
  Just vazio <- loadJuicy "../imagens/vazio.png"
  Just jogar <- loadJuicy "../imagens/jogar.png"
  Just sair <- loadJuicy "../imagens/sair.png"
  selectRuins <- loadBMP "../imagens/selectRuins.bmp"
  selectSnowdin <- loadBMP "../imagens/selectSnowdin.bmp"
  selectWaterfall <- loadBMP "../imagens/selectWaterfall.bmp"
  ruins <- loadBMP "../imagens/lvl1.bmp"
  snowdin <- loadBMP "../imagens/lvl2.bmp"
  Just porta <- loadJuicy "../imagens/portav2.png"
  waterfall <- loadBMP "../imagens/lvl3.bmp"
  block3 <- loadBMP "../imagens/block4.bmp"
  Just brilho <- loadJuicy "../imagens/brilho.png"

  let file = "../ficheirosTexto/recordes.txt" 
  contents <- readFile file 
  let recordes = organize (map words (lines contents)) 

  let file1 = "../ficheirosTexto/mapa1.txt" 
  contents1 <- readFile file1
  let jogo1 = organiza (map words (lines contents1))

  let file2 = "../ficheirosTexto/mapa2.txt" 
  contents2 <- readFile file2
  let jogo2 = organiza (map words (lines contents2))

  let file3 = "../ficheirosTexto/mapa3.txt" 
  contents3 <- readFile file3
  let jogo3 = organiza (map words (lines contents3))

  let file4 = "../ficheirosTexto/timer.txt" 
  contents4 <- readFile file4
  let timer1 = organize (map words (lines contents4)) 

  let imagens = [jogar,sair,bloco,caixa,vazio,jogadorDireita,jogadorEsquerda,ruins,snowdin,selectRuins,selectSnowdin,selectWaterfall,bloco2,porta,waterfall,block3,brilho]
  let estadoInicial = (Controlador Jogar, [jogo1,jogo2,jogo3], imagens, timer1, recordes, (0,0,0))

  playIO dm 
         black  
         fr            
         estadoInicial 
         draw          
         event 
         time 
