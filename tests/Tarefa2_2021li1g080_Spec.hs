module Tarefa2_2021li1g080_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g080
import Fixtures

testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
    , "Tarefa 2 - Teste Construir Mapa m2" ~: m2r ~=? constroiMapa m2
    , "Tarefa 2 - Teste Construir Mapa m3" ~: m3r ~=? constroiMapa m3
    , "Tarefa 2 - Teste Construir Mapa m4" ~: m4r ~=? constroiMapa m4 
    , "Tarefa 2 - Teste Construir Mapa m5" ~: m5r ~=? constroiMapa m5 
    , "Tarefa 2 - Teste Construir Mapa m6" ~: m6r ~=? constroiMapa m6 
    , "Tarefa 2 - Teste Construir Mapa m7" ~: m7r ~=? constroiMapa m7 
    , "Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa []
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa []
    , "Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , "Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (7, 4))] ~=?  constroiMapa [(Porta, (7, 4)), (Porta, (7, 4))]
    ]
  