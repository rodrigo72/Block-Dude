module Tarefa1_2021li1g080_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g080
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Chao m2r" ~: verificaChao m2 ~=? True 
    , "Tarefa 1 - Teste Valida Chao m3r" ~: verificaChao m3 ~=? True
    , "Tarefa 1 - Teste Valida Chao m4r" ~: verificaChao m4 ~=? False
    , "Tarefa 1 - Teste Valida Chao m5r" ~: verificaChao m5 ~=? True
    , "Tarefa 1 - Teste Valida Chao m6r" ~: verificaChao m6 ~=? False 
    , "Tarefa 1 - Teste Valida Chao m7r" ~: verificaChao m7 ~=? False
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa sem espaços vazios" ~: validaPotencialMapa [(Bloco, (0,0)), (Bloco, (0,1)), (Bloco,(1,0)),(Bloco,(1,1))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com mais de uma porta" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa sem portas" ~: validaPotencialMapa [(Bloco, (0,1)),(Bloco,(1,1))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com caixas a flutuar" ~: validaPotencialMapa [(Caixa,(0,0)),(Caixa,(1,0)),(Porta,(1,1)),(Bloco,(0,2)),(Bloco,(1,2))] ~=? False
    ]
