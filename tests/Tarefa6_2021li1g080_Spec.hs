module Tarefa6_2021li1g080_Spec where

import Test.HUnit
import LI12122
import Tarefa6_2021li1g080
import Fixtures

-- Tarefa 6
testsT6 =
  test
    [ 
      "Tarefa 6 - Verifica a soluçao" ~: (resolveJogo 5 jogoTeste) ~=? Just [Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda]
    , "Tarefa 6 - Verifica a soluçao" ~: (resolveJogo 7 jogoTeste2) ~=? Just [InterageCaixa,AndarEsquerda,InterageCaixa,Trepar,Trepar,AndarEsquerda,AndarEsquerda]
    ]
