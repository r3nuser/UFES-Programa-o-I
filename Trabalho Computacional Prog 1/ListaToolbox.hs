--------------------------------------------------------------------------------
-- TRABALHO COMPUTACIONAL - 2018/1 - Prof: Claudia Boeres                     -- 
--------------------------------------------------------------------------------
-- Participante 1: Ezequiel Schneider Reinholtz                               --
-- Participante 2: Renan Moreira Gomes                                        --
-- Data: 29/06/2018                                                           --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- MODULO LISTA TOOLBOX                                                       --
--------------------------------------------------------------------------------
-- Este modulo contem funcoes que manipulam listas                            --
--------------------------------------------------------------------------------
-- Funcoes Utilizaveis:                                                       --
-- - somatorio : Retorna o somatorio dos valores de uma lista de inteiros     --
-- - maximo : Retonr ao maximo valor inteiro de uma lista                     --
--                                                                            --
-- Funcoes Internas:                                                          --
-- - maior_que : Retorna quais valores sao maiores que x numa lista xs        --
--------------------------------------------------------------------------------

module ListaToolbox(
    somatorio,
    maximo
)where

--------------------------------------------------------------------------------
-- DEFINICAO DAS FUNCOES                                                      --
--------------------------------------------------------------------------------    

--------------------------------------------------------------------------------
-- FUNCOES QUE RETORNA O SOMATORIO DE UMA LISTA DE INTEIROS                   --
--------------------------------------------------------------------------------
somatorio::[Int]->Int
somatorio [] = 0
somatorio (x:xs) = x + somatorio xs
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA QUAIS VALORES SAO MAIORES QUE X NUMA LISTA XS             --
--------------------------------------------------------------------------------
maior_que::Int->[Int]->[Int]
maior_que x xs = [ y | y <- xs, y > x]
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA O MAXIMO VALOR INTEIRO DE UMA LISTA                             --
--------------------------------------------------------------------------------
maximo::[Int]->Int
maximo xs = [ x | x <- xs, null (maior_que x xs) ]!!0