--------------------------------------------------------------------------------
-- TRABALHO COMPUTACIONAL - 2018/1 - Prof: Claudia Boeres                     -- 
--------------------------------------------------------------------------------
-- Participante 1: Ezequiel Schneider Reinholtz                               --
-- Participante 2: Renan Moreira Gomes                                        --
-- Data: 29/06/2018                                                           --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- MODULO OBJETOS                                                             --
--------------------------------------------------------------------------------
-- Este modulo contem todas as definicoes de tipos                            --
--------------------------------------------------------------------------------
-- Tipos:                                                                     --
-- - Berco : Molde do berco                                                   --
-- - Navio : Molde do navio                                                   --
-- - VetorAtendimento : Molde de um vetor de tempo de atendimento             --
-- - ListaDeNavios : Molde de uma lista de navios                             --
-- - ListaDeBercos : Molde de uma lista de bercos                             --
-- - ListaTempoAtendimento : Molde de lista de vetor de tempo de atendimento  --
-- - NaviosNoBerco : Molde de dupla de navios no berco                        --
-- - NaviosAlocadosBerco : Molde de lista de dupla de navio no berco          --
--------------------------------------------------------------------------------

module Objetos 
(Berco, 
 Navio,
 VetorAtendimento,
 ListaDeNavios,
 ListaDeBercos,
 ListaTempoAtendimento,
 NaviosNoBerco,
 NaviosAlocadosBerco
 ) where

--------------------------------------------------------------------------------
-- DEFINICAO DOS TIPOS                                                        --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- MOLDE DO BERCO                                                             --
--------------------------------------------------------------------------------
type Berco = (Int, Int, Int)
--------------------------------------------------------------------------------
-- MOLDE DO NAVIO                                                             --
--------------------------------------------------------------------------------
type Navio = (Int, Int, Int, Int)
--------------------------------------------------------------------------------
-- MOLDE DE UMA LISTA DE NAVIOS                                               --
--------------------------------------------------------------------------------
type ListaDeNavios = [Navio]
--------------------------------------------------------------------------------
-- MOLDE DE UMA LISTA DE BERCOS                                               --
--------------------------------------------------------------------------------
type ListaDeBercos = [Berco]
--------------------------------------------------------------------------------
-- MOLDE DE UM VETOR DE TEMPO DE ATENDIMENTO                                           --
--------------------------------------------------------------------------------
type VetorAtendimento = [Int]
--------------------------------------------------------------------------------
-- MOLDE DE UMA LISTA DE VETOR DE TEMPO DE ATENDIMENTO                        --
--------------------------------------------------------------------------------
type ListaTempoAtendimento = [VetorAtendimento]
--------------------------------------------------------------------------------
-- MOLDE DE DUPLA DE NAVIOS NO BERCO                                          --
--------------------------------------------------------------------------------
type NaviosNoBerco = (Berco,ListaDeNavios)
--------------------------------------------------------------------------------
-- LISTA DE DUPLA DE NAVIOS NO BERCO                                          --
--------------------------------------------------------------------------------
type NaviosAlocadosBerco = [NaviosNoBerco]

 