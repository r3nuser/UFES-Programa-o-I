--------------------------------------------------------------------------------
-- TRABALHO COMPUTACIONAL - 2018/1 - Prof: Claudia Boeres                     -- 
--------------------------------------------------------------------------------
-- Participante 1: Ezequiel Schneider Reinholtz                               --
-- Participante 2: Renan Moreira Gomes                                        --
-- Data: 29/06/2018                                                           --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- MODULO BERCOTOOLBOX                                                        --
--------------------------------------------------------------------------------
-- Este modulo contem funcoes que manipulam objetos do tipo berco             --
--------------------------------------------------------------------------------
-- Funcoes Utilizaveis:                                                       --
-- - id_berco : Retorna o id do berco                                         --
-- - tempoAtendimentoBerco : Retorna o vetor de tempo de atendimento do berco --
-- - tempoAberto : Retorna o tempo de atividade de um berco                   --
-- - get_abertura : Retorna a hora de abertura do berco                       --
--                                                                            --
-- Funcoes Internas:                                                          --
--------------------------------------------------------------------------------

module BercoToolbox(
    tempoAtendimentoBerco,
    tempoAberto,
    id_berco,
    get_abertura
)where
--------------------------------------------------------------------------------
-- Importacoes:                                                               --
-- - Objetos.hs                                                               --
--------------------------------------------------------------------------------
import Objetos

--------------------------------------------------------------------------------
-- DEFINICAO DAS FUNCOES                                                      --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA O ID DO BERCO                                           --
--------------------------------------------------------------------------------
id_berco::Berco->Int 
id_berco (id, abertura, fechamento) = id
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA O VETOR DE TEMPO DE ATENDIMENTO DE UM BERCO             --
--------------------------------------------------------------------------------
tempoAtendimentoBerco::Berco->ListaTempoAtendimento->VetorAtendimento
tempoAtendimentoBerco berco infoPorto = infoPorto!!(id_berco berco - 1)
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA O TEMPO DE ATIVIDADE DE UM BERCO                        --
--------------------------------------------------------------------------------
tempoAberto::Berco->Int
tempoAberto (id, abertura, fechamento) = fechamento - abertura
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA A HORA DE ABERTURA DO BERCO                             --
--------------------------------------------------------------------------------
get_abertura::Berco->Int
get_abertura (id, abertura, fechamento) = abertura

