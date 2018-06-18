module BercoToolbox(
    tempoAtendimentoBerco,
    tempoAberto,
    id_berco,
    get_abertura
)where

import Objetos
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

