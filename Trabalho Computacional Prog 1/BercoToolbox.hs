module BercoToolbox(
    tempoAtendimentoBerco,
    tempoAberto,
    id_berco,
    get_abertura
)where

import Objetos

id_berco::Berco->Int 
id_berco (id, abertura, fechamento) = id

tempoAtendimentoBerco::Berco->ListaTempoAtendimento->VetorAtendimento
tempoAtendimentoBerco berco infoPorto = infoPorto!!(id_berco berco - 1)

tempoAberto::Berco->Int
tempoAberto (id, abertura, fechamento) = fechamento - abertura

get_abertura::Berco->Int
get_abertura (id, abertura, fechamento) = abertura

