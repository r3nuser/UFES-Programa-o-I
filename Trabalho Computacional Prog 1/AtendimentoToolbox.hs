module AtendimentoToolbox(
    tempoAtendimento
) where

import Objetos
import NavioToolbox
import BercoToolbox
import ListaToolbox

tempoAtendimento::Navio->Berco->ListaTempoAtendimento->Int
tempoAtendimento navio berco infoPorto = (tempoAtendimentoBerco berco infoPorto)!!(id_navio navio - 1)
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA UMA LISTA DE TEMPOS OCIOSOS DE UMA LISTA DE BERCOS E    --
-- UMA LISTA DE NAVIOS NO BERCO                                               --
--------------------------------------------------------------------------------
listaTempoOcioso::ListaDeBercos->NaviosAlocadosBerco->[(Berco,Int)]
listaTempoOcioso bercos naviosAlocados = [ (berco, tempo) | berco<-bercos, tempo<-[tempoOcioso berco (naviosAlocados!!(id_berco berco - 1))] ]
--------------------------------------------------------------------------------
-- FUNCAO QUE INFORMA OS NAVIOS QUE PODEM ATACAR EM UM BERCO ESPECIFICO       --
--------------------------------------------------------------------------------
naviosCandidatos::Berco->ListaDeNavios->ListaDeNavios
naviosCandidatos berco listaNavios = [ y | y <- listaNavios, atendido y berco infoPorto]

 