module AtendimentoToolbox(
    tempoAtendimento
) where

import Objetos
import NavioToolbox
import BercoToolbox
import ListaToolbox

tempoAtendimento::Navio->Berco->ListaTempoAtendimento->Int
tempoAtendimento navio berco infoPorto = (tempoAtendimentoBerco berco infoPorto)!!(id_navio navio - 1)




 