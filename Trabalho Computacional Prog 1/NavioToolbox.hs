module NavioToolbox(
    id_navio,
    get_navio,
    get_chegada,
    concatNavio,
    vetor_cargas
)where

import Objetos

id_navio::Navio->Int
id_navio (id, chegada, partida, qnt) = id

get_navio::ListaDeNavios->Int->Navio
get_navio listaDeNavios id = listaDeNavios!!(id - 1)

get_chegada::Navio->Int
get_chegada (id, chegada, partida, qnt) = chegada

get_carga::Navio->Int
get_carga (id, chegada, partida, qnt) = qnt

concatNavio::Navio->ListaDeNavios->ListaDeNavios
concatNavio navio listaNavios = [navio] ++ listaNavios

vetor_cargas::ListaDeNavios->[Int]
vetor_cargas listaNavios = [ get_carga x | x <- listaNavios]


