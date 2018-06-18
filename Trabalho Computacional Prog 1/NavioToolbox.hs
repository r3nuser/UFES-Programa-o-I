module NavioToolbox(
    id_navio,
    get_navio,
    get_chegada
)where

import Objetos

id_navio::Navio->Int
id_navio (id, chegada, partida, qnt) = id

get_navio::ListaDeNavios->Int->Navio
get_navio listaDeNavios id = listaDeNavios!!(id - 1)

get_chegada::Navio->Int
get_chegada (id, chegada, partida, qnt) = chegada

