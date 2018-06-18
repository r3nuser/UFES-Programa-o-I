module NavioToolbox(
    id_navio,
    get_navio,
    get_chegada,
    concatNavio,
    vetor_cargas
)where

import Objetos

--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA O ID DO NAVIO                                           --
--------------------------------------------------------------------------------
id_navio::Navio->Int
id_navio (id, chegada, partida, qnt) = id
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA UM NAVIO PELO ID NUMA LISTA DE NAVIOS                   --
--------------------------------------------------------------------------------
get_navio::ListaDeNavios->Int->Navio
get_navio listaDeNavios id = listaDeNavios!!(id - 1)
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA A HORA DA CHEGADA DO NAVIO                              --
--------------------------------------------------------------------------------
get_chegada::Navio->Int
get_chegada (id, chegada, partida, qnt) = chegada
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA A CARGA DO NAVIO                                        --
--------------------------------------------------------------------------------
get_carga::Navio->Int
get_carga (id, chegada, partida, qnt) = qnt
--------------------------------------------------------------------------------
-- FUNCAO QUE CONCATENA UM NAVIO NUMA LISTA DE NAVIOS                         --
--------------------------------------------------------------------------------
concatNavio::Navio->ListaDeNavios->ListaDeNavios
concatNavio navio listaNavios = [navio] ++ listaNavios
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA VETOR DE CARGAS DE UMA RESPECTIVA LISTA DE NAVIOS       --
--------------------------------------------------------------------------------
vetor_cargas::ListaDeNavios->[Int]
vetor_cargas listaNavios = [ get_carga x | x <- listaNavios]


