--------------------------------------------------------------------------------
-- TRABALHO COMPUTACIONAL - 2018/1 - Prof: Claudia Boeres                     -- 
--------------------------------------------------------------------------------
-- Participante 1: Ezequiel Schneider Reinholtz                               --
-- Participante 2: Renan Moreira Gomes                                        --
-- Data: 29/06/2018                                                           --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- MODULO NAVIOTOOLBOX                                                        --
--------------------------------------------------------------------------------
-- Este modulo contem funcoes que manipulam objetos do tipo navio             --
--------------------------------------------------------------------------------
-- Funcoes Utilizaveis:                                                       --
-- - id_navio : Retorna o id do navio                                         --
-- - get_navio : Retorna o navio pelo id em uma lista de navios               --
-- - get_chegada : Retorna a hora de chegada do navio                         --
-- - get_saida : Retorna a hora de saida do navio                             --
-- - concatNavio : Concatena um navio em uma lista de navios                  --
-- - vetor_cargas : Retorna um vetor de cargas de uma lista de navios         --
--                                                                            --
-- Funcoes Internas:                                                          --
-- - get_carga : Retorna a carga de um navio                                  --
--------------------------------------------------------------------------------

module NavioToolbox(
    id_navio,
    get_navio,
    get_chegada,
    get_saida,
    concatNavio,
    vetor_cargas
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
-- FUNCAO QUE RETORNA A HORA DE CHEGADA DO NAVIO                              --
--------------------------------------------------------------------------------
get_chegada::Navio->Int
get_chegada (id, chegada, partida, qnt) = chegada
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA A HORA DE SAIDA DO NAVIO                              --
--------------------------------------------------------------------------------
get_saida::Navio->Int
get_saida (id, chegada, partida, qnt) = partida
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


