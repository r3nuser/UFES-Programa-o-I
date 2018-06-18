import AtendimentoToolbox
import BercoToolbox
import ListaToolbox
import NavioToolbox
import Objetos

--------------------------------------------------------------------------------
-- EXEMPLOS UTILIZADOS PARA A REALIZACAO DOS TESTES NECESSARIOS               --
--------------------------------------------------------------------------------
listaNavios::ListaDeNavios
listaNavios = [(1,5,16,30), (2,6,18,30), (3,3,12,50), (4,4,22,50), (5,11,20,80)]

listaBercos::ListaDeBercos
listaBercos = [(1,4,20), (2,3,18)]

infoPorto::ListaTempoAtendimento
infoPorto = [[1, 6, 4, 4, 6], [2, 0, 1, 0, 5]]

naviosAlocadosBerco1::NaviosNoBerco
naviosAlocadosBerco1 = ((1,4,20), [(4,4,22,50), (5,11,20,80)])

naviosAlocadosBerco2::NaviosNoBerco
naviosAlocadosBerco2 = ((2,3,18), [(3,3,12,50), (1,5,16,30)])

naviosAlocadosBerco::NaviosAlocadosBerco
naviosAlocadosBerco = [naviosAlocadosBerco1, naviosAlocadosBerco2]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA SE UM VALOR C ESTÁ NO INTERVALO (A,B) OU (B,A) SE A > B --
--------------------------------------------------------------------------------
mod_intervalo::Int->Int->Int->Bool
mod_intervalo a b c | a > b = a >= c && b <= c
                    | otherwise = a <= c && b >= c 

--------------------------------------------------------------------------------
-- DADO UM NAVIO, UM BERCO E AS INFORMAÇÕES DOS TEMPOS DE ATENDIMENTO DOS     --
-- NAVIOS NOS BERÇOS DO PORTO, A FUNCAO VERIFICA SE O NAVIO PODE SER ATENDIDO --
--                                                                            --
-- PARAMETRO DE AVALIACAOO:                                                   --
-- SE O TEMPO DE ATENDIMENTO FOR NULO O NAVIO NÃO PODERA ATRACAR NO BERCO.    --                                                   --
--------------------------------------------------------------------------------
atendido::Navio->Berco->ListaTempoAtendimento->Bool
atendido navio berco atendimento = (atendimento!!(id_berco berco - 1))!!(id_navio navio - 1)/=0

--------------------------------------------------------------------------------
-- DADO UMA LISTA DE NAVIOS, ESSA FUNCAO RETORNA A ORDEM DE CHEGADA DO MESMO  --
--                                                                            --
-- FUNCIONAMENTO DA FUNÇÃO:                                                   --
-- A FUNÇÃO ORDENA OS NAVIOS DE ACORDO COM A HORA DA FUNÇÃO. PARA ORDENAR FOI --
-- UTILIZADO UMA IMPLEMENTACAO DO ALGORITMO QUICKSORT, QUE DIVIDE A TAREFA EM --
-- TAREFAS MENORES E GERA SUBLISTAS MENORES DE MANEIRA RECURSIVA.             --
--------------------------------------------------------------------------------
filaNavios::ListaDeNavios->ListaDeNavios
filaNavios [] = []
filaNavios (s:xs) = inicio_meio ++ [s] ++ meio_fim
                   where
                       inicio_meio = filaNavios [ x | x <- xs,get_chegada x < get_chegada s]
                       meio_fim = filaNavios [ x | x <- xs,get_chegada x >= get_chegada s]

--------------------------------------------------------------------------------
-- FUNCAO QUE CALCULA A JANELA DE TEMPO OCIOSO NO BERCO DENTRO DE SUA JANELA  --
-- DE TRABALHO.                                                               --
--------------------------------------------------------------------------------
tempoOcioso::Berco->NaviosNoBerco->Int
tempoOcioso berco naviosAlocadosBerco = tempoAberto berco - somatorio listaAtendimentoNavio
                                       where
                                           listaAtendimentoNavio = [tempoAtendimento x berco infoPorto | x <- navios ]
                                               where
                                                   navios = snd(naviosAlocadosBerco)

listaTempoOcioso::ListaDeBercos->NaviosAlocadosBerco->[(Berco,Int)]
listaTempoOcioso bercos naviosAlocados = [ (berco, tempo) | berco<-bercos, tempo<-[tempoOcioso berco (naviosAlocados!!(id_berco berco - 1))] ]

--------------------------------------------------------------------------------
-- FUNCAO QUE CALCULA QUAL BERCO TEM O MAIOR TEMPO OCIOSO E RETORNA SEU ID    --
--------------------------------------------------------------------------------
bercoOcioso::ListaDeBercos->NaviosAlocadosBerco->Int
bercoOcioso bercos naviosAlocadosBerco = id_berco(fst([ x | x <- lista, maximo listaMapeada == snd x]!!0))
                                           where
                                              lista = listaTempoOcioso listaBercos naviosAlocadosBerco
                                              listaMapeada = map snd lista

--------------------------------------------------------------------------------
-- FUNÇÃO QUE INFORMA OS NAVIOS QUE PODEM ATACAR NOS BERCOS                   --
--------------------------------------------------------------------------------
--naviosCandidatosBerco::ListaDeBercos->ListaDeNavios->[(Int,ListaDeNavios)]
--naviosCandidatosBerco bercos listaNavios = 

--------------------------------------------------------------------------------
-- --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- --
--------------------------------------------------------------------------------