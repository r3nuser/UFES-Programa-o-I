--------------------------------------------------------------------------------
-- TRABALHO COMPUTACIONAL - 2018/1 - Prof: Claudia Boeres                     -- 
--------------------------------------------------------------------------------
-- Participante 1: Ezequiel Schneider Reinholtz                               --
-- Participante 2: Renan Moreira Gomes                                        --
-- Data: 29/06/2018                                                           --
--------------------------------------------------------------------------------

import BercoToolbox
import ListaToolbox
import NavioToolbox
import Objetos
--------------------------------------------------------------------------------
-- EXEMPLOS UTILIZADOS PARA A REALIZACAO DOS TESTES NECESSARIOS               --
--------------------------------------------------------------------------------
-- LISTA DE NAVIOS                                                            --
listaNavios::ListaDeNavios
listaNavios = [(1,5,16,30), (2,6,18,30), (3,3,12,50), (4,4,22,50), (5,11,20,80)]
-- LISTA DE BERCOS                                                            --
listaBercos::ListaDeBercos
listaBercos = [(1,4,20), (2,3,18)]
-- INFORMACOES DO PORTO                                                       --
infoPorto::ListaTempoAtendimento
infoPorto = [[1, 6, 4, 4, 6], [2, 0, 1, 0, 5]]
-- NAVIOS ALOCADOS NO BERCO 1                                                 --
naviosAlocadosBerco1::NaviosNoBerco
naviosAlocadosBerco1 = ((1,4,20), [(4,4,22,50), (5,11,20,80)])
-- NAVIOS ALOCADOS NO BERCO 2                                                 --
naviosAlocadosBerco2::NaviosNoBerco
naviosAlocadosBerco2 = ((2,3,18), [(3,3,12,50), (1,5,16,30)])
-- LISTA DE NAVIOS ALOCADOS EM TODOS OS BERCOS                                --
naviosAlocadosBerco::NaviosAlocadosBerco
naviosAlocadosBerco = [naviosAlocadosBerco1, naviosAlocadosBerco2]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA SE UM VALOR C ESTÁ NO INTERVALO (A,B) OU (B,A) SE A > B --
--------------------------------------------------------------------------------
intervalo::Int->Int->Int->Bool
intervalo a b c | a > b = a >= c && b <= c
                | otherwise = a <= c && b >= c 
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA O TEMPO DE ATENDIMENTO DE UM NAVIO NO BERCO             --
--------------------------------------------------------------------------------
-- A função busca pelo id do navio, seu respectivo tempo de atendimento no    --
-- berco.                                                                     --
--------------------------------------------------------------------------------
tempoAtendimento::Navio->Berco->ListaTempoAtendimento->Int
tempoAtendimento navio berco infoPorto = (tempoAtendimentoBerco berco infoPorto)!!(id_navio navio - 1)
--------------------------------------------------------------------------------
-- FUNCAO QUE INFORMA OS NAVIOS QUE PODEM ATACAR EM UM BERCO ESPECIFICO       --
--------------------------------------------------------------------------------
naviosCandidatos::Berco->ListaDeNavios->ListaTempoAtendimento->ListaDeNavios
naviosCandidatos berco listaNavios infoPorto = [ y | y <- listaNavios, atendido y berco infoPorto]
--------------------------------------------------------------------------------
-- DADO UM NAVIO EM UMA LISTA DE NAVIOS, PROCURA O ELEMENTO ANTERIOR          --
--------------------------------------------------------------------------------
searchAnterior::Navio->ListaDeNavios->Navio 
searchAnterior navio fila | fila!!1==navio = head fila
                          | otherwise = searchAnterior navio (tail fila)
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA O TEMPO DE ESPERA DE UM NAVIO QUE ->PODE<- SER ALOCADO  --
--------------------------------------------------------------------------------
tempoEspera::Navio->ListaDeNavios->Berco->ListaTempoAtendimento->Int
tempoEspera navio fila berco infoPorto  | tempo < 0 = 0
                                        | otherwise = tempo
                                          where 
                                              navioAnterior = searchAnterior navio fila
                                              chegadaAnterior = get_chegada navioAnterior
                                              chegadaAtual = get_chegada navio
                                              tempo = ((tempoAtendimento navioAnterior berco infoPorto) + chegadaAnterior) - chegadaAtual
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA SE O NAVIO PODE SER ATENDIDO NO BERCO                   --
--------------------------------------------------------------------------------
-- A função recebe o navio, o berço e lista do tempo de atendimento e apartir --
-- daí, ela verifica se o tempo de atendimento é diferente de 0 e se a        --
-- chegada do navio é maior ou igual a abertura do berco e se a saida do navio--
-- é menor ou igual a hora de fechamento do berco                             --
--------------------------------------------------------------------------------
atendido::Navio->Berco->ListaTempoAtendimento->Bool
atendido navio berco atendimento = tempoAtendimento/=0 && chegadaAbertura
                                   where 
                                       tempoAtendimento = (atendimento!!(id_berco berco - 1))!!(id_navio navio - 1)
                                       chegadaAbertura = get_chegada navio >= get_abertura berco
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
-- A função pega o somatório do tempo de atendimento dos navios alocados no   --
-- berço e este valor é subtraido do tempo total de funcionamento do berco    --
--------------------------------------------------------------------------------
tempoOcioso::Berco->NaviosNoBerco->ListaTempoAtendimento->Int
tempoOcioso berco naviosAlocadosBerco infoPorto = tempoAberto berco - somatorio listaAtendimentoNavio
                                                  where
                                                      listaAtendimentoNavio = [tempoAtendimento x berco infoPorto | x <- navios ]
                                                       where
                                                           navios = snd(naviosAlocadosBerco)
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA UMA LISTA DE TEMPOS OCIOSOS DE UMA LISTA DE BERCOS E    --
-- UMA LISTA DE NAVIOS NO BERCO                                               --
--------------------------------------------------------------------------------
listaTempoOcioso::ListaDeBercos->NaviosAlocadosBerco->ListaTempoAtendimento->[(Berco,Int)]
listaTempoOcioso bercos naviosAlocados infoPorto = [ (berco, tempo) | berco<-bercos, tempo<-[tempoOcioso berco (naviosAlocados!!(id_berco berco - 1)) infoPorto] ]
--------------------------------------------------------------------------------
-- FUNCAO QUE CALCULA QUAL BERCO TEM O MAIOR TEMPO OCIOSO E RETORNA SEU ID    --
-- A função pega o maximo do segundo elemento de cada tupla e retorna o id do --
-- berço correspondente daquela tupla.                                        --
--------------------------------------------------------------------------------
bercoOcioso::ListaDeBercos->NaviosAlocadosBerco->ListaTempoAtendimento->Int
bercoOcioso bercos naviosAlocadosBerco infoPorto = id_berco(fst([ x | x <- lista, maximo listaMapeada == snd x]!!0))
                                         where
                                             lista = listaTempoOcioso listaBercos naviosAlocadosBerco infoPorto
                                             listaMapeada = map snd lista
--------------------------------------------------------------------------------
-- FUNCAO QUE INFORMA OS NAVIOS QUE PODEM ATRACAR EM UMA LISTA DE BERCOS      --
-- A função utiliza o "naviosCandidatos" que retorna uma lista de navios que  --
-- podem ser alocados no berco, e organiza em uma tupla com o id do berco e a --
-- lista em si.                                                               --
--------------------------------------------------------------------------------
naviosCandidatosBerco::ListaDeBercos->ListaDeNavios->ListaTempoAtendimento->[(Int,ListaDeNavios)]
naviosCandidatosBerco bercos listaNavios infoPorto = [ (id_berco x, naviosCandidatos x listaNavios infoPorto) | x <- bercos ]
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA SE UM DADO NAVIO PODE SER ATENDIDO NO BERCO, QUANTIDADE --
-- TOTAL DE PRODUTOS CARREGADOS NO NAVIO E OS NAVIOS QUE FORAM ALOCADOS       --
-- A função avalia se o navio pode ser atendido ou não, após isso de acordo   --
-- com o resultado anterior faz a somatoria das cargas dos navios e mostra    --
-- os navios ordenados.                                                       --
-- OBS:: No primeiro argumento, ao invez de retornar um Int, retornamos um    --
-- valor booleano, indicando True se o navio pode ser alocado e False caso o  --
-- navio não possa.                                                           --
--------------------------------------------------------------------------------
insereNavioBerco::Navio->NaviosNoBerco->(Bool, Int, ListaDeNavios)
insereNavioBerco navio naviosAlocadosBerco = (isAtendido, qnt_total, filaNavios naviosAlocados)
                                             where 
                                                 berco = fst naviosAlocadosBerco
                                                 navios = snd naviosAlocadosBerco
                                                 isAtendido = atendido navio berco infoPorto
                                                 naviosAlocados | isAtendido = concatNavio navio navios  
                                                                | otherwise = navios
                                                 qnt_total = somatorio (vetor_cargas naviosAlocados)
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA O TEMPO DE ESPERA DO NAVIO CASO ELE TENHA SIDO ALOCADO  --
--------------------------------------------------------------------------------
esperaNavio::Navio->NaviosNoBerco->String
esperaNavio navio naviosAlocadosBerco | atendido navio berco infoPorto = show(tempoEspera navio fila berco infoPorto)
                                      | otherwise = "O navio "++show (id_navio navio)++" nao foi alocado no berco "++show (id_berco berco)
                                        where
                                            berco = fst naviosAlocadosBerco
                                            navios = snd naviosAlocadosBerco
                                            fila = filaNavios (concatNavio navio navios)
--------------------------------------------------------------------------------
-- DADO UM NUMERO INTEIRO, SE FOR POSITIVO RETORNA 0                          --
--------------------------------------------------------------------------------
nonNegative::Int->Int
nonNegative number = if number > 0 then number else 0
--------------------------------------------------------------------------------
-- RETORNA QUAL TEMPO OCIOSO NETRE NAVIOS                                     --
--------------------------------------------------------------------------------
ociosoEntreNavios::ListaDeNavios->Berco->ListaTempoAtendimento->Int->Int
ociosoEntreNavios navios berco infoPorto h | length navios < 2 = h
                                           | otherwise = ociosoEntreNavios (tail navios) berco infoPorto (h + ociosoDoisNavios)
                                             where 
                                                 ociosoDoisNavios = nonNegative(get_chegada (navios!!1) - (get_chegada (navios!!0) + (tempoAtendimento (navios!!0) berco infoPorto)))
                                            
                                               
--------------------------------------------------------------------------------
-- RETORNA QUAIS NAVIOS PODERAM ATRACAR DE ACORDO COM O TEMPO DE ATENDIMENTO  --
--------------------------------------------------------------------------------
podeAtracar::ListaDeNavios->VetorAtendimento->Berco->ListaDeNavios
podeAtracar navios vetorAtendimento berco | (somatorio (vetorAtendimento)+ocioso) > (tempoAberto berco) = caseMaior
                                          | otherwise = navios
                                            where
                                                caseMaior = podeAtracar (init navios) (init vetorAtendimento) berco
                                                ocioso = ociosoEntreNavios navios berco infoPorto 0
--------------------------------------------------------------------------------
-- RETORNA TERCEIRO ELEMENTO DE UMA TRIPLA DE INTEIROS                        --
--------------------------------------------------------------------------------
trd::(Int,Int,Int)->Int
trd (x,y,z) = z
--------------------------------------------------------------------------------
-- RETORNA O MAIOR VALOR ENTRE DOIS VALORES                                   --
--------------------------------------------------------------------------------
maior::Int->Int->Int
maior a b = if a > b then a else b
--------------------------------------------------------------------------------
-- RETORNA QUAL HORARIO DE ATENDIMENTO DE CADA NAVIO ATRACADO                 --
-- Dada uma tupla contendo uma lista de navios e seus respectivos tempos de   --
-- atendimento, realiza recursivamente o calculo de chegada do navio e sua    --
-- saida de acordo com o tempo de atendimento.                                --
--------------------------------------------------------------------------------
horariosAtendimento::(ListaDeNavios,VetorAtendimento)->[(Int,Int,Int)]->[(Int,Int,Int)]
horariosAtendimento (navios,atendimento) n | null navios = n
                                           | n == [] = horariosAtendimento (tail navios, tail atendimento) ((id_navio (navios!!0), (get_chegada (navios!!0)), (get_chegada (navios!!0)) + (atendimento!!0)  ):n)
                                           | otherwise = horariosAtendimento (tail navios, tail atendimento) (n++[(id_navio navio_atual, chegada,saida)])
                                                         where
                                                             navio_atual = navios!!0
                                                             tempo_atendimento = atendimento!!0
                                                             chegada = maior (trd (last n)) (get_chegada navio_atual)
                                                             saida = chegada + tempo_atendimento
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA UMA TRIPLA COM TODAS AS INFORMACOES DE ATENDIMENTO DADO --
-- UM BERCO E UMA LISTA DE NAVIOS                                             --
-- Primeiramente é realizada uma ordenação da lista de navios que foi dada    --
-- como entrada. Após isso utilizados a funcao "naviosCandidatos" para avaliar--
-- quais navios dessa lista são candidatos a alocação.                        --
-- Logo após, utilizamos a função "podeAtracar" para filtrar de fato quem     --
-- poderá ser atendido no respectivo berço, respeitando o tempo de atendimento--
-- do navio e o tempo de funcionamento do berço.                              --
-- Após isso, realizamos o somatório de cargas dos navios que atracaram.      --
-- Por fim, é utilizada a função "horariosAtendimento" para calcular o tempo  --
-- de inicio e o final de carregamento dos navios, para então, dar como       --
-- resultado uma tiṕla contendo o berço, somatório das cargas e os navios     --
-- alocados com o id, tempo de entrada e tempo de saida.                      --
--------------------------------------------------------------------------------
constroiAlocacaoBerco::Berco->ListaDeNavios->ListaTempoAtendimento->(Berco,Int,[(Int,Int,Int)])
constroiAlocacaoBerco berco navios infoPorto = (berco, qnt_total, navioAtendido)
                                               where
                                                   naviosOrdenados = filaNavios navios
                                                   candidatos = naviosCandidatos berco naviosOrdenados infoPorto
                                                   vetorAtendimento = [ tempoAtendimento x berco infoPorto | x <- candidatos ]
                                                   naviosAtracados = podeAtracar candidatos vetorAtendimento berco
                                                   qnt_total = somatorio (vetor_cargas naviosAtracados)
                                                   navioAtendido = horariosAtendimento (naviosAtracados,[ tempoAtendimento x berco infoPorto | x <- naviosAtracados ]) []  