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
tempoAtendimento::Navio->Berco->ListaTempoAtendimento->Int
tempoAtendimento navio berco infoPorto = (tempoAtendimentoBerco berco infoPorto)!!(id_navio navio - 1)
--------------------------------------------------------------------------------
-- FUNCAO QUE INFORMA OS NAVIOS QUE PODEM ATACAR EM UM BERCO ESPECIFICO       --
--------------------------------------------------------------------------------
naviosCandidatos::Berco->ListaDeNavios->ListaDeNavios
naviosCandidatos berco listaNavios = [ y | y <- listaNavios, atendido y berco infoPorto]
--------------------------------------------------------------------------------
-- DADO UM NAVIO EM UMA LISTA DE NAVIOS, PROCURA O ELEMENTO ANTERIOR          --
--------------------------------------------------------------------------------
searchAnterior::Navio->ListaDeNavios->Navio 
searchAnterior navio fila | fila!!1==navio = head fila
                          | otherwise = searchAnterior navio (tail fila)
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA O TEMPO DE ESPERA DE UM NAVIO QUE ->PODE-< SER ALOCADO  --
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
-- DADO UM NAVIO, UM BERCO E AS INFORMAÇÕES DOS TEMPOS DE ATENDIMENTO DOS     --
-- NAVIOS NOS BERÇOS DO PORTO, A FUNCAO VERIFICA SE O NAVIO PODE SER ATENDIDO --
--                                                                            --
-- PARAMETRO DE AVALIACAOO:                                                   --
-- SE O TEMPO DE ATENDIMENTO FOR NULO O NAVIO NÃO PODERA ATRACAR NO BERCO.    --                                                   --
--------------------------------------------------------------------------------
atendido::Navio->Berco->ListaTempoAtendimento->Bool
atendido navio berco atendimento = tempoAtendimento/=0 && get_chegada navio >= get_abertura berco
                                    where 
                                        tempoAtendimento = (atendimento!!(id_berco berco - 1))!!(id_navio navio - 1)
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
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA UMA LISTA DE TEMPOS OCIOSOS DE UMA LISTA DE BERCOS E    --
-- UMA LISTA DE NAVIOS NO BERCO                                               --
--------------------------------------------------------------------------------
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
-- FUNCAO QUE INFORMA OS NAVIOS QUE PODEM ATRACAR EM UMA LISTA DE BERCOS      --
--------------------------------------------------------------------------------
naviosCandidatosBerco::ListaDeBercos->ListaDeNavios->[(Int,ListaDeNavios)]
naviosCandidatosBerco bercos listaNavios = [ (id_berco x, naviosCandidatos x listaNavios) | x <- bercos ]
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA SE UM DADO NAVIO PODE SER ATENDIDO NO BERCO, QUANTIDADE --
-- TOTAL DE PRODUTOS CARREGADOS NO NAVIO E OS NAVIOS QUE FORAM ALOCADOS       --
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
-- RETORNA QUAL HORARIO DE ATENDIMENTO DE CADA NAVIO ATRACADO                 --
--------------------------------------------------------------------------------
--horariosAtendimento::ListaDeNavios->[(Int,Int,Int)]

--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA UMA TRIPLA COM TODAS AS INFORMACOES DE ATENDIMENTO DADO --
-- UM BERCO E UMA LISTA DE NAVIOS                                             --
--------------------------------------------------------------------------------
--constroiAlocacaoBerco::Berco->ListaDeNavios->(Berco,Int,[(Int,Int,Int)]])
--constroiAlocacaoBerco berco navios infoPorto = (berco, qnt_total, navioAtendido)
--                                where
--                                    naviosOrdenados = filaNavios navios
--                                    candidatos = naviosCandidatos berco naviosOrdenados
--                                    vetorAtendimento = [ tempoAtendimento x berco infoPorto | x <- naviosOrdenados ]
--                                    naviosAtracados = podeAtracar candidatos vetorAtendimento berco
--                                    qnt_total = somatorio (vetor_cargas naviosAtracados)
                                    --navioAtendido = horariosAtendimento naviosAtracados