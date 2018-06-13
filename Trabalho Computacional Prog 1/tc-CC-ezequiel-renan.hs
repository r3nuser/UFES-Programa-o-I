import Objetos


listaNavios::ListaDeNavios
listaNavios = [(1,5,16,30), (2,6,18,30), (3,3,12,50), (4,4,22,50), (5,11,20,80)]

listaBercos::ListaDeBercos
listaBercos = [(1,4,20), (2,3,18)]

infoPorto::ListaTempoAtendimento
infoPorto = [[1, 6, 4, 4, 6], [2, 0, 1, 0, 5]]

naviosAlocadosBerco1 = ((1,4,20), [(4,4,22,50), (5,11,20,80)])
naviosAlocadosBerco2 = ((2,3,18), [(3,3,12,50), (1,5,16,30)])
naviosAlocadosBerco = [naviosAlocadosBerco1, naviosAlocadosBerco2]

mod_intervalo::Int->Int->Int->Bool
mod_intervalo a b c | a > b = a >= c && b <= c
                    | otherwise = a <= c && b >= c 

--atendido::Navio->Berco->ListaTempoAtendimento->Bool



