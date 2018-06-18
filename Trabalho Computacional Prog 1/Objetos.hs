module Objetos 
(Berco, 
 Navio,
 VetorAtendimento,
 ListaDeNavios,
 ListaDeBercos,
 ListaTempoAtendimento,
 NaviosNoBerco,
 NaviosAlocadosBerco
 ) where

type Berco = (Int, Int, Int)
type Navio = (Int, Int, Int, Int)
type ListaDeNavios = [Navio]
type ListaDeBercos = [Berco]
type VetorAtendimento = [Int]
type ListaTempoAtendimento = [VetorAtendimento]
type NaviosNoBerco = (Berco,ListaDeNavios)
type NaviosAlocadosBerco = [NaviosNoBerco]

 