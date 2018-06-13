module Objetos 
(Berco, 
 Navio,
 VetorAtendimento,
 ListaDeNavios,
 ListaDeBercos,
 ListaTempoAtendimento,
 NaviosNoBerco
 ) where

type Berco = (Int, Int, Int)
type Navio = (Int, Int, Int, Int)
type VetorAtendimento = [Int]
type ListaDeNavios = [Navio]
type ListaDeBercos = [Berco]
type ListaTempoAtendimento = [VetorAtendimento]
type NaviosNoBerco = (Berco,ListaDeNavios)

 