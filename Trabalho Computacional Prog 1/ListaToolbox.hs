module ListaToolbox(
    somatorio,
    maximo
)where

--------------------------------------------------------------------------------
-- FUNCOES QUE RETORNA O SOMATORIO DE UMA LISTA DE INTEIROS                   --
--------------------------------------------------------------------------------
summer::[Int]->Int->Int
summer xs soma | null xs = soma
               | otherwise = summer (tail xs) (xs!!0 + soma)
somatorio::[Int]->Int
somatorio xs = summer xs 0
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA QUAIS VALORES SAO MAIOR QUE X NUMA LISTA XS             --
--------------------------------------------------------------------------------
maior_que::Int->[Int]->[Int]
maior_que x xs = [ y | y <- xs, y > x]
--------------------------------------------------------------------------------
-- FUNCAO QUE RETORNA O MAXIMO VALOR INTEIRO DE UMA LISTA                             --
--------------------------------------------------------------------------------
maximo::[Int]->Int
maximo xs = [ x | x <- xs, null (maior_que x xs) ]!!0 