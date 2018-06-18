module ListaToolbox(
    somatorio,
    maximo
)where

summer::[Int]->Int->Int
summer xs soma | null xs = soma
               | otherwise = summer (tail xs) (xs!!0 + soma)
somatorio::[Int]->Int
somatorio xs = summer xs 0

maior_que::Int->[Int]->[Int]
maior_que x xs = [ y | y <- xs, y > x]
maximo::[Int]->Int
maximo xs = [ x | x <- xs, null (maior_que x xs) ]!!0 