{-- 

TESTE AS SEGUINTES FUNÇÕES E COMENTE SEU COMPORTAMENTO

--}

-- foo0 = compare 2 (mod 17 2) == EQ
-- A função foo0 compara os valores 2 e resto da divisão de 17 por 2 e verifica se eles são o mesmo numero

-- foo1 = x + y 
--        where 
--            x = y 
--            y = 2
-- A funcao foo1 e uma função constante e sem entrada que soma os valores x + y onde x = y e y = 2

-- foo2 = x + y
--        where 
--            x = y
--                where y = 2         
-- A função foo2 é uma função constante e sem entrada que dá erro pois o 'x + y' ele não tem um valor para y. O where que define valor para o y só vale para o 'x = y' de acordo com a função.

-- foo3 x = if (x <= 30) then 'D'
--          else if (x <= 50) then 'C'
--          else if (x <= 80) then 'B'
--          else 'A'
-- A função foo3 é uma simples função que recebe algum valor e de acordo com esse valor dá um resultado diferente, no caso se a entrada for menor ou igual que 30 então aparece D, senão se for maior ou igual que 50 então C, senão se for menor ou igual que 80 então b, senão A

{-- 

Exercício 2: 
Para cada uma das expressões abaixo, faça o que se pede:
I) Escreva descrições usando listas, para as segu
intes listas constantes:

--}
--a) múltiplos de 5 maiores que 0 e menores que 80;
multiplos_de_cinco::[Integer]
multiplos_de_cinco = [5,10..75]

--b) meses de um ano;
meses_do_ano::[String]
meses_do_ano = ["Janeiro","Fevereiro","Marco",
                "Abril","Maio","Junho",
                "Julho","Agosto","Setembro",
                "Outubro","Novembro","Dezembro"]

--c) número de dias por cada mês de um ano;
fevereiro_relativo::Bool->[Integer]
fevereiro_relativo bissexto | bissexto  = [ x | x <- [1 .. 29] ]
                            | otherwise = [ x | x <- [1 .. 28] ] 

n_dias_mes_do_ano::Bool->[[Integer]]
n_dias_mes_do_ano bissexto= [jan,fev,mar,abr,mai,jun,jul,ago,set,out,nov,dez]
                            where
                                jan = [ x | x <- [1 .. 31] ]
                                fev = fevereiro_relativo bissexto
                                mar = [ x | x <- [1 .. 31] ]
                                abr = [ x | x <- [1 .. 30] ]
                                mai = [ x | x <- [1 .. 31] ]
                                jun = [ x | x <- [1 .. 30] ]
                                jul = [ x | x <- [1 .. 31] ]
                                ago = [ x | x <- [1 .. 31] ]
                                set = [ x | x <- [1 .. 30] ]
                                out = [ x | x <- [1 .. 31] ]
                                nov = [ x | x <- [1 .. 30] ]
                                dez = [ x | x <- [1 .. 31] ]

--d) dias da semana;
dias_da_semana::[String]
dias_da_semana = ["Domingo","Segunda","Terca","Quarta","Quinta","Sexta","Sabado"]

--e) relação das disciplinas em que você está matriculado.
disciplinas_matriculadas::[String]
disciplinas_matriculadas = ["Calculo I", "Prog I", "Algebra Linear", "Introcomp", "ATC I"]

{-- 
II) 
Escreva as listas resultantes das descrições abaixo e depois compare com a resposta da avaliação 
da lista no interpretador:

--}

--a) [3*5, 4*5+2.. 100 - 5]

-- Resposta do GHCI: [15,22,29,36,43,50,57,64,71,78,85,92]
-- Descrição: Lista dá razão entre 4*5+2 e 3*5 até 100-5

--b) [2, 2*2 .. 4 * 5]

-- Resposta do GHCI: [2,4,6,8,10,12,14,16,18,20]
-- Descrição: Lista dá razão entre 2*2 e 2 até 4*5

--c) f x r t = [x, x + r .. t]

-- Resposta do GHCI: Depende da entrada
-- Descrição: Lista da razão entre x + r e x até t

{-- 

Exercício 3: 
Escreva um script com as definições das funções a seguir, de maneira que:
 i) identifique e utilize, quando necessário, a modularização
ii) sejam definições genéricas
iii) use definição local apenas quando necessário (promovendo a legibilidade do programa)
iv) comente seu código sempre que possível
v) resolva utilizando descrição por listas

--}

--a) Obter o menor valor de uma lista de números.
pmev::[Integer]->Integer->Integer
pmev xs menor | length xs == 0 = menor
              | last xs < menor = pmev (init xs) (last xs)
              | last xs >= menor = pmev (init xs) menor 
menor_valor::[Integer]->Integer
menor_valor xs = pmev xs (last xs)

--b) Dada uma lista xs, fornecer uma dupla contendo o menor e o maior elemento dessa lista.
pmav::[Integer]->Integer->Integer
pmav xs maior | length xs == 0 = maior
              | last xs >= maior = pmav (init xs) (last xs)
              | last xs < maior = pmav (init xs) maior
menor_e_maior::[Integer]->(Integer,Integer)
menor_e_maior xs = (pmav xs (last xs),pmev xs (last xs))

--c) Produzir uma lista dos múltiplos de um dado número n, menores ou iguais a um dado limite lim. Exemplo: g 5 20 -> [5, 10, 15, 20]
lista_de_multiplos::Integer->Integer->[Integer]
lista_de_multiplos n lim = [x | x <- [n, 2*n .. lim]]

--d)Dividir uma lista pela metade e apresentar cada uma das partes em uma dupla. Exemplo: divideLista [1,3,5,8,15] = ([1,3],[5,8,15] )
lista_metade::[a]->([a],[a])
lista_metade xs = (take meio xs, drop meio xs)
                  where
                      meio = div (length xs) 2                                             
                  
--e) Duplicar os elementos de uma lista. Exemplo: duplicaLista [1,2,3] -> [1,1,2,2,3,3]
dup_hack::[a]->[a]->Int->[a]
dup_hack xt xs index| length xt == 2 * length xs = xt
                    | otherwise = dup_hack (xt ++ xs !! (index + 1) : [(xs !! (index + 1))] ) xs (index + 1)
duplicar_elementos::[a]->[a]
duplicar_elementos xs = dup_hack [] xs (-1)

--f) Dadas duas listas de elementos distintos, determinar a união delas.
uniao_lista::[a]->[a]->[a]
uniao_lista xs xt = xs ++ xt;

-- g)Dadas duas listas de elementos distintos, determinar a interseção delas.
elemento_incluso ::(Eq a) => a -> [a] -> Bool
elemento_incluso elemento xs | length xs == 0 = False
                             | head xs == elemento  = True
                             | otherwise = elemento_incluso elemento (tail xs)
busca_intersec::(Eq a) => [a]->[a]->[a]->[a]
busca_intersec intersecs xs xt | length xs == 0 = intersecs
                               | elemento_incluso (xs !! 0) xt = busca_intersec (intersecs ++ head xs : []) (tail xs) xt
                               | otherwise = busca_intersec intersecs (tail xs) xt
intersec_listas::(Eq a) => [a]->[a]->[a]
intersec_listas xs xt = busca_intersec [] xs xt

--h) Calcule a distância de Hamming entre dois números inteiros que possuam, cada um, exatamente n algarismos. A distância de Hamming corresponde ao número de algarismos que diferem em suas posições correspondentes.

--i) Dada uma lista l, contendo uma quantidade igual de números inteiros pares e ímpares (em qualquer ordem), defina uma função que, quando avaliada, produz uma lista na qual esses números pares   e   ímpares   encontram-se   alternados.   Exemplo:   alternaLista   [10,2,31,45,6,18,5,20,15,19]   ->[10,31,2,45,6,5,18,15,20,19]
concat_pares_impares::[Integer]->[Integer]->[Integer]->[Integer]
concat_pares_impares xs xt xu | length xs == 0 = xu
                              | otherwise = concat_pares_impares (tail xs) (tail xt) (xu ++ head xs : [] ++ head xt : [])
pares_e_impares::[Integer]->[Integer]
pares_e_impares xs = concat_pares_impares [x | x <- xs, even x] [x | x <- xs, odd x] []

--j) Implemente as funções take e drop.

--k) Verificar se um caracter dado como entrada é uma letra.

--l) Verificar se um caracter dado como entrada é um dígito.

--m) Verificar se uma cadeia de caracteres é uma palavra (ou seja, é formada apenas de letras).

--n) Verificar se uma cadeia de caracteres representa um número inteiro positivo (ou seja, a cadeia de caracteres só é formada por dígitos).

--o) Dada uma cadeia de caracteres, contar o número de ocorrências de vogais, para cada vogal.

