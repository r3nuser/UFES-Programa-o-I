-- Lista 2 Exercicios

--Módulos

	--Conversão de data:
mes_para_dias a = a*30
anos_para_mes a = (a*12) + 5; 
anos_para_dias a = mes_para_dias(anos_para_mes a)
	--Quantificadores
get_maior a b = if a < b then b else a
get_menor a b = if a > b then b else a
get_maior_all a b c = get_maior (get_maior a b) c 
get_menor_all a b c = get_menor (get_menor a b) c
get_meio a b c = get_maior (get_menor a c) b
	--Math
delta a b c = (b*b)-4*a*c

--Exercícios

-- Exercicio A
exerc_a a = (a>0 && a<100) && (mod a 3 == 0) && (mod a 5 == 0)

-- Exercicio B
oux a b = (a || b) && not(a && b)

-- Exercicio C
	-- d_a == Dia inicial
	-- m_a == Mes inicial
	-- y_a == Ano inicial
	-- d_d == Dia final
	-- m_d == Mes final
	-- y_d == Ano final
days [d_a,m_a,y_a] [d_d,m_d,y_d] = d_d - d_a +
									mes_para_dias m_d - mes_para_dias m_a +
									anos_para_dias y_d - anos_para_dias y_a

-- Exercicio D
forma_triangulo_qm l1 l2 l3 = get_meio l1 l2 l3 + get_menor_all l1 l2 l3 >
			    				get_maior_all l1 l2 l3

-- Exercicio E
-- 1
pert_retangulo [a,b] [e1,e2] [d1,d2] =  a >= e1 && b <= e2 && a <= d1 && b >= d2
-- 2
inc_reta [a,b] [x,y] = (y-b)/(x-a);
pert_losango [a,b] [e1,e2] [s1,s2] =  a >= e1 && a<= e1 + dist_horizontal && 
									  b <= s2 && b>= s2 - dist_vertical && (
									  --ACIMA PERTENCE RETANGULO
									  --ABAIXO PERTENCE AO LOSANGO USANDO INC RETA
									  inc_reta [e1, e2] [a, b] <= m1a &&
									  inc_reta [e1, e2] [a, b] >= m1b &&
									  inc_reta [e3, e2] [a, b] >= m2a &&
									  inc_reta [e3, e2] [a, b] <= m2b)
				      					where 
											dist_horizontal = (s1 - e1)*2
											dist_vertical = (s2 - e2)*2
											e3 = e1+dist_horizontal
											m1a = inc_reta [e1, e2] [s1, s2]
											m1b = inc_reta [e1, e2] [s1,(s2 - dist_vertical)]
											m2a = inc_reta [e3, e4] [s1, s2] 
											m2b = inc_reta [e3, e4] [s1,(s2 - dist_vertical)]

-- 3
--pert_circulo
pert_circulo [a,b] [c1,c2] r = abs(c1 - a) <= r && abs(c2 - b) <= r

-- Exercicio F
relation a b c = if a/=b && a/=c && b/=c then "Todos diferentes"
					else if a==b && b==c then "Todos iguais"
					else "Apenas dois iguais" 

-- Exercicio G
	-- a,b == Posição do canto superior esquerdo
	-- x,y == Posição do canto inferior direito
quad_into [a,b] [x,y] = if ( a > x || b < y ) || (a == 0 && x == 0 && b == 0 && y == 0) then 0
						else 
							if a > 0 then
								if b > 0 then
									if y > 0 then 1 else 2
								else 1
							else 
								if a < 0 then
									if b > 0 then
										if y < 0 then
											if x > 0 then 4 else 2
										else
											if x > 0 then 2 else 1
									else
										if x > 0 then 2 else 1 
								else 0

-- Exercicio H
-- x = tamanho horizontal do tabuleiro 
-- y = tamanho vertical do tabuleiro
-- a = pos x do tabuleiro
-- b = pos y do tabuleiro 
-- c = movimento pos x
-- d = movimento pos y 
pert_tabuleiro [x,y] [a,b] [c,d] = (not (a <= 0 || a > x || c <= 0 || c > x 
								   || b <= 0 || b > y || d <= 0 || d > y)) &&
								   (abs (a-c) <= 1 && abs (b-d) <= 1)

-- Exercicio I
maior_tres_numeros a b c = get_maior_all a b c

-- Exercicio J
quad_sucessor_maior a b c = (succ(get_maior_all a b c))^2

-- Exercicio K
preco_passagem passagem idade = if idade >= 60 then passagem*0.6 else
								if idade <= 2 then passagem*0.1 else
								if idade <= 10 then passagem*0.50 else passagem

-- Exercicio L
equ_segundo_grau a b c = if delta a b c<0	then "Nao ha raizes reais"
							else "Raizes: " ++ show [((-b)+(sqrt(delta a b c)))/2*a,((-b)-(sqrt(delta a b c)))/2*a]

-- Exercicio M
abono horas_trab horas_falt = if pontos >= 1 && pontos <= 10 then 100.00
								else if pontos >= 11 && pontos <= 20 then 200.00
								else if pontos >= 21 && pontos <= 30 then 300.00
								else if pontos >= 31 && pontos <= 40 then 300.00
								else if pontos >= 41 then 500.00
								else 0
								where
									pontos = horas_trab - ((2/3)*horas_falt) 

