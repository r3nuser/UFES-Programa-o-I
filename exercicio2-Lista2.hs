-- Lista 2 Exercicios

-- Exercicio A
exerc_a a = (a>0 && a<100) && (mod a 3 == 0) && (mod a 5 == 0)

-- Exercicio B
exerc_b :: Bool -> Bool -> Bool
exerc_b a b = (a || b) && not(a && b)

-- Exercicio C

-- d_a == Dia inicial
-- m_a == Mes inicial
-- y_a == Ano inicial
-- d_d == Dia final
-- m_d == Mes final
-- y_d == Ano final
mes_para_dias a = a*30
anos_para_mes a = (a*12) + 5; 
anos_para_dias a = mes_para_dias(anos_para_mes a)

days [d_a,m_a,y_a] [d_d,m_d,y_d] = d_d - d_a +
				   mes_para_dias m_d - mes_para_dias m_a +
				   anos_para_dias y_d - anos_para_dias y_a

-- Exercicio D
get_maior a b = if a < b then b else a
get_menor a b = if a > b then b else a
get_maior_all a b c = get_maior (get_maior a b) c 
get_menor_all a b c = get_menor (get_menor a b) c
get_meio a b c = get_maior (get_menor a c) b

forma_triangulo_qm l1 l2 l3 = get_meio l1 l2 l3 + get_menor_all l1 l2 l3 >
			      get_maior_all l1 l2 l3

-- Exercicio E


