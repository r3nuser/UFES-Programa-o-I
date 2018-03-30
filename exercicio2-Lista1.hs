-- Banco imobiliario
banco_min_notas x = div x 500 +
			div (mod x 500) 100 +
			div (mod x 100) 50 +
			div (mod x 50) 10+
			div (mod x 10) 5+
			div (mod x 5) 1
			   
-- Area do retangulo
arearet a b = a*b

-- Area do circulo
areacirc r = pi * r^2 

-- Distancia Euclidiana
disteuclid a b c d = sqrt((a-c)^2+(c-d)^2)

-- Raizes de uma equação do segundo grau
delta a b c = ((b^2)-4*a*c)
equ_segundo_grau a b c = 
	if sqrt(delta a b c)<0
	then [0,0]
	else [((-b)+(sqrt(delta a b c)))/2*a,((-b)-(sqrt(delta a b c)))/2*a]

-- Potencia de um numero elevado a um expoente negativo
pot_neg a b = 1/a^(-b)

-- Temperatura Celsius -> Farenheit
cel_fah c = c * (9/5) + 32

-- Ganho anual de valor X com 0.5% rendimento ao mes
ganho_anual x = x * 1.05^12

-- Determinar area cinza
area_cinza r = (areacirc (r*1.5)) - (areacirc r)  

-- X e Y qual quadrante do plano cartesiano
planXY :: Float -> Float -> Int
planXY x y = if x<0 
		then if y<0 
			then 3
			else 2
		else if y<0
			then 4
			else 1

-- Determinar se X pertence a A, B
 
pertAB :: Float -> Float -> Float -> Bool
pertAB x a b = if a<b then a<=x && b>=x else False
