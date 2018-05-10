--TYPES
type Circle = (Point, Float)
type Equation2c = (Float, Float, Float)
type Point = (Float, Float)
type Vector2f = (Float, Float)
type Vector3f = (Float, Float, Float)
type Pos = (Integer, Integer)
--Exercicio 1
--Questão i
--A primeira entrada sao as coordenadas que se deseja verificar a pertinencia
--A segunda entrada sao as coordenadas do ponto superior esquerdo do retangulo
--A terceira entrada sao as coordenadas do ponto inferior direito do retangulo
pert_retangulo::Point->Point->Point->Bool
pert_retangulo (x,y) (ex,ey) (dx, dy) = x >= ex && x <= dx && y <= ey && y >= dy
--Questao ii
--MODULO INCLINACAO DA RETA
--As entradas sao dois pontos no plano cartesiano para calcular a inclinacao
inc_reta::Point->Point->Float
inc_reta (x1,y1) (x2,y2) = (y2 - y1) / (x2 - x1)
--A primeira entrada sao as coordenadas que se deseja verificar a pertinencia
--A segunda entrada sao as coordenadas do ponto lateral esquerdo do losango
--A terceira entrada sao as coordenadas do ponto superior do losango
--m1a = inclinacao da reta do ponto (ex,ey) em relacao a (sx,sy)
--m2a = inclinacao da reta no ponto (ex,ey) em relacao ao ponto inferior do losango
--m1b = inclinacao da reta do ponto lateral direito em relacao a (sx, sy)
--m2b = inclinacao da reta do ponto lateral dir em rel ao ponto inferior do losango 
pert_losango::Point->Point->Point->Bool
pert_losango (x, y) (ex, ey) (sx, sy) = pert_retangulo (x,y) (ex,sy) (ex+dist_horizontal,sy-dist_vertical) &&
                                        inc_reta(ex,ey)(x,y) <= m1a &&
                                        inc_reta(ex,ey)(x,y) >= m2a &&
                                        inc_reta(ex+dist_horizontal,ey)(x,y) >= m1b &&
                                        inc_reta(ex+dist_horizontal,ey)(x,y) <= m2b
                                        where
                                          dist_horizontal = (sx - ex) * 2
                                          dist_vertical = (sy - ey) * 2
                                          m1a = inc_reta(ex,ey)(sx,sy)
                                          m2a = inc_reta(ex,ey)(sx,sy-dist_vertical)
                                          m1b = inc_reta(ex+dist_horizontal,ey)(sx,sy)
                                          m2b = inc_reta(ex+dist_horizontal,ey)(sx,sy-dist_vertical)
--Questao iii
--MODULO DISTANCIA EUCLIDIANA
--As entradas sao dois pontos no plano cartesiano para calcular a distancia euclidiana
dist_euclidiana::Point->Point->Float
dist_euclidiana (x1,y1) (x2, y2) = sqrt( ((x2-x1)^2) + ((y2-y1)^2) )
--A primeira entrada sao as coordenadas que se deseja verificar a pertinencia
--A segunda entrada sao as coordenadas do centro
--A terceira e o raio
pert_circulo::Point->Circle->Bool
pert_circulo (x, y) ((cx, cy), r) = dist_euclidiana (x, y) (cx, cy) <= r
--Exercicio 2
dist_tres_pontos::Point->Point->Point->Vector3f
dist_tres_pontos (x1, y1) (x2, y2) (x3, y3) = (a_b, a_c, b_c)
                                               where
                                                a_b = dist_euclidiana (x1,y1) (x2, y2)
                                                a_c = dist_euclidiana (x1,y1) (x3, y3)
                                                b_c = dist_euclidiana (x2,y2) (x3, y3)
--Exercicio 3
--MÓDULO DELTA DA EQUAÇÃO DO SEGUNDO GRAU
delta::Equation2c->Float
delta (a,b,c) = b^2 - 4*a*c
--MÓDULO CALCULAR RAIZES
raizes::Equation2c->Float->Vector2f
raizes (a,b,c) delta = (x1,x2)
                        where
                          x1 = ((-b)+sqrt(delta))/2*a 
                          x2 = ((-b)-sqrt(delta))/2*a
equ_segundo_grau::Equation2c -> String
equ_segundo_grau (a,b,c) | delta (a,b,c) >= 0 = "Raizes: " ++ show( raizes (a,b,c) (delta(a, b, c)) )
                         | otherwise = "Raizes nao sao reais"
--Exercicio 4
-- I
relation::Vector3f -> String
relation (a,b,c) | a==b && a/=c = "Apenas duas iguais !"
                 | a==b && a==c = "Todas iguais !"
                 | otherwise = "Os tres sao diferentes !"
-- II
--(a,b) = canto superior esquerdo
--(c,d) = canto inferior direito
--retorna -1 caso o segundo ponto estiver acima ou a esquerda do primeiro

--MODULO CALCULAR QUANTOS QUADRANTES PARA VALOR DE A POSITIVOS
positive_a_quads::Point->Point->Integer
positive_a_quads (a,b) (c,d) = if b>=0 
                               then
                                   if d>=0 
                                   then 1
                                   else 2  
                               else 1
--MODULO CALCULAR QUANTOS QUADRANTES PARA VALOR DE A NEGATIVOS
negative_a_quads::Point->Point->Integer
negative_a_quads (a,b) (c,d) = if b>=0
                               then
                                   if c >= 0
                                   then
                                       if d >= 0
                                       then 2
                                       else 4
                                   else 
                                       if d >= 0
                                       then 1
                                       else 2
                                else
                                    if c >= 0
                                    then 2
                                    else 1  
--FUNÇÃO PRINCIPAL
num_quads::Point->Point->Integer
num_quads (a,b) (c,d) | c < a || d > b = -1
                      | a >= 0 = positive_a_quads (a,b) (c,d)
                      | otherwise = negative_a_quads (a,b) (c,d)
--Exercicio 5
--MÓDULO DE CALCULO DO IMC
imc::Vector2f->Float
imc (peso,altura) = peso/(altura^2)
--FUNÇAO PRINCIPAL
class_imc::Vector2f->String
class_imc (peso,altura) | valor < 18.5 = "Abaixo do peso"
                        | valor >= 18.5 && valor < 25 = "Peso normal"
                        | valor >= 25 && valor < 30 = "Sobrepeso"
                        | otherwise = "Obesidade"
                          where
                            valor = imc (peso,altura)
--Exercicio 6
desc_imposto_renda::Float->String
desc_imposto_renda ganhos | ganhos < 500 = "0% Isento"
                          | ganhos >= 500 && ganhos < 1500 = "10%"
                          | ganhos >= 1500 && ganhos < 2500 = "15%"
                          | otherwise = "25%"
--Exercicio 7
--Modulo de pegar o maior lado de tres numeros
maior_tres::Vector3f->Float
maior_tres (a,b,c) = if b>c 
                     then
                         if b>a 
                         then b
                         else a 
                     else
                         if c>a
                         then c 
                         else a
--Função Principal
tipo_triangulo::Point->Point->Point->String
tipo_triangulo (x1, y1) (x2, y2) (x3, y3) | ab == bc && ab == ca = "Equilatero"
                                          | ab == bc || bc == ca = "Isosceles"
                                          | ab /= bc && bc /= ca = "Escaleno" 
                                          | otherwise = "Nao e um triangulo"
                                             where
                                               ab = dist_euclidiana (x1, y1) (x2, y2)
                                               bc = dist_euclidiana (x2, y2) (x3, y3)
                                               ca = dist_euclidiana (x3, y3) (x1, y1)
--Exercicio 8
--MÓDULOS CONVERSAO DE DATA
year_to_day year = year * 365
month_to_day month = month * 30
--Funcao Principal
calc_idade::(RealFrac a, Integral b) => (a, a, a) -> (a, a, a) -> b
calc_idade (d1, m1, y1) (d2, m2, y2) = truncate (((year_to_day y2 + month_to_day m2 + d2) - (year_to_day y1 + month_to_day m1 + d1)) / 365)
--Exercicio 9
bispo_valido::Pos->Char
bispo_valido (posX, posY) | posX > 8 || posY > 8 || posY==0 || posX==0 = '0'
                          | posX == 8 = 'D'
                          | posX == 1 = 'E'
                          | otherwise = error "O bispo se move em mais de uma direção. Não foi dado o que fazer nesta situação."
--Exercicio 10
idade_relativa::Integer->String->String
idade_relativa segundos planeta | planeta == "terra" = "Idade:" ++ show(truncate(idade_na_terra  ))
                                | planeta == "mercurio" = "Idade:" ++ show(truncate(idade_na_terra / 0.2408467))
                                | planeta == "venus" = "Idade:" ++ show(truncate(idade_na_terra / 0.61519726))
                                | planeta == "marte" = "Idade:" ++ show(truncate(idade_na_terra / 1.8808158))
                                | planeta == "jupiter" = "Idade:" ++ show(truncate(idade_na_terra / 11.862615))
                                | planeta == "saturno" = "Idade:" ++ show(truncate(idade_na_terra / 29.447498))
                                | planeta == "urano" = "Idade:" ++ show(truncate(idade_na_terra / 84.016846))
                                | planeta == "netuno" = "Idade:" ++ show(truncate(idade_na_terra / 164.79132))
                                | otherwise = "Planeta Invalido"
                                  where
                                    idade_na_terra = (((fromIntegral(segundos)/60)/60)/24)/30/12