--TYPES
type Point = (Int, Int)

--Exercicio 1

--Questão i

--A primeira entrada sao as coordenadas que se deseja verificar a pertinencia
--A segunda entrada sao as coordenadas do ponto superior esquerdo do retangulo
--A terceira entrada sao as coordenadas do ponto inferior direito do retangulo
pert_retangulo (x,y) (ex,ey) (dx, dy) = x >= ex && x <= dx && y <= ey && y >= dy

--Questao ii

--MODULO INCLINACAO DA RETA
--As entradas sao dois pontos no plano cartesiano para calcular a inclinacao
inc_reta (x1,y1) (x2,y2) = (y2 - y1) / (x2 - x1)

--A primeira entrada sao as coordenadas que se deseja verificar a pertinencia
--A segunda entrada sao as coordenadas do ponto lateral esquerdo do losango
--A terceira entrada sao as coordenadas do ponto superior do losango
--m1a = inclinacao da reta do ponto (ex,ey) em relacao a (sx,sy)
--m2a = inclinacao da reta no ponto (ex,ey) em relacao ao ponto inferior do losango
--m1b = inclinacao da reta do ponto lateral direito em relacao a (sx, sy)
--m2b = inclinacao da reta do ponto lateral dir em rel ao ponto inferior do losango 
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
dist_euclidiana (x1,y1) (x2, y2) = sqrt( ((x2-x1)^2) + ((y2-y1)^2) )

--A primeira entrada sao as coordenadas que se deseja verificar a pertinencia
--A segunda entrada sao as coordenadas do centro
--A terceira e o raio
pert_circulo (x, y) (cx, cy) r = dist_euclidiana (x, y) (cx, cy) <= r

--Exercicio 2
dist_tres_pontos::Point->Point

