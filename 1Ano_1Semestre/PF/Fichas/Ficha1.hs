module Ficha1 where

import Data.Char

data Hora = H Int Int deriving (Show,Eq)
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
data Figura = Circulo Ponto Double | Retangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

--1.a)
perimetro :: Double -> Double
perimetro x = 2 * pi * x

--1.b)
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt (((y2-y1)^2)+((x1-x2)^2))

--1.c)
primUlt :: [a] -> (a,a)
primUlt [x] = (x,x)
primUlt (x:xs) = (head (x:xs),last (x:xs))

--1.d)
multiplo :: Int -> Int -> Bool
multiplo m n = if (mod m n) == 0 then True
                else False

--1.e)
truncaImpar :: [a] -> [a]
truncaImpar l = if ((length l) `mod` 2) /= 0 then (tail l)
                else l

--1.f)
max2 :: Int -> Int -> Int
max2 x y = if x > y then x
            else y

--1.g)
max3 :: Int -> Int -> Int -> Int
max3 x y z = if max2 x y == x then max2 x z
                 else max2 y z


--2.a) FDN
--nRaizes :: Double -> Double -> Double -> Int
--nRaizes x y z = if d < 0 then 0
               --  else if d == 0 then 1
               --  	else 2
               --  	where d = formula resolvente

--2.b) FDN
--raizes :: Double -> Double -> Double -> Double
--raizes x y z = formula resolvente

--3.a)
horaVal :: (Int,Int) -> Bool
horaVal (x,y) = if x > 24 then False
                   else if y > 60 then False
                        else True

--3.b)
horaMaior :: (Int,Int) -> (Int,Int) -> Bool
horaMaior (x1,y1) (x2,y2) | x1 > x2 = True
                          | x1 == x2 && y1 > y2 = True
                          | otherwise = False

--3.c)
convertHour :: (Int,Int) -> Int
convertHour (x,y) = x*60 + y

--3.d)
convertMin :: Int -> (Int,Int)
convertMin x = ((div x 60), (mod x 60))

--3.e)
diffHour :: (Int,Int) -> (Int,Int) -> Int
diffHour (x1,y1) (x2,y2) = if x1 == x2 then abs(y2-y1)
                             else if (abs (x2-x1)) == 1 then (60-y1+y2)
                                 else (abs (x2-x1)) * 60 + (abs (y2-y1))

--3.f) 
moreMin :: Int -> (Int,Int) -> (Int,Int)
moreMin z (x,y) = if (y + z) > 60 then (x+1,abs(60-y-z))
                       else (x,y+z)

--4.a)
horaVal2 :: Hora -> Bool
horaVal2 (H x y) = if x > 24 then False
                       else if y > 60 then False
                          else True

--4.b)
horaMaior2 :: Hora -> Hora -> Bool
horaMaior2 (H x1 y1) (H x2 y2) | x2 > x1 = True
                               | x2 == x1 && y2 > y1 = True
                               | otherwise = False

--4.c)
convertHour2 :: Hora -> Int
convertHour2 (H x y) = x*60 + y

--4.d)
convertMin2 :: Int -> Hora
convertMin2 x = (H (div x 60) (mod x 60))

--4.e)
diffHour2 :: Hora -> Hora -> Int
diffHour2 (H x1 y1) (H x2 y2) = if x1 == x2 then abs(y2-y1)
                             else if (abs (x2-x1)) == 1 then (60-y1+y2)
                                 else (abs (x2-x1)) * 60 + (abs (y2-y1))

--4.f)
moreMin2 :: Int -> Hora -> Hora
moreMin2 z (H x y) = if (y + z) > 60 then ( H (x+1) (abs(60-y-z)))
                       else (H (x) (y+z))

--5.a)
next :: Semaforo -> Semaforo
next x = if x == Verde then Amarelo
            else if x == Amarelo then Vermelho
                 else Verde

--5.b)
stop :: Semaforo -> Bool
stop x = if x == Amarelo || x == Vermelho then True
             else False

--5.c)
safe :: Semaforo -> Semaforo -> Bool
safe x y = if x == Verde && y == Verde then False
               else if x == Verde && y == Amarelo then False
                   else if x == Amarelo && y == Verde then False
                      else if x == Amarelo && y == Amarelo then False
                         else True

--6.a)
posx :: Ponto -> Double
posx (Cartesiano x y) = x

--6.b)
posy :: Ponto -> Double
posy (Cartesiano x y) = y

--6.c)
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)

--6.d)
--angulo :: Ponto -> Double
--angulo (Cartesiano x y) = tan^-1 (y/x)

--6.e)
distance :: Ponto -> Ponto -> Double
distance (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt (((x2-x1)^2)+(y2-y1)^2)

--7.a)
poligono ::Figura -> Bool
poligono (Triangulo x y z) = (posy y-posy x) / (posx y - posx x) /= (posy z - posy y) / (posx z - posx y)
poligono (Circulo x w) = False
poligono (Retangulo x y) = posx x /= posx y && posy x /= posy y


--7.b)
vertices :: Figura -> [Ponto]
vertices (Triangulo x y z) = if poligono (Triangulo x y z) then x:y:z:[] else []
vertices (Circulo x w) = []
vertices (Retangulo x y) = if poligono (Retangulo x y) then x:y:(Cartesiano (posx x)(posy y)):[] else []

{-
--7.c)
area :: Figura -> Double
area (Triangulo p1 p2 p3) = 
	let a = dist p1 p2
	    b = dist p2 p3
	    c = dist p3 p1
	    s = (a+b+c) / 2
	 in sqrt (s*(s-a)*(s-b)*(s-c))
area (Circulo _ r) = pi * (r^2)
area (Retangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)

--7.d)
perimetro2 :: Figura -> Double
perimetro2 (Retangulo p1 p2) = ((posx p2 - posx p1))*((posy p2) - (posy p1))
perimetro2 (Circulo _ r) = 2 * pi * r
perimetro2 (Triangulo p1 p2 p3) = distance p1 p2 + distance p2 p3 + distance p1 p3

-}

--8.a)
isLower1 :: Char -> Bool
isLower1 ch = elem ch ['a'..'z']

--8.b)
isDigit1 :: Char -> Bool
isDigit1 d = elem d ['0'..'9']

--8.c)
isUpper1 :: Char -> Bool
isUpper1 ch = elem ch ['A'..'Z']

isAlpha1 :: Char -> Bool
isAlpha1 ch = isLower1 ch || isUpper1 ch

--8.d)
toUpper1 :: Char -> Char
toUpper1 ch = if isLower ch then chr ((ord ch)-32)
                  else ch

--8.e)
intToDigit1 :: Int -> Char
intToDigit1 n = chr (n+48)

--8.f)
digitToInt1 :: Char -> Int
digitToInt1 ch = ord (ch) -48