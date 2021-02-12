module Ficha2 where

import Data.Char

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--1.a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)
--funA [2,3,5,1] = 2^2 + (funA [3,5,1])
--funA [3,5,1] = 3^2 + (funA [5,1])
--funA [5,1] = 5^2 + (funA [1])
--funA [1] = 1^2 + (funA [])
--funA [2,3,5,1] = 39

--1.b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2) == 0 then h:(funB t)
                        else (funB t)
--funB [8,5,12] = (8: (funB [5,12]))
--funB [5,12] = (funB [12])
--funB [12] = 12:(funB [])
--funB [] = []
--funB [8,5,12] = [8,12]

--1.c)
funC :: [Int] -> [Int]
funC (x:y:t) = (funC t)
funC [x] = []
funC [] = []
--funC [1,2,3,4,5] = (funC [3,4,5])
--funC [3,4,5] = (funC [5])
--funC [5] = []
--funC [1,2,3,4,5] = []

--1.d)
funD l = g [] l
g l [] = l
g l (h:t) = g (h:l) t
--funD "otrec" = g [] "otrec"
--g [] 'o':trec =
--g 'o':[]:"trec" =
--g ['o'] 't':"rec" =
--g 't':['o'] "rec" =
--g "to" 'r':"ec" =
--g 'r':"to""ec" =
--g "rto" 'e':['c'] =
--g 'e':"rto" ['c'] =
--g "erto" 'c' [] =
--g 'c':"erto" [] = "certo"

--2.a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros [x] = [2*x]
dobros (x:xs) = (2*x: (dobros xs))

--2.b)
numOcorre :: Char -> String -> Int
numOcorre z [] = 0
numOcorre z (x:xs) = if z == x then (1 + (numOcorre z xs))
                        else numOcorre z xs

--2.c)
positivos :: [Int] -> Bool
positivos [] = True
positivos (x:xs) | x>0 = positivos xs
                 | x == 0 = False
                 | x<0 = False

--2.d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) = if x > 0 then x:(soPos xs)
                   else (soPos xs)

--2.e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) = if x < 0 then (x + (somaNeg xs))
                     else (somaNeg xs)

--2.f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (x:xs) = if (length (x:xs)) <= 3 then (x:xs)
                  else (tresUlt xs)

--2.g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos [(x,y)] =  [y]
segundos ((x,xs):t) = (xs: (segundos t))

--2.h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros z [] = False
nosPrimeiros z ((x,y):t) = if z == x then True
                                else (nosPrimeiros z t)

--2.i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(x,y,z)] = (x,y,z)
sumTriplos ((x1,y1,z1):(x2,y2,z2):t) = sumTriplos ((x1+x2,y1+y2,z1+z2):t)

--3.a)
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) = if isDigit x == True then x:(soDigitos xs)
                    else soDigitos xs

--3.b)
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) = if x `elem` ['a'..'z'] then 1 + minusculas xs
                         else minusculas xs

--3.c)
nums :: String -> [Int]
nums [] = []
nums (x:xs) = if x `elem` ['0'..'9'] then (ord (x)-48) : nums xs
                        else nums xs

--4.a)
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((x,xs):t) = if n == xs then 1 + conta n t
                            else conta n t

--4.b)
grau :: Polinomio -> Int
grau [] = 0
grau [(x,xs)] = xs
grau ((x,xs):(y,ys):t) = if xs > ys then grau ((x,xs):t)
                             else grau ((y,ys):t)

--4.c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((x,xs):t) = if n == xs then ((x,xs):(selgrau n t))
                              else selgrau n t

--4.d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,xs):t) = (((fromIntegral xs)*x,xs-1): (deriv t))

--4.e)
calcula :: Float -> Polinomio -> Float
calcula n [] = 0
calcula n ((x,xs):t) = ((x*n^xs) + calcula n t)

--4.f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,xs):t) = if xs == 0 then simp t
                       else ((x,xs):simp t)

--4.g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (y,ys) ((x,xs):t) = (x*y,ys+xs) : mult (y,ys) t

--4.h)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(x,xs)] = [(x,xs)]
normaliza ((x,xs):(y,ys):t) = if xs == ys then (x+y,xs): normaliza t
                                   else ((x,xs):(normaliza ((y,ys):t)))

--4.i)
soma :: Polinomio -> Polinomio -> Polinomio
soma l z = normaliza (l ++ z)

--4.j)
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (h:t) l = soma (mult h l) (produto t l)

--4.k) FDN
--ordena :: Polinomio -> Polinomio
--ordena [] = []

--4.l)
equiv :: Polinomio -> Polinomio -> Bool
equiv [] _ = True
equiv _ [] = True
equiv ((x,xs):t) ((y,ys):z) | xs == ys = equiv t z
                            | xs /= ys = False