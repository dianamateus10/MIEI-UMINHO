module Ficha5 where

import Data.List

type Polinomio = [Monomio]
type Monomio = (Float,Int)

type Mat a = [[a]]


--1.a)
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (h:t) = if f h then True
                     else any' f t

--1.b)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

--1.c)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x:takeWhile' p xs
                    | otherwise = []

--1.d)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

--1.e)
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' f (h:t) = if f h then (h:s1,s2)
                   else ([],(h:t))
                   where
                      (s1,s2) = span' f t

--1.f)
{-
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ [] = []
deleteBy'f x (y:ys) = if f x y then ys
                            else y:deleteBy' f x ys
-}

--1.g)
sortOn' :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (h:t) = insere (h) (sortOn' f t)
               where
               	insere x [] = [x]
               	insere x (a:b) = if f x <= f a then x:a:b
               		               else a:insere x b

--2.a)
selgrau1 :: Int -> Polinomio -> Polinomio
selgrau1 a p = filter (\x -> snd x == a) p

--2.b)
conta1 :: Int -> Polinomio -> Int
conta1 a p = length (selgrau1 a p)

--2.c)
grau1 :: Polinomio -> Int
grau1 [(c,e)] = e
grau1 ((c,e):t) = max e (grau1 t)

--2.d)
deriv1 :: Polinomio -> Polinomio
deriv1 pol = map (\(c,e) -> (c*(fromIntegral e),e-1)) pol

--2.e)
calcula1 :: Float -> Polinomio -> Float
calcula1 _ [] = 0
calcula1 x ((c,e):t) = ((x^e)*c) + (calcula1 x t)

--2.f)
simp1 :: Polinomio -> Polinomio
simp1 pol = filter (\(c,e) -> c /= 0) pol

--2.g)
mult1 :: Monomio -> Polinomio -> Polinomio
mult1 _ [] = []
mult1 (c,e) p = map (\(x,y) -> (x*c,e+y)) p

--2.h)
ordena1 :: Polinomio -> Polinomio
ordena1 [] = []
ordena1 (h:t) = ordAux h (ordena1 t)
                where
                	ordAux m [] = [m]
                	ordAux (c,e) ((x,y):t) = if (e<y) then (c,e):(x,y):t
                	                              else (x,y):(ordAux (c,e) t)

--2.i)
normaliza1 :: Polinomio -> Polinomio
normaliza1 [] = []
normaliza1 (h:t) = acresAux h (normaliza1 t)

--função auxiliar
acresAux :: Monomio -> Polinomio -> Polinomio
acresAux (c,e) [] = [(c,e)]
acresAux (c,e) ((x,y):t) = if (e == y) then (c+x,e):t
                               else (x,y):acresAux (c,e) t

--2.j)
soma1 :: Polinomio -> Polinomio -> Polinomio
soma1 [] p = p
soma1 (h:t) p = soma1 t (acresAux h p)

--2.k)
produto1 :: Polinomio -> Polinomio -> Polinomio
produto1 [] p = p
produto1 p [] = p
produto1 [x] p = mult1 x p
produto1 (x:xs) p = (mult1 x p) ++ (produto1 xs p)

--2.l)
equiv1 :: Polinomio -> Polinomio -> Bool
equiv1 p1 p2 = (ordena1 (normaliza1 p1)) == ordena1 (normaliza1 p2)

--3.a)

dimOK :: Mat a -> Bool
dimOK []      = True
dimOK [x]     = True
dimOK ((x:xs):t) = if (length (x:xs)) /= (length (head t)) then False
                      else dimOK t

--3.b)
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat m  = (length m,length (head m))

--3.c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat [] p = p
addMat p [] = p
addMat (h:t) (x:xs) = (soma_listas h x): addMat t xs

soma_listas :: (Num a) => [a] -> [a] -> [a]
soma_listas [] [] = []
soma_listas l [] = l
soma_listas [] l = l
soma_listas (h:t) (x:xs) = (x+h):soma_listas t xs

--3.d)
transpose1 :: Mat a -> Mat a
transpose1 ([]:_) = []
transpose1 m = (map head m) : transpose1 (map tail m)

--3.e)
multMat :: (Num a,Eq a) => Mat a -> Mat a -> Mat a
multMat [] _ = []
multMat m1 m2 = [map (produto_Interno (head m1)) (transpose m2)] ++ multMat (tail m1) m2

produto_Interno :: (Num a,Eq a) => [a] -> [a] -> a
produto_Interno l1 l2 = sum (zipWith (*) l1 l2)

--3.f)
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f m1 m2 = zipWith' (zipWith' f) m1 m2

--3.g)
triSup :: (Num a,Eq a) => Mat a -> Bool
triSup [] = False
triSup m = ver_linhas m 1

ver_linhas :: (Num a,Eq a) => Mat a -> Int -> Bool
ver_linhas [] _ = True
ver_linhas (h:t) i | (compara_zeros h i) = ver_linhas t (i+1)
                   | otherwise = False

compara_zeros :: (Num a,Eq a) => [a] -> Int -> Bool
compara_zeros _ 1 = True
compara_zeros (h:t) a | h == 0 = compara_zeros t (a-1)
                      | otherwise = False

--3.h)
rotateLeft :: Mat a -> Mat a
rotateLeft ([]:_) = []
rotateLeft m = (map last m):rotateLeft (map init m)