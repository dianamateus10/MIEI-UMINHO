module Ficha4 where

import Data.Char

--1.a)
--[x | x <- [1..20], mod x 2 == 0, m0d x 3 = 0]: multiplos de 2 e de 3
-- x = [6, 12, 18]

--1.b)
--[x | x <- [y | <- [1..20], mod y 2 == 0], mod x 3 == 0]
--[x | x <- [2,4,6,8,10,12,14,16,18,20], mod x 3 == 0]
-- x = [ 6, 12, 18]

--1.c)
--[(x,y) | x <- [1..20], y <- [0..20], x+y == 30]
--x = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
--y = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
-- [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]

--1.d)
--[sum [y | y <- [1..x], odd y] | x <- [1..10]]
--[sum [números ímpares] | x pertença de 1 a 10]
--[1,1,4,4,9,9,16,16,25,25]

--2.a)
--[2^x | x <- [0..10]]

--2.b)
--[(x,y) | x <- [1..5], y <- [1..5], x+y == 6]

--2.c)
-- [ [1..x] | x <- [1..5]]

--2.d)
-- [ replicate x 1 | x <- [1..5]]

--2.e)
--[fatorial x | x <- [1..6]]
-- where fatorial 0 = 1
   --    fatorial x = x * fatorial (x-1)
-- [product [1..x] | x <- [1..6]]

--3)
digitAlpha :: String -> (String,String)
digitAlpha string = foldl (\(alpha,digit) x -> if isDigit x then (alpha, digit ++ [x])
	                                              else if isAlpha x then (alpha ++ [x], digit)
	                                              	else (alpha,digit)) ("","") string

--4)
nzp :: [Int] -> (Int,Int,Int)
nzp = foldl (\(n,z,p) x -> if x < 0 then (n+1,z,p)
                            else if x > 0 then (n,z,p+1)
                            	else (n,z+1,p)) (0,0,0)

--5)
divMod1 :: Integral a => a -> a -> (a,a)
divMod1 x y | y > x = (0,x)
            | otherwise = (1+d,m)
            where (d,m) = divMod1 (x-y) y

--6)
{-
fromDigits1 :: [Int] -> Int
fromDigits1 [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits1 t
-}
fromDigits1 :: [Int] -> Int
fromDigits1 l = fromDigitsaux l 0
          where fromDigitsaux [] x = x
                fromDigitsaux (y:ys) x = fromDigitsaux ys (y*10^(length ys)+x)

--7)
{-
maxSumInit :: (Num a,Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]
-}


--8)
{-
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-}


