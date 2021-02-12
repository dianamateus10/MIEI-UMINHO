module Ficha9 where

import System.Random

--1.a)

randomList :: Int -> (Int,Int) -> IO [Int]
randomList 0 _ = return []
randomList n (a,b) = do
	                  x <- randomRIO (a,b)
	                  l <- randomList (n-1) (a,b)
	                  return (x:l)

permutacao :: [a] -> IO [a]
permutacao [] = return []
permutacao [x] = return [x]
permutacao l = do indice <- randomRIO (0,length l -1)
                  let x = l !! indice
                  xs <- permutacao (retira indice l)
                  return (x:xs)

retira :: Int -> [a] -> [a]
retira 0 l = tail l
retira n (h:t) = h:retira (n-1) t

bingo :: IO ()
bingo = bingo' [1..90]

my_delete :: (Eq a) => a -> [a] -> [a]
my_delete a [] = []
my_delete a (h:t) | a == h = my_delete a t
                  | otherwise = h:my_delete a t

bingo' :: [Int] -> IO ()
bingo' l = do
	getLine
	i <- randomRIO (0,length l -1)
	let num = (l !! i)
	putStrLn (show num)
	bingo' (my_delete num l)

{-1.b)
mastermind :: IO ()
masterminf = undefined

gera_chave :: IO (Int,Int,Int,Int)
gera_chave = do
	          a <- randomRIO (0,9)
	          b <- randomRIO (0,9)
	          c <- randomRIO (0,9)
	          d <- randomRIO (0,9)
	          return (a,b,c,d)

adivinha_chave :: IO (Int,Int,Int,Int)
adivinha_chave = do
	              x <- getLine
-}

data Aposta = Ap [Int] (Int,Int)

--2.a)
valida :: Aposta -> Bool
valida (Ap [] _) = True
valida (Ap (x:xs) (a,b)) | x>=1 && x<=50 = valida (Ap xs (a,b))
                         | a>=1 && a <=9 && b>=1 && b <= 9 = True
                         | otherwise = False

--2.b)
--falta fazer

--2.c)
--falta fazer

--2.d)
--falta fazer

--2.e)
geraChave :: IO Aposta
geraChave = do
             n1 <- randomRIO (1,50)
             n2 <- randomRIO (1,50)
             n3 <- randomRIO (1,50)
             n4 <- randomRIO (1,50)
             n5 <- randomRIO (1,50)
             e1 <- randomRIO (1,9)
             e2 <- randomRIO (1,9)
             let 
               ap = (Ap [n1,n2,n3,n4,n5] (e1,e2))
             return ap

--2.d)
--falta fazer