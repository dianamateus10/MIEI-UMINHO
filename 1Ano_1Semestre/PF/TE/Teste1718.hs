module Teste1718 where
import Data.Maybe
import Data.List
import System.Random

-------------------------------------Exercício 1------------------------------------------------------
--Apresentar uma definição de recursiva da função pré-definida insert que dado um 
--elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente
--um elemento na lista
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys) | x<y = x:y:ys
                 | otherwise = y:insert' x ys

-------------------------------------Exercício 2------------------------------------------------------
--Apresentar uma definição recursiva da função pré-definida catMaybes que coleciona
--os elementos do tipo a de uma lista
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (m:ms) = case m of Nothing -> catMaybes' ms
                              Just x -> x:catMaybes' ms

------------------------------------Exercício 3-------------------------------------------------------
data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)
--Definir Exp a como instancia de classe show de forma a que show (Mais (Var "x"))
--(Mult (Const 3) (Const 4)) = (x + (3*4))
instance Show a => Show (Exp a) where
	show (Mult a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
	show (Mais x y) = "(" ++ show x ++ "+" ++ show y ++ ")"
	show (Var x) = x
	show (Const a) = show a

------------------------------------Exercício 4---------------------------------------------------------
--Apresentar a definição da função sortOn que ordena uma lista comparando os resultados
--de aplicar uma função de extração de uma chave a cada elemento de uma lista
--Ex: sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)]
sortOn' :: Ord b => (a->b) -> [a] -> [a]
sortOn' _ [] = []
sortOn' f (h:t) = inserir h (sortOn' f t)
          where
          	inserir a [] = [a]
          	inserir a (x:y) = if f a > f x then x:inserir a y
          	                     else a:x:y

-----------------------------------Exercício 5----------------------------------------------------------
--a)Definir uma função amplitude que calcula a amplitude de uma lista, sendo que a
--amplitude de uma lista define-se como a diferença entre o maior e o menor dos
--elementos
amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = max - min
        where
        	(max,min) = foldl (\(a,b) n -> (if n > a then n else a,if n<b then n else b)) (head l,head l) l

--b) Definir uma função parte que parte de uma lista de inteiros em duas, minimizando
--a soma das amplitudes
parte :: [Int] -> ([Int],[Int])
parte l = foldl1 (\(acc1,acc2) (a,b) -> if amplitude acc1 + amplitude acc2 < amplitude a + amplitude b then (acc1,acc2) else (a,b)) combinacoes
    where combinacoes = foldl (\acc n -> splitAt n sl : acc) [] [1..(length l - 1)]
          sl = sort l

----------------------------------Exercício 6-----------------------------------------------------------
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]
--Ex: Imagem = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),Quadrado 4,Mover (4,3)(Quadrado 2)])
--a) Definir a função conta que conta quantos quadrados tem uma imagem
conta :: Imagem -> Int
conta (Quadrado _) = 1
conta (Mover (_,_) im) = conta im
conta (Juntar l) = sum (map conta l)

--b) Definir o programa apaga que apaga um dos quadrados da imagem à sorte
apaga :: Imagem -> IO Imagem
apaga im = do
	let indquad = indices_quadrados im
	randNum <- randomRIO (1,length indquad)
	let indtoremove = indquad !! (randNum -1)
	return $ apaga_indice indtoremove im

indices_quadrados :: Imagem -> [Int]
indices_quadrados (Quadrado n) = [n]
indices_quadrados (Mover (_,_) im) = indices_quadrados im
indices_quadrados (Juntar l) = concatMap indices_quadrados l

apaga_indice :: Int -> Imagem -> Imagem
apaga_indice x (Quadrado n) = if n == x then Juntar [] else Quadrado n
apaga_indice x (Mover (a,b) im) = Mover (a,b) (apaga_indice x im)
apaga_indice x (Juntar l) = Juntar (map (apaga_indice x) l)