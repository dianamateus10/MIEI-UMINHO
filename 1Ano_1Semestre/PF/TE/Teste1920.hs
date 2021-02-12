module Teste1920 where
import Data.List

----------------------------------------Exercício 1---------------------------------------------------
--a) Apresente uma definição recursiva da função intersect que retorna a lista resultante
--de remover da primeira lista os elementos que não pertencem à segunda
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' l l2 = foldr (\x acc -> if x `elem` l2 then x:acc else acc) [] l 

--b) Definir a função tails que calcula a list dos sufixos de uma lista.
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l:tails' (tail l)

----------------------------------------Exercício 2----------------------------------------------------
type ConjInt = [Intervalo]
type Intervalo = (Int,Int)
--conjunto {1,2,3,4,7,8,19,21,22,23} = [(1,4),(7,8),(19,19),(21,23)]
--a) Definir a função elems que dado um conjunto dá como resultado a lista de elementos
--desse conjunto
elems :: ConjInt -> [Int]
elems [] = []
elems ((x,y):xs) | x==y = x:elems xs
                 | otherwise = x : elems ((succ x, y):xs)

--b) Definir a função geraconj que recebe uma lista de inteiros, ordenada por ordem
--crescente e sem repetições e gera um conjunto
geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj (h:t) = (h,d):geraconj (dropWhile (<=d) t)
           where d = foldl (\acc x -> if x == succ acc then x else acc) h t

-----------------------------------------Exercício 3----------------------------------------------------
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show
type Nome = String
type Agenda = [(Nome,[Contacto])]
--a) Definir a função acrescEmail que dado um nome e um email acrescenta essa informação
--à agenda
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n x [] = [(n,[Email x])]
acrescEmail n x ((a,b):ys) | n == a = ((n,(Email x):b):ys)
                           | otherwise = acrescEmail n x ys

--b) Definir a função verEmails que dado um noem e uma agenda retorna a lista dos emails
--associados a esse nome.Se o nome não existir na agenda deve retornar Nothing
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails n ((a,b):xs) | n == a = Just (getEmail b)
                       | otherwise = verEmails n xs
 
getEmail :: [Contacto] -> [String]
getEmail [] = []
getEmail ((Email x):xs) = [x] ++ getEmail xs
getEmail (_:xs) = getEmail xs

--c) Definir a função consulta que dada lista de contactos retorna o par com a lista
--de números de telefone (fixos e móveis) e a lista de emails, dessa lista. 
--Implemente de modo a fazer uma travessia da lista de contactos
consulta :: [Contacto] -> ([Integer],[String])
consulta = foldr (\x (i,s) -> case x of Email e -> (i,e:s); otherwise -> (n x:i,s)) ([],[])
    where n x = case x of Casa num -> num
                          Trab num -> num
                          Tlm num -> num

--d) Definir a função consultaIO que dada uma agenda, lê do teclado o nome que pretende
--consultar e apresenta no ecrã os contactos associados a esse nome na agenda
consultaIO :: Agenda -> IO ()
consultaIO l = do
	nome <- getLine
	let contactos = aux nome agenda
	putStr (concat [show x ++ "\n" | x <- contactos])
	where aux _ [] = []
	      aux nome ((name,contactos):t) = if name == nome then contactos else aux nome t

------------------------------------------Exercício 4------------------------------------------------------
data RTree a = R a [RTree a] deriving (Show,Eq)
--a) Definiri a função paths que dada uma destas árvores calcula todos os caminhos
--desde a raiz até às folhas
--Ex: paths (R 1 [R 2 [],R 3 [R 4 [R 5 [],R 6 []]],R 7 []]) = [[1,2],[1,3,4,5],[1,3,4,6],[1,7]]
paths :: RTree a -> [[a]]
paths (R node []) = [[node]]
paths (R node branches) = [node: x | x <- concat [paths branch | branch <- branches]]

--b) Definir a função unpaths tal que é a inversa da anterior e (paths t) == t, para
--qualquer árvore t :: Eq a => RTree a
unpaths :: Eq a => [[a]] -> RTree a
unpaths [[x]] = R x []
unpaths l = R n [unpaths (foldl (\acc branch -> if head branch == y then branch:acc else acc) [] branches) | y <- heads]
       where n = head $ head l
             branches = [tail x | x <- l]
             heads = nub [head x | x <- branches]
