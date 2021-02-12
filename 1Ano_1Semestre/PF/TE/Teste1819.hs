module Teste1819 where

import Data.Char
import System.Random

--------------------------------Exercício 1---------------------------------------------------------
--a) Definir elemIndices que calcula a lista de posições em que um elemento ocorre numa lista
elemIndic :: Eq a => a -> [a] -> [Int]
elemIndic n [] = []
elemIndic n (x:xs) | n == x = 0:map (+1) (elemIndic n xs)
                   | otherwise = map (+1) (elemIndic n xs)

--b) Definir isSubsequenceOf que teste se os elementos de uma lista ocorrem noutra
--pela mesma ordem relativa
isSubsequence :: Eq a => [a] -> [a] -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence (x:xs) (y:ys) =
      x == y && isSubsequence xs ys || isSubsequence (x:xs) ys

--------------------------------Exercício 2----------------------------------------------------------
data BTree a = Empty | Node a (BTree a) (BTree a)
--a) Definir a função lookupAP que generaliza a função lookup para árvores binárias
--de procura
lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP n (Node (a,b) y z) | n == a = Just b
                            | n < a = lookupAP n y
                            | otherwise = lookupAP n z

--b) Definir a função zipWithBT que generaliza a função zipWith para árvores binárias
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty _  = Empty
zipWithBT f _  Empty = Empty
zipWithBT f (Node x1 y1 z1) (Node x2 y2 z2) = Node (f x1 x2) (zipWithBT f y1 y2) (zipWithBT f z1 z2)

---------------------------------Exercício 3----------------------------------------------------------
--Definir a função digitAlpha que dada uma string, devolve um par de strings: uma
--apenas com os números presentes nessa string, e outra com as letas presentes na
--string.Implemente de forma a fazer uma única travessia da string.
digitAlp :: String -> (String,String)
digitAlp = foldr (\x (ds,as) -> if isDigit x then (x:ds,as) else if isAlpha x then (ds,x:as) else (ds,as)) ([],[])

--------------------------------Exercício 4-----------------------------------------------------------
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)
--a) Definir a função firstSeq que recebe uma sequência não vazia e devolve o seu
--primeiro elemento
firstSeq :: Seq a -> a
firstSeq (Cons a s) = a
firstSeq (App Nil s) = firstSeq s
firstSeq (App s _)  = firstSeq s

--b) Definir a função dropSeq tal que dropSeq n s elimina os n primeiros elementos
--da sequência s. A função deve manter a estrutura da sequência
dropSeq :: Int -> Seq a -> Seq a
dropSeq _ Nil = Nil
dropSeq n (Cons a s) = dropSeq (n-1) s
dropSeq n (App s1 s2) | n > nx = dropSeq (n-nx) s2
                      | n == nx = s2
                      | otherwise = (App (dropSeq n s1) s2)
                      where
                      	nx = contaCons s1

contaCons :: Seq a -> Int
contaCons Nil = 0
contaCons (Cons _ s) = 1 + contaCons s
contaCons (App s1 s2) = contaCons s1 + contaCons s2

--c) Declarar (Seq a) como instância da classe Show de forma a obter o seguinte comportamento
--Ex: App (Cons 1 Nil) (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil)) = <<1,7,5,3>>
instance Show a => (Show (Seq a)) where
	show x = "<<" ++ mostra x ++ ">>"

mostra :: Show a => Seq a -> String
mostra Nil = ""
mostra (Cons a Nil) = show a
mostra (Cons a s) = show a ++ "," ++ mostra s
mostra (App s1 s2) = mostra s1 ++ "," ++ mostra s2

--------------------------------Exercício 5-----------------------------------------------------------
type Mat a = [[a]]
--a) Definir a função getElem que seleciona aleatoriamente um elemento da matriz.
--Utilize a função randomRIO
getElem :: Mat a -> IO a
getElem mat = do
	let (linhas,colunas) = (length mat, length (head mat))
	randLine <- randomRIO (0,linhas-1)
	randRow <- randomRIO (0,colunas-1)
	return $ (mat !! randLine) !! randRow

--b) Definir a função magic que verifica se uma matriz quadrada é um quadrado mágico.
--Um quadrado mágico é uma matriz quadrada de inteiros em que a soma de qualquer linha,
--coluna e das duas diagonais é uma constante
magic :: Mat Int -> Bool
magic mat = linhasIguaisA n mat && colunasIguaisA n mat && diagonaisIguaisA n mat
       where n = sum(head mat)

linhasIguaisA :: Int -> Mat Int -> Bool
linhasIguaisA n = foldl (\acc l -> sum l == n && acc) True

colunasIguaisA :: Int -> Mat Int -> Bool
colunasIguaisA n mat = foldl (\acc x -> sum (map (\l -> l !! x) mat) == n && acc) True [0..(length mat-1)]

diagonaisIguaisA :: Int -> Mat Int -> Bool
diagonaisIguaisA n mat = sum (map (\n -> (mat !! n) !! n) [0..ln]) == n && sum (map (\n -> (mat !! n) !! (ln-n)) [0..ln]) == n
           where
           	ln = length mat -1

	