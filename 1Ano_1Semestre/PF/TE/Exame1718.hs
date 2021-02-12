module Exame1718 where

import Data.List

---------------------------------Exercício 1--------------------------------------------------------
--Apresentar uma definição recursiva da função (!!) que dada uma lista e um inteiro,
--calcula o elemento da lista que se encontra nessa posição
calcula :: [a] -> Int -> a
calcula (x:xs) 0 = x
calcula (x:xs) n = calcula xs (n-1)

---------------------------------Exercício 2--------------------------------------------------------
data Movimento = Norte | Sul | Este | Oeste deriving Show
--Definir a função posicao que dada uma posicao inicial (abcissa e ordenada) e uma
--lista de movimentos, calcula a posicao final do robot após a sequência de movimentos
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao n [] = n
posicao (a,b) (Norte:xs) = posicao (a,b+1) xs
posicao (a,b) (Sul:xs) = posicao (a,b-1) xs
posicao (a,b) (Este:xs) = posicao (a+1,b) xs
posicao (a,b) (Oeste:xs) = posicao (a-1,b) xs

---------------------------------Exercício 3--------------------------------------------------------
--Apresentar uma definição recursiva da função any que testa se um predicado é verdade
--para algum elemento de uma lista
any :: (a->Bool) -> [a] -> Bool
any f = foldl (\acc x -> if f x then True else acc) False

---------------------------------Exercício 4---------------------------------------------------------
type Mat a = [[a]]
--Definir a função triSup que testa se uma matriz quadrada é triangular superiro
triSup :: (Num a,Eq a) => Mat a -> Bool
triSup m = all (\n -> all ((==) 0 . (!!) (m !! n)) [0..(n-1)]) [1..(length m -1)]

----------------------------------Exercício 5--------------------------------------------------------
--Definir a função movimenta que lê uma sequẽncia de comandos do teclado ('N' para
--Norte, 'S' para Sul, 'E' para Este e 'O' para Oeste) e qualquer outro caracter para
--parar, e devolve a posicao final do robot (assumindo que a inicial é (0,0))
movimenta :: IO (Int,Int)
movimenta = moveFrom (0,0)

moveFrom :: (Int,Int) -> IO (Int,Int)
moveFrom (x,y) = do
	dir <- getChar
	case dir of 'N' -> moveFrom (x,y+1)
	            'S' -> moveFrom (x,y-1)
	            'E' -> moveFrom (x+1,y)
	            'O' -> moveFrom (x-1,y)
	            'W' -> moveFrom (x-1,y)
	            otherwise -> return (x,y)

-------------------------------------Exercício 6-----------------------------------------------------
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]
--a) Definir a função vazia que testa se uma imagem tem ou não nenhum quadrado
vazia :: Imagem -> Bool
vazia (Quadrado _) = False
vazia (Mover _ im) = vazia im
vazia (Juntar img) | null img = True
                   | otherwise = or (map vazia img)

--b) Definir a função maior que calcula a largura do maior quadrado de uma imagem
maior :: Imagem -> Maybe Int
maior (Quadrado n) = Just n
maior (Mover _ im) = maior im
maior (Juntar img) | null img = Nothing
                   | otherwise = maximum' (filter (/=Nothing) (map maior img))
                   where
                   	maximum' [] = Nothing
                   	maximum' l = maximum l

--c) Definir Imagem como instância de Eq de forma a que duas imagens são iguais se
--forem compostas pelos mesmos quadrados nas mesmas posições
instance Eq Imagem where
	img1 == img2 = null $ (quadPos img1 (0,0)) \\ (quadPos img2 (0,0))

quadPos :: Imagem -> (Int,Int) -> [(Int,(Int,Int))]
quadPos (Quadrado n) pos = [(n,pos)]
quadPos (Mover (a,b) img) (x,y) = quadPos img (x+a,y+b)
quadPos (Juntar imgs) pos = concatMap (\x -> quadPos x (pos)) imgs
