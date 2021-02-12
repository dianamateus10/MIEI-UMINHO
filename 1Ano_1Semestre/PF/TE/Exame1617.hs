module Exame1617 where

------------------------------Exercício 1--------------------------------------------------------
--a) Definir uma função unlines que junta todas as strings da lista numa só,
--separando-as pelo caracter '\n'
unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

--b) Definir a função (\\) que retorna a lista resultante de remover (as primeiras
--ocorrências) dos elementos da segunda lista da primeira
--Ex: (\\) [1,2,3,4,5,1] [1,5] == [2,3,4,1]
remove :: (Eq a) => [a] -> [a] -> [a]
remove l r = foldl (\acc x -> let (a,b) = span (/= x) acc in a ++ (if null b then [] else tail b)) l r

-------------------------------Exercício 2--------------------------------------------------------
data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a
--a) Definir a função primeiro que recebe uma sequência não vazia e devolve o primeiro
--elemento
primeiro :: Seq a -> a
primeiro (Inicio a _) = a
primeiro (Fim Nil a) = a
primeiro (Fim s a) = primeiro s

--b) Definir a função semUltimo que recebe uma sequência não vazia e devolve a sequência
--sem o seu último elemento
semUltimo :: Seq a -> Seq a
semUltimo (Inicio a Nil) = Nil
semUltimo (Inicio a s) = Inicio a (semUltimo s)
semUltimo (Fim s a) = s

--------------------------------Exercício 3-------------------------------------------------------
data BTree a = Empty | Node a (BTree a) (BTree a)
--a) Definir a função prune que remove de uma árvore todos os elementos a partir de uma
--determinada profundidade
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node x l r) = Node x (prune (n-1) l) (prune (n-1) r)

--b) Definir uma função semMinimo que remove o menor elemento de uma árvore binária
--de procura não vazia
semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node _ Empty r) = r
semMinimo (Node x l r) = Node x (semMinimo l) r

--------------------------------Exercício 4-------------------------------------------------------
type Tabuleiro = [String]
--a) Definir posicoes que determina as posicoes onde se encontram as rainhas num tabuleiro
--de forma a que posicoes exemplo = [(2,0),(0,1),(3,2),(1,3)]
posicoes :: Tabuleiro -> [(Int,Int)]
posicoes tab = foldl (\acc y -> acc ++ (foldl (\acc2 x -> if (tab !! y) !! x == 'R' then acc2 ++ [(x,y)] else acc2)) [] [0..(length (head tab)-1)]) [] [0..(length tab-1)]

--b) Usando a função anterior definir a função valido que testa se num tabuleiro nenhuma
--rainha ataca a outra. No caso do tabuleiro exemplo a resposta deve ser True.
--Pode-se testar se duas rainhas estao na mesma diagonal vendo se a soma ou a diferença
--entre a coluna e a linha em que estão colocadas é igual
valido :: Tabuleiro -> Bool
valido tab = foldl (\acc (x,y) -> if length (filter (\(a,b)-> (a,b) /= (x,y) && (a ==x || b == y || a-b == x-y || b-a == y-x)) (posicoes tab)) > 0 then False else acc) True (posicoes tab)

--c) Utilizando funções de ordem superior definir a função bemFormado que dado um
--tamanho n e um tabuleiro t testa se este é bem formado, isto é, se tem n linhas,
--n colunas, n rainhas e os restantes caracteres do tabuleiro são '.'
bemFormado :: Int -> Tabuleiro -> Bool
bemFormado n tab = length tab == n && foldr (\x -> (&&) $ (==) n $ length x) True tab && foldl (\acc (x,y) -> if (tab !! y) !! x == 'R' then acc+1 else acc) 0 [(a,b) | a <- [0..n-1],b <- [0..n-1]] == n