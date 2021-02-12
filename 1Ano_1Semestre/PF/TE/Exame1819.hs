module Exame1819 where

------------------------------------------Exercício 1---------------------------------------------
--a)Apresentar a definição recursiva da função isSorted que testa se uma lista está
--ordenada por ordem crescente
isSorted :: (Ord a) => [a] -> Bool
isSorted [x] = True
isSorted (x:y:xs) = if x < y then isSorted (y:xs)
                         else False 

--b) Apresentar a definição de função inits que calcula a lista de prefixos de uma lista
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

-----------------------------------------------Exercício 2-----------------------------------------
--Definir a função maximumBD que dá o maior elemento de uma lista de elementos do
--tipo Maybe a
maximumBD :: (Ord a) => [Maybe a] -> Maybe a
maximumBD = foldl (\acc x -> if x `maiorQue` acc then x else acc) Nothing
            where
            	maiorQue (Just a) (Just b) = a > b
            	maiorQue _ Nothing = True
            	maiorQue Nothing _ = False

-----------------------------------------Exercício 3-----------------------------------------------
data LTree a = Tip a | Fork (LTree a) (LTree a)
--a) Definir a função listaLT que dá a lista das folhas de uma árvore
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork a b) = listaLT a ++ listaLT b

--b) Definir uma instância da classe Show para este tipo que apresente uma folha por
--casa linha, precedida de tantos pontos quanto a profundidade da árvore
instance (Show a) => Show (LTree a) where
	show (Tip a) = show a ++ "\n"
	show (Fork a b) = mostra 1 a ++ mostra 1 b

mostra :: (Show a) => Int -> LTree a -> String
mostra n (Tip a) = replicate n '.' ++ show a ++ "\n"
mostra n (Fork a b) = mostra (n+1) a ++ mostra (n+1) b

------------------------------------------Exercício 4----------------------------------------------
--Utilizando uma função auxiliar com acumuladores, otimize a seguinte definição que
--determina a soma do segmento inicial de uma lista com soma máxima 
maxSumInit :: (Num a,Ord a) => [a] -> a
maxSumInit l = foldl (\acc x -> max (sum x) acc) (sum l) (inits' l)

-------------------------------------------Exercício 5--------------------------------------------------
type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = (a,a->[a])

convLP :: RelL a -> RelP a
convLP l = concat (map junta l)
           where junta (x,xs) = map (\y -> (x,y)) xs

--a) Definir a função convPL 
convPL :: (Eq a) => RelP a -> RelL a
convPL [(x,y)] = [(x,[y])]
convPL (h:t) = junta h (convPL t)
    where junta (a,b) l = if a `elem` map (fst) l 
                          then map (\(c,d) -> if c == a then (c,b:d) else (c,d)) l 
                          else (a,[b]):l

--b) Definir criaRelPint tal que permite ao utilizador criar uma relação de inteiros
--com n pares
criaRelPint :: Int -> IO (RelP Int)
criaRelPint 0 = return []
criaRelPint n = do
    putStr "Introduz dois numeros (separados por um espaco): "
    (num1,num2) <- fmap (span (/= ' ')) getLine
    fmap ((read num1,read num2) :) $ criaRelPint (n - 1)

--c) Definir as funções de conversão
convFP :: (Eq a) => RelF a -> RelP a
convFP (l,f) = convLP $ map (\x -> (x,f x)) l

convPF :: (Eq a) => RelP a -> RelF a
convPF x = ((map fst y),f)
    where y = convPL x
          f a = foldl (\acc (b,c) -> if a == b then c else acc) [] y