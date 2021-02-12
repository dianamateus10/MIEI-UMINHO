module Ficha7 where

data ExpInt = Const Int 
             | Simetrico ExpInt 
             | Mais ExpInt ExpInt 
             | Menos ExpInt ExpInt 
             | Mult ExpInt ExpInt

--1.a)
calcula :: ExpInt -> Int
calcula (Const i) = i
calcula (Simetrico i) = - (calcula i)
calcula (Mais i j) = (calcula i) + (calcula j)
calcula (Menos i j) = (calcula i) - (calcula j)
calcula (Mult i j) = (calcula i) * (calcula j)

{-1.b)
infixa :: ExpInt -> String
infixa (Const i) = [intToDigit (i)]
infixa (Simetrico i) = ['-']++(infixa i)
infixa (Mais i j) = ['(']++(infixa i)++['+']++(infixa j)++[')']
infixa (Menos i j) = ['(']++(infixa i)++['-']++(infixa j)++[')']
infixa (Mult i j) = ['(']++(infixa i)++['*']++(infixa j)++[')']

--1.c)
posfixa :: ExpInt -> String
posfixa (Const i) = [intToDigit (i)]
posfixa (Simetrico i) = ['-']++(posfixa i)
posfixa (Mais i j) = (posfixa i)++(posfixa j)++['+']
posfixa (Menos i j) = (posfixa i)++(posfixa j)++['-']
posfixa (Mult i j) = (posfixa i)++(posfixa j)++['*']
-}


data RTree a = R a [RTree a]

--2.a)
soma' :: Num a => RTree a -> a
soma' (R x []) = x
soma' (R x l) = x + sum (map soma' l)

--2.b)
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1 + maximum (map altura l)

--2.c)
prune :: Int -> RTree a -> RTree a
prune 0 (R v l) = (R v [])
prune n (R v l) = (R v (map (prune (n-1)) l) )

--2.d)
mirror :: RTree a -> RTree a
mirror (R v []) = (R v [])
mirror (R v l)  = (R v (reverse (map (mirror) l)) )

--2.e)
postorder :: RTree a -> [a]
postorder (R x []) = [x]
postorder (R x l) = (concat_Map (postorder) l) ++ [x]

concat_Map :: (a -> [b]) -> [a] -> [b]
concat_Map f [] = []
concat_Map f (h:t) = (f h) ++ (concat_Map f t)


data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)

--3.a)
ltSum :: Num a => LTree a -> a
ltSum (Tip a ) = a
ltSum (Fork e d) = ltSum e + ltSum d

--3.b)
listaLT :: LTree a -> [a]
listaLT (Tip a ) = [a]
listaLT (Fork e d) = (listaLT e) ++ (listaLT d)

--3.c)
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)


data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

--4.a)
splitFTree :: FTree a b -> (BTree a,LTree b)
splitFTree (Leaf n) = (Empty,Tip n)
splitFTree (No a e d) = (Node a (fst (splitFTree e)) (fst (splitFTree d)), 
                         Fork   (snd (splitFTree e)) (snd (splitFTree d)))

--4.b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip n) = Just (Leaf n)
joinTrees (Node a e d) (Fork b c) = Just (No a junta_esq junta_dir)
                                         where
                                             Just junta_esq = joinTrees e b
                                             Just junta_dir = joinTrees d c
joinTrees _ _ = Nothing