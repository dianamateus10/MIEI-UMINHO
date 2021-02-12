module Ficha6 where

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

--1.a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node x y d) = 1 + max (altura y) (altura d)
--altura (Node 5 (Node 2 Empty Empty) (Node 8 (Node 9 Empty Empty) (Node 5 Empty Empty)))
--1 + max (altura (Node 2 Empty Empty)) (altura (Node 8 (Node 9 Empty Empty) (Node 5 Empty Empty))))
--max 2 4 = 4 (número mais alto dos 2 obtidos)

--1.b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x y d) = 1 + (contaNodos y) + (contaNodos d)

--1.c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x y d) = folhas y + folhas d

--1.d)
prune :: Int -> BTree a -> BTree a
prune _ Empty            = Empty
prune 0 _                = Empty
prune x (Node a esq dir) = Node a (prune (x-1) esq) (prune (x-1) dir)

--1.e)
path :: [Bool] -> BTree a -> [a]
path [] _ = []
path _ Empty = []
path (x:xs) (Node d y z) | x == True = d:(path xs y)
                         | otherwise = d:(path xs z)

--1.f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x y d) = Node x (mirror d) (mirror y)

--1.g)
zipWithBT' :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT' f (Node a x y) (Node b xs ys) = Node (f a b) (zipWithBT' f x xs) (zipWithBT' f y ys)
zipWithBT' _ _ _ = Empty

--1.h)
unzipBT' :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT' Empty = (Empty,Empty,Empty)
unzipBT' (Node (x,y,z) d n) = (Node x unzip_esq1 unzip_dir1, 
                               Node y unzip_esq2 unzip_dir2,
                               Node z unzip_esq3 unzip_dir3)
                                   where
                                    (unzip_esq1,unzip_esq2,unzip_esq3) = unzipBT' d
                                    (unzip_dir1,unzip_dir2,unzip_dir3) = unzipBT' n


--arvoreprocura = raiz maior que esquerda e menor que direita
--2.a)
minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x y d) = minimo y

--2.b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty _) = Empty
semMinimo (Node x y d) = Node x (semMinimo y) d 

--2.c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty _) = (r,Empty)
minSmin (Node x y d) = (a,Node x b d)
                            where
                               (a,b) = minSmin y

--2.d)
--Falta fazer - Dúvida


type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep | Faltou deriving Show
type Turma = BTree Aluno

--3.a)
inscNum :: Numero -> Turma -> Bool
inscNum x Empty = False
inscNum x (Node (a,_,_,_) e d) = x == a || inscNum x (if x < a then e else d)

--3.b)
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty                           = False
inscNome name (Node (_,nome,_,_) esq dir) = name == nome ||
                                            inscNome name esq ||
                                            inscNome name dir 

--3.c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (x,y,z,_) e d) = 
      case z of TE -> [(x,y)]; otherwise -> [] ++ trabEst e ++ trabEst d

--3.d)
nota :: Numero -> Turma -> Maybe Classificacao
nota a Empty = Nothing
nota a (Node (x,y,z,w) e d) | a == x = Just w
                            | otherwise = nota a (if a < x then e else d)

--3.e)
percFaltas :: Turma -> Float
percFaltas Empty = 0.0
percFaltas turma = (calcula_faltas turma / calcula_totalalunos turma) * 100
                   where
                      calcula_faltas Empty = 0
                      calcula_faltas (Node (_,_,_,w) e d) = case w of
                                                              Faltou -> 1;
                                                              otherwise -> 0 + calcula_faltas e + calcula_faltas d
                      calcula_totalalunos Empty = 0
                      calcula_totalalunos (Node x e d) = 1 + calcula_totalalunos e + calcula_totalalunos d

--3.f)
mediaAprov :: Turma -> Float
mediaAprov Empty = 0.0
mediaAprov turma = (soma_medias turma / calcula_alunosaprov turma)
                  where
                    soma_medias Empty = 0
                    soma_medias (Node (_,_,_,w) e d) = case w of
                  	                                       Aprov x -> (fromIntegral x) + soma_medias e + soma_medias d;
                  	                                       otherwise -> 0 + soma_medias e + soma_medias d
                    calcula_alunosaprov Empty = 0
                    calcula_alunosaprov (Node (_,_,_,w) e d) = case w of
                                                                Aprov x -> 1.0 + calcula_alunosaprov e + calcula_alunosaprov d;
                                                                otherwise -> 0 + calcula_alunosaprov e + calcula_alunosaprov d

--3.g)
--falta fazer - dúvida
