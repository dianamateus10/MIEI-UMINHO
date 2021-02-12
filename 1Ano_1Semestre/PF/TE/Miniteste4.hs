module Miniteste4 where

----------------------------------Exercício 1---------------------------------------------------
data BTree a = Vazia | Nodo a (BTree a) (BTree a)
--a) Definir a função filtra que coloca numa lista todos os elementos da árvore que
--satisfazem uma dada condição
filtra :: (a->Bool)->(BTree a) -> [a]
filtra f Vazia = []
filtra f (Nodo a b c) = if f a then a: filtra f b ++ filtra f c
                              else filtra f b ++ filtra f c

--b)
data Contacto = Casa Integer
               | Trab Integer
               | Tlm Integer
               | Email String
               deriving Show
type Nome = String
type Agenda = [(Nome,[Contacto])]
--i) Definir a função acrescEmail qeu,dado um nome, um email e uma agenda acrescenta
--essa informação à agenda
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n x [] = [(n,[Email x])]
acrescEmail n x ((y1,y2):ys) | n == y1 = ((y1,(y2 ++ [Email x])):ys)
                             | otherwise = acrescEmail n x ys

--ii) Definir a função verEmails que, dado um nome e uma agenda, retorna a lista dos
--emails associados a esse nome. Se esse nome não existir, deve retornar Nothing
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails x ((y1,y2):ys) = if  x == y1 then Just (getEmails y2)
                                  else verEmails x ys

getEmails :: [Contacto] -> [String]
getEmails [] = []
getEmails ((Email e):xs) = e:getEmails xs
getEmails (_:xs) = getEmails xs

----------------------------------Exercício 2------------------------------------------------------
data LTree a = Leaf a | Fork (LTree a) (LTree a)
--a) Definir uma instancia de classe Show para este tipo por forma a obter o seguinte
--comportamento: >Fork (Fork (Leaf 0) (Leaf 1)) (Leaf 2) = ((0/\1)/\2)
instance (Show a) => Show (LTree a) where
    show (Leaf x) = show x
    show (Fork x y) = "(" ++ show x ++ "/\\" ++ show y ++ ")"

--b) Definir a função mktree que constrói uma árvore balanceada com um dado número
--de folhas todas iguais ao segundo argumento. Ex: mktree 5 0 ((0/\0)/\(0/\(0/\0)))
--mktree :: Int -> a -> LTree a

---------------------------------Exercício 3--------------------------------------------------------
type Dakar = [Piloto]
data Piloto = Carro Numero Nome Categoria
              | Mota Numero Nome Categoria
              | Camiao Numero Nome
              deriving Show
type Numero = Int
data Categoria = Competicao | Maratona
              deriving Show
--a) Escrever uma função que verifica se existem, ou não, dois pilotos com igual número
verifNum :: Dakar -> Bool
verifNum xs = repeticoes $ map numPiloto xs

repeticoes :: Eq a => [a] -> Bool
repeticoes [] = False
repeticoes (x:xs) = elem x xs || repeticoes xs

numPiloto :: Piloto -> Numero
numPiloto (Carro n _ _) = n
numPiloto (Mota n _ _) = n
numPiloto (Camiao n _) = n

--b) Escrever a função inserePil que insere ordenadamente um novo piloto na lista
inserePil :: Piloto -> Dakar -> Dakar
inserePil p [] = [p]
inserePil p (x:xs) = if numPiloto x < numPiloto p then (x:p:xs)
                        else (x:(inserePil p xs))

--c) Escrever a função menor que calcula o piloto com número menor
type DakarB = BTree Piloto
menor :: DakarB -> Piloto
menor (Nodo a Vazia _) = a
menor (Nodo a b _) = menor b

--------------------------------Exercício 4--------------------------------------------------------
--a)Definir uma função minimo que calcula o menor elemento de uma árvore binária de
--procura não vazia
minimo :: (Ord a) => BTree a -> a
minimo (Nodo x Vazia _) = x
minimo (Nodo x y _) = minimo y

--b) Definir uma função semMinimo que remove o menor elemento de uma árvore binária
--de procura não vazia
semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo (Nodo x Vazia y ) = y
semMinimo (Nodo x y z) = Nodo x (semMinimo y) z

--c) Definir uma função minSmin que calcula com uma única consulta da árvore o resultado
--das duas funções anteriores
minSmin :: (Ord a) => BTree a -> (a,BTree a)
minSmin (Nodo x Vazia y) = (x,y)
minSmin (Nodo x y z) = (minimo y,Nodo x semM z)
                     where
                        (minim, semM) = minSmin y

---------------------------------Exercício 5--------------------------------------------------------
--a) Definir a função minAB que, dada uma árvore de procura, calcule o menor elemento
--dessa árvore, caso exista
--minAB :: BTree a -> Maybe Int
--minAB Vazia = Nothing
--minAB (Nodo x Vazia _) = Just x
--minAB (Nodo x y _) = minAB y

--b)
type BD = [Video]
data Video = Filme String Int
           | Serie String Int Int
           | Show String Int
           | Outro String
--i) Definir a função espetaculos que indica o título e ano de todos os espetáculos 
--da base de dados
espetaculos :: BD -> [(String,Int)]
espetaculos [] = []
espetaculos ((Show t a):xs) = (t,a):espetaculos xs
espetaculos (_:xs) = espetaculos xs

--ii) Definir a função filmesAno que indica os títulos dos filmes de um dado ano
--que existem na base de dados
filmesAno :: Int -> BD -> [String]
filmesAno _ [] = []
filmesAno n ((Filme x y):xs) | n == y = (x:filmesAno n xs)
                             | otherwise = filmesAno n xs
filmesAno n (_:xs) = filmesAno n xs

----------------------------------Exercício 6------------------------------------------------------
--a) Definir uma instância da classe Eq para este tipo idêntica à que seria obtida
--com a diretiva deriving Eq
instance Eq a => Eq (LTree a) where
    Leaf a == Leaf b = a==b
    Fork a b == Fork c d = (a==c)&&(b==d)
    _        == _        = False

--b) Definir a função mapLT que aplica uma função a todas as folhas de uma árvore
--Ex: mapLT succ (Fork (Leaf 0) (Leaf 1) (Leaf 2)) = Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)
mapLT :: (a -> b) -> LTree a -> LTree b
mapLT f (Leaf a)   = (Leaf (f a))
mapLT f (Fork a b) = (Fork (mapLT f a) (mapLT f b))

 ---------------------------------Exercício 7------------------------------------------------------
type Biblio = [Livro]
data Livro  = Romance Titulo Autor Ano Lido
            | Ficcao  Titulo Autor Ano Lido
             deriving Show
type Titulo = String
type Autor  = String
type Ano    = Int
data Lido   = Sim | Nao
             deriving Show
--a) Escrever uma função que verifica se existem ou não livros repetidos
repLivros :: Biblio -> Bool
repLivros [x] = False
repLivros (x:y:xs) | getNome x == getNome y = True
                   | otherwise = repLivros (x:xs)
                   where
                   	getNome :: Livro -> Titulo
                   	getNome (Romance x _ _ _) = x
                   	getNome (Ficcao x _ _ _) = x

--b) Definir uma função lido que marca um dado livro como lido na biblioteca
lido :: Biblio -> Titulo -> Biblio
lido  [] _ = error "This book doesn't exist"
lido (x@(Romance t a an _):xs) t1 = if t == t1 then (Romance t a an Sim):xs
                                     else x:(lido xs t1)
lido (x@(Ficcao t a an _):xs) t1 = if t == t1 then (Ficcao t a an Sim):xs
                                     else x:(lido xs t1)

--c) Escrever uma função livroAutor que devolve a lista de livros do autor dado
type BiblioA = BTree Livro
livroAutor :: Biblio -> Autor -> [Livro]
livroAutor xs a = filter ((==a).autor) xs

autor :: Livro -> Autor
autor (Romance _ a _ _) = a
autor (Ficcao _ a _ _ ) = a

----------------------------------Exercício 8-------------------------------------------------------
--a) Apresente a definição da função mapBT que aplica uma função a todos os elementos
--de uma árvore binária
mapBT :: (a->b) -> BTree a -> BTree b
mapBT _ Vazia = Vazia
mapBT f (Nodo a b c) = Nodo (f a) (mapBT f b) (mapBT f c)

--b.i) Definir a função consTelefs que, dada uma lista de contactos, retorna a lista
--de todos os números de telefone dessa lista (fixos e telemóveis)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa tel):xs) = tel:(consTelefs xs)
consTelefs ((Trab tel):xs) = tel:(consTelefs xs)
consTelefs ((Tlm tel):xs) = tel:(consTelefs xs)
consTelefs (_:xs) = consTelefs xs

--b.ii) Definir a função casa que, dado um nome e uma agenda retorna o número de 
--telefone de casa (caso exista)
casa :: Nome -> Agenda -> Maybe Integer
casa n agenda | null contact = Nothing
              | otherwise = (contactCasa.snd.head) contact
             where
             	contact = filter ((==n).fst) agenda

contactCasa :: [Contacto] -> Maybe Integer
contactCasa [] = Nothing
contactCasa ((Casa tel):xs) = Just tel
contactCsa (_:xs) = contactCasa xs

-----------------------------------Exercício 9------------------------------------------------------
data LTree' a = Leaf' a | Fork' (LTree' a) (LTree' a)
--a) Definir uma instância Eq para este tipo que considere iguais quaisquer duas
--árvores com a mesma forma
instance (Eq (LTree' a)) where
	Leaf' x == Leaf' y = True
	Fork' a b == Fork' c d = (a==c) && (b==d)
	_ == _ = False

--b) Definir a função build que constrói uma árvore com uma folha por casa elemento
--da lista argumento
build :: [a] -> LTree a
build [x] = Leaf x
build (x:y:xs) = (Fork (Fork (Leaf x) (Leaf y))) (build xs)

---------------------------------Exercício 10-------------------------------------------------------
--a) Definir uma fũnção maxBT que, dada uma árvore de procura, calcule o maior elemento
--dessa árvore, caso exista
maxBT :: BTree Float -> Maybe Float
maxBT Vazia = Nothing
maxBT (Nodo x _ Vazia ) = Just x
maxBT (Nodo x y z) = maxBT z

--b.i) Definir a função outros que seleciona da base de dados todos os vídeos que não
--são filmes, séries ou shows
outros :: BD -> BD
outros = filter eOutro

eOutro :: Video -> Bool
eOutro (Outro _) = True
eOutro _ = False

--b.ii) Definir a função totalEp que indica quantos episódios de uma dada série
--existem na base de dados
totalEp :: String -> BD -> Int
totalEp n xs = length $ filter (\(Serie nome _ _) -> nome == n) (filter eSerie xs)

eSerie :: Video -> Bool
eSerie (Serie _ _ _) = True
eSerie _ = False

--------------------------------Exercício 11--------------------------------------------------------
--a) Definir uma instância da classe Show para este tipo que apresente uma folha para
--cada linha, precedida de tantos pontos quanta a profundidade da árvore
instance (Show a) => Show (LTree' a) where
	show lt = unlines $ map (\(e,d) -> (replicate d '.' ++ show e)) (travessia lt)

travessia lt :: LTree' a -> [(a,Int)]
travessia (Leaf' a) = [(x,0)]
travessia (Fork' e d) = map (\(x,n) -> (x,n+1)) (travessia e ++ travessia d)

--b) Definir uma função cresce que cresce uma árvore duplicando todas as folhas
cresce :: LTree a -> LTree a
cresce (Leaf a) = For (Leaf a) (Leaf a)
cresce (Fork x y) = Fork x y

