module Miniteste3 where

--------------------------------Exercício 1-------------------------------------------------------
type RegAlcool = [(Nome,Sexo,Idade,NA)]
type Nome = String
type Sexo = Char         -- 'M':Masculino|'F':Feminino
type Idade = Int
type NA = Float        -- Nível de Álcool
--a) Escrever uma função de ordem superior que dados os testes realizados, devolve
--informação dos testes realizados a pessoas menores de 21 anos
menorVinteUm :: RegAlcool -> RegAlcool
menorVinteUm = filter $ (\(_,_,i,_) -> i <= 21)

--b) Escrever uma função que dados os testes realizados, produz uma lista onde se
--indica: o nome, a idade, e o valor de multa a pagar
multa :: RegAlcool -> [(Nome,Idade,Float)]
multa [] = []
multa ((x,y,z,w):xs) | w > 0.5 = ((x,z,w*100):multa xs)
                     | otherwise = ((x,z,0):multa xs)
--multa xs = map (\(n,_,i,na) -> (n,i, if na > 0.5 then 100*na else o)) xs

--c) Escrever uma função de ordem superior que calcula a idade média das pessoas que
--realizaram o teste
media :: RegAlcool -> Float
media xs = fromIntegral (sum(map (\(_,_,i,_) -> i) xs))/ fromIntegral (length xs)

-------------------------------Exercício 2--------------------------------------------------------
--a) Alternativa com recursividade explícita à função de ordem superior apresentada
func :: Eq a => a -> [a] -> Bool
func x l = not (null (filter (x==) l))

func' :: Eq a => a -> [a] -> Bool
func' n [] = False
func' n (x:xs) = if n == x then True
                    else func' n xs

--b) Definir a função zero que testa se uma matriz contém apenas zeros, sem usar
--recursividade explícita
type Matriz a = [[a]]
zero :: (Eq a,Num a) => Matriz a -> Bool
zero xs = and $ map (all (==0)) xs

-----------------------------Exercício 3----------------------------------------------------------
type Polinomio = [Monomio]
type Monomio = (Float,Int)
--a) Definir uma função conta de forma a que (conta n p) indica quantos monómios de
--grau n existem em p
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((x,y):xs) | n == y = 1 + conta n xs
                   | otherwise = conta n xs
--conta n p = length $ filter (==n) (map snd p)

--b) Definir uma função selgrau de forma a que (selgrau n p) selecione do polinómio
--p os monómios de grau superior a n
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((x,y):xs) | n >= y = selgrau n xs
                     | otherwise = (x,y):selgrau n xs
--selgrau n p = filter ((>n).snd) p

--c) Completar a função deriv de forma a que calcule a derivada de um polinomio
deriv :: Polinomio -> Polinomio
deriv p = map (\(c,e) -> (c*(fromIntegral e),e-1)) p

----------------------------Exercício 4------------------------------------------------------------
type Radar = [(Hora,Matricula,VelAutor,VelCond)]
type Hora = (Int,Int)
type Matricula = String
type VelAutor = Int
type VelCond = Float
--a) Escrever uma função que verifica se o radar está a funcionar corretamente 
--(velocidade do condutor maior que a velocidade autorizada), através de uma função
--de ordem superior filter
verifRadar :: Radar -> Bool
verifRadar xs = null (filter (\(_,_,x,y) -> y<(fromIntegral x)) xs)

--b) Escrever uma função que dado os registos de infrações de um dia, devolve a lista
--com a matricula do carro e o excesso de velocidade a que se deslocava
registInf :: Radar -> [(Matricula,Float)]
registInf [] = []
registInf ((x,y,z,w):xs) = ((y,(w-(fromIntegral z))):registInf xs)
--registInf xs = map (\(_,m,va,vc) -> (m,vc-va)) xs

--c) Escrever uma função de ordem superior que dado a lista produzida na alínea
--anterior, calcula o total de excesso de velocidade
totalExc :: [(Matricula,Float)] -> Float
totalExc xs = sum $ map (\(_,x) -> x) xs
--totalExc xs = sum $ map snd xs

------------------------------Exercício 5------------------------------------------------------------
--a) Apresentar uma definição alternativa da função usando recursividade explícita
func5 :: [[a]] -> [Int]
func5 l = map length (filter null l)

func5' :: [[a]] -> [Int]
func5' [] = []
func5' ([]:xs) = 0:func5' xs
func5' (_:xs) = func5' xs

--b) Definir a função elem que testa se um elemento pertence a um multi-conjunto
--sem usar a recursividade explícita
type MSet a = [(a,Int)]

elemento' :: Eq a => a -> MSet a -> Bool
elemento' n l = any (==n) (map fst l)

------------------------------Exercício 6------------------------------------------------------------
prod :: [a] -> [b] -> [(a,b)]
prod l1 l2 = [(a,b) | a <- l1, b <- l2]
--Ex: prod [1,2] ['a','b','c'] = [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c')]
--a) Apresente uma definição explicitamente recursiva da função criaPares que recebe
--um elemento do tipo a e uma lista e cria uma lista de pares, cuja primeira 
--componente é sempre a mesma; e completar a definição apresentada
criaPares :: a -> [b] -> [(a,b)]
criaParea n [] = []
criaPares n (x:xs) = ((n,x):criaPares n xs)

criaPares' :: a -> [b] -> [(a,b)]
criaPares' a bs = map (acrescenta a) bs
            where
              acrescenta a x = (a,x)

--b) Definir a função prod concatenando todas as linhas produzidas pela função anterior
--através da definição da função concat que concatena uma lista de listas numa só
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs
--concat' = foldl (++) []

------------------------------Exercício 7------------------------------------------------------------
--a) Definir a função calcula que calcula o valor de um polinómio dado um valor
calcula :: Float -> Polinomio -> Float
calcula n [] = 0
calcula n ((x,y):xs) = (x*n^y) + calcula n xs
--calcula v xs = sum $ map (\(c,e) -> c*v^e) xs

--b) Definir a função simp que retira de um polinomio os monómios de coeficiente
--zero. Usar de preferência funções de ordem superior
simp :: Polinomio -> Polinomio 
simp = filter ((/=0).fst)

--c) Completar a definição da função mult de forma a que esta calcule o resultado da
--multiplicação de um monómio por um polinómio
mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) p = map (\(c1,c2) -> (c*c1,e+c2)) p

--------------------------------Exercício 8----------------------------------------------------------
--a) Escrever umã função de ordem superior que dados os testes realizados, devolve
--os testes realizados a mulheres apenas
genero :: RegAlcool -> RegAlcool
genero = filter (\(_,s,_,_) -> s == 'F')

--b) Escrever uma função que dados os testes realizados, produz uma lista onde se
--indica: o nome da pessoa, e uma string a indicar se a condução é legal ou ilegal
resulTeste :: RegAlcool -> [(Nome,String)]
resulTeste [] = []
resulTeste ((x,y,z,w):xs) | w > 0.5 = ((x,"ilegal")):resulTeste xs
                          | otherwise = ((x,"legal")):resulTeste xs
--resulTeste = map (\(n,_,_,na) -> (n, if na > 0.5 then "ilegal" else "legal"))

--c) Escrever uma função de ordem superior que dados os testes realizados calcula
--o seu valor de acordo com a regra NA*100, caso o nível de alcool seja superior
--ao permitido
totalMulta :: RegAlcool -> Float
totalMulta xs = sum $ map (\(_,_,_,na) -> if na > 0.5 then na*100 else 0) xs

-------------------------------Exercício 9-----------------------------------------------------------
--a) Apresentar uma definição alternativa usando a recursividade explícita
func9 :: Eq a => [a] -> [a] -> Bool
func9 l m = and (zipWith (==) l m)

func9' :: Eq a => [a] -> [a] -> Bool
func9' [] [] = True
func9' [] _ = False
func9' _ [] = False
func9' (x:xs) (y:ys) = x == y && func9' xs ys

--b) Definir a função quadrada que testa se uma matriz é quadrada, sem usar a 
--recursividade explícita
quadrada :: Matriz a -> Bool
quadrada xs = all (==length xs) (map length xs)

-------------------------------Exercício 10-----------------------------------------------------------
--a) Escrever uma função de ordem superior que dado as infrações registadas pelo
--radar num dia, aplica uma tolerância de 10% à tolerância registada
tolerancia :: Radar -> Radar
tolerancia = filter (\(_,_,va,vc) -> vc > (1.1 * (fromIntegral va)))

--b) Escrever ums função de ordem superior que dado a matrícula de um carro e as
--infrações registadas num dia, devolve uma lista com as infrações desse carro
infracoes :: Matricula -> Radar -> Radar
infracoes mat = filter (\(_,m,va,vc) -> m == mat )

--c) Escrever umã função de ordem superior que calcula o total de excesso de
--velocidade num dia.
totalVel :: Radar -> Float
totalVel xs = sum $ map (\(_,_,va,vc) -> vc-(fromIntegral va)) xs

-------------------------------Exercício 11-------------------------------------------------------------
type Jornada = [Jogo]
type Jogo = ((Equipa,Golos),(Equipa,Golos))
type Equipa = String
type Golos = Int
--a) Definir a função totalGolos que calcula o total de golos da jornada
totalGolos :: Jornada -> Int
totalGolos [] = 0
totalGolos (((x1,y1),(x2,y2)):xs) = (y1+y2) + totalGolos xs
--totalGolos xs = sum $ map (\(_,g1),(_,g2) -> g1+g2)xs )

--b)Definir a função numGolos de forma a que (numGolos x j) represente a lista de
--jogos com mais de x golos marcadores. Usar de preferência funções de ordem superior
numGolos :: Int -> Jornada -> [Jogo]
numGolos x l = filter (\((_,g1),(_,g2)) -> (g1+g2) > x) l

--c) Definir as funções vc e casa de forma a que a função venceCasa calcule a lista
--das equipas que venceram em casa numa dada jornada
venceCasa :: Jornada -> [Equipa]
venceCasa j = map casa (filter vc j)
               where
                   casa :: Jogo -> Equipa
                   casa ((e,_),(_,_)) = e
                   vc :: Jogo -> Bool
                   vc ((_,g1),(_,g2)) = g1>g2

-----------------------------Exercício 12----------------------------------------------------------------
--a) Definir a função criaLinhas que aplica a função anterior a cada elemento da lista
--Ex: criaLinhas [1,2] ['a','b','c'] = [[(1,'a'),(1,'b'),(1,'c')],[(2,'a'),(2,'b'),(2,'c')]]
criaLinhas :: [a] -> [b] -> [[(a,b)]]
criaLinhas [] _ = []
criaLinhas _ [] = []
criaLinhas (x:xs) ys = zip (repeat x) ys : criaLinhas xs ys

-----------------------------Exercício 13-----------------------------------------------------------------
--a) Apresentar uma definição alternativa usando a recursividade explícita
func13 :: Ord a => a -> [a] -> Int
func13 x l = length (filter (>= x) l)

func13' :: Ord a => a -> [a] -> Int
func13' n [] = 0
func13' n (x:xs) | n == x = 1 + func13' n xs
                 | otherwise = func13' n xs

--b) Definir a função size que determina o tamanho de um multi-conjunto, sem usar a
--recursividade explícita
size :: MSet a -> Int
size l = sum $ map snd l

--------------------------------Exercício 14--------------------------------------------------------------
--a) Definir a função pontos que calcula os pontos que cada equipa obteve numa jornada
--(vence - 3 pontos; perde - 0 pontos, empate - 1 ponto)
pontos :: Jornada -> [(Equipa,Int)]
pontos [] = []
pontos (((x1,g1),(x2,g2)):xs) | g1>g2 = ((x1,3):(x2,0):pontos xs)
                              | g1==g2 = ((x1,1):(x2,1):pontos xs)
                              | otherwise = ((x1,0):(x2,3):pontos xs)

--b) Definir a função empates que seleciona os jogos da jornada em que ocorreram
--empates. Usar, de preferência, funções de ordem superior
empates :: Jornada -> [Jogo]
empates = filter (\((x1,g1),(x2,g2)) -> g1 == g2)

--c) Definir a função soma de forma a que a função golosMarcados calcule o número total
--de golos marcados numa jornada
golosMarcados :: Jornada -> Int
golosMarcados j = sum (map soma j)
                where
                   soma ((_,g1),(_,g2)) = (g1+g2)