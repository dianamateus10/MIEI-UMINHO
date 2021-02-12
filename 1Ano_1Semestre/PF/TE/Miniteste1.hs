module Miniteste1 where


------------------Exercício 1----------------------------------------------------------
--a) Definir a função nome, o seu tipo. Recebe uma string e testa se o segundo
--caracter é uma letra minúscula
nome :: String -> Bool
nome [] = True
nome (x:xs) = if (head xs) `elem` ['a'..'z'] then True
                      else False
--nome (x:y:xs) = isLower y

--b) Escrever função equipaComXgolos (e o tipo) que dado um jogo e o número de
--golos, dá como resultado o nome da equipa que marcou esses golos. Caso nenhuma
--equipa tenha marcado esse número de golos então a funlão devolve uma string vazia
type Jogo = (String,Int,String,Int)

equipaComXgolos :: Jogo -> Int -> String
equipaComXgolos (x1,y1,x2,y2) n | n==y1 = x1
                                | n==y2 = x2
                                | otherwise = []

--c)Escrever a função jogosComXGOlos, que dado uma lista de jogos e um número de
--golo, indica o número de jogos onde uma das equipas marcou esse número de golos
jogosComXGOlos :: [Jogo] -> Int -> Int
jogosComXGOlos [] _ = 0
jogosComXGOlos ((x1,y1,x2,y2):xs) n | n==y1 && n==y2 = 2 + jogosComXGOlos xs n
                                    | n==y1 && n/=y2 = 1 + jogosComXGOlos xs n
                                    | n/=y1 && n==y2 = 1 + jogosComXGOlos xs n
                                    | otherwise = jogosComXGOlos xs n
--jogosComCGOlos xs golos = 
	--length (filter (\j <- equipaComXgolos j golos /= "") xs)

--------------------Exercício 2--------------------------------------------------------
type Ponto = (Float,Float)
type Retangulo = (Ponto,Ponto)
--Definir quadrado que testa se um retangulo é um quadrado e contaQuadrados que,
--dada uma lista com retangulos, conta quantos deles são quadrados
quadrado :: Retangulo -> Bool
quadrado ((x1,y1),(x2,y2)) = if abs (x1-x2) == abs (y1-y2) then True
                                  else False

contaQuadrados :: [Retangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (((x1,y1),(x2,y2)):ys) = length (filter quadrado ys)

-------------------Exercício 3----------------------------------------------------------
--a) Definir a função dist que data um ponto do plano Cartesiano calcula a sua
--distância à origem
dist :: (Float,Float) -> Float
dist (x,y) = sqrt (x^2 + y^2)

--b) Indicar como é o interpretador avalia a expressão (fun[3,-5,0,-3,2])
fun3 :: [Float] -> [Float]
fun3 [] = []
fun3 (h:t) = if h>=0 then h:(fun3 t)
               else (fun3 t)
--fun [3,-5,0,-3,2] = if 3>=0 then 3:(fun [-5,0,-3,2])
--3:(fun [-5,0,-3,2] = else fun [0,-3,2])
--3:(fun [0,-3,2]) = if 0>=0 then 3:0:(fun [-3,2])
--3:0:(fun [-3,2]) = else 3:0:(fun [2:[]])
--3:0:(fun [2:[]]) = if 2>=0 then 3:0:2:(fun [])
--3:0:2:(fun []) = 3:0:2:[] = 3:0:2 = [3,2,0]

--c) Definir a função somaNeg que soma todos os números negativos de uma lista
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if h<0 then h + somaNeg t
                    else somaNeg t
--somaNeg xs = sum $ filter (<0) xs

------------------Exercício 4----------------------------------------------------------
--Definir as funções escala que escala um retângulo de acordo com um dado fator
--(mantendo o primeiro ponto) e escalaTudo que, dado um fator e uma lista com
--retângulos, escala todos os retângulos de acordo com a definição anterior
escala :: Float -> Retangulo -> Retangulo
escala n ((x1,y1),(x2,y2)) = ((x1,y1),((n*(x2-x1)+x1),n*(y2-y1)+y1))

escalaTudo :: Float -> [Retangulo] -> [Retangulo]
escalaTudo _ [] = []
escalaTudo n (((x1,y1),(x2,y2)):ys) = (escala n ((x1,y1),(x2,y2))):(escalaTudo n ys)
--escalaTudo e xs = map (escala e) xs

------------------Exercício 5------------------------------------------------------------
--a) Definir a função mults que recebe três numeros interior e testa se algum é 
--múltiplo dos outros dois.
mults :: Int -> Int -> Int -> Bool
mults x y z | (mod x y) == 0 = True
            | (mod x z) == 0 = True
            | (mod y z) == 0 = True
            | (mod y x) == 0 = True
            | (mod z x) == 0 = True
            | (mod z y) == 0 = True
            | otherwise = False
--emult :: Int -> Int -> Bool
--emult x y = (x `mod` y) == 0
--mults a b c = emult ab || emult a c || emult b a || emult b c || emult c a || emult c b

--b) Indique como o interpretador avalia a expressão fun [3,5,2]
fun5 :: [Float] -> Float
fun5 [] = 1
fun5 (x:xs) = x* (fun5 xs)
--fun [3,5,2] = 3*(fun [5,2])
--3*(fun [5,2]) = 3*(5*(fun [2]))
--3*(5*(fun [2])) = 3*(5*(2*(fun [])))
--3*(5*(2*(fun []))) = 3*5*2*1 = 30

--c) Definir a função triplos que recebe uma lista de inteiros e produz a lista
--dos seus triplos
triplos :: [Int] -> [Int]
triplos [] = []
triplos (x:xs) = ((3*x):(triplos xs))
--triplos n = map (*3) n

----------------Exercício 6---------------------------------------------------------------
--a)
p :: Int -> Bool
p 0 = True
p 1 = False
p x | x > 1 = p (x-2)
--p 5 | 5 > 1 = p (5-2)
--p 3 | 3 > 1 = p (3-2)
--p 1 = False

--b)
type Circulo = (Ponto,Float) -- (Centro(Float,Float),Raio(Float))
--i) Definir uma função dentro que testa se um ponto está dentro do círculo
dentro :: Ponto -> Circulo -> Bool
dentro (x1,y1) ((x2,y2),z) = if sqrt((x2-x1)^2 + (y2-y1)^2) < z then True
                                 else False
--distP :: (Float,Float) -> (Float,Float) -> Float
--distP (a,b) (c,d) = sqrt ((a-c)^2 + (b-d)^2)
--dentro p (c,r) = distP p c < r

--ii) Definir uma funçõ filtra que, dado um ponto e uma lista de círculos, determina
--quantos círculos contêm o ponto dado
filtra :: Ponto -> [Circulo] -> Int
filtra (x1,y1) [] = 0
filtra (x1,y1) (((x2,y2),z):xs) 
           | dentro (x1,y1) ((x2,y2),z) == True = 1 + filtra (x1,y1) xs
           | otherwise = filtra (x1,y1) xs
--filtra p xs = length $ filter (dentro p) xs

-----------------Exercício 7-------------------------------------------------------------
--a) Definir a função somaIgual que recebe três números inteiros e testa se a soma
--de dois desses argumentos é igual ao valor do argumento restante
somaIgual :: Int -> Int -> Int -> Bool
somaIgual x y z | x+y==z = True
                | x+z==y = True
                | y+z==x = True
                | otherwise = False
--somaIgual a b c = a == (b+c) || b = (a+c) || c == (a+b)

--b) Escrever a função resJogo que dado um jogo devolve um caracter com valor '1',
--'x' e '2'. Ex: resJogo ("Alemanha",2,"Portugal",3) dá como resultado '2'
resJogo :: Jogo -> Char
resJogo (x1,y1,x2,y2) | y1 > y2 = '1'
                      | y1 == y2 = 'x'
                      | otherwise = '2'

--c) Escrever uma função que dado uma lista de jogos indica quantos jogos venceram
--as equipas visitadas
contaJogos :: [Jogo] -> Int
contaJogos [] = 0
contaJogos ((x1,y1,x2,y2):xs) | resJogo (x1,y1,x2,y2) == '2' = 1 + contaJogos xs
                              | otherwise = contaJogos xs
--contaJogos xs = length (filter (\j <- resJogo j == '2') xs)

---------------------Exercício 8---------------------------------------------------------
--a) Definir a função supSoma que recebe três númeos inteiros e testa se algum deles
--é superior à soma dos outros dois
supSoma :: Int -> Int -> Int -> Bool
supSoma x y z | x+y < z = True
              | y+z < x = True
              | x+z < y = True
              | otherwise = False
--supSima a b c = (a+b) < c || (a+c) > b || (b+c) > a

--b) Indique o como o interpretador avalia a expressão (fun [2,3,5])
fun8 :: [Float] -> Float
fun8 [] = 0
fun8 (y:ys) = y^2 + (fun8 ys)
--fun [2,3,5] = 2^2 + (fun [3,5])
--4 + (fun [3,5]) = 4 + (3^2 + (fun [5]))
--4 + 9 + (fun [5]) = 4+9+5^2 + (fun [])
--4+9+15 + fun [] = 4+9+25+0=38

--c)Definir a função soDigitos que recebe uma lista de caracteres e seleciona dessa
--lista os caracteres que são algarismos
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) = if x `elem` ['0'..'9'] then x:soDigitos xs
                       else soDigitos xs 
--soDigitos xs = filter isDigit xs

---------------------Exercício 9----------------------------------------------------------
--Definir as funções area que determina a área de um retangulo e areaTotal que dada
--uma lista de retangulos, determina a área total que eles ocupam
area :: Retangulo -> Float
area ((x1,y1),(x2,y2)) = abs (x1-x2) * abs (y1-y2)

areaTotal :: [Retangulo] -> Float
areaTotal [] = 0
areaTotal (((x1,y1),(x2,y2)):xs) = area ((x1,y1),(x2,y2)) + areaTotal xs
--areaTotal xs = sum $ map area xs

--------------------Exercício 10----------------------------------------------------------
--b.i) Definir uma função fora que testa se um ponto está fora de um círculo
fora :: Ponto -> Circulo -> Bool
fora (x1,y1) ((x2,y2),z) = if sqrt((x2-x1)^2 + (y2-y1)^2) > z then True
                                 else False
--fora p (c,r) = distP p c > r

--b.ii) Definir uma função que dado um círculo e uma lista de pontos, determina quantos
--pontos da lista estão fora do círculo dado
filtra' :: Circulo -> [Ponto] -> Int
filtra' ((x1,y2),z) ((x2,x3):xs) = 
    if fora (x2,x3) ((x1,y2),z) == True then 1 + filtra' ((x1,y2),z) xs
        else filtra' ((x1,y2),z) xs
--filtra' c xs = length $ filter (\p <- fora p c) xs

-------------------Exercício 11-----------------------------------------------------------
--a) Definir uma função mult a que (mult n a b) testa se n é simultaneamente
--múltiplo de a e de b.
mult' :: Int -> Int -> Int -> Bool
mult' n a b = if (mod n a) == 0 && (mod n b) == 0 then True
                      else False
--mult' n a b = emult n a && emult n b

--b) Indicar como o interpretador avalia a expressão (fun [8,5,12,7])
fun10 :: [Int] -> [Int]
fun10 [] = []
fun (h:t) = if (mod h 2)==0 then h:(fun t)
               else (fun t)
--fun [8,5,12,7] = (mod 8 2) == 0 then 8:(fun [5,12,7])
--8:(fun [5,12,7]) = (mod 5 2) /=0 then 8:(fun [12,7])
--8:(fun [12,7]) = (mod 12 2) == 0 then 8:12:(fun[7])
--8:12:(fun [7]) = (mod 7 2) /=0 then 8:12:(fun [])
--8:12:(fun[]) = 8:12:[] = 8:[12] = [8,12]

--c) Definir a função minusculas que recebe uma lista de caracteres e conta
--quantos desses caracteres são letras minúsculas
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) = if x `elem` ['a'..'z'] then 1 + minusculas xs
                       else minusculas xs
--minusculas xs = length $ filter isLower xs

-------------------Exercício 12-----------------------------------------------------------
--a) Definir a função nome que recebe uma string como argumento e testa se o seu
--primeiro caracter é uma letra maiúscula
nome' :: String -> Bool
nome' l = if (head l) `elem` ['A'..'Z'] then True
                   else False
--nome' = (isUpper.head)

--b) Escrever uma função golosEquipa que dado um jogo e o nome de uma equipa, dá
--como resultado os golos que essa equipa marcou. Caso a equipa não tenha participado
--a função devolve o valor -1
golosEquipa :: Jogo -> String -> Int
golosEquipa (x1,x2,y1,y2) n | x1 == n = x2
                            | y1 == n = y2
                            | otherwise = -1

--c) Escrever função golos que dado uma lista de jogos e o nome de equipa, indica
--quantos golos a equipa marcou em todos os jogos
golos :: [Jogo] -> String -> Int
golos [] _ = 0
golos ((x1,y1,x2,y2):xs) n | x1 == n = y1 + golos xs n
                           | x2 == n = y2 + golos xs n
                           | otherwise = golos xs n
--golos xs equipa = sum $ map (\j -> golosEquipa j equipa ) xs

-------------------Exercício 13-----------------------------------------------------------
--Definir as funções roda que roda um retangulo 90º, centrado no primeiro ponto e
--rodaTudo que dada uma lista com retangulos, roda todos os retangulos
roda :: Retangulo -> Retangulo
roda ((x1,x2),(y1,y2)) = ((x1,x2),(x1+(y2-y1),(y1+(x2-x1))))

rodaTudo :: [Retangulo] -> [Retangulo]
rodaTudo [] = []
rodaTudo (((x1,x2),(y1,y2)):xs) = roda ((x1,x2),(y1,y2)): rodaTudo xs
--rodaTudo = map roda

--------------------Exercício 14----------------------------------------------------------
--a) Definir a função maior que recebe três números inteiros e testa se algum deles
--é maior que a soma dos outros dois
maior :: Int -> Int -> Int -> Bool
maior x y z | x+y<z = True
            | y+z<x = True
            | x+z<y = True
            | otherwise = False
--maior a b c = a > (b+c) || b > (a+c) || c > (a+b)

--b) Definir resultJogo que dado um jogo devolve uma string que indica se ganhou
--a equipa da casa. Ex: resultJogo ("Alemanha",2,"Portugal",3) dá como resultado
--"ganhou a equipa visitante"
resultJogo :: Jogo -> String
resultJogo (x1,x2,y1,y2) | x2 > y2 = "ganhou equipa da casa"
                         | x2 == y2 = "empate"
                         | otherwise = "ganhou a equipa visitante"

--c) Escrever uma função que dado uma lista de jogos indica quantos jogos terminaram
--em empate
empate :: [Jogo] -> Int
empate [] = 0
empate ((x1,x2,y1,y2):xs) = if x2==y2 then 1 + empate xs
                               else empate xs
--empate xs = length $ filter (\j -> resultJogo j == "empate") xs