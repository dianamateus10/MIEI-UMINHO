module Miniteste2 where

------------------------Exercício 1------------------------------------------------------------------
type MSet a = [(a,Int)]
--a) Definir a função insere que insere um elemento num multi-conjunto
insere :: Eq a => a -> MSet a -> MSet a
insere n [] = [(n,1)]
insere n ((a,b):xs) | n == a = ((a,b+1):xs)
                    | otherwise = (a,b):insere n xs

--b) Definir uma função moda que devolve a lista dos elementos com maior número de
--ocorrências
moda :: MSet a -> [a]
moda xs = map fst $ filter ((==maxim).snd) xs
     where maxim = maximum $ map snd xs

-----------------------Exercício 2-----------------------------------------------------------------
type Radar = [(Hora,Matricula,VelAutor,VelCond)]
type Hora = (Int,Int)
type Matricula = String
type VelAutor = Int      -- velocidade autorizada
type VelCond = Float     -- velocidade condutor

--a) Escrever função que verifica se o radar está a funcionar corretamente
checkRadar :: Radar -> Bool
checkRadar [(_,_,x,y)] | y > (fromIntegral x) = True
                       | otherwise = False
--checkRadar [] = True
--checkRadar ((_,_,va,vc):xs) = vc > va && checkRadar xs

--b) Escrever função que calcula o total de excesso de velocidade nesse dia
totalExcess :: Radar -> Float
totalExcess [] = 0
totalExcess ((_,_,x,y):xs) = (y-(fromIntegral x)) + totalExcess xs


--c) Escrever função que calcula o maior período de tempo sem infrações durante o dia
hora2mins :: Hora -> Int
hora2mins (x,y) = x*60+y

greaterPeriod :: Radar -> Int
greaterPeriod [(x,_,_,_),(y,_,_,_)] = hora2mins y - hora2mins x
greaterPeriod ((x,_,_,_):y@(z,_,_,_):xs) = max (hora2mins z - hora2mins x) (greaterPeriod (y:xs))

----------------------Exercício 3------------------------------------------------------------------
type Inscritos = [(Numero,Nome,Curso,Ano)]
type Numero = Integer
type Nome = String
type Curso = String
type Ano = Integer

--a) Definir a função aluCA que calcula o número de alunos inscritos num determinado
--ano de um dado curso
aluCA :: (Curso,Ano) -> Inscritos -> Int
aluCA (x,y) [] = 0
aluCA (x,y) ((_,_,z,w):xs) | x == z && y == w = 1 + aluCA (x,y) xs
                           | otherwise = aluCA (x,y) xs

--b) Definir a função quantos que dado um curso c, uma lista de numeros l e uma tabela
--t, calcula quantos números da lista l correspondem a alunos inscritos no curso c
quantos :: Curso -> [Numero] -> Inscritos -> Int
quantos _ _ [] = 0
quantos c l ((x,y,z,w):xs) | c == z && x `elem` l = 1 + quantos c l xs
                           | otherwise = quantos c l xs

--c) Definir a função doAno que seleciona todos os alunos que frequentam um ano
doAno :: Ano -> Inscritos -> [(Numero,Nome,Curso)]
doAno _ [] = []
doAno a ((x,y,z,w):xs) | a == w = ((x,y,z):doAno a xs)
                       | otherwise = doAno a xs

------------------------Exercício 4----------------------------------------------------------------
--a) Definir a função elimina que elimina um determinado elemento de um multi-conjunto
elimina :: Eq a => a -> MSet a -> MSet a
elimina _ [] = []
elimina a ((x,y):xs) | a == x && y>1 = ((x,y-1):xs)
                     | a == x && y==1 = xs
                     | otherwise = elimina a xs

--b) Definir a função ordena que ordena um multi-conjunto pelo número crescente de 
--ocorrências
ordena :: MSet a -> MSet a
ordena [] = []
ordena (h:t) = inserir h (ordena t)
        where
            inserir h [] = [h]
            inserir (a,b) ((c,d):ys) = if b < d then (a,b):(c,d):ys
                                          else (c,d):inserir (a,b) ys

--------------------------Exercício 5---------------------------------------------------------------
type PlayList = [(Titulo,Interprete,Duracao)]
type Titulo = String
type Interprete = String
type Duracao = Int       -- duracao da música em segundos
--a) Definir a função total que calcula o tempo total da playlist
total :: PlayList -> Int
total [] = 0
total ((_,_,x):xs) = x + total xs

--b) Definir uma função temMusicas tal que testa se todos os interpretes que aparecem
--na lista têm alguma música na playlist
temMusicas :: [Interprete] -> PlayList -> Bool
temMusicas _ [] = False
temMusicas l ((_,x,_):xs) = if x `elem` l then True
                                 else temMusicas l xs
--temMusicas [] _ = True
--temMusicas (x:xs) pl = elem x interpretes && temMusicas xs pl
--                           where
--                               interpretes = map (\(_,i,_) -> i) pl

--c) Definir uma função que indica o titulo e a duração de uma das musicas de maior
--duração na playlist
maior :: PlayList -> (Titulo,Duracao)
maior [(x,y,z)] = (x,z)
maior ((x,y,z):xs) = if z > snd (result) then (x,z)
                        else result
                        where
                           result = maior xs

----------------------------Exercício 6--------------------------------------------------------------
--a) Escrever a função que dado a matricula de um carro, calcula o excesso de velocidade
--desse carro nesse dia. O carro pode ter mais do que uma infração
excVel :: Matricula -> Radar -> Float
excVel x [] = 0
excVel c ((x,y,z,w):xs) | c == y = (w-(fromIntegral z)) + excVel c xs
                        | otherwise = excVel c xs

--b) Escrever função que recebe a primeira componente do par Hora e devolve quantas
--infrações se realizaram num período de uma hora
quantasInf :: Int -> Radar -> Int
quantasInf _ [] = 0
quantasInf h (((a,b),y,z,w):xs) | h == a = 1 + quantasInf h xs
                                | otherwise = quantasInf h xs

--c) Definir uma função que verifica se o radar está a funcionar corretamente. O radar
--deve registar as infrações por ordem crescente de hora
verRadar :: Radar -> Bool
verRadar [x] = True
verRadar (((a,b),x1,y1,w1):((c,d),x2,y2,w2):xs) 
        | (hora2mins (c,d) - hora2mins (a,b)) >=0 = verRadar (((c,d),x2,y2,w2):xs)
        | otherwise = False

-------------------------------Exercício 7-------------------------------------------------------------
type TabAbrev = [(Abreviatura,Palavra)]
type Abreviatura = String
type Palavra = String
--a) Definir uma função existe que verifica se uma data abreviatura existe na tabela
existe :: Abreviatura -> TabAbrev -> Bool
existe _ [] = False
existe c ((x,y):xs) | c == x = True
                    | otherwise = existe c xs

--b) Definir a função substitui que recebe um texto (lista de Strings) e uma tabela
--de abreviaturas, substitui todas as abreviaturas que aparecem no texto pelas respetivas
--palavras associadas
substitui :: [String] -> TabAbrev -> [String]
substitui xs [] = xs 
substitui xs ((abrev, palav):ys) = substitui (map (replace abrev palav) xs) ys

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace find repl s@(x:xs) = if take findS s == find
                    then repl ++ (replace (drop findS s) find repl)
                    else x : (replace (xs) find repl)
    where findS = length find

--c) Definir uma função estaOrdenada que testa se a tabela de abreviaturas está
--ordenada por ordem crescente de abreviatura
estaOrdenada :: TabAbrev -> Bool
estaOrdenada [] = True
estaOrdenada [x] = True
estaOrdenada ((a1,b1):(a2,b2):xs) = a1 < a2 && estaOrdenada ((a2,b2):xs)

------------------------------Exercício 8--------------------------------------------------------------
--a) Qual o valor de f "otrec"
f l = g [] l
g l [] = l
g l (h:t) = g (h:l) t
--f "otrec" = g [] "otrec"
--g "otrec" [] = g ('o':[]) "trec"
--g ('o':[]) "trec" = g 't':"o" "rec"
--g 't':"o" "rec" = g 'r':"to" "ec"
--g 'r':"to" "ec" = g 'e':"rto" "c"
--g 'e':"rto" "c" = g 'c':"erto" ""
--g 'c':"erto" "" = "certo"

--b) Assuma que os polinómios têm no máximo um monómio para cada grau e que não são
--armazenados monómios com coeficiente nulo
type Monomio = (Float,Int) -- (Coeficiente,Expoente)
type Polinomio = [Monomio]
--i) Definir coef que calcula o coeficiente de um dado grau (0 se não existir)
coef :: Polinomio -> Int -> Float
coef ((x,y):xs) n = if y == n then x
                         else coef xs n

--ii) Definir uma função poliOk que testa se um polinómio está bem construído
poliOk :: Polinomio -> Bool
poliOk [(x,y)] = if y == 0 then False
                      else True
poliOk ((x,y):(z,w):xs) = if y == 0 || w == 0 || y == w then False
                                else poliOk ((z,w):xs)

----------------------------------Exercício 9---------------------------------------------------------------
--a)Definir uma função size que calcula o tamanho de um multi-conjunto.
size :: MSet a -> Int
size [] = 0
size ((x,y):xs) = y + size xs

--b) Definir uma função union que calcula a união de dois multi-conjuntos
union :: Eq a => MSet a -> MSet a -> MSet a
union [] x = x
union x [] = x
union (x:xs) ys = union xs (adicionar x ys)

adicionar :: Eq a => (a,Int) -> MSet a -> MSet a
adicionar t [] = [t]
adicionar (a,b) ((x,y):xs) = if a == x then (x,y+b):xs
                                else (x,y):(adicionar (a,b) xs)

-----------------------------Exercício 10----------------------------------------------------------
--a) Escreva a função que verifica se houve algum carro apanhado em excesso de
--velocidade mais do que uma vez
verExcVel :: Radar -> Bool
verExcVel xs = repeticao (map (\ (_,m,_,_) -> m) xs)

repeticao :: Eq a => [a] -> Bool
repeticao [] = False
repeticao (x:xs) = elem x xs || repeticao xs

--b) Escrever uma função que dado a matricula de um carro, devolve a lista com
--as infrações desse carro. Esta lista contém pares com a hora e a velocidade em
--excesso do carro
infraCar :: Matricula -> Radar -> [(Hora,Float)]
infraCar n [] = []
infraCar n ((x,y,z,w):xs) = if n == y then (x,w-(fromIntegral z)):infraCar n xs
                              else infraCar n xs

-----------------------------Exercício 11--------------------------------------------------------
type TabTemp = [(Data,Temp,Temp)]
type Data = (Int,Int,Int)
type Temp = Float
--a) Definir a função medias que constroi a lista com as temperaturas médias de cada dia
medias :: TabTemp -> [(Data,Temp)]
medias [] = []
medias ((x,y,z):xs) = (x,((y+z)/2)):medias xs

--b) Definir a função decrescente que testa se a tabela está ordenada por ordem
--decrescente de data
decrescente :: TabTemp -> Bool
decrescente [(d,n,m)] = True
decrescente ((x1,y1,z1):(x2,y2,z2):xs) = if x1 > x2 then decrescente ((x2,y2,z2):xs)
                                        else False
          
----------------------------Exercício 12----------------------------------------------------------
--a) Escrever uma função que verifica se o rador registou infrações à mesma hora
igualHora :: Radar -> Bool
igualHora [(x1,y1,z1,w1)] = False
igualHora ((x1,y1,z1,w1):(x2,y2,z2,w2):xs) = if x1 == x2 then True
                                                else igualHora ((x1,y1,z1,w1):xs)

--b) Escrever uma função que calcula a maior infração registada (diferença entre
--a velocidade do conduto e a autorizada)
maiorVel :: Radar -> Radar
maiorVel [x] = [x]
maiorVel ((x1,y1,z1,w1):(x2,y2,z2,w2):xs) 
     |(w1-(fromIntegral z1)) > (w2-(fromIntegral z2)) = maiorVel ((x1,y1,z1,w1):xs)
     |(w1-(fromIntegral z1)) == (w2-(fromIntegral z2)) = ((x1,y1,z1,w1):(x2,y2,z2,w2):maiorVel xs)
     |otherwise = maiorVel ((x2,y2,z2,w2):xs)

--c)Escrever a função que calcula o menor período de tempo (em minutos) sem infrações
menorPeriodo :: Radar -> Int
menorPeriodo [(x,y,z,w)] = hora2mins x
menorPeriodo ((x1,y1,z1,w1):(x2,y2,z2,w2):xs)
     | (hora2mins x1) >= (hora2mins x2) = menorPeriodo ((x2,y2,z2,w2):xs)
     | otherwise =  menorPeriodo ((x1,y1,z1,w1):xs)

---------------------------Exercício 13------------------------------------------------------------
--a) Qual o valor de f "exif"
--f "exif" = g [] "exif"
--g ('e':[]) "xif" = g 'x':"e" "fi"
--g 'x':"e" "fi" = g 'i':"xe" "f"
--g 'i':"xe" "f" = g 'f':"ixe" ""
--g 'f':"ixe" "" = "fixe"

--b) Definir as funções addM que adicona um polinomio a um monómio e addP que adiciona
--dois polinomios
addM :: Polinomio -> Monomio -> Polinomio
addM [] (z,w) = [(z,w)]
addM ((x,y):xs) (z,w) | w == y && x == -z = xs
                      | w == y = ((x+z),y):xs
                      | otherwise = (x,y):addM xs (z,w)

addP :: Polinomio -> Polinomio -> Polinomio
addP [] y = y
addP ((x1,y1):xs) ys = addP xs (addM ys (x1,y1))

-------------------------Exercício 14---------------------------------------------------------------
--a) Definir uma função elem  que testa se um determinado elemento pertence a um
--multi-conjunto
elemento' :: Eq a => a -> MSet a -> Bool
elemento' _ [] = False
elemento' n ((x,y):xs) | n == x = True
                       | otherwise = elemento' n xs

--b) Definir a função converte que converte uma lista para um multi-conjunto
converter :: Eq a => [a] -> MSet a
converter [x] = [(x,1)]
converter (x:xs) = insere x (converter xs)