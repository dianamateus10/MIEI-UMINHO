module Ficha3 where

import Ficha1

type Etapa = (Hora, Hora)
type Viagem = [Etapa]
type Poligonal = [Ponto]

data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
type Nome = String
type Agenda = [(Nome,[Contacto])]

type Dia = Int
type Mes = Int
type Ano = Int
data Data = D Dia Mes Ano deriving Show
type TabDN = [(Nome,Data)]

data Movimento = Credito Float | Debito Float deriving Show
data Extrato = Ext Float [(Data,String,Movimento)] deriving Show


--1.a)
etapaConst :: Etapa -> Bool
etapaConst (x,xs) = if horaMaior2 x xs == True && horaVal2 x == True && horaVal2 xs == True then True
                        else False

--1.b) FDN
viagemConst :: Viagem -> Bool
viagemConst [] = True
viagemConst [x] = True
viagemConst (((H a b),(H c d)):((H e f),(H g h)):t) = if (testaHoras_comparacao1 (H a b) (H c d) == False) && (testaHoras_comparacao1 (H c d) (H e f) == False)
                                                          then viagemConst (((H e f),(H g h)):t)
                                                             else False

testaHoras_comparacao1 :: Hora -> Hora -> Bool
testaHoras_comparacao1 (H a b) (H c d) | (a > c) && horaVal2 (H a b) && horaVal2 (H c d) = True
                                       | (a == c) && horaVal2 (H a b) && horaVal2 (H c d) && (b > d) = True
                                       | otherwise = False

--1.c)
horaViagem :: Viagem -> (Hora,Hora)
horaViagem [h] = (fst h, snd h)
horaViagem h = (fst (head h), snd (last h))

--1.d)
tempototal :: Viagem -> Int
tempototal x = diffHour2 (fst (horaViagem x)) (snd (horaViagem x))

--1.e)
tempoEspera :: Viagem -> Int
tempoEspera [h] = 0
tempoEspera h = diffHour2 (snd (head h)) (fst (head (tail h))) + tempoEspera (tail h)

--1.f)
tempoTotalViagem :: Viagem -> Int
tempoTotalViagem h = diffHour2 (fst (head h)) (snd (last h))

--2.a)
compLinha :: Poligonal -> Double
compLinha (x:xs:t) = distance (x) (xs) + compLinha t

--2.b)
linhaPoligonal :: Poligonal -> Bool
linhaPoligonal [p1,p2] = False
linhaPoligonal [p1, p2, p3] = p1 == p3
linhaPOligonal (p1:p2:p3:t) = linhaPoligonal t

--2.c)
triangula :: Poligonal -> [Figura]
triangula [p1,p2,p3] = [(Triangulo p1 p2 p3)]
triangula (p1:p2:p3:t) = (Triangulo p1 p2 p3):triangula (p1:p3:t)

--2.d)
areaPoligono :: Poligonal -> Double
areaPoligono p = areaAux (triangula p)

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
                          let a = distance p1 p2
                              b = distance p2 p3
                              c = distance p3 p1
                              s = (a+b+c)/2
                          in sqrt (s*(s-a)*(s-b)*(s-c))
area (Circulo _ r) = pi * (r^2)
area (Retangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2-posy p1)

areaAux :: [Figura] -> Double
areaAux [] = 0.0
areaAux [x] = area x
areaAux (h:x:t) = area h + areaAux (x:t)

--2.e)
mover :: Poligonal -> Ponto -> Poligonal
mover l p = p:l

--2.f)
zoom :: Double -> Poligonal -> Poligonal
zoom _ [] = []
zoom x ((Cartesiano a b):t) = (Cartesiano (a*x) (b*x)) : (zoom x t)
zoom x ((Polar a b):t) = (Polar (a*x) b) : (zoom x t)

--3.a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail q p [] = [(q,[Email p])]
acrescEmail q p ((x,xs):t) = if q == x then ((q,[Email p]++xs): t)
                                 else ((q,[Email p]):(x,xs):t)

--3.b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails q [] = Nothing
verEmails q ((x,xs):t) = if q == x then Just (emails xs)
                             else verEmails q t

emails :: [Contacto] -> [String]
emails [] = []
emails ((Email e):t) = e: emails t
emails (_:t) = emails t

--3.c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (x:xs) = (contactoCasa (x:xs)) ++ (contactoTlm (x:xs)) ++ (contactoTrab (x:xs))

contactoCasa :: [Contacto] -> [Integer]
contactoCasa [] = []
contactoCasa ((Casa e):t) = e:contactoCasa t
contactoCasa (_:t) = contactoCasa t

contactoTlm :: [Contacto] -> [Integer]
contactoTlm [] = []
contactoTlm ((Tlm e):t) = e:contactoTlm t
contactoTlm (_:t) = contactoTlm t

contactoTrab :: [Contacto] -> [Integer]
contactoTrab [] = []
contactoTrab ((Trab e):t) = e:contactoTrab t
contactoTrab (_:t) = contactoTrab t

--3.d)
casa :: Nome -> Agenda -> Maybe Integer
casa q [] = Nothing
casa q ((x,xs):t) = if q == x then contCasa xs
                         else casa q t

contCasa :: [Contacto] -> Maybe Integer
contCasa [] = Nothing
contCasa ((Casa e):t) = Just e
contCasa (_:t) = contCasa t

--4.a)
procura :: Nome -> TabDN -> Maybe Data
procura q [] = Nothing
procura q ((x,xs):t) = if q == x then Just xs
                          else procura q t

--4.b)
idade :: Data -> Nome -> TabDN -> Maybe Int
idade (D x1 y1 z1) q [] = Nothing
idade (D x1 y1 z1) q ((x,xs):t) = if q == x then contaAnos (D x1 y1 z1) xs
                                       else idade (D x1 y1 z1) q t

contaAnos :: Data -> Data -> Maybe Int
contaAnos (D x1 y1 z1) (D x2 y2 z2) | z1 /= z2 && y2 > y1 = Just (z1-z2-1)
                                    | z1 /= z2 && y2 == y1 && x1>x2 = Just (z1-z2-1)
                                    | z1 /= z2 && y2 == y1 && x1<x2 = Just (z1-z2-z1-1)
                                    | z1 == z2 = Just 0
                                    | otherwise = Just (z1-z2)

--4.c)
anterior :: Data -> Data -> Bool
anterior (D x1 y1 z1) (D x2 y2 z2) | z1<z2 = True
                                   | z1 == z2 && y1<y2 = True
                                   | z1 == z1 && y1 == y2 && x1<x2 = True
                                   | otherwise = False

--4.d)
ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):t) = insert1 (n,d) (ordena t)

insert1 :: (Nome, Data) -> TabDN -> TabDN
insert1 (n,d) [] = [(n,d)]
insert1 (n,d) ((nh,dh):t) | anterior dh d = (nh,dh):insert1 (n,d) t
                          | otherwise = (n,d):(nh,dh):t

--4.e)
porIdade :: Data -> TabDN -> [(Nome, Int)]
porIdade (D x1 y1 z1) [(x,xs)] = [(x, idade1 (D x1 y1 z1) x [(x,xs)])]
porIdade (D x1 y1 z1) ((x,xs):t) = porIdade (D x1 y1 z1) [(x,xs)] ++ porIdade (D x1 y1 z1) t

idade1 :: Data -> Nome -> TabDN -> Int
idade1 _ _ [] = 0
idade1 (D x1 y1 z1) q [(n,(D x2 y2 z2))] | z1 == z2 && y1 == y2 = (calculo_dias)
                                         | z1 == z2 && x1 == x2 = (calculo_meses)
                                         | z1 == z2 = (calculo_dias + 30*(calculo_meses))
                                         | y1 == y2 && x1==x2 = (calculo_anos)
                                         | otherwise = (calculo_dias +30*(calculo_meses)+30*12*(calculo_anos))
                                        where
                                        calculo_dias = x1-x2
                                        calculo_meses = y1-y2
                                        calculo_anos = z1-z2

--5.a)
extValor :: Extrato -> Float -> [Movimento]
extValor (Ext x [(d,s,m)]) z = if z < valorMov (m) then [m]
                                     else []
extValor (Ext x ((d,s,m):t)) z = if z < valorMov (m) then [m]++ (extValor (Ext x t) z)
                                      else extValor (Ext x t) z

valorMov :: Movimento -> Float
valorMov (Credito x) = x
valorMov (Debito x) = x

--5.b)
filtro :: Extrato -> [String] -> [(Data,Movimento)]
filtro (Ext x [(d,s,m)]) [] = []
filtro (Ext x [(d,s,m)]) [h] = if h == s then [(d,m)]
                                  else []
filtro (Ext x [(d,s,m)]) (h:t) = if h == s then [(d,m)] ++ (filtro (Ext x [(d,s,m)]) t)
                                     else filtro (Ext x [(d,s,m)]) t
filtro (Ext x ((d,s,m):z)) (h:t) = if h == s then [(d,m)] ++ (filtro (Ext x [(d,s,m)]) t)++ (filtro (Ext x z) (h:t))
                                       else (filtro (Ext x [(d,s,m)]) t)++ (filtro (Ext x z) (h:t))

--5.c)
creDeb :: Extrato -> (Float,Float)
creDeb (Ext x [(d,s,m)]) = if verCred m then (valorMov m,0)
                                else (0, valorMov m)
creDeb (Ext x ((d,s,m):t)) = soma_pares (calcula_1elemento) (calcula_resto)
                              where 
                              	calcula_1elemento = creDeb (Ext x [(d,s,m)])
                              	calcula_resto = creDeb (Ext x t)

soma_pares :: (Float,Float) -> (Float,Float) -> (Float,Float)
soma_pares (a,b) (c,d) = (a+c,b+d)

verCred :: Movimento -> Bool
verCred (Credito _) = True
verCred _ = False

--5.d)
saldo :: Extrato -> Float
saldo (Ext x [(d,s,m)]) = if verCred m == True then x+(valorMov m)
                              else x-(valorMov m)
saldo (Ext x ((d,s,m):t)) = saldo (Ext x [(d,s,m)]) + saldo (Ext x t)


