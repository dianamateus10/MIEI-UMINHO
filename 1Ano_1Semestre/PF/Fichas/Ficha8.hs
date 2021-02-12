module Ficha8 where

data Frac = F Integer Integer

--1.a)
normaliza :: Frac -> Frac 
normaliza (F i j) = (F (signum b) * a) (abs b))
                where
                	a = div j (mdc (abs i) (abs j))
                	b = div j (mdc (abs i) (abs j))

mdc :: Integer -> Integer -> Integer
mdc x y |x == 0 = y
        |y == 0 = x
        |x > y  = mdc (x-y) y
        | otherwise = mdc x (y-x)

--1.b)
instance Eq Frac where
	(F n1 d1) == (F n2 d2) = n1 * d2 = n2 * n1

--1.c)
instance Ord Frac where
	f1 <= f2 = n1 * d2 <= n2 * n1
	          where
	          	(F n1 d1) = normaliza f1
	          	(F n2 d2) = normaliza f2

--1.d)
instance Show Frac where
	show (F a b) = "(" ++ show a ++ "/" ++ show b ++ ")"

--1.e)
instance Num Frac where
	(F n1 d1) + (F n2 d2) = normaliza (F ((n1*d2)+(n2*d1)) (d1*d2))
	(F n1 d1) - (F n2 d2) = normaliza (F ((n1*d2)-(n2*d1)) (d1*d2))
	(F n1 d1) * (F n2 d2) = normaliza (F ((n1*n2) (d1*d2))
	abs (F n d) = (F (abs n) (abs d))
	signum (F n d) = (F (signum (n*d) (1))
	fromInteger x = F x 1


data Exp a = Const a
            | Simetrico (Exp a)
            | Mais (Exp a) (Exp a)
            | Menos (Exp a) (Exp a)
            | Mult (Exp a) (Exp a)

--2.a)
instance Show a => Show (Exp a) where
	show (Const a) = show a
	show (Simetrico e) = "-" ++ "(" ++ show e ++ ")"
	show (Mais a b) = show a ++ "+" ++ show b
	show (Menos a b) = show a ++ "-" ++ show b
	show (Mult a b) = show a ++ "*" ++ show b

--2.b)
instance (Num a,Eq a) => Eq (Exp a) where
exp_to_val :: (Num a) => Eq a -> a
exp_to_val (Const a) = a
exp_to_val (Simetrico a) = - (exp_to_val a)
exp_to_val (Mais a b) = exp_to_val a + exp_to_val b
exp_to_val (Menos a b) = exp_to_val a - exp_to_val b
exp_to_val (Mult a b) = exp_to_val a * exp_to_val b

--2.c)
instance (Num a,Eq a,Ord a) => Num (Exp a) where
x + y = Const (exp_to_val x + exp_to_val y)
x - y = Const (exp_to_val x - exp_to_val y)
x * y = Const (exp_to_val x * exp_to_val y)

negate (Const a) = Const (-a)
negate (Simetrico a) = a
negate (Mais a b) = Mais (-a) (-b)
negate (Menos a b) = Menos b (a)
negate (Mult a b) = Mult (-a) b

fromInteger x = Const (fromInteger x)

abs (Const a) = Const (abs a)
abs (Simetrico a) = abs a
abs (Mais a b) = abs (a + b)
abs (Menos a b) = abs (a - b)
abs (Mult a b) = abs (a * b)

signum (Const a) = Const (if abs a == a then if a == 0 then 0 else 1 else (-1))
signum (Simetrico a) = - signum a
signum (Mais a b) = Const (if abs (a+b) == a+b then if a+b == 0 then 0 else 1 else (-1))
signum (Menos a b) = Const (if abs (a-b) == a-b then if a-b == 0 then 0 else 1 else (-1))
signum (Mult a b) = Const (if abs (a*b) == a*b then if a*b == 0 then 0 else 1 else (-1))

{-  
  signum x     = Const (retornaSinal x)

retornaSinal :: (Num a, Ord a, Eq a) => Exp a -> a
retornaSinal (Const a) |exp_to_val a > 0 = 1
                       |exp_to_val a < 0 = -1
                       |otherwise = 0

retornaSinal (Simetrico a) |exp_to_val a > 0 = -1
                           |exp_to_val a < 0 = 1
                           |otherwise = 0

retornaSinal (Mais a b) |(retornaSinal (exp_to_val a)) + (retornaSinal (exp_to_val b)) > 0 = 1
                        |(retornaSinal (exp_to_val a)) + (retornaSinal (exp_to_val b)) < 0 = -1
                        |otherwise = 0

retornaSinal (Menos a b) |(retornaSinal (exp_to_val a)) - (retornaSinal (exp_to_val b)) > 0 = 1
                         |(retornaSinal (exp_to_val a)) - (retornaSinal (exp_to_val b)) < 0 = -1
                         |otherwise = 0

retornaSinal (Mult a b)  |(retornaSinal (exp_to_val a)) * (retornaSinal (exp_to_val b)) > 0 = 1
                         |(retornaSinal (exp_to_val a)) * (retornaSinal (exp_to_val b)) < 0 = -1
                         | otherwise = 0
-}



data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data,String,Movimento)]

--3.a)
instance Ord Data where
	compare (D n1 m1 a1) (D n2 m2 a2) | a1 > a2 || a1 == a2 && (m1>m2 || m1==m2 && n1>n2) = GT
	                                  | a1 == a2 && m1==m2 && n1==n2 = Eq
	                                  | otherwise = LT

--3.b)
--falta fazer

--3.c)
--falta fazer

--3.d)
--falta fazer