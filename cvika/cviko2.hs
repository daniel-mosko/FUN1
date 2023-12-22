{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

odhadOdmocniny1 x odhad pocet =
  if pocet == 0
    then odhad
    else odhadOdmocniny1 x (((x / odhad) + odhad) / 2) (pocet - 1)

odhadOdmocniny2 x odhad 0 = odhad
odhadOdmocniny2 x odhad pocet = odhadOdmocniny2 x (priemer (x / odhad) odhad) (pocet - 1)

priemer x y = (x / y) / 2

odhadOdmocniny3 :: (Eq t1, Fractional t2, Num t1) => t2 -> t2 -> t1 -> t2
odhadOdmocniny3 x odhad pocet
  | pocet == 0 = odhad
  | otherwise = odhadOdmocniny3 x (priemer (x / odhad) odhad) (pocet - 1)
  where
    priemer x y = (x + y) / 2
    podiel x y = x / y

-- >>> odhadOdmocniny1 9 1 10
-- 3.0

odmocnina x = odhadOdmocniny1 x 1 16

quad1 a b c = (-b + odmocnina d) / 2 * a
  where
    d = b * b - 4 * a * c
quad1 a b c = (-b - odmocnina d) / 2 * a
  where
    d = b * b - 4 * a * c

-- >>> quad1 1 2 3

-- LISTY

{-# options -Wall #-}

data List = Cons Int List | Nil
  deriving Show

l = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

-- >>> l
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

hlava :: List -> Int
hlava (Cons a _) = a
hlava Nil = error "prazdny list"

dlzka :: List -> Int
dlzka (Cons _ xs) = dlzka xs + 1
dlzka Nil = 0

posledny :: List -> Int
posledny (Cons a Nil) = a
posledny (Cons _ as ) = posledny as
posledny Nil = error "prazdny list"

_ = ()

-- >>> posledny l
-- 4

-- >>> [1..10]
-- [1,2,3,4,5,6,7,8,9,10]

odDo :: Int -> Int -> List
odDo a b | a > b = Nil
odDo a b = Cons a (odDo (a + 1) b)

-- >>> odDo 1 5
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))

odDo' :: Int -> Int -> [Int]
odDo' a b | a > b = []
odDo' a b = a : odDo' (a + 1) b

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Double Double
  | Pi
  deriving Show

eval :: Expr -> Double
eval Pi = pi
eval (Double a) = a
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a / eval b

_ = ()

-- >>> eval (Add (Double 1) (Double 2))
-- 3.0

-- >>> eval (Mul (Add Pi (Double 1)) (Div (Double 10) (Double 2)))
-- 20.707963267948966

-- (++) :: String -> String
-- "string" :: String
-- show :: Double -> String
pretty :: Expr -> String
pretty Pi = show pi
pretty (Double n) = show n
pretty (Add a b) = "(" ++ pretty a ++ " + " ++ pretty b ++ ")"
pretty (Sub a b) = "(" ++ pretty a ++ " - " ++ pretty b ++ ")"
pretty (Mul a b) = "(" ++ pretty a ++ " * " ++ pretty b ++ ")"
pretty (Div a b) = "(" ++ pretty a ++ " / " ++ pretty b ++ ")"

_ = ()

-- >>> pretty (Add (Double 1) (Div (Double 2) Pi))
-- "(1.0 + (2.0 / 3.141592653589793))"