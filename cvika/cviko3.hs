{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

zoznamDelitelov x = [y | y <- [1 .. x], mod x y == 0]

zoznamPrvocisel x = [y | y <- [1 .. x], length (zoznamDelitelov y) == 2]

zoznamPrvociselnychDelitelov x = [y | y <- zoznamDelitelov x, x <- zoznamPrvocisel x, x == y]

-- >>> zoznamPrvociselnychDelitelov 222
-- [2,3,37]

prienikZoznamov z1 z2 = [x | x <- z1, y <- z2, (x == y)]

-- >>> prienikZoznamov [1,2,3,4] [2,4,5,6]
-- [2,4]

nsd a b
  | a == b = a
  | a > b = nsd (a - b) b
  | a < b = nsd a (b - a)

-- >>> nsd 15 5
-- 5

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (p : t) =
  qsort [x | x <- t, x < p] ++ p : qsort [x | x <- t, x > p]

list = [5, 1, 2, 213, 26, 2, 43]

-- >>> qsort list
-- [1,2,5,26,43,213]

sumadm a b
  | a > b = 0
  | otherwise = (a * a) + sumadm (a + 1) b

-- >>> sumadm 1 3
-- 14

suma f a b
  | a > b = 0
  | otherwise = f a + suma f (a + 1) b

druhaMocnina x = x * x

-- >>> suma druhaMocnina 1 3
-- 14

data Coord = Coord Double Double
  deriving (Show)

zero :: Coord
zero = Coord 0 0

addC :: Coord -> Coord -> Coord
addC (Coord x y) (Coord x' y') = Coord (x + x') (y + y')

-- >>> addC (Coord 3 7) (Coord 1 2)

scalarMulC :: Coord -> Coord -> Double
scalarMulC (Coord x y) (Coord x' y') = x * x' + y * y'

sizeC :: Coord -> Coord -> Double
sizeC x y = sqrt (scalarMulC x x) * sqrt (scalarMulC y y)

angleC :: Coord -> Coord -> Double
angleC x y = acos (scalarMulC x y / sizeC x y)