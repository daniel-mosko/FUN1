module Main where

import Data.List (union, (\\))

vzdialenost :: (Fractional a) => [Int] -> [Int] -> a
vzdialenost v1 v2 =
  let intersectionSum = fromIntegral (sum (prienik v1 v2))
      unionSum = fromIntegral (sum (zjednot v1 v2))
   in 1.0 - (intersectionSum / unionSum)

-- >>> vzdialenost [1,0,1] [1,1,0]
-- 0.6666666666666667
prienik :: (Num a) => [a] -> [a] -> [a]
prienik v1 v2 = [signum (x * y) | (x, y) <- zip v1 v2]

-- >>> prienik [1,0,1] [1,0,0]
-- [1,0,0]

zjednot :: [Int] -> [Int] -> [Int]
zjednot v1 v2 = [signum (x + y) | (x, y) <- zip v1 v2]

-- >>> zjednot [1,0,1] [1,1,0]
-- [1,1,1]

-- >>> komb matica
-- [([1,0,0,1,1],[1,1,1,0,0]),([1,0,0,1,1],[1,0,1,1,1]),([0,1,1,0,1],[1,1,1,0,0]),([0,1,1,0,1],[1,0,1,1,1]),([0,0,1,1,0],[0,1,0,1,0]),([0,0,1,1,0],[1,0,1,1,1]),([0,1,0,1,0],[1,0,1,1,1])]

komb :: (Ord b) => [b] -> [(b, b)]
komb [] = []
komb [x] = []
komb (x : xs) = [(x, y) | y <- xs, y > x] ++ komb xs

najmensiaVzdialenost :: (Ord a, Fractional a) => [[Int]] -> a
najmensiaVzdialenost zVekt = minimum [vzdialenost v1 v2 | (v1, v2) <- komb zVekt]

-- https://ics.science.upjs.sk/wp-content/uploads/2020/11/uinf_fca.pdf
-- work = D
-- out = C
-- najblizsie E = {(X1,X2) | p(X1,X2) = m}
-- V = XD

riceSiff :: [[Int]] -> [[Int]] -> [[Int]]
riceSiff work out
  | length work <= 1 = out
  | otherwise = riceSiff (uprav work najblizsie) (prihod out najblizsie)
  where
    najblizsie = [(v1, v2) | (v1, v2) <- komb work, vzdialenost v1 v2 == najmensiaVzdialenost work]

-- vSet [[Int]]
-- work [[Int]]
-- nSet [[Int]]
-- najblizsie [([Int],[Int])]

uprav :: [[Int]] -> [([Int], [Int])] -> [[Int]]
uprav work najblizsie =
  let vSet = [x | x <- work, y <- work, (x, y) `elem` najblizsie]
      nSet = [zjednot x1 x2 | (x1, x2) <- najblizsie]
   in (work \\ vSet) `union` nSet

prihod :: [[Int]] -> [([Int], [Int])] -> [[Int]]
prihod out najblizsie =
  out `union` [zjednot x1 x2 | (x1, x2) <- najblizsie]

-- vrati podmaticu pre vektor (Ukazal Matej na cviceni)
satisfying :: [Int] -> [[Int]] -> [[Int]]
satisfying attrs = filter (all (uncurry (<=)) . zip attrs)

-- podcet vyskytov vektora vo vsetkych podmaticiach
pocetVyskytov :: (Eq a) => a -> [a] -> Int
pocetVyskytov x = length . filter (== x)

main :: IO ()
main = do
  let matrix = [[1, 1, 0, 1, 1], [1, 1, 1, 1, 0], [1, 0, 0, 1, 1], [1, 1, 1, 0, 1], [0, 1, 0, 1, 1]]
  let work = matrix
  let out = matrix

  let riceResult = riceSiff work out
  print riceResult

  let satisfyingResults = map (`satisfying` matrix) riceResult
  let pocetVyskytovPreVektor = map (\vec -> pocetVyskytov vec (concat satisfyingResults)) matrix

  print pocetVyskytovPreVektor
  