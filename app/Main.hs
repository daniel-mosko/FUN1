{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Monad.Reader
import Data.Aeson
import Data.IORef
import Data.List (union, (\\))
import Data.Map qualified as Map
import GHC.Generics
import Web.Scotty.Trans

vzdialenost :: [Double] -> [Double] -> Double
vzdialenost v1 v2 = 1.0 - (sum (prienik v1 v2) / sum (zjednot v1 v2))

-- >>> vzdialenost [1,0,1] [1,1,0]
-- 0.6666666666666667

prienik :: [Double] -> [Double] -> [Double]
prienik v1 v2 = [signum (x * y) | (x, y) <- zip v1 v2]

-- >>> prienik [1,0,1] [1,0,0]
-- [1,0,0]

zjednot :: [Double] -> [Double] -> [Double]
zjednot v1 v2 = [signum (x + y) | (x, y) <- zip v1 v2]

-- >>> zjednot [1,0,1] [1,1,0]
-- [1,1,1]

-- >>> komb matica
-- [([1,0,0,1,1],[1,1,1,0,0]),([1,0,0,1,1],[1,0,1,1,1]),([0,1,1,0,1],[1,1,1,0,0]),([0,1,1,0,1],[1,0,1,1,1]),([0,0,1,1,0],[0,1,0,1,0]),([0,0,1,1,0],[1,0,1,1,1]),([0,1,0,1,0],[1,0,1,1,1])]

komb :: (Ord b) => [b] -> [(b, b)]
komb [] = []
komb [x] = []
komb (x : xs) = [(x, y) | y <- xs, y > x] ++ komb xs

najmensiaVzdialenost :: [[Double]] -> Double
najmensiaVzdialenost zVekt = minimum [vzdialenost v1 v2 | (v1, v2) <- komb zVekt]

-- https://ics.science.upjs.sk/wp-content/uploads/2020/11/uinf_fca.pdf
-- work = D
-- out = C
-- najblizsie E = {(X1,X2) | p(X1,X2) = m}
-- V = XD

riceSiff :: [[Double]] -> [[Double]] -> [[Double]]
riceSiff work out
  | length work <= 1 = out
  | otherwise = riceSiff (uprav work najblizsie) (prihod out najblizsie)
  where
    najblizsie = [(v1, v2) | (v1, v2) <- komb work, vzdialenost v1 v2 == najmensiaVzdialenost work]

-- vSet [[Double]]
-- work [[Double]]
-- nSet [[Double]]
-- najblizsie [([Double],[Double])]

uprav :: [[Double]] -> [([Double], [Double])] -> [[Double]]
uprav work najblizsie =
  let vSet = [x | x <- work, y <- work, (x, y) `elem` najblizsie]
      nSet = [zjednot x1 x2 | (x1, x2) <- najblizsie]
   in (work \\ vSet) `union` nSet

prihod :: [[Double]] -> [([Double], [Double])] -> [[Double]]
prihod out najblizsie =
  out `union` [zjednot x1 x2 | (x1, x2) <- najblizsie]

-- vrati podmaticu pre vektor (Ukazal Matej na cviceni)
satisfying :: [Double] -> [[Double]] -> [[Double]]
satisfying attrs = filter (all (uncurry (<=)) . zip attrs)

-- podcet vyskytov vektora vo vsetkych podmaticiach
pocetVyskytov :: (Eq a) => a -> [a] -> Int
pocetVyskytov x = length . filter (== x)

data Matrix = Matrix
  { id_ :: Maybe Int,
    data_ :: [[Double]]
  }
  deriving (Generic, ToJSON, FromJSON)

type MatrixId = Int

type MatrixMap = Map.Map MatrixId Matrix

getNextMatrixId :: IORef MatrixId -> IO MatrixId
getNextMatrixId ref = atomicModifyIORef' ref (\x -> (x + 1, x + 1))

transposeMatrix :: [[a]] -> [[a]]
transposeMatrix [] = []
transposeMatrix ([] : _) = []
transposeMatrix rows = (map head rows) : transposeMatrix (map tail rows)

handleRiceSiff :: ActionT App ()
handleRiceSiff = do
  matrixId <- queryParam "matrixId"
  ref <- lift $ asks matrixRef
  matrices <- liftIO $ readIORef ref
  case Map.lookup matrixId matrices of
    Just matrix ->
      json $ Matrix {id_ = Just matrixId, data_ = riceSiff (data_ matrix) (data_ matrix)}
    Nothing ->
      json $ object ["error" .= ("Matrix not found" :: String)]


handleTranspose :: ActionT App ()
handleTranspose = do
  matrixId <- queryParam "matrixId"
  ref <- lift $ asks matrixRef
  matrices <- liftIO $ readIORef ref
  case Map.lookup matrixId matrices of
    Just matrix ->
      json $ Matrix {id_ = Just matrixId, data_ = transposeMatrix (data_ matrix)}
    Nothing ->
      json $ object ["error" .= ("Matrix not found" :: String)]

handleInsertMatrix :: ActionT App ()
handleInsertMatrix = do
  reqMatrix <- jsonData :: ActionT App Matrix
  matrixId <- lift $ asks matrixIdCounter
  newMatrixId <- liftIO $ getNextMatrixId matrixId
  ref <- lift $ asks matrixRef
  liftIO $ modifyIORef ref (Map.insert newMatrixId reqMatrix)
  json $ object ["id_" .= newMatrixId]

data AppConfig = AppConfig
  { matrixRef :: IORef MatrixMap,
    matrixIdCounter :: IORef MatrixId
  }

type App = ReaderT AppConfig IO

main :: IO ()
main = do
  matrixIdCounter <- newIORef 0
  matrixRef <- newIORef Map.empty
  let config = AppConfig {matrixRef = matrixRef, matrixIdCounter = matrixIdCounter}
  scottyT 8080 (`runReaderT` config) do
    post "/insert" do
      handleInsertMatrix
    post "/transpose" $ do
      handleTranspose

-- {
-- 	"data_": [
-- 						 [1, 0, 1, 0, 0, 1],
-- 						 [0, 1, 1, 0, 1, 0],
-- 						 [1, 0, 0, 1, 1, 1],
-- 						 [0, 1, 0, 1, 0, 0]
-- 					 ]
-- }