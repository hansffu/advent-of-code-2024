{-# LANGUAGE TupleSections #-}

module Solutions.Day18 (solution, test) where

import Text.Megaparsec.Char (char, eol)

import Control.Monad (guard)
import Data.Array (assocs)
import Data.Maybe (catMaybes)
import Lib.Array2d (Array2d, replaceArr2D, safe2dLookup, toArray2d)
import Lib.Dfs (dfs)
import Lib.Dijkstra (shortestPath)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many)
import Text.Megaparsec.Char.Lexer (decimal)

solution :: Solution Input Int (Int, Int)
solution = Solution 18 parser part1 part2

boardSize :: Int
boardSize = 71
end :: (Int, Int)
end = (boardSize - 1, boardSize - 1)

part1 :: Input -> IO Int
part1 input = do
  return $ shortestPath (getNeighbours arr) vertexes (0, 0) end
 where
  iterations = 1024
  byteLocations = take iterations input
  arr' = toArray2d $ replicate boardSize $ replicate boardSize Open
  arr = foldr (\(x, y) -> replaceArr2D y x Wall) arr' byteLocations
  vertexes = do
    (y, a) <- assocs arr
    (x, b) <- assocs a
    guard $ not $ isWall b
    return (x, y)

getNeighbours :: Array2d Tile -> (Int, Int) -> [(Int, (Int, Int))]
getNeighbours arr (x, y) = (1,) . snd <$> b
 where
  a = catMaybes [safe2dLookup arr (x + 1, y), safe2dLookup arr (x - 1, y), safe2dLookup arr (x, y + 1), safe2dLookup arr (x, y - 1)]
  b = filter (\(t, _) -> not $ isWall t) a

getNeighbours2 :: Array2d Tile -> (Int, Int) -> [(Int, Int)]
getNeighbours2 arr (x, y) = snd <$> b
 where
  a = catMaybes [safe2dLookup arr (x + 1, y), safe2dLookup arr (x - 1, y), safe2dLookup arr (x, y + 1), safe2dLookup arr (x, y - 1)]
  b = filter (\(t, _) -> not $ isWall t) a

data Tile = Open | Wall deriving (Ord, Eq)
instance Show Tile where
  show Open = "."
  show Wall = "#"

isWall :: Tile -> Bool
isWall Wall = True
isWall _ = False

part2 :: Input -> IO (Int, Int)
part2 input = do
  return $ go arr input
 where
  arr = toArray2d $ replicate boardSize $ replicate boardSize Open

go :: Array2d Tile -> [(Int, Int)] -> (Int, Int)
go _ [] = error "out of bytes"
go prev (b@(x, y) : bs) = if end `elem` a then go arr bs else b
 where
  arr = replaceArr2D y x Wall prev
  a = dfs (getNeighbours2 arr) (0, 0)

type Input = [(Int, Int)]
parser :: Parser Input
parser = many p
 where
  p = do
    x <- decimal <* char ','
    y <- decimal <* eol
    return (x, y)

test :: IO (Int, (Int, Int))
test = testSolution solution
