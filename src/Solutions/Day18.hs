{-# LANGUAGE TupleSections #-}

module Solutions.Day18 (solution, test) where

import Text.Megaparsec.Char (char, eol)

import Control.Monad (guard)
import Data.Array (assocs)
import Data.Maybe (catMaybes)
import Lib.Array2d (Array2d, replaceArr2D, safe2dLookup, toArray2d)
import Lib.Dijkstra (shortestPath)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many)
import Text.Megaparsec.Char.Lexer (decimal)

solution :: Solution Input Int Int
solution = Solution 18 parser part1 part2

part1 :: Input -> IO Int
part1 input = do
  return $ shortestPath (getNeighbours arr) vertexes (0, 0) (70, 70)
 where
  boardSize = 71
  iterations = 1024
  byteLocations = take iterations input
  arr' = toArray2d $ replicate boardSize $ replicate boardSize Open
  arr = foldr (\(x, y) -> replaceArr2D y x Wall) arr' byteLocations
  -- getNeighbours' x = (\(_, a) -> (1, (Open, a))) <$> getNeighbours (const . not . isWall) arr x
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

data Tile = Open | Wall deriving (Ord, Eq)
instance Show Tile where
  show Open = "."
  show Wall = "#"

isWall :: Tile -> Bool
isWall Wall = True
isWall _ = False

part2 :: Input -> IO Int
part2 input = return 0

type Input = [(Int, Int)]
parser :: Parser Input
parser = many p
 where
  p = do
    x <- decimal <* char ','
    y <- decimal <* eol
    return (x, y)

test :: IO (Int, Int)
test = testSolution solution
