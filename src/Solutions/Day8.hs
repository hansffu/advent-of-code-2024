module Solutions.Day8 (solution, test) where

import Text.Megaparsec.Char (char, eol, printChar)

import Data.Function (on)
import Data.List (groupBy, sort)
import Data.List.Utils (join)
import qualified Data.Set as S
import Lib.Parser (Parser)
import Lib.Solution
import Lib.Utils (debug)
import Text.Megaparsec (MonadParsec (try), many, sepBy, some, (<|>))
import Prelude hiding (getLine)

solution :: Solution Input Int Int
solution = Solution 8 parser part1 part2

part1 :: Input -> IO Int
part1 input = do
  prettyPrint input
  return $ length $ S.fromList $ filter isInBounds allAntinodes
 where
  antennas = sort $ findIndexes2d isAntenna input
  antennasByType = (snd <$>) <$> groupBy ((==) `on` fst) antennas
  pairs = antennasByType >>= getPairs
  isInBounds (y, x) = x >= 0 && y >= 0 && x < length (head input) && y < length input
  allAntinodes = pairs >>= getAntinodes

part2 :: Input -> IO Int
part2 input = return $ length $ S.fromList $ filter isInBounds allAntinodes
 where
  antennas = sort $ findIndexes2d isAntenna input
  antennasByType = (snd <$>) <$> groupBy ((==) `on` fst) antennas
  pairs = antennasByType >>= getPairs
  allAntinodes = pairs >>= getLine isInBounds
  isInBounds (y, x) = x >= 0 && y >= 0 && x < length (head input) && y < length input

-- allAntinodes = pairs >>= getAntinodes

data Tile = Empty | Antenna Char deriving (Ord, Eq)
instance Show Tile where
  show Empty = "."
  show (Antenna c) = [c]

isAntenna :: Tile -> Bool
isAntenna (Antenna _) = True
isAntenna _ = False

getLine :: ((Int, Int) -> Bool) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
getLine isinBounds ((y1, x1), (y2, x2)) = takeWhile isinBounds (iterate next (y1, x1)) ++ takeWhile isinBounds (iterate prev (y1, x1))
 where
  next (y, x) = (y + dy, x + dx)
  prev (y, x) = (y - dy, x - dx)
  (dy, dx) = (y2 - y1, x2 - x1)

getPairs :: [a] -> [(a, a)]
getPairs xs =
  [ (x, y)
  | (n, x) <- zip [0 ..] xs
  , y <- drop (n + 1) xs
  ]

getAntinodes :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
getAntinodes ((y1, x1), (y2, x2)) = [(y1 - dy, x1 - dx), (y2 + dy, x2 + dx)]
 where
  (dy, dx) = (y2 - y1, x2 - x1)

type Input = [[Tile]]
parser :: Parser Input
parser = do
  many $ some (try emptyTile <|> antenna) <* eol
 where
  emptyTile = char '.' >> return Empty
  antenna = Antenna <$> printChar

test :: IO (Int, Int)
test = testSolution solution

prettyPrint :: (Show a) => [[a]] -> IO ()
prettyPrint = putStrLn . join "\n" . (mconcat . (show <$>) <$>)

findIndexes2d :: (a -> Bool) -> [[a]] -> [(a, (Int, Int))]
findIndexes2d p rows = do
  (y, row) <- zip [0 ..] rows
  (x, a) <- zip [0 ..] row
  if p a then return (a, (y, x)) else []

t = groupBy ((==) `on` fst) [('a', 1), ('b', 2), ('a', 3)]
