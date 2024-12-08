module Solutions.Day8 (solution, test) where

import Text.Megaparsec.Char (char, eol, printChar)

import Data.Function (on)
import Data.List (groupBy, sort)
import Data.List.Utils (join)
import qualified Data.Set as S
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (MonadParsec (try), many, sepBy, (<|>))

solution :: Solution Input Int String
solution = Solution 8 parser part1 part2

part1 :: Input -> IO Int
part1 input = do
  -- prettyPrint input
  -- print antennas
  -- print $ length antennasByType
  -- print $ length antennaTypes
  -- print $ input
  -- print $ antennasByType
  -- print $ length pairs
  -- print pairs
  -- print allAntinodes
  -- print $ filter isInBounds allAntinodes
  -- print $ length (head input)
  return $ length $ S.fromList $ filter isInBounds allAntinodes
 where
  antennas = sort $ findIndexes2d isAntenna input
  -- antennaTypes = S.toList $ S.fromList $ fst <$> antennas
  -- antennasByType' = (\antennaType -> (antennaType, snd <$> filter ((== antennaType) . fst) antennas)) <$> antennaTypes
  -- antennasByType = (\antennaType -> (antennaType, snd <$> filter ((== antennaType) . fst) antennas)) <$> antennaTypes
  -- antennasByType' = groupBy ((==) `on` fst) antennas
  antennasByType = (snd <$>) <$> groupBy ((==) `on` fst) antennas
  pairs = antennasByType >>= getPairs
  isInBounds (y, x) = x >= 0 && y >= 0 && x < length (head input) && y < length input
  allAntinodes = pairs >>= getAntinodes

part2 :: Input -> IO String
part2 = todo

data Tile = Empty | Antenna Char deriving (Ord, Eq)
instance Show Tile where
  show Empty = "."
  show (Antenna c) = [c]

isAntenna :: Tile -> Bool
isAntenna (Antenna _) = True
isAntenna _ = False

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
  a <- many (try emptyTile <|> antenna) `sepBy` eol
  return $ filter (not . null) a
 where
  emptyTile = char '.' >> return Empty
  antenna = Antenna <$> printChar

test :: IO (Int, String)
test = testSolution solution

prettyPrint :: (Show a) => [[a]] -> IO ()
prettyPrint = putStrLn . join "\n" . (mconcat . (show <$>) <$>)

findIndexes2d :: (a -> Bool) -> [[a]] -> [(a, (Int, Int))]
findIndexes2d p rows = do
  (y, row) <- zip [0 ..] rows
  (x, a) <- zip [0 ..] row
  if p a then return (a, (y, x)) else []
