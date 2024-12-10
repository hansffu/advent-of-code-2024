module Solutions.Day10 (solution, test) where

import Text.Megaparsec.Char (digitChar, eol)

import Data.Array.Base ((!?))
import Data.Char (digitToInt)
import Data.List (nub)
import Data.List.Extra (groupOn)
import Data.Maybe (catMaybes)
import Lib.Dfs (dfs)
import Lib.Parser (Parser)
import Lib.Solution
import Lib.Utils (Array2d, findIndexes2d, toArray2d)
import Text.Megaparsec (many, some)

solution :: Solution Input Int Int
solution = Solution 10 parser part1 part2
type Node = (Int, (Int, Int))
part1 :: Input -> IO Int
part1 input = do
  return $ sum $ length . nub . filter (\(n, _) -> n == 9) . dfs (getNeighbours arr) <$> trailHeads
 where
  arr = toArray2d input
  trailHeads = findIndexes2d (== 0) input

part2 :: Input -> IO Int
part2 input = return $ sum $ findUnique <$> trails
 where
  arr = toArray2d input
  trailHeads = findIndexes2d (== 0) input
  trails = dfs (getNeighbours arr) <$> trailHeads

findUnique :: [Node] -> Int
findUnique nodes = go $ groupOn fst nodes
 where
  go :: [[Node]] -> Int
  go [] = 0
  go [cur] = length cur
  go (cur : next : rest) = sum $ gogo <$> cur
   where
    gogo x =
      let neighbours = filter (isNeighbour x) next
       in go (neighbours : rest)

isNeighbour :: Node -> Node -> Bool
isNeighbour (_, (y, x)) (_, c) = c `elem` [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]

getNeighbours :: Array2d Int -> Node -> [Node]
getNeighbours arr (item, (y, x)) =
  filter ((== item) . pred . fst) $
    catMaybes
      [ safe2dLookup arr (y + 1, x)
      , safe2dLookup arr (y - 1, x)
      , safe2dLookup arr (y, x + 1)
      , safe2dLookup arr (y, x - 1)
      ]

type Input = [[Int]]
parser :: Parser Input
parser = many lineP
 where
  lineP = some (digitToInt <$> digitChar) <* eol

test :: IO (Int, Int)
test = testSolution solution

safe2dLookup :: Array2d a -> (Int, Int) -> Maybe (a, (Int, Int))
safe2dLookup arr (y, x) = do
  row <- arr !? y
  item <- row !? x
  return (item, (y, x))
