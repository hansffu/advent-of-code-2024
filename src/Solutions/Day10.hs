module Solutions.Day10 (solution, test) where

import Text.Megaparsec.Char (digitChar, eol)

import Data.Array.Base ((!?))
import Data.Char (digitToInt)
import Data.List (nub)
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
  return $ sum $ length . nub . filter (\(n, _) -> n == 9) . dfs getNeighbours <$> trailHeads
 where
  arr = toArray2d input
  trailHeads = findIndexes2d (== 0) input
  getNeighbours :: Node -> [Node]
  getNeighbours (item, (y, x)) =
    filter ((== item) . pred . fst) $
      catMaybes
        [ safe2dLookup arr (y + 1, x)
        , safe2dLookup arr (y - 1, x)
        , safe2dLookup arr (y, x + 1)
        , safe2dLookup arr (y, x - 1)
        ]

-- arr !? (y + 1) >>= (\e -> e !? y + 1)

safe2dLookup :: Array2d a -> (Int, Int) -> Maybe (a, (Int, Int))
safe2dLookup arr (y, x) = do
  row <- arr !? y
  item <- row !? x
  return (item, (y, x))

part2 :: Input -> IO Int
part2 _ = return 0

type Input = [[Int]]
parser :: Parser Input
parser = many lineP
 where
  lineP = some (digitToInt <$> digitChar) <* eol

test :: IO (Int, Int)
test = testSolution solution
