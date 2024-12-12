module Solutions.Day12 (solution, test) where

import Text.Megaparsec.Char (alphaNumChar, eol)

import Data.Array.Base ((!?))
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Tuple (swap)
import Lib.Dfs (dfs)
import Lib.Parser (Parser)
import Lib.Solution
import Lib.Utils (Array2d, index2d, toArray2d)
import Text.Megaparsec (many, some)

solution :: Solution Input Int String
solution = Solution 12 parser part1 part2

part1 :: Input -> IO Int
part1 input = do
  return $ sum $ (\enc -> fencesAroundEnclosure arr enc * length enc) <$> S.toList enclosures
 where
  arr = toArray2d input
  cells = swap <$> concat (index2d input)
  enclosures = S.fromList $ findEnclosure arr <$> cells

part2 :: Input -> IO String
part2 = todo

type Input = [[Char]]
parser :: Parser Input
parser = many (some alphaNumChar <* eol)

type Node a = (a, (Int, Int))

fencesAroundEnclosure :: Array2d Char -> [Node Char] -> Int
fencesAroundEnclosure arr enclosure = sum $ (\x -> 4 - length (getNeighbours (==) arr x)) <$> enclosure

findEnclosure :: Array2d Char -> Node Char -> [Node Char]
findEnclosure arr = dfs (getNeighbours (==) arr)

getNeighbours :: (Char -> Char -> Bool) -> Array2d Char -> Node Char -> [Node Char]
getNeighbours predicate arr (item, (y, x)) =
  filter (predicate item . fst) $
    catMaybes
      [ safe2dLookup arr (y + 1, x)
      , safe2dLookup arr (y - 1, x)
      , safe2dLookup arr (y, x + 1)
      , safe2dLookup arr (y, x - 1)
      ]
test :: IO (Int, String)
test = testSolution solution

safe2dLookup :: Array2d a -> (Int, Int) -> Maybe (a, (Int, Int))
safe2dLookup arr (y, x) = do
  row <- arr !? y
  item <- row !? x
  return (item, (y, x))
