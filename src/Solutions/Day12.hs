module Solutions.Day12 (solution, test) where

import Text.Megaparsec.Char (alphaNumChar, eol)

import qualified Data.Array as A
import Data.List (nub)
import Lib.Array2d (Array2d, Node, getNeighbours, toArray2d)
import Lib.Dfs (dfs)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many, some)

solution :: Solution Input Int String
solution = Solution 12 parser part1 part2

part1 :: Input -> IO Int
part1 input = do
  return $ sum $ (\enc -> fencesAroundEnclosure arr enc * length enc) <$> getEnclosures arr
 where
  arr = toArray2d input

part2 :: Input -> IO String
part2 = todo

type Input = [[Char]]
parser :: Parser Input
parser = many (some alphaNumChar <* eol)

fencesAroundEnclosure :: Array2d Char -> [Node Char] -> Int
fencesAroundEnclosure arr enclosure = sum $ (\x -> 4 - length (getNeighbours (==) arr x)) <$> enclosure

findEnclosure :: Array2d Char -> Node Char -> [Node Char]
findEnclosure arr = dfs (getNeighbours (==) arr)

getEnclosures :: Array2d Char -> [[Node Char]]
getEnclosures arr = nub $ findEnclosure arr <$> cells
 where
  cells = [(c, (y, x)) | (y, rows) <- A.assocs arr, (x, c) <- A.assocs rows]

test :: IO (Int, String)
test = testSolution solution
