module Solutions.Day22 (solution, test) where

import Text.Megaparsec.Char (eol)

import Data.Bits (xor)
import qualified Data.Map as M
import GHC.Plugins (nTimes)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (some)
import Text.Megaparsec.Char.Lexer (decimal)

solution :: Solution Input Int Int
solution = Solution 22 parser part1 part2

part1 :: Input -> IO Int
part1 input = return $ sum (nTimes 2000 calculateNext <$> input)

part2 :: Input -> IO Int
part2 input = return $ maximum $ snd <$> M.toList scoresByPattern
 where
  prices = ((`mod` 10) <$>) . iterate calculateNext <$> input
  diffs = getDiffs <$> prices
  pairs = toPairs (tail <$> prices) diffs
  patterns = take 2000 . getPattern <$> pairs
  maps = toMap <$> patterns
  scoresByPattern = M.unionsWith (+) maps

type Pattern = (Int, Int, Int, Int)

toMap :: [(Pattern, Int)] -> M.Map Pattern Int
toMap = M.fromListWith (\_ x -> x)

getPattern :: [(Int, Int)] -> [(Pattern, Int)]
getPattern xs@((_, a) : (_, b) : (_, c) : (price, d) : _) = ((a, b, c, d), price) : getPattern (tail xs)
getPattern _ = []

toPairs :: [[Int]] -> [[Int]] -> [[(Int, Int)]]
toPairs [] _ = []
toPairs _ [] = []
toPairs (a : as) (b : bs) = zip a b : toPairs as bs

getDiffs :: [Int] -> [Int]
getDiffs nums = zipWith (-) (tail onePositions) onePositions
 where
  onePositions = (`mod` 10) <$> nums

calculateNext :: Int -> Int
calculateNext secretNumber =
  let a = prune $ mix secretNumber $ secretNumber * 64
      b = prune $ mix a $ a `div` 32
   in prune $ mix b $ b * 2048

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (`mod` 16777216)

type Input = [Int]
parser :: Parser Input
parser = some (decimal <* eol)

test :: IO (Int, Int)
test = testSolution solution
