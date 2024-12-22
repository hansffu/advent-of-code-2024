module Solutions.Day22 (solution, test) where

import Text.Megaparsec.Char (eol)

import Data.Bits (xor)
import GHC.Plugins (nTimes)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (some)
import Text.Megaparsec.Char.Lexer (decimal)

solution :: Solution Input Int String
solution = Solution 22 parser part1 part2

part1 :: Input -> IO Int
part1 input = return $ sum (nTimes 2000 calculateNext <$> input)

calculateNext :: Int -> Int
calculateNext secretNumber =
  let a = prune $ mix secretNumber $ secretNumber * 64
      b = prune $ mix a $ a `div` 32
   in prune $ mix b $ b * 2048

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (`mod` 16777216)

part2 :: Input -> IO String
part2 = todo

type Input = [Int]
parser :: Parser Input
parser = some (decimal <* eol)

test :: IO (Int, String)
test = testSolution solution
