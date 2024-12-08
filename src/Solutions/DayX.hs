module Solutions.DayX (solution, test) where

import Text.Megaparsec.Char (string)

import Lib.Parser (Parser)
import Lib.Solution

solution :: Solution Input String String
solution = Solution 0 parser part1 part2

part1 :: Input -> IO String
part1 = todo

part2 :: Input -> IO String
part2 = todo

type Input = String
parser :: Parser Input
parser = string "todo"

test :: IO (String, String)
test = testSolution solution
