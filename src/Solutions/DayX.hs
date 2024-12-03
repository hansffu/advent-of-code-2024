module Solutions.DayX (solution) where

import Text.Megaparsec.Char (string)

import Lib.Parser (Parser)
import Lib.Solution

solution :: Solution String String String
solution = Solution 0 parser part1 part2

part1 :: String -> IO String
part1 = todo

part2 :: String -> IO String
part2 = todo

parser :: Parser String
parser = string "todo"
