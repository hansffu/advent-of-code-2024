module Solutions.DayX (dayX) where

import Text.Megaparsec.Char (string)

import Lib.Parser (Parser)
import Lib.Solution

dayX :: Solution String Int Int
dayX = Solution 1 parser part1 part2

part1 :: String -> IO Int
part1 = todo

part2 :: String -> IO Int
part2 = todo

parser :: Parser String
parser = string "todo"
