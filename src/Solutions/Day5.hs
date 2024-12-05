module Solutions.Day5 (solution) where

import Text.Megaparsec.Char (char, eol)

import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many, sepBy)
import Text.Megaparsec.Char.Lexer (decimal)

type Rule = (Int, Int)
type Update = [Int]
type Input = ([Rule], [Update])

solution :: Solution Input String String
solution = Solution 5 parser part1 part2

part1 :: Input -> IO String
part1 (rules, updates) =
  return $ show $ sum $ getMiddle <$> inOrder
 where
  inOrder = filter isInOrder updates
  getMiddle list = list !! (length list `div` 2)
  isInOrder (x : y : xs) = notElem (y, x) rules && isInOrder (y : xs)
  isInOrder _ = True

part2 :: Input -> IO String
part2 = todo

parser :: Parser Input
parser = do
  rules <- many ruleP
  pageNumbers <- many ((decimal `sepBy` char ',') <* eol)
  return (rules, tail pageNumbers)
 where
  ruleP = do
    a <- decimal <* char '|'
    b <- decimal <* eol
    return (a, b)

test = testSolution solution
