module Solutions.Day5 (solution, test) where

import Text.Megaparsec.Char (char, eol)

import GHC.Plugins (nTimes)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many, sepBy)
import Text.Megaparsec.Char.Lexer (decimal)

type Rule = (Int, Int)
type Update = [Int]
type Input = ([Rule], [Update])

solution :: Solution Input Int Int
solution = Solution 5 parser part1 part2

part1 :: Input -> IO Int
part1 (rules, updates) = return $ sum $ getMiddle <$> inOrder
 where
  inOrder = filter isInOrder updates
  isInOrder (x : y : xs) = notElem (y, x) rules && isInOrder (y : xs)
  isInOrder _ = True

part2 :: Input -> IO Int
part2 (rules, updates) = return $ sum $ getMiddle <$> inOrder
 where
  inOrder = (\u -> nTimes (length u) reorder u) <$> outOfOrder
  reorder (x : y : xs)
    | (y, x) `elem` rules = y : reorder (x : xs)
    | otherwise = x : reorder (y : xs)
  reorder xs = xs

  outOfOrder = filter (not . isInOrder) updates
  isInOrder (x : y : xs) = notElem (y, x) rules && isInOrder (y : xs)
  isInOrder _ = True

getMiddle :: [a] -> a
getMiddle list = list !! (length list `div` 2)

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

test :: IO (Int, Int)
test = testSolution solution
