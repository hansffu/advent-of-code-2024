module Solutions.Day2 (day2) where

import Lib.Parser (Parser)
import Lib.Solution
import Lib.Utils (dropIndex)
import Text.Megaparsec
import Text.Megaparsec.Char (hspace1, newline)
import Text.Megaparsec.Char.Lexer (decimal)

day2 :: Solution [[Int]] Int Int
day2 = Solution 2 parser part1 part2

part1 :: [[Int]] -> IO Int
part1 input = return $ length $ filter isSafe' input

part2 :: [[Int]] -> IO Int
part2 input = return $ length $ filter (anySafe . combinations) input
 where
  anySafe :: [[Int]] -> Bool
  anySafe = any isSafe'

parser :: Parser [[Int]]
parser = many $ decimal `sepBy` hspace1 <* newline

combinations :: [Int] -> [[Int]]
combinations xs = flip dropIndex xs <$> [0 .. (length xs - 1)]

isSafe' :: [Int] -> Bool
isSafe' xs = isSafe (Increase <$> xs) || isSafe (Decrease <$> xs)

newtype Increase a = Increase a
newtype Decrease a = Decrease a

class SafePattern a where
  isSafeStep :: a -> a -> Bool
  isSafe :: [a] -> Bool
  isSafe (x : y : xs) = isSafeStep x y && isSafe (y : xs)
  isSafe _ = True

instance (Num a, Ord a) => SafePattern (Increase a) where
  isSafeStep (Increase x) (Increase y) = x < y && y - x <= 3

instance (Num a, Ord a) => SafePattern (Decrease a) where
  isSafeStep (Decrease x) (Decrease y) = x > y && x - y <= 3
