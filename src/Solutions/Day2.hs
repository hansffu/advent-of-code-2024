module Solutions.Day2 (day2) where

import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec
import Text.Megaparsec.Char (hspace1, newline)
import Text.Megaparsec.Char.Lexer (decimal)

day2 :: Solution [[Int]] Int Int
day2 = Solution 2 parser part1 part2

part1 :: [[Int]] -> IO Int
part1 input = return $ length $ filter isSafe input

part2 :: [[Int]] -> IO Int
part2 input = return $ length $ filter (anySafe . combinations) input
 where
  anySafe :: [[Int]] -> Bool
  anySafe = any isSafe

parser :: Parser [[Int]]
parser = many $ decimal `sepBy` hspace1 <* newline

combinations :: [Int] -> [[Int]]
combinations xs = [let (a, b) = splitAt i xs in a ++ tail b | i <- [0 .. (length xs - 1)]]

isSafe :: [Int] -> Bool
isSafe xs = safeIncrease xs || safeDecrease xs

safeDecrease :: [Int] -> Bool
safeDecrease [_] = True
safeDecrease (x : y : xs) = x > y && x - y <= 3 && safeDecrease (y : xs)
safeDecrease _ = error "too small list"

safeIncrease :: [Int] -> Bool
safeIncrease [_] = True
safeIncrease (x : y : xs) = x < y && y - x <= 3 && safeIncrease (y : xs)
safeIncrease _ = error "too small list"
