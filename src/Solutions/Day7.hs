{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Solutions.Day7 (solution) where

import Text.Megaparsec.Char (char, newline, string)

import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many, sepBy)
import Text.Megaparsec.Char.Lexer (decimal)

type Input = [(Int, [Int])]

solution :: Solution Input Int Int
solution = Solution 7 parser part1 part2

part1 :: Input -> IO Int
part1 input = return $ sum $ fst <$> filter check input
 where
  check :: (Int, [Int]) -> Bool
  check (expected, nums) = expected `elem` combine operators nums
  operators :: [Int -> Int -> Int]
  operators = [(+), (*)]

part2 :: Input -> IO Int
part2 input = return $ sum $ fst <$> filter check input
 where
  check :: (Int, [Int]) -> Bool
  check (expected, nums) = expected `elem` combine operators nums
  operators :: [Int -> Int -> Int]
  operators = [(+), (*), pipe]

pipe :: Int -> Int -> Int
pipe a b = read $ show b <> show a

combine :: [Int -> Int -> Int] -> [Int] -> [Int]
combine operators = go . reverse
 where
  go :: [Int] -> [Int]
  go (x : y : xs) =
    let results = go (y : xs)
        ops = ($ x) <$> operators
     in ops <*> results
  go [x] = [x]
  go [] = []

parser :: Parser Input
parser = many lineP
 where
  lineP :: Parser (Int, [Int])
  lineP = do
    res <- decimal <* string ": "
    nums <- decimal `sepBy` char ' '
    _ <- newline
    return (res, nums)

test = testSolution solution
