{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Solutions.Day7 (solution) where

import Text.Megaparsec.Char (char, newline, space, string)

import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many, sepBy)
import Text.Megaparsec.Char.Lexer (decimal)

type Input = [(Int, [Int])]

solution :: Solution Input Int String
solution = Solution 7 parser part1 part2

part1 :: Input -> IO Int
part1 input = do
  return $ sum $ fst <$> filter check input
 where
  check :: (Int, [Int]) -> Bool
  check (expected, nums) = expected `elem` combine nums

part2 :: Input -> IO String
part2 = todo

combine :: [Int] -> [Int]
combine = go . reverse
 where
  go :: [Int] -> [Int]
  go (x : y : xs) =
    let results = go (y : xs)
        ops = ($ x) <$> operators
     in ops <*> results
  go [x] = [x]
  go [] = []

operators :: [Int -> Int -> Int]
operators = [(+), (*)]

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
