module Solutions.Day4 (solution, test) where

import Text.Megaparsec.Char (alphaNumChar, eol)

import Data.List (isPrefixOf, transpose)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many)

solution :: Solution [String] Int Int
solution = Solution 4 parser part1 part2

part1 :: [String] -> IO Int
part1 input = return $ sum $ findXmas <$> forward
 where
  rotated = transpose input
  shiftedR = transpose $ pad input
  shiftedL = transpose $ pad $ reverse input
  forward = input ++ rotated ++ shiftedR ++ shiftedL

part2 :: [String] -> IO Int
part2 input = return $ sum $ findX <$> squares input

pad :: [[Char]] -> [[Char]]
pad ss = (\(i, s) -> replicate i '.' ++ s) <$> zip [0 ..] ss

parser :: Parser [String]
parser = many $ many alphaNumChar <* eol

test :: IO (Int, Int)
test = testSolution solution

findXmas :: String -> Int
findXmas xs
  | "XMAS" `isPrefixOf` xs = 1 + findXmas (tail xs)
  | "SAMX" `isPrefixOf` xs = 1 + findXmas (tail xs)
  | null xs = 0
  | otherwise = findXmas $ tail xs

squares :: [String] -> [[String]]
squares input =
  [ take3x3 (drop x <$> drop y input)
  | y <- [0 .. length input - 3]
  , x <- [0 .. length (head input) - 3]
  ]

findX :: [String] -> Int
findX square = result
 where
  result = if (shiftedL !! 2 == "MAS" || shiftedL !! 2 == "SAM") && (shiftedR !! 2 == "MAS" || shiftedR !! 2 == "SAM") then 1 else 0
  shiftedR = transpose $ pad square
  shiftedL = transpose $ pad $ reverse square

take3x3 :: [String] -> [String]
take3x3 = take 3 . (take 3 <$>)
