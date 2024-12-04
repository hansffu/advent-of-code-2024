module Solutions.Day4 (solution, test) where

import Text.Megaparsec.Char (alphaNumChar, eol)

import Data.Array (Ix (index))
import Data.List (isPrefixOf, transpose)
import Lib.Parser (Parser)
import Lib.Solution
import Lib.Utils (index2d)
import Text.Megaparsec (many)

solution :: Solution [String] Int String
solution = Solution 4 parser part1 part2

part1 :: [String] -> IO Int
part1 input = do
  putStrLn (unlines input)
  putStrLn (unlines $ transpose input)
  putStrLn (unlines shiftedR)
  putStrLn (unlines shiftedL)
  return $ sum $ findXmas <$> forward
 where
  pad ss = (\(i, s) -> replicate i '.' ++ s) <$> zip [0 ..] ss
  rotated = transpose input
  shiftedR = transpose $ pad input
  shiftedL = transpose $ pad $ reverse input
  forward = input ++ rotated ++ shiftedR ++ shiftedL

part2 :: [String] -> IO String
part2 = todo

parser :: Parser [String]
parser = many $ many alphaNumChar <* eol

test :: IO (Int, String)
test = testSolution solution

findXmas :: String -> Int
findXmas xs
  | "XMAS" `isPrefixOf` xs = 1 + findXmas (tail xs)
  | "SAMX" `isPrefixOf` xs = 1 + findXmas (tail xs)
  | null xs = 0
  | otherwise = findXmas $ tail xs
