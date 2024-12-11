module Solutions.Day11 (solution, test) where

import Text.Megaparsec.Char (space)

import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many)
import Text.Megaparsec.Char.Lexer (decimal)

solution :: Solution Input Int String
solution = Solution 11 parser part1 part2

part1 :: Input -> IO Int
part1 input = return $ length $ iterate blink input !! 25

blink :: [Int] -> [Int]
blink a = a >>= go
 where
  go :: Int -> [Int]
  go 0 = [1]
  go n
    | even (length $ show n) =
        let digits = show n
            b = splitAt (length digits `div` 2) digits
         in read <$> [fst b, snd b]
    | otherwise = [n * 2024]

part2 :: Input -> IO String
part2 = todo

type Input = [Int]
parser :: Parser Input
parser = many (decimal <* space)

test :: IO (Int, String)
test = testSolution solution
