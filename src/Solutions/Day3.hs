module Solutions.Day3 (solution) where

import Text.Megaparsec.Char (char, string)

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (MonadParsec (..), anySingle, many, (<|>))
import Text.Megaparsec.Char.Lexer (decimal)

solution :: Solution [Mul] Int String
solution = Solution 3 parser part1 part2

part1 :: [Mul] -> IO Int
part1 input =
  return $ sum $ mulMul <$> input

part2 :: [Mul] -> IO String
part2 = todo

type Mul = (Int, Int)
mulMul :: Mul -> Int
mulMul (a, b) = a * b

parser :: Parser [Mul]
parser = catMaybes <$> many (try mul <|> nothing)
 where
  mul = do
    a <- string "mul(" *> decimal <* char ','
    b <- decimal <* char ')'
    return $ Just (a, b)
  nothing = anySingle $> Nothing
