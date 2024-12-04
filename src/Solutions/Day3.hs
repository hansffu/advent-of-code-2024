module Solutions.Day3 (solution) where

import Text.Megaparsec.Char (char, string)

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (MonadParsec (..), anySingle, many, (<|>))
import Text.Megaparsec.Char.Lexer (decimal)

solution :: Solution [Statement] Int Int
solution = Solution 3 parser part1 part2

part1 :: [Statement] -> IO Int
part1 = return . sum . (mulOrNull <$>)

part2 :: [Statement] -> IO Int
part2 = return . stateRes . foldl nextStep (DoNext 0)

data State = DoNext Int | SkipNext Int deriving (Show)

stateRes :: State -> Int
stateRes (DoNext res) = res
stateRes (SkipNext res) = res

nextStep :: State -> Statement -> State
nextStep state Do = DoNext $ stateRes state
nextStep state Dont = SkipNext $ stateRes state
nextStep (DoNext res) (Mul a b) = DoNext (res + a * b)
nextStep (SkipNext res) _ = SkipNext res

data Statement
  = Mul Int Int
  | Do
  | Dont
  deriving (Show)

mulOrNull :: Statement -> Int
mulOrNull (Mul a b) = a * b
mulOrNull _ = 0

parser :: Parser [Statement]
parser = catMaybes <$> many (try mul <|> try do' <|> try dont <|> nothing)
 where
  mul = do
    a <- string "mul(" *> decimal <* char ','
    b <- decimal <* char ')'
    return $ Just $ Mul a b
  do' = string "do()" $> Just Do
  dont = string "don't()" $> Just Dont
  nothing = anySingle $> Nothing
