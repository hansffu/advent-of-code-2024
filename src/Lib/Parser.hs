module Lib.Parser (Parser, parseInput, intP) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, many, parse)
import Text.Megaparsec.Char (digitChar)

type Parser = Parsec Void String

parseInput :: Parser a -> String -> String -> a
parseInput parser fileName input = case parse parser fileName input of
  Right res -> res
  Left err -> error $ errorBundlePretty err

intP :: Parser Int
intP = read <$> many digitChar
