{-# LANGUAGE TupleSections #-}

module Solutions.Day1 (day1) where

import Data.IntMap (findWithDefault, fromListWith)
import Data.List (sort)
import Lib.Parser (Parser, intP)
import Lib.Solution (Solution (..))
import Text.Megaparsec (many)
import Text.Megaparsec.Char (eol, space1)

day1 :: Solution [(Int, Int)] Int Int
day1 = Solution 1 parser part1 part2

part1 :: [(Int, Int)] -> IO Int
part1 rows = return $ sum $ zipWith (\a b -> abs (a - b)) (sort $ fst <$> rows) (sort $ snd <$> rows)

part2 :: [(Int, Int)] -> IO Int
part2 rows = return $ sum $ (\a -> a * findWithDefault 0 a counts) . fst <$> rows
 where
  counts = fromListWith (+) $ (,1) <$> (snd <$> rows)

parser :: Parser [(Int, Int)]
parser = many rowP
 where
  rowP :: Parser (Int, Int)
  rowP = do
    a <- intP <* space1
    b <- intP <* eol
    return (a, b)
