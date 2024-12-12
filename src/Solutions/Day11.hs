{-# LANGUAGE TupleSections #-}

module Solutions.Day11 (solution, test) where

import Text.Megaparsec.Char (space)

import qualified Data.IntMap as IntMap
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many)
import Text.Megaparsec.Char.Lexer (decimal)

solution :: Solution Input Int Int
solution = Solution 11 parser part1 part2

part1 :: Input -> IO Int
part1 input = do
  return $ length $ iterate blink input !! 25

part2 :: Input -> IO Int
part2 input = do
  let a = IntMap.fromList $ (,1) <$> input
  let x = iterate blinkMap a
  let final = x !! 75
  return $ sum $ snd <$> IntMap.assocs final

blinkMap :: IntMap.IntMap Int -> IntMap.IntMap Int
blinkMap prev = IntMap.fromListWith (+) next
 where
  next = IntMap.assocs prev >>= (\(stone, count) -> (,count) <$> nextStone stone)

blink :: [Int] -> [Int]
blink a = a >>= nextStone

type Input = [Int]
parser :: Parser Input
parser = many (decimal <* space)

test :: IO (Int, Int)
test = testSolution solution

nextStone :: Int -> [Int]
nextStone 0 = [1]
nextStone n
  | even (length $ show n) =
      let digits = show n
          b = splitAt (length digits `div` 2) digits
       in read <$> [fst b, snd b]
  | otherwise = [n * 2024]
