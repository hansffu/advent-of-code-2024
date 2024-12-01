{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Solutions.Day1 (day1) where

import Data.IntMap (findWithDefault, fromListWith)
import Data.List (sort, transpose)
import Lib.Solution
import Lib.Utils (readInt)

day1 :: Solution Int Int
day1 = Solution 1 part1 part2

part1 :: [String] -> IO Int
part1 rows =
  return $ sum $ zipWith (\a b -> abs (a - b)) sortedAs sortedBs
 where
  [as, bs] = transpose $ words <$> rows
  sortedAs = sort $ readInt <$> as
  sortedBs = sort $ readInt <$> bs

part2 :: [String] -> IO Int
part2 rows = do
  return $ sum $ (\a -> a * findWithDefault 0 a counts) <$> as
 where
  [as, bs] = transpose $ (readInt <$>) . words <$> rows
  counts = fromListWith (+) $ (,1) <$> bs
