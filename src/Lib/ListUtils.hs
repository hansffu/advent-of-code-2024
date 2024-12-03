module Lib.ListUtils (dropIndex) where

dropIndex :: Int -> [a] -> [a]
dropIndex i xs = let (a, b) = splitAt i xs in a ++ tail b
