module Lib.Utils (
  readInt,
  juxt,
  debug,
  debug',
  prettyPrint,
  index2d,
  applyT2,
  findIndexes2d,
) where

import Data.List.Utils (join)
import Debug.Trace (traceShow)

readInt :: String -> Int
readInt = read

juxt :: (Applicative f) => f (a -> b) -> a -> f b
juxt fns = (fns <*>) . pure

debug :: (Show b) => b -> b
debug x = traceShow x x

debug' :: (Show b) => String -> b -> b
debug' label x = traceShow (label <> ": " <> show x) x

prettyPrint :: (Show a) => [a] -> IO ()
prettyPrint = putStrLn . join "\n" . map show

index2d :: [[a]] -> [[((Int, Int), a)]]
index2d = zipWith (\i -> zipWith (\j x -> ((i, j), x)) [0 ..]) [0 ..]

findIndexes2d :: (a -> Bool) -> [[a]] -> [(a, (Int, Int))]
findIndexes2d p rows = do
  (y, row) <- zip [0 ..] rows
  (x, a) <- zip [0 ..] row
  if p a then return (a, (y, x)) else []

applyT2 :: (t -> a, t -> b) -> t -> (a, b)
applyT2 (f1, f2) b = (f1 b, f2 b)
