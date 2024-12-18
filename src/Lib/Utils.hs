module Lib.Utils (
  readInt,
  juxt,
  debug,
  debug',
  prettyPrint,
  prettyPrintS,
  index2d,
  applyT2,
  wrapSucc,
  wrapPred,
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

prettyPrintS :: (Show a) => [[a]] -> IO ()
prettyPrintS = putStrLn . join "\n" . map (mconcat . (show <$>))

index2d :: [[a]] -> [[((Int, Int), a)]]
index2d = zipWith (\i -> zipWith (\j x -> ((i, j), x)) [0 ..]) [0 ..]

applyT2 :: (t -> a, t -> b) -> t -> (a, b)
applyT2 (f1, f2) b = (f1 b, f2 b)

wrapSucc :: (Eq a, Enum a, Bounded a) => a -> a
wrapSucc x
  | x == maxBound = minBound
  | otherwise = succ x

wrapPred :: (Eq a, Enum a, Bounded a) => a -> a
wrapPred x
  | x == minBound = maxBound
  | otherwise = pred x
