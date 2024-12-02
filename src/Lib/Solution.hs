module Lib.Solution (Solution (..), Part, solve, testSolution, todo) where

import Lib.Parser (Parser)
import Lib.TaskRunner (InputType (..), readInput)

type Part a = [String] -> IO a

todo :: i -> IO Int
todo = const $ return 0

data (Show a, Show b) => Solution i a b = Solution
  { day :: Int
  , inputParser :: Parser i
  , part1Solution :: i -> IO a
  , part2Solution :: i -> IO b
  }

solve :: (Show a, Show b) => Solution i a b -> IO (a, b)
solve solution = run solution Input

testSolution :: (Show a, Show b) => Solution i a b -> IO (a, b)
testSolution solution = run solution Sample

run :: (Show a, Show b) => Solution i a b -> (Int -> InputType) -> IO (a, b)
run solution inputType = do
  let inputSpec = inputType $ day solution
  input <- readInput inputSpec $ inputParser solution
  p1 <- part1Solution solution input
  p2 <- part2Solution solution input
  return (p1, p2)
