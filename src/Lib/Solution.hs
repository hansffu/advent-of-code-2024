module Lib.Solution (Solution (..), Part, solve, testSolution, todo) where

import Lib.Parser (Parser)
import Lib.TaskRunner (InputType (..), readInput)

type Part i o = i -> IO o

todo :: i -> IO String
todo = const $ return "TODO"

data (Show o1, Show o2) => Solution i o1 o2 = Solution
  { day :: Int
  , inputParser :: Parser i
  , part1Solution :: Part i o1
  , part2Solution :: Part i o2
  }

solve :: (Show o1, Show o2) => Solution i o1 o2 -> IO (o1, o2)
solve solution = run solution Input

testSolution :: (Show o1, Show o2) => Solution i o1 o2 -> IO (o1, o2)
testSolution solution = run solution Sample

run :: (Show o1, Show o2) => Solution i o1 o2 -> (Int -> InputType) -> IO (o1, o2)
run solution inputType = do
  let inputSpec = inputType $ day solution
  input <- readInput inputSpec $ inputParser solution
  p1 <- part1Solution solution input
  p2 <- part2Solution solution input
  return (p1, p2)
