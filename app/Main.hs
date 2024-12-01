module Main where

import Lib.Solution (Solution (day), solve)
import Solutions.Day1 (day1)

currentDay :: Int
currentDay = 1

runAll :: Bool
runAll = False

main :: IO ()
main = mapM_ run (filter (\s -> runAll || day s == currentDay) solutions)
 where
  solutions =
    [day1
    ]

run :: (Show a, Show b) => Solution a b -> IO ()
run solution = do
  putStrLn $ "Day " <> show (day solution)
  (part1, part2) <- solve solution
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2
