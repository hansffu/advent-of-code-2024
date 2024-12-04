module Main where

import Lib.Solution (Solution (day), solve)

import Solutions.Day4 (solution)

main :: IO ()
main = do
  putStrLn $ "Day " <> show (day solution)
  (part1, part2) <- solve solution
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2
