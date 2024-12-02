module Main where

import Lib.Solution (Solution (day), solve)

-- import Solutions.Day1 (day1)
import Solutions.Day2 (day2)

-- currentDay :: Int
-- currentDay = 2

runAll :: Bool
runAll = False

main :: IO ()
main = run day2

-- main :: IO ()
-- main = mapM_ run (filter (\s -> runAll || day s == currentDay) solutions)
--  where
--   solutions :: forall i a b. Solution i a b => [Solution i a b]
--   solutions =
--     [ day1
--     , day2
--     ]

run :: (Show a, Show b) => Solution i a b -> IO ()
run solution = do
  putStrLn $ "Day " <> show (day solution)
  (part1, part2) <- solve solution
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2
