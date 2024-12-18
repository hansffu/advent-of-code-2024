{-# LANGUAGE OverloadedRecordDot #-}

module Solutions.Day17 (solution, test) where

import Text.Megaparsec.Char (char, digitChar, eol, newline, string)

import Control.Monad.Writer (MonadWriter (..), Writer, execWriter)
import Data.Array (Array, bounds, elems, listArray, (!))
import Data.Bits (xor)
import Data.Char (digitToInt)
import Data.List (intercalate)
import GHC.Real (powImpl)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (sepBy1)
import Text.Megaparsec.Char.Lexer (decimal)

solution :: Solution Input String Int
solution = Solution 17 parser part1 part2

part1 :: Input -> IO String
part1 (initialState, instructions) = do
  let res = execWriter (runProgram instructions initialState)
  return $ intercalate "," $ show <$> res

part2 :: Input -> IO Int
part2 (initialState, instructions) = do
  -- let res = execWriter (runProgram instructions initialState{regA = 117440})
  -- print res
  -- print $ res == elems instructions

  return $ fst $ head $ filter snd $ zip [0 ..] results
 where
  check n = elems instructions == execWriter (runProgram instructions initialState{regA = n})
  results = check <$> [0 ..]

data ComputerState = ComputerState {regA :: Int, regB :: Int, regC :: Int, ip :: Int} deriving (Show)

getOpValue :: ComputerState -> Int -> Int
getOpValue _ 0 = 0
getOpValue _ 1 = 1
getOpValue _ 2 = 2
getOpValue _ 3 = 3
getOpValue s 4 = s.regA
getOpValue s 5 = s.regB
getOpValue s 6 = s.regC
getOpValue _ _ = error "invalid operand"

runProgram :: Array Int Int -> ComputerState -> Writer [Int] ComputerState
runProgram instructions s =
  if s.ip <= snd (bounds instructions)
    then do
      s' <- runCmd cmd
      runProgram instructions s'
    else return s
 where
  cmd = instructions ! s.ip
  op = instructions ! (s.ip + 1)
  combo = getOpValue s op
  runCmd :: Int -> Writer [Int] ComputerState
  runCmd 0 = return s{regA = s.regA `div` (2 `pow` combo), ip = s.ip + 2}
  runCmd 1 = return s{regB = s.regB `xor` op, ip = s.ip + 2}
  runCmd 2 = return s{regB = combo `mod` 8, ip = s.ip + 2}
  runCmd 3 =
    if s.regA == 0 || s.ip == op
      then return s{ip = s.ip + 2}
      else return s{ip = op}
  runCmd 4 = return $ s{regB = s.regB `xor` s.regC, ip = s.ip + 2}
  runCmd 5 = do
    let res = combo `mod` 8
    tell [res]
    return s{ip = s.ip + 2}
  runCmd 6 = return s{regB = s.regA `div` (2 `pow` combo), ip = s.ip + 2}
  runCmd 7 = return s{regC = s.regA `div` (2 `pow` combo), ip = s.ip + 2}
  runCmd n = error $ "inst " <> show n -- return s{ip = s.ip + 2}

pow :: (Num a, Integral b) => a -> b -> a
pow a b = if b == 0 then 1 else powImpl a b

type Input = (ComputerState, Array Int Int)
parser :: Parser Input
parser = do
  a <- string "Register A: " *> decimal <* eol
  b <- string "Register B: " *> decimal <* eol
  c <- string "Register C: " *> decimal <* eol
  _ <- newline >> string "Program: "
  instructions <- digitChar `sepBy1` char ','
  return (ComputerState{regA = a, regB = b, regC = c, ip = 0}, listArray (0, length instructions - 1) (digitToInt <$> instructions))

test :: IO (String, Int)
test = testSolution solution
