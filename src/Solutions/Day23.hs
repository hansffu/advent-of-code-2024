module Solutions.Day23 (solution, test) where

import Text.Megaparsec.Char (alphaNumChar, char, eol, string)

import Data.List (isPrefixOf, nub, sort)
import Data.Map ((!))
import qualified Data.Map as M
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many)

solution :: Solution Input Int String
solution = Solution 23 parser part1 part2

part1 :: Input -> IO Int
part1 input = do
  -- print $ M.fromListWith (++) $ (\(a, b) -> (a, [b])) <$> (filter hasTComp input >>= fromTComp)
  -- print $ toMap input
  -- print ts
  -- print $ findCycle m "ta"
  -- print $ nub $ ts >>= findCycle m
  return $ length $ nub $ ts >>= findCycle m
 where
  m = toMap input
  ts = filter isT $ M.keys m

findCycle :: M.Map String [String] -> String -> [[String]]
findCycle m t = nub $ sort <$> findCycle'
 where
  findCycle' = do
    a <- m ! t
    b <- m ! a
    c <- m ! b
    if c == t
      then return [a, b, c]
      else []

part2 :: Input -> IO String
part2 = todo

hasTComp :: (String, String) -> Bool
hasTComp (a, b) = isT a || isT b

toMap :: [(String, String)] -> M.Map String [String]
toMap xs = M.fromListWith (++) (xs >>= (\(a, b) -> [(a, [b]), (b, [a])]))

fromTComp :: (String, String) -> [(String, String)]
fromTComp (a, b)
  | isT a && isT b = [(a, b), (b, a)]
  | isT a = [(a, b)]
  | otherwise = [(b, a)]

isT :: String -> Bool
isT = isPrefixOf "t"

type Input = [(String, String)]
parser :: Parser Input
parser = many lineP
 where
  lineP = do
    a <- many alphaNumChar <* char '-'
    b <- many alphaNumChar <* eol
    return (min a b, max a b)

test :: IO (Int, String)
test = testSolution solution
